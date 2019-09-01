#########
# Acceleration Factor Calculation

#set working directory: 
setwd("directory")

library(car)
library(datasets)
library(readxl)
library(ggplot2)

# Read the file from 2nd row
all_content = readLines("Weather data.csv")
skip_second = all_content[-1]
dat = read.csv(textConnection(skip_second), header = TRUE, stringsAsFactors = FALSE)


# Separate the weather data for different states
AZ=dat[,c(1:4)]
NY=dat[,c(6:9)]
CO=dat[,c(11:14)]
CA=dat[,c(16:19)]

##Go through each row and determine if a value is zero
row_sub = apply(AZ, 1, function(row) all(row !=0 ))
##Subset as usual
AZ=AZ[row_sub,]

##Go through each row and determine if a value is zero
row_sub = apply(NY, 1, function(row) all(row !=0 ))
##Subset as usual
NY=NY[row_sub,]

##Go through each row and determine if a value is zero
row_sub = apply(CO, 1, function(row) all(row !=0 ))
##Subset as usual
CO=CO[row_sub,]

##Go through each row and determine if a value is zero
row_sub = apply(CA, 1, function(row) all(row !=0 ))
##Subset as usual
CA=CA[row_sub,]


# Separate the weather data for different parameters
AZ_Temp = AZ[,c(1)]
NY_Temp = NY[,c(1)]
AZ_UV = AZ[,c(2)]
NY_UV = NY[,c(2)]

# Declare the variables
Arrhenius <- array()
Mod_Arrhenius <- array()
Ea=0.27;

#A=4.30;
n=0.60; # for UV
m= 2.2; # for RH
Rd = 0; # Degrdation rate (Rd_AZ = 0.43, Rd_NY = 0.59)

for(i in 1:4977) #AZ = 4977, NY = 4788, CO = 5118, CA = 5069;    180/AZ_UV[i]
  Mod_Arrhenius[i] = exp(-Ea*11605*(1/350.75 - 1/AZ_Temp[i])) * ((2860/(20*AZ_UV[i]))^(n));
i=i+1;
end

AF <- array()
AF_real <- array()
AF= Mod_Arrhenius

for(i in 1:4977)
  if (AF[i] <= 500 & AF[i]>0)
    AF_real[i] = AF[i]
end
end

sprintf("The Acceleration Factor (AF) = %f", mean(AF_real, na.rm=TRUE))
AF_Final = mean(AF_real, na.rm=TRUE);

#Isc_acc = 0.004367;
Isc_acc = 0.00268; # (% per hour)
#Isc_AZ = 0.43 #(% per year)

# We Know that AF_Final= Isc_acc/ Isc_AZ;
Isc_AZ = Isc_acc * 4900 / AF_Final

sprintf("Predicted Isc_AZ = %f", Isc_AZ)
# Interval : 0.301 - 0.559 % per year

###############

#Color Plots for degrdation data for field model
#par(mfrow=c(1,1))
# Acc_Factor=AF_real
# plot.ts(Acc_Factor, col=4)
#  plot.ts(Acc_Factor, col=4, 
#       xlab="Time (hrs)", ylab="Acceleration Factor",
      # xlim=c(0, 5000), ylim=c(0, 500))
# #par(mfrow=c(2,2))
# plot.ts(AZ_Temp, col=6, 
#         xlab="Time (hrs)", ylab="AZ_Module temp (K)",
#         xlim=c(0, 5000), ylim=c(250, 350))
# plot.ts(NY_Temp, col=2, 
#         xlab="Time (hrs)", ylab="NY_Module temp (K)",
#         xlim=c(0, 5000), ylim=c(250, 350))
# plot.ts(AZ_UV, col=3, 
#         xlab="Time (hrs)", ylab="AZ_UV irradiance (W/m2)",
#         xlim=c(0, 5000), ylim=c(0, 55))
# plot.ts(NY_UV, col=4, 
#         xlab="Time (hrs)", ylab="NY_UV irradiance (W/m2)",
#         xlim=c(0, 5000), ylim=c(0, 55))
