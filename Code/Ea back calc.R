rm(list=ls())
setwd("TMY data")

library(car)
library(datasets)
library(readxl)
library(ggplot2)

all_content = readLines("AZ.csv")
q1 = read.csv(textConnection(all_content), header = TRUE, stringsAsFactors = FALSE)
q1=q1[complete.cases(q1),]

all_content2 = readLines("NY.csv")
q2 = read.csv(textConnection(all_content2), header = TRUE, stringsAsFactors = FALSE)
q2=q2[complete.cases(q2),]

##Go through each row and determine if a value is zero
row_sub = apply(q1, 1, function(row) all(row !=0 ))
##Subset as usual
q1=q1[row_sub,]

##Go through each row and determine if a value is zero
row_sub = apply(q2, 1, function(row) all(row !=0 ))
##Subset as usual
q2=q2[row_sub,]

# Separate the weather data for different parameters

q1_NOCT = q1[,c(3)]
q1_RH = q1[,c(4)]

# Separate the weather data for different parameters

q2_NOCT = q2[,c(3)]
q2_RH = q2[,c(4)]

# Declare the variables
Arrhenius <- array()
Mod_Arrhenius <- array()
Ea=0.1;
A=4.5;
n=0.6; # for UV
m= -0.6; # for RH
Rd = 0; # Degrdation rate (Rd_AZ = 0.43, Rd_NY = 0.59)
AF <- array();
a = length(q1_RH)

#Section 1---------------------------------------------------------------------------------------
for (j in 1:15000)
  if ((Rd > 2.40) & (Rd < 2.45))
  {
    break('Optimum Ea is found')
  } else
  {
    Ea = Ea + 0.0001;
    i=1;
    for(i in 1:a) #AZ = 4977, NY = 4788, CO = 5118, CA = 5069;
      #Arrhenius[i] = A*(exp(-Ea*11605*(1/AZ_Temp[i])));
      AF[i] = exp(-Ea*11605*((1/q1_NOCT[i]) - (1/q2_NOCT[i]))) * ((q1_RH[i]/100)/(q2_RH[i]/100))^(m)
    #AF[i] = exp(-Ea*11605*((1/358.15) - (1/q1_NOCT[i])))  * ((0.85)/(q1_RH[i]/100))^(m)
    
    #AF[i] = exp(-Ea*11605*((1/AZ_Temp[i]) - (1/NY_Temp[i]))) * ((AZ_RH[i]/100)/(NY_RH[i]/100))^(m)
    i=i+1;
    end
    
    #Rd = sum(Arrhenius, na.rm=TRUE);
    Rd = mean(AF, na.rm=TRUE);
  }
j = j+1;
end


# sprintf("The Acceleration Factor (AF) = %f", mean(AF_real, na.rm=TRUE))
sprintf("The Estimated Activation Energy (Ea) = %f", Ea)
#sprintf("The Rate of Degradation (Rd) = %f", Rd)
