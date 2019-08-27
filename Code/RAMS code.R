
rm(list = ls())
#Home Directory
#setwd("C:/Users/Arun/Google Drive/Ph.D/RAMS/Code")

#PRL Directory: 
setwd("C:/Users/abalas18/Google Drive/Ph.D/RAMS/Code")


#install.packages("readxl")
library(car)
# library(data.table)
library(datasets)
library(readxl)
#library(ggplot2)
q=read_xlsx("Data1.xlsx")
q1=q[,c(2,4,5,6,8,9,10,11,12,13)]
q2=q[,c(11,13,15)]
q1=q1[complete.cases(q1),]
#q1= q1[q1[,1] <= -5,]
q2=q2[complete.cases(q2),]
b1=(q1$`1/NOCT`) * 11605
b2=q1$`ln(RH)`

fit1 <- lm(q1$`ln(y1)` ~  q1$`1/NOCT` , data=q1)
#y.lm <- lm(log(ozone$O3)~bs(ozone$temp)+bs(ozone$wind))

fit2 <- lm(q2$`ln(y2)` ~ q2$`1/T1`+ q2$`ln(RH1)`, data=q2)
summary(fit1)
temp = 0.002968
y = exp(20.8588 - (6868.1742* temp))
y_h = exp(22.8124 - (7506.43859 * temp))
y_l = exp(18.905 - (6229.9098 * temp))
summary(fit2)
plot(q1$`1/T`,q1$`ln(y)`, ylab="ln(%Isc Degradation per hour)", xlab="1/T")
abline(-7840.631, 22)
# Other useful functions 
coefficients(fit1) # model coefficients
confint(fit1, level=0.95) # 90% CI for model parameters 
fitted(fit1) # predicted values
residuals(fit1) # residuals
anova(fit1) # Anova table 
# vcov(fit) # covariance matrix for model parameters 
influence(fit1) # regression diagnostics
# diagnostic plots 
fit1_res = resid(fit1)
plot(q1$`1/NOCT`, fit1_res, 
     ylab="Residuals", xlab="Waiting Time", 
     main="Old Faithful Eruptions") 
abline(0, 0) 

layout(matrix(c(1,2),2,2)) # optional 4 graphs/page 
#plot(fit1)
plot(q1$`1/T`,q1$`ln(y)`, col=4,data=q1)
plot(q2$`1/T1`,q2$`ln(y1)`, col=4,data=q2)
# plot(q$bike_3h_ago,q$Bikes, pch=16, ylab = "Bikes ", cex.lab = 1.3, col = "red")
# 
# abline(lm(q$Bikes ~ q$bike_3h_ago), col = "blue")
# plot(fit,Bikes,type='l',col='red',main='Linear relationship')
# plot(fit)

# b2 <- q$bike_3h_ago^2
# quadratic.model <-lm(Bikes ~ bike_3h_ago + b2, data=q)
# summary(quadratic.model)
