#USD/Swiss Franc Exchange Rate Analysis
setwd("/Users/Kigen/Documents/r")

#read in data (downloaded from MacroTrends)
UsdSwiss = read.csv("UsdSwiss.csv", header = TRUE, sep = ",")
str(UsdSwiss)  #we notice that date is read in as factor
UsdSwiss$date = as.Date(UsdSwiss$date, "%m/%d/%y") #convert to factor to date format


#plot data to understand pattern
par(mfrow=c(2,1))
plot(UsdSwiss)
plot(1/UsdSwiss$value)

#compute monthly averages of exchange rate and store in a new dataframe
UsdSwiss$date = as.Date(UsdSwiss$date, "%m/%d/%y")
str(UsdSwiss)
plot(UsdSwiss)

#suppose we are interested in data from 2010 and onwards
exRate = subset(UsdSwiss, date >= as.Date("2010-01-01"), select = c(date, value))
par(mfrow=c(1,1))
plot(exRate)

length(exRate$value)
exRate$valueLag = c(exRate$value[2:length(exRate$value)], 0)
exRate = exRate[1:2734,]
exRate$t = 1:2734

#time series regression

#linear model
linMod = lm(formula = value ~ t, data = exRate)

##linear models generally not good for economic data
##this model shows an insignificant t-value for t as an independent variable
##the R-Squared is also a meager 6%. We will reject the model.

#predict using the model anyways
predExRate = predict(linMod, exRate)
par(mfrow=c(2,1))
plot(exRate$value[1:200], col = "red")
plot(predExRate[1:200], col = "blue")
#definitely useless.


tsModel = lm(formula = value ~ valueLag, data = exRate)
tsModel
par(mfrow=c(1,1))
plot(tsModel$residuals)

##t value is very high, hence we can reject the Null Hypothesis that
##the value of the slope coefficient is close to or is 0


#let's fit a log-linear model instead of the form y = exp(bo + b1*t)
#so ln y = bo + b1*t
#exponential growth at a constant rate

exRate$valueExp = log(exRate$value)
plot(exRate$valueExp)
loglinMod = lm(exRate$valueExp ~ t, data = exRate)
summary(loglinMod)

par(mfrow=c(2,1))
plot(linMod$residuals)
plot(loglinMod$residuals)

anova(linMod)
anova(loglinMod)

##note that the F-value has increased, meaning the overall fit of the model has improved
##we see that SSreg and SSres are actually higher now, meaning the previous model
##understated errors

#check for variance-covariance

varVector= rep(12,13) #initiaize vector to store variances

for (i in 1:13) {
  varVector[i] = var(exRate$value[(i*200-199):(i*200)])
}

par(mfrow=c(1,1))
plot(varVector, type = "h", col = "blue")


  
