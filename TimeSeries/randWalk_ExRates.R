#Random Walk models
#of the form X(t) = X(t-1) + e(t)
#assumptions: e(t) is N(0,var), where var(e) = E(e*e) = E(e^2) is CONSTANT
#also assume that cov(e(t), e(s)) for t <> s
#it is also an example of an AR(1) model with b0 = 0
#the best forecast of future exchange rates is the current exchange rate.
#an example of a TS that is a random walk is exchange rates
#continuing analysis on the USD/Swiss exchange rate

plot(exRate$value)

#note that we cannot use standard regression methods for Random walks since
#they violate stationarity of variance and mean
#to perform regressions, we can difference the time series

#create a function to difference a series
diffFunction = function (value_, result_) {
  for (i in 1:length(value_)) {
    if (i == 1) {
      result_[i] = 0
    } else {
      result_[i] = (value_[i] - value_[i-1])
    }
  }
  return(result_)
}

exRateDiff = rep(0, length(exRate$value)) #initialize vector
exRate$diff = diffFunction(exRate$value, exRateDiff)
exRate$diffLag = diffFunction(exRate$valueLag, exRateDiff)

plot(exRate$diff)

#convert the exRate data frame to a time series object
exRateTS = ts(exRate)
plot(exRateTS) #plotting a time series object will always give Time on the X axis versus all other variables
par(mfrow=c(2,1))
acf(exRateTS[,2]) #plot the autocorrelations of lagged variables
pacf(exRateTS[,2]) #plot the partial autocorrelations

plot(exRateTS[,6])


tsModel = lm(valueLag ~ value, data = exRate)
summary(tsModel)

#fit first-different TS model
tsDiffModel = lm(diffLag ~ diff, data = exRate)
summary(tsDiffModel)

##for a random walk model, the coefficients should not be significantly different from 0
##therefore, we can set our H0 = coefficients are not 0
##and H1 = coefficients are 0
##then we proceed with our normal tests
##we see that we reject the null in the tsDiffModel 

#compare the models
modelCompare = list(summary(tsModel), summary(tsDiffModel))
print(modelCompare)
