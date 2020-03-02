#analyze seasonality effects in regressions and correct for them

#read in Starbucks sales data from 2005 to 2019
sbux = read.csv("sbuxSales.csv", header = TRUE, sep = ",")
sbux$Quarter = as.Date(sbux$Quarter, "%m/%d/%Y")


par(mfrow=c(2,2))
plot(sbux$Quarter,sbux$Sales, type = "l")

##we see that Starbucks sales exhibit a seasonal effect every March

result_ = rep(0, length(sbux$Sales))

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

sbux$salesLag = c(sbux$sales[2:(length(sbux$sales))], 0)
sbux$salesLagTwo = c(sbux$sales[3:(length(sbux$sales))], 0, 0)

sbux$salesDiff = diffFunction(sbux$sales, result_)
sbux$salesDiffLag = c(0, sbux$salesDiff[1:(length(sbux$Sales)-1)])
plot(sbux$salesDiffLag, type = "l")

#fit an AR(1) model on first-differenced log of sales
sbuxDataA = sbux[1:(length(sbux$sales)-2),]
logModelA = lm(log(sales/salesLag) ~ log(salesLag/salesLagTwo), data = sbuxDataA)
summary(logModelA)

acf(logModelA$residuals)
pacf(logModelA$residuals)

#note the significant autocorrelation in lags 1 and 4
#in this model, the 4th autocorrelation captures the seasonality since
#our data is quarterly

#let's fit an AR(1) model with a Seasonal Lag on first-differenced log of sales to capture
#the seasonality piece
#because this data is quarterly, we use the difference between lags 3 and 4.

sbux$salesLagThree = c(sbux$sales[4:(length(sbux$sales))], 0, 0, 0)
sbux$salesLagFour = c(sbux$sales[5:(length(sbux$sales))], 0, 0, 0, 0)

sbuxDataB = sbux[1:(length(sbux$sales)-4),]
logModelB = lm(log(sales/salesLag) ~ log(salesLag/salesLagTwo)
                                  + log(salesLagThree/salesLagFour),
              data = sbuxDataB)
summary(logModelB)

acf(logModelB$residuals)
pacf(logModelB$residuals)

#we have significantly reduced the autocorrelation associated with the 4-th lag
