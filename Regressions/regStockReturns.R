#read in data downloaded from Yahoo
FTSPC = read.csv("FSPTX.csv", header = TRUE, sep = ",") #don't recall ;)
GSPC = read.csv("GSPC.csv", header = TRUE, sep = ",") #S&P 500
RUSS = read.csv("RUSS.csv", header = TRUE, sep = ",") #Russell 2000

par(mfrow=c(3,1))

plot(FTSPC$Close)
plot(GSPC$Close)
plot(RUSS$Close)

stockData = data.frame(sp = GSPC$Close, rus = RUSS$Close[1:2551], other = FTSPC$Close)

#function to compute returns - QuantMod provides better ways to handle this
retFunction = function (value_, result_) {
  for (i in 1:length(value_)) {
    if (i == 1) {
      result_[i] = 0
    } else {
      result_[i] = (value_[i] - value_[i-1])/value_[i-1]
    }
  }
  return(result_)
}

#initialize stock return vector to be used by the retFunction
stockRet = rep(0.00, length(stockData$sp))

par(mfrow=c(1,1))
plot(retFunction (stockData$sp, stockRet))
par(mfrow=c(3,1))

stockRetData = data.frame(
  spRet = retFunction (stockData$sp, stockRet),
  rusRet = retFunction (stockData$rus, stockRet),
  otherRet = retFunction(stockData$other, stockRet)
)

#perform multiple regression of otherRet on SP and Russell

stockModel = lm(formula = otherRet ~ spRet*rusRet, data = stockRetData)
stockModelSummary = summary(stockModel)
stockModelAnova = anova(stockModel)


#We reject the model because:
#1. the coefficients are not significant at the 5% level
#2. the F-statistic of 1.038 is too small
#3. the p-value of the entire model, 0.3744, is too high.

#MSR = Mean Regression Sum of Squares = SSreg/k
#MSE = Mean Squared Error = SSres/(n-k-1)
#F-Statistic = MSR/MSE

#adjusted Rsq = 1-((n-1)/(n-k-1))*(1-Rsq)

adjR = 1-((2551-1)/(2551-3-1))*(1-0.001222)




