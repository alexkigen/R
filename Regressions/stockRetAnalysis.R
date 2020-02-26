getwd()
setwd("C:/Users/akigen/Desktop/RD/R")

#read in index values

GSPC = read.csv("GSPC.csv", header = TRUE, sep = ",") #S&P 500
RUSS = read.csv("RUSS.csv", header = TRUE, sep = ",") #Russell 2000
LargeCap = read.csv("LargeCap.csv", header = TRUE, sep = ",") #Wilshere Large Cap
SmallCap = read.csv("SmallCap.csv", header = TRUE, sep = ",") #Wilshere Small Cap

#a proxy for a large cap stock
msft = read.csv("MSFT.csv", header = TRUE, sep = ",") #Wilshere Large Cap
#a proxy for a small cap stock
tivo = read.csv("TIVO.csv", header = TRUE, sep = ",") #Wilshere Large Cap

#organize plotting window
par(mfrow = c(3, 2))

#plot separately for a quick view
plot(GSPC$Close)
plot(RUSS$Close)
plot(LargeCap$Close)
plot(msft$Close)
plot(SmallCap$Close)
plot(tivo$Close)

#this chart reveals the downward spiral of Tivo and roughly exponential growth for MSFT.
#they may therefore not be the best for this example

#create dataframe with closing values of all four
stockData = data.frame(sp = GSPC$Close,
                       rus = RUSS$Close,
                       largeCap = LargeCap$Close,
                       smallCap = SmallCap$Close,
                       msft = msft$Close,
                       tivo = tivo$Close)

#function to compute returns - QuantMod provides better ways to handle this
retFunction = function(value_, result_) {
    for (i in 1:length(value_)) {
        if (i == 1) {
            result_[i] = 0
        } else {
            result_[i] = (value_[i] - value_[i - 1]) / value_[i - 1]
        }
    }
    return(result_)
}

#initialize stock return vector to be used by the retFunction
stockRet = rep(0.00, length(stockData$sp))

stockRetData = data.frame(
  spRet = retFunction(stockData$sp, stockRet),
  rusRet = retFunction(stockData$rus, stockRet),
  lcRet = retFunction(stockData$largeCap, stockRet),
  scRet = retFunction(stockData$smallCap, stockRet),
  msftRet = retFunction(stockData$msft, stockRet),
  tivoRet = retFunction(stockData$tivo, stockRet)
)

#determine and display the covariance correlation matrix of returns
CovCorrRet = list(Covariance = cov(stockRetData), Correlation = cor(stockRetData))
print(CovCorrRet)

##there is high correlation between S&P and Large Cap;
##therefore, we should avoid using these two in a regression together

#let's perform a linear regression between Microsoft and S&P

msftModel = lm(msftRet ~ spRet, data = stockRetData)
summary(msftModel)

#let's add large cap as a second variable to the model above

msftModelexpanded = lm(msftRet ~ spRet + lcRet, data = stockRetData)
summary(msftModelexpanded)

##we note that the R-Squared has not improved from the addition of Large Cap
##furthermore, none of the variables has a significant p-value
##the extra highly correlated variable is taking away significance

#let's repeat the process for TIVO and use S&P first

tivoModel = lm(tivoRet ~ spRet, data = stockRetData)
summary(tivoModel)
##the spRet variable is significant in explaining Tivo returns but the intercept isn't

#add Small Cap returns to model

tivoModelexpanded = lm(tivoRet ~ spRet + scRet, data = stockRetData)
summary(tivoModelexpanded)

##R-Squared in this case grows (albeit from 13% to 17%)
##Adjusted R-Squared also increases, so the increase is not purely from the addition of
##a new variable, a common phenomenon in multiple regressions

#now let's regress Microsoft returns against Large Cap and Small Cap

msftModel = lm(msftRet ~ lcRet + scRet, data = stockRetData)
summary(msftModel)

##the R-Squared increases, but only marginally
##the intercept is still not significant, suggesting we may need to add variables
##or adjust model fit or it is simply meaningless. Article below is useful in expanding the discussion on
##interpreting intercepts
##https://blog.minitab.com/blog/adventures-in-statistics-2/regression-analysis-how-to-interpret-the-constant-y-intercept

#we could also fit an exponential model

#create logged dataframe
stockRetDataExp = as.data.frame(sapply(log(stockData), retFunction, result_ = stockRet))

msftModelexp = lm(msft ~ largeCap + smallCap, data = stockRetDataExp)
summary(msftModelexp)

#additional tools for assessing regression results: using the msftModel fitted above

#pull residuals from a model
res_msftModel = residuals(msftModel)

#plot residuals

par(mfrow=c(1,1))
plot(res_msftModel) #check for heteroscedasticity

#perform an analysis of variance, i.e. ANOVA

anova(msftModel)

##F-Statistic = MSReg/MSRes = (SSReg/(k))/(SSRes/(n-k-1))

