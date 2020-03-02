#continuing from the starbucks model (see seasonality_SBUX.R)

salesResA = unname(logModelA$residuals)
salesResB = unname(logModelB$residuals)

par(mfrow=c(2,1))
plot(salesResA, type = "l")
plot(salesResB, type = "l")


salesRes = data.frame(res = salesResA[1:57], resLag = salesResA[2:58])

##fit an ARCH(1) model. H0: coefficient on the independent lagged variable is 0
##Ha: coefficient 
arch = lm((resLag^2) ~ (res^2), data = salesRes)

summary(arch)
plot(arch$residuals)

#we can use ARCH models for two things:
#1. identify heteroscedasticity
#2. forecast variance



