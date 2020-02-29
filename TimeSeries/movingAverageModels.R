##create a moving average model

aveTime = 20

#function to compute SIMPLE moving average
movingAverage = function (value_, result_) {
  for (i in 1:length(result_)) {
    if (i < aveTime) {
      result_[i] = 0
    } else {
      result_[i] = mean(value_[i + 1 - aveTime: i])
    }
  }
  return(result_)
}

result_= rep(0, length(exRate$value))
exRate$maValue= movingAverage(exRate$value, result_)
exRate$maValueDiff= movingAverage(exRate$diff, result_)

#compare moving average to earlier to base
par(mfrow=c(1,2))
#values
plot(exRate$maValue[20:length(exRate$maValue)],
     ylim = c(0.8, 1.2), col = "blue", main = "Spot Rates")
lines(exRate$value[20:length(exRate$diff)], col = "red")
#differences
plot(exRate$maValueDiff[20:length(exRate$maValue)],
    ylim = c(-0.05, 0.05), col = "blue", main = "Spot Differences")
lines(exRate$diff[20:length(exRate$diff)], col = "red")

#moving average gives equal weight to all observations.
#what if we wanted to give more recent observations more weight?

