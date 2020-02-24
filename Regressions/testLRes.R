##test CLT on linear regression

alpha_ = 1
beta_ = 2

x = 3

results_vector = rep(1:10)


paths = 1000

for (j in 1:10) {
runningSum = 0

  for (i in 1:paths) {
    noise = rnorm(1, 0, 1)
    y = alpha_ + beta_*x + noise
    runningSum = runningSum + y
  }
  
  resultSim = runningSum/paths

results_vector[j] = resultSim
}

print (results_vector)
plot(results_vector)

