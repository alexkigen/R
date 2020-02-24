#multiple regression

model = lm(formula = hwy ~ cyl * drv, data = mpg)

model_function = paste("y = ", model$coefficients[1], sign(model$coefficients[2]), abs(model$coefficients[2]), "*", sep = "")

#predict function: takes a dataframe with same-named variables as the independent variables of the model

data.frame(mpg)

test_data = data.frame(cyl = rep(1:3, 3), drv = c(rep("f", 3), rep("4", 3), rep("r", 3)))

#save predicted values of hwy in the testing dataframe
test_data$hwy_pred = predict(model, newdata = test_data)


require(ggplot2)

ggplot(data = test_data, aes(cyl, hwy_pred)) +
  geom_point()

#compute standard error of prediction
#SEp = (1+ 1/n + ((x-mean(x))/(n-1)SEe))
model_summary = summary(model)

#extract standard error of estimate
SEe = model_summary$sigma

#compute mean of prediction values
cylMean = mean(test_data$cyl)
n = length(mpg)

#compute standard error of predicted values and store in dataframe
test_data$SEp = (1 + (1/n) + (((test_data$cyl- cylMean)^2)/(n-1)*SEe))

print (test_data)

