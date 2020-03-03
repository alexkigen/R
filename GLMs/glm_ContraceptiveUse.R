contraUse = read.table("https://data.princeton.edu/wws509/datasets/cuse.dat", header = TRUE)

contraUse$p = contraUse$using/(contraUse$using + contraUse$notUsing)

#boxplot - R automatically recognizes categorical data
plot(contraUse$age, contraUse$p)


lrfit = glm( cbind(using, notUsing) ~ age + education + wantsMore, data = cuse, family = binomial)
lrfit

#test significance of residuals
pchisq(29.92, 10, lower.tail = FALSE)

##we need a better model
##let's update with the interaction term age & wantsMore

lrfit1 = update(lrfit, ~ . + age*wantsMore)
lrfit1


