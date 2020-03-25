#demonstrate principal component analysis in R

setwd("/Users/Kigen/Documents/r")

#Step 1: Read in Data
smith = read.csv("smith.csv", header = TRUE, sep = ",") #S&P 500

#Step 2: Visualize Data
par(mfrow=c(1,1))
plot(smith$x, smith$y)

#Step 3: Standardize Data - this way they have 
meanSmith = sapply(smith, FUN = mean)
smith3 = smith - sapply(smith, FUN = mean)

#Step 4: Obtain the covariance matrix
covSmith = cov(smith)
print(covSmith)

#Step 5: Calculate the eigenvectors and eigenvalues of the matrix
eigSmith = eigen(covSmith)
eigVecSmith = eigSmith$vectors
eigValSmith = eigSmith$values

print(eigVecSmith)
print(eigValSmith)

#Step 6: Analysis of variance explained by each component
varExpSmith = data.frame(pc1 = eigValSmith[1], pc2 = eigValSmith[2])/sum(eigValSmith)

#Step 7: Say we choose only the first principal component since it explains 96% of the variance
featureVector = matrix(eigVecSmith[,1])
print(featureVector)

#Step 8: Derive New Data using the first principal component
smith8 = t((featureVector)) %*% t((smith3))
print(smith8)

#Step 9: Visualize model
par(mfrow=c(1,1))
plot(smith3$x, smith3$y, type = "p", col = "blue")
lines(smith3$x, smith8, type = "p", col = "red")

