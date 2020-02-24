#Graphing and plotting

attach(mtcars)
plot(wt, mpg, xlab = "weight", ylab = "miles per gallon", 
     main = "Plot of Weight vs MPG", type = 'p')
#points(wt,mpg)
#types include l, p, b, o, n, h, n
#n is for no plotting - useful when you want to create only the titles and axes and
#then plot the data using lines and points
# http://www.dummies.com/programming/r/how-to-create-different-plot-types-in-r/ 
#e.g.

x <- seq(0.5, 1.5, 0.25)
y <- rep(1, length(x))
plot(x, y, type="n")
points(x, y)

par(mfrow=c(2,2)) #create a 2 by 2 charting framework

hist(mtcars$mpg)
hist(mtcars$mpg,breaks = 12, col = 'lightgreen')


#using ggplot2
install.packages('ggplot2')
require(ggplot2)

#qplot - easier, faster, less 
#qplot(x, y, data=, color=, shape=, size=, alpha=, 
#     geom=, method=, formula=, facets=, 
#     xlim=, ylim= xlab=, ylab=, main=, sub=)

data(mtcars)   #can use data() to load data that comes with base R
#into the global environment

mtcars$am = as.factor(mtcars$am)
qplot(hp, mpg, data=mtcars, shape=am, color=am, 
             facets=gear~cyl, size=I(3),
             xlab="Horsepower", ylab="Miles per Gallon")

# https://www.statmethods.net/advgraphs/ggplot2.html 

#Can assign names to chart objects/store charts by assigning them names e.g.
p <- qplot(hp, mpg, data=mtcars, shape=am, color=am, 
           ##facets=gear~cyl, size=I(3), ##facets provides a view for each of the factors/combination of in the chart
           xlab="Horsepower", ylab="Miles per Gallon")

#qqplot()
#start off with a base plot and then add on layers/elements using +
require(ggplot2)

ggplot(data = mtcars, aes(x = hp, y = mpg)) + #creates the base plot
  geom_point() + #adds the points of plot(x = hp, y = mpg)
  geom_smooth() #adds a smooth line to aid in visualization; can specify method e.g. method = 'lm'

