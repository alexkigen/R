#visualize covid-19 cases in the United States
#data from New York Times: Each row of data reports cumulative counts

#read in data
covidUS = read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

#aggregate data by date
covidUSAggregate = aggregate(data.frame(cases = covidUS$cases, deaths = covidUS$deaths), by = list(date = covidUS$date), sum)
covidUSAggregate$day = seq_along(covidUSAggregate$date)

#visualize aggregate
require(ggplot2)

ggplot(data = covidUSAggregate, aes(x = day, y = cases)) +
  geom_point()

#visualize first 40 days since first case
ggplot(data = covidUSAggregate[1:40,], aes(day, cases)) +
  geom_point() +
  geom_smooth()

#visualize later dates
terminal = max(covidUSAggregate$day)
ggplot(data = covidUSAggregate[41:terminal, ], aes(day, cases)) +
  geom_point() +
  geom_smooth()

#compute conditional cumulative mortality rate
covidUSAggregate$mortality = with(covidUSAggregate,  deaths/cases)
plot(covidUSAggregate$mortality, type = "l", col = "red")

ggplot(data =  covidUSAggregate[60:terminal, ], aes(x = day, y = mortality))+
  geom_point() +
  geom_smooth()

mu = mean(covidUSAggregate$mortality[60:terminal])
stdev = sd(covidUSAggregate$mortality[60:terminal])

#fit a histogram based assuming a normal distribution
sampleData = rnorm(60*60, mean = mu, sd = stdev)
histogram = hist(sampleData,
                 breaks = 60)

xfit<-seq(min(sampleData), max(sampleData), length=60)
yfit<-dnorm(xfit, mean = mu, sd = stdev)
yfit <- yfit*diff(histogram$mids[1:2])*length(sampleData)
lines(xfit, yfit, col="blue", lwd=2)
box()
