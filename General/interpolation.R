#determine dynLps as factorA*factorB
#for this exercise, let's ssume factor A of 25%
#trying to understand impact of interpolating in two ways: above and below

factorA = 0.25

#create factor B dataframes

factorB = data.frame(diff = seq(0,600, by = 10),
                    factorB = c(seq(0,100, 2.5), c(rep(100, 20))))

factorB_current = data.frame(diff = factorB$diff,
                             factorB = c(rep(0,15), rep(5,5), rep(50,10), rep(75,10), rep(100,21)),
                             dynLps = factorA*c(rep(0,15), rep(5,5), rep(50,10), rep(75,10), rep(100,21)))

proposed = read.csv("factorB.txt", header = FALSE, sep = ",")

factorB_proposed = data.frame(diff = factorB$diff,
                              factorB = proposed$V1,
                              dynLps = proposed$V1*factorA)

par(mfrow=c(1,1))
colors = rainbow(4) #colors to use with plots

plot(factorB_current$diff, factorB_current$factorB, type = "l",
     xlab = "MR-CR Difference",
     ylab = "Factor B",
     main = "Factor B Comparison: Current vs Proposed",
     col = colors[1])

#layer on additional plots for analysis
lines(factorB_proposed$diff, factorB_proposed$factorB, col = colors[2])
lines(factorB_proposed$diff, factorB_current$dynLps, col = colors[3])
lines(factorB_proposed$diff, factorB_proposed$dynLps, col = colors[4])

#add legend
legend(x= 2, y = 100,
       legend = c("factorB - current", "factorB - proposed", "dynamic lapse - current", "dynamic lapse - proposed"),
       col = colors, lty = c(rep(1,4)))

#add text
#text(x = 200, y = 60, "Proposed Method lowers factor B across the board")
#text(x = 200, y = 56, "As a result, dynamic lapses are lower (this assumes factor A of 25%)")

