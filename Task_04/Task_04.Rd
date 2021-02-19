source("http://jonsmitchell.com/code/fxn05.R")

Pop1 <- simPop(Popsize = 50, nGenerations = 100, initial_p = 0.5, h = 1, s = 0)

plot(1:nrow(Pop1), Pop1[,1], ylim=c(0,1), type = "l", xlab="generation", ylab="allele freq.", lwd=2)

lines(1:nrow(Pop1), Pop1[,2], lwd=2, col = 'red')

legend("topleft", legend = c("a", "b"), col = c("black", "red"), lwd = 2, bty = "n")

plotFit(nruns = 10, n = 50, ngens = 100, init_p = 0.5, h = 1, s = 0)

Expectation <- c(10, 10, 10, 10)

Observed <- c(15, 15, 5, 5)

Chisqs <- sum(((Expectation - Observed) ^ 2)/ Expectation)

barplot(rbind(Expectation, Observed), beside = T, main = bquote(chi ^ 2 ~ "=" ~. (Chisq)), legend.text=c("expected", "observed"))


Observed <- c(10, 10, 10, 10)

# x^2 = 0 when they are all 10, the more even the bar is the lower the x^2 value will be

results <- 

read.csv("http://jonsmitchell.com/data/biol112labresults.csv", stringsAsFactors = F)

counts <- results[,c("yellow", "red", "green", "blue", "black", "tan")]\


backgroundsCol <- c("White", "#d53e4f", "#fee80b", "#abdda4", "#3288bd", "black")


calcChi(counts[1,])

plotChis(counts)

Chisqs <- apply(counts, 1, calcChi)

plotChis(counts)

#when the chi squared is high the bars aren't even, when it is low they are exactly even. The plotChis function shows the variation between the different categories

#about 85

Avg <- mean(Chisqs)

#it is much higher, yes

backgroundAvgs <- tapply(Chisqs, results[,3], mean)


propSig <- length( which( Chisqs > 11.70) ) / length(Chisqs)

percSig <- round(100 * propSig)

#I did not expect it to be so high and no there could be many factors that would cause this.

par(las = 1, mar = c(4, 4, 1, 1), mgp = c(2, 0.5, 0), tck = -0.01, cex.axis=1)

hist(Chisqs, main="", xlab="chi-squared values", ylab="frequency")

par(las = 1, mar = c(4, 4, 1, 1), mgp = c(2, 0.5, 0), tck = -0.01, cex.axis=1)

plot(1, 1, xlim=c(0, 400), ylim=c(1, 8.5), xlab="", ylab="", type="n", yaxt="n")

backgroundCol <- c("white", "d53e4f", "#fee08b", "#abdda4", "#3288bd", "black")

mtext(side = 1, expression(chi^2), cex=1.75, line=2.5)

counter <- 1

for(i in backgrounds) {

Data <- Chisqs[which(results[,3] == i)]

addHist(Y=counter, Dat=Data, Color=backgroundCol[counter])

counter <- counter + 1 

{

abline( v = 11.70, lty=2, lwd = 2, col='black')


#no

Simulation <- simDraws(10000)

addHist(Y=7, Dat=Simulation, Color="lightgray")


mtext(side=2, at=7, line=0, "simulated")

abline(v = 11.70, lty=2, lwd=2)

Fit <- c(1, 1, 1, 1, 1, 1)

names(Fit <- 1:6)

Simulation2 <- simDraws(1e4, w = Fit)

addHist(Y=8, Dat=Simulation2, Color=rgb(0,0,0,0.25))

Fit <- c(0.1, 1, 1, 1, 1, 1)

names(Fit) <- 1:6

Simulation4 <- simDraws(1e4, w = fit)

addHist(Y=8, Dat=Stimulation3, Color=rgb(0,0,0,0.25))

Fit <- c(0.5, 0.6, 0.7, 1, 1, 1)

names(Fit) <- 1:6

Simulation5 <- simDraws(1e4, w = Fit)

addHist(y=8, Dat=Stimlation5, Color=rgb(0,0,0,0.25))

Fit <- c(0.1, 0.1, 0.1, 0.1, 0.1, 1)

names(Fit) <- 1:6

Simulation6 <- simDraws(1e4, w = Fit)
addHist(Y=8, Dat=Simulation6, Color=rbg(0,0,0,0.25))

mtext(side=2, at=8, line=0, "sel. sim.")

#in this simulation it appears that the backgrounds do play a role, no I would say that they seem to follow the same patteren. 

#genetic drift

#natural selection

#They are similar

#comparing them to the simulated numbers

#This would effect the genetic drift








