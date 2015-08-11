#---------------------------------------------------------------------------------------------------------------------#
# TITLE:        week6exercise_distributions.R
#
# COURSE:       Computational Data Analysis (FOR504)
#               Assignment #3
#
# AUTHOR:       Erich Seamon
#
# INSTITUITON:  College of Natural Resources
#               University of Idaho
#
# DATE:         23 Sept. 2014
#
# COMMENTS:     This file contains code to test the fit of dataset to varying distributions
#
#
#---------------Setting the working directory and clearing the workspace----------------------------------------------#

setwd("X:/Dropbox/ES Research/ES Classwork/FOR504/data") # Set working directory
rm(list=ls())   # Clear workspace (remove varialbes)
graphics.off()  # Close current graphing windows
cat("\14")      # Clear command prompt

library(MASS)
library(scatterplot3d)

#-------Loading the dataset to use for analysis ----------------------------------------------------------------------#

week6data = read.csv("Week6Data.csv", # Read in random seedling data
                     header=TRUE, 
                     skip=1, 
                     col.names=c("var1", "var2", "var3", "var4"),)   

#-------Creating variables for each column in dataset-----------------------------------------------------------------#

var1 = week6data[,1]   # [site] The first column, sites, in seedlingdata
var2 = week6data[,2] # [class]  The second column, class, in seedlingdata
var3 = week6data[,3]   # [Spp] The third column, spp, in seedlingdata
var4 = week6data[,4]   # [Ht] The fourth column, ht (height), in seedlingdata 


set.seed(1)    # Used to repeat functions that generate random data
n = 1000       # Number of random numbers to use.

mu = 100  # Set the mean.
sigma = 2 # Set the standard deviation.
randNorm = rnorm(n, mu, sigma) # Use rnorm function to generate random numbers.


muExp = 10  # Set the mean.
randExp = sort(rexp(n = n,rate = 1/muExp))

b = 150  # Set the scale parameter, b.
c = 1.5  # Set the shape parameter, c.
randWbl = sort(rweibull(n = n, scale = b,shape = c)) 

lambda = 10 # Set the mean. 
randPois = sort(rpois(n = n, lambda = lambda)) 

fit = fitdistr(randNorm,"normal") # Fit normal distribution to data.
muHat = fit$estimate[1]  # MLE mean
sigmaHat = fit$estimate[2]  # MLE standard deviation
nHat = dnorm(sort(randNorm),muHat,sigmaHat)
win.graph()
hist(randNorm, breaks=50, freq = FALSE, col=rgb(0.75,0.75,0.75),
     ylab = "Probability density",
     xlab = "x",
     main = paste("mu = ", mu, "; sigma = ", sigma))
lines(sort(randNorm),nHat,lwd=2)
text(min(randNorm),max(nHat), pos = 4,
     paste("Fit: mu = ",round(muHat*100)/100))
text(min(randNorm),0.9*max(nHat), pos = 4,
     paste("sigma = ", round(sigmaHat*100)/100))

fit = fitdistr(randExp,"exponential")         
muHat = 1/fit$estimate[1]  # The parameter for the exponential distirubtion
# is the rate; the mean of an exponential distribution is 1 / rate.
nHat = dexp(randExp,fit$estimate[1])

win.graph()
hist(randExp,breaks=50, freq = FALSE, col=rgb(0.75,0.75,0.75),
     ylab = "Probability density",
     xlab = "x",
     main = paste("mu = ", muExp))
lines(randExp,nHat,lwd=2)
text(max(randExp),max(nHat), pos = 2,
     paste("Fit: mu = ",round(muHat*100)/100))


fit = fitdistr(randWbl, "weibull")
bHat = fit$estimate[2]  # Estiamted scale parameter
cHat = fit$estimate[1]  # Estimated shape parameter
nHat = dweibull(randWbl, cHat, bHat)

win.graph()
hist(randWbl, breaks=50, freq = FALSE, col=rgb(0.75,0.75,0.75),
     ylab = "Probability density",
     xlab = "x",
     main = paste("b = ", b,"; c = ", c))
lines(randWbl,nHat,lwd=2)
text(max(randWbl),max(nHat), pos = 2,
     paste("Fit: b = ", round(bHat*100)/100, "; c = ", round(cHat*100)/100))

fit = fitdistr(randPois, "Poisson")
lambdaHat = fit$estimate
nHat = dpois(randPois,lambdaHat)

win.graph()
hist(randPois, breaks = c(min(randPois):max(randPois)), freq = FALSE, col=rgb(0.75,0.75,0.75),
     ylab = "Probability density",
     xlab = "x",
     main = paste("lambda = ", lambda))
points(randPois,nHat,lwd=2)
text(min(randPois),max(nHat), pos = 4,
     paste("Fit: lambda = ", round(lambdaHat*100)/100))

