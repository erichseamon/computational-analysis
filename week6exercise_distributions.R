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

#graphics.off()

#set.seed(1)    # Used to repeat functions that generate random data
#n = 1000     # Number of random numbers to use.

setwd("X:/Dropbox/ES Research/ES Classwork/FOR504/Data")   #----set the working directory

mu = mean(var1)
sigma = sd(var1)
#mu = 100  # Set the mean.
#sigma = 2 # Set the standard deviation.

#randNorm = rnorm(n, mu, sigma) # Use rnorm function to generate random numbers.

muExp = mean(var1)
#muExp = 10  # Set the mean.
#randExp = sort(rexp(n = n,rate = 1/muExp))

b = 150  # Set the scale parameter, b.
c = 1.5  # Set the shape parameter, c.
#randWbl = sort(rweibull(n = n, scale = b,shape = c))  

lambda = mean(var1)
#lambda = 10 # Set the mean. 
#randPois = sort(rpois(n = n, lambda = lambda)) 

fit = fitdistr(var1,"normal") # Fit normal distribution to data.
muHat = fit$estimate[1]  # MLE mean
sigmaHat = fit$estimate[2]  # MLE standard deviation
nHat = dnorm(sort(var1),muHat,sigmaHat)
win.graph()
hist(var1, breaks=50, freq = FALSE, col=rgb(0.75,0.75,0.75),
     ylab = "Probability density",
     xlab = "x",
     main = paste("var1 normal - ", "mu = ", mu, "; sigma = ", sigma))
lines(sort(var1),nHat,lwd=2)
text(min(var1),max(nHat), pos = 4,
     paste("Fit: mu = ",round(muHat*100)/100))
text(min(var1),0.9*max(nHat), pos = 4,
     paste("sigma = ", round(sigmaHat*100)/100))


#fit = fitdistr(i,"exponential")         
#muHat = 1/fit$estimate[1]  # The parameter for the exponential distirubtion
# is the rate; the mean of an exponential distribution is 1 / rate.
#nHat = dexp(i,fit$estimate[1])

#win.graph()
#hist(i,breaks=50, freq = FALSE, col=rgb(0.75,0.75,0.75),
     #ylab = "Probability density",
     #xlab = "x",
     #main = paste("mu = ", muExp))
#lines(i,nHat,lwd=2)
#text(max(i),max(nHat), pos = 2,
     #paste("Fit: mu = ",round(muHat*100)/100))


#fit = fitdistr(i, "weibull")
#bHat = fit$estimate[2]  # Estiamted scale parameter
#cHat = fit$estimate[1]  # Estimated shape parameter
#nHat = dweibull(i, cHat, bHat)

#win.graph()
#hist(i, breaks=50, freq = FALSE, col=rgb(0.75,0.75,0.75),
     #ylab = "Probability density",
     #xlab = "x")
     #main = paste("b = ", b,"; c = ", c))
#lines(i,nHat,lwd=2)
#text(max(i),max(nHat), pos = 2,
     #paste("Fit: b = ", round(bHat*100)/100, "; c = ", round(cHat*100)/100))

#fit = fitdistr(i, "Poisson")
#lambdaHat = fit$estimate
#nHat = dpois(i,lambdaHat)

#win.graph()
#hist(i, breaks = c(min(i):max(i)), freq = FALSE, col=rgb(0.75,0.75,0.75),
     #ylab = "Probability density",
     #xlab = "x",
     #main = paste("lambda = ", lambda))
#points(i,nHat,lwd=2)
#text(min(i),max(nHat), pos = 4,
     #paste("Fit: lambda = ", round(lambdaHat*100)/100))

  
  mu = mean(var2)
  sigma = sd(var2)
  #mu = 100  # Set the mean.
  #sigma = 2 # Set the standard deviation.
  
  #randNorm = rnorm(n, mu, sigma) # Use rnorm function to generate random numbers.
  
  muExp = mean(var2)
  #muExp = 10  # Set the mean.
  #randExp = sort(rexp(n = n,rate = 1/muExp))
  
  b = 150  # Set the scale parameter, b.
  c = 1.5  # Set the shape parameter, c.
  #randWbl = sort(rweibull(n = n, scale = b,shape = c))  
  
  lambda = mean(var2)
  #lambda = 10 # Set the mean. 
  #randPois = sort(rpois(n = n, lambda = lambda)) 
  
  fit = fitdistr(var2,"normal") # Fit normal distribution to data.
  muHat = fit$estimate[1]  # MLE mean
  sigmaHat = fit$estimate[2]  # MLE standard deviation
  nHat = dnorm(sort(var2),muHat,sigmaHat)
  win.graph()
  hist(var2, breaks=50, freq = FALSE, col=rgb(0.75,0.75,0.75),
       ylab = "Probability density",
       xlab = "x",
       main = paste("var2 normal - ","mu = ", mu, "; sigma = ", sigma))
  lines(sort(var2),nHat,lwd=2)
  text(min(var2),max(nHat), pos = 4,
       paste("Fit: mu = ",round(muHat*100)/100))
  text(min(var1),0.9*max(nHat), pos = 4,
       paste("sigma = ", round(sigmaHat*100)/100))
  
  
  fit = fitdistr(var2,"exponential")         
  muHat = 1/fit$estimate[1]  # The parameter for the exponential distirubtion
  # is the rate; the mean of an exponential distribution is 1 / rate.
  nHat = dexp(var2,fit$estimate[1])
  
  win.graph()
  hist(var2,breaks=50, freq = FALSE, col=rgb(0.75,0.75,0.75),
       ylab = "Probability density",
       xlab = "x",
       main = paste("var2 exp - ","mu = ", muExp))
  lines(var2,nHat,lwd=2)
  text(var2,max(nHat), pos = 2,
       paste("Fit: mu = ",round(muHat*100)/100))

  
  fit = fitdistr(var2, "weibull")
  bHat = fit$estimate[2]  # Estiamted scale parameter
  cHat = fit$estimate[1]  # Estimated shape parameter
  nHat = dweibull(var2, cHat, bHat)
  
  win.graph()
  hist(var2, breaks=50, freq = FALSE, col=rgb(0.75,0.75,0.75),
       ylab = "Probability density",
       xlab = "x",
       main = paste("var2 weibull - ","b = ", b,"; c = ", c))
  lines(var2,nHat,lwd=2)
  text(max(var2),max(nHat), pos = 2,
       paste("Fit: b = ", round(bHat*100)/100, "; c = ", round(cHat*100)/100))
  
  #fit = fitdistr(i, "Poisson")
  #lambdaHat = fit$estimate
  #nHat = dpois(i,lambdaHat)
  
  #win.graph()
  #hist(i, breaks = c(min(i):max(i)), freq = FALSE, col=rgb(0.75,0.75,0.75),
       #ylab = "Probability density",
       #xlab = "x",
       #main = paste("lambda = ", lambda))
  #points(i,nHat,lwd=2)
  #text(min(i),max(nHat), pos = 4,
       #paste("Fit: lambda = ", round(lambdaHat*100)/100))
  


mu = mean(var3)
sigma = sd(var3)
#mu = 100  # Set the mean.
#sigma = 2 # Set the standard deviation.

#randNorm = rnorm(n, mu, sigma) # Use rnorm function to generate random numbers.

muExp = mean(var3)
#muExp = 10  # Set the mean.
#randExp = sort(rexp(n = n,rate = 1/muExp))

b = 150  # Set the scale parameter, b.
c = 1.5  # Set the shape parameter, c.
#randWbl = sort(rweibull(n = n, scale = b,shape = c))  

lambda = mean(var3)
#lambda = 10 # Set the mean. 
#randPois = sort(rpois(n = n, lambda = lambda)) 
  
  
  mu = mean(var3)
  sigma = sd(var3)
  #mu = 100  # Set the mean.
  #sigma = 2 # Set the standard deviation.
  
  #randNorm = rnorm(n, mu, sigma) # Use rnorm function to generate random numbers.
  
  muExp = mean(var3)
  #muExp = 10  # Set the mean.
  #randExp = sort(rexp(n = n,rate = 1/muExp))
  
  #b = 150  # Set the scale parameter, b.
  #c = 1.5  # Set the shape parameter, c.
  #randWbl = sort(rweibull(n = n, scale = b,shape = c))  
  
  lambda = mean(var3)
  #lambda = 10 # Set the mean. 
  #randPois = sort(rpois(n = n, lambda = lambda)) 
  
  fit = fitdistr(var3,"normal") # Fit normal distribution to data.
  muHat = fit$estimate[1]  # MLE mean
  sigmaHat = fit$estimate[2]  # MLE standard deviation
  nHat = dnorm(sort(var3),muHat,sigmaHat)
  win.graph()
  hist(var3, breaks=50, freq = FALSE, col=rgb(0.75,0.75,0.75),
       ylab = "Probability density",
       xlab = "x",
       main = paste("var3 normal - ","mu = ", mu, "; sigma = ", sigma))
  lines(sort(var3),nHat,lwd=2)
  text(min(var3),max(nHat), pos = 4,
       paste("Fit: mu = ",round(muHat*100)/100))
  text(min(var3),0.9*max(nHat), pos = 4,
       paste("sigma = ", round(sigmaHat*100)/100))
  
  
  fit = fitdistr(var3,"exponential")         
  muHat = 1/fit$estimate[1]  # The parameter for the exponential distirubtion
  # is the rate; the mean of an exponential distribution is 1 / rate.
  nHat = dexp(var3,fit$estimate[1])
  
  win.graph()
  hist(var3,breaks=50, freq = FALSE, col=rgb(0.75,0.75,0.75),
       ylab = "Probability density",
       xlab = "x",
       main = paste("var3 exp - ","mu = ", muExp))
  lines(var3,nHat,lwd=2)
  text(max(var3),max(nHat), pos = 2,
       paste("Fit: mu = ",round(muHat*100)/100))
  
  
  fit = fitdistr(var3, "weibull")
  bHat = fit$estimate[2]  # Estiamted scale parameter
  cHat = fit$estimate[1]  # Estimated shape parameter
  nHat = dweibull(var3, cHat, bHat)
  
  win.graph()
  hist(var3, breaks=50, freq = FALSE, col=rgb(0.75,0.75,0.75),
       ylab = "Probability density",
       xlab = "x",
       main = paste("var3 weibull - ","b = ", b,"; c = ", c))
  lines(var3,nHat,lwd=2)
  text(max(var3),max(nHat), pos = 2,
       paste("Fit: b = ", round(bHat*100)/100, "; c = ", round(cHat*100)/100))
  
  #fit = fitdistr(i, "Poisson")
  #lambdaHat = fit$estimate
  #nHat = dpois(i,lambdaHat)
  
  #win.graph()
  #hist(i, breaks = c(min(i):max(i)), freq = FALSE, col=rgb(0.75,0.75,0.75),
       #ylab = "Probability density",
       #xlab = "x",
       #main = paste("lambda = ", lambda))
  #points(i,nHat,lwd=2)
  #text(min(i),max(nHat), pos = 4,
       #paste("Fit: lambda = ", round(lambdaHat*100)/100))
  

  
  mu = mean(var4)
  sigma = sd(var4)
  #mu = 100  # Set the mean.
  #sigma = 2 # Set the standard deviation.
  
  #randNorm = rnorm(n, mu, sigma) # Use rnorm function to generate random numbers.
  
  muExp = mean(var4)
  #muExp = 10  # Set the mean.
  #randExp = sort(rexp(n = n,rate = 1/muExp))
  
  #b = 150  # Set the scale parameter, b.
  #c = 1.5  # Set the shape parameter, c.
  #randWbl = sort(rweibull(n = n, scale = b,shape = c))  
  
  lambda = mean(var4)
  #lambda = 10 # Set the mean. 
  #randPois = sort(rpois(n = n, lambda = lambda)) 
  
  fit = fitdistr(var4,"normal") # Fit normal distribution to data.
  muHat = fit$estimate[1]  # MLE mean
  sigmaHat = fit$estimate[2]  # MLE standard deviation
  nHat = dnorm(sort(var4),muHat,sigmaHat)
  win.graph()
  hist(var4, breaks=50, freq = FALSE, col=rgb(0.75,0.75,0.75),
       ylab = "Probability density",
       xlab = "x",
       main = paste("var4 normal - ","mu = ", mu, "; sigma = ", sigma))
  lines(sort(var4),nHat,lwd=2)
  text(min(var4),max(nHat), pos = 4,
       paste("Fit: mu = ",round(muHat*100)/100))
  text(min(var4),0.9*max(nHat), pos = 4,
       paste("sigma = ", round(sigmaHat*100)/100))
  
  
  #fit = fitdistr(i,"exponential")         
  #muHat = 1/fit$estimate[1]  # The parameter for the exponential distirubtion
  # is the rate; the mean of an exponential distribution is 1 / rate.
  #nHat = dexp(i,fit$estimate[1])
  
  #win.graph()
  #hist(i,breaks=50, freq = FALSE, col=rgb(0.75,0.75,0.75),
       #ylab = "Probability density",
       #xlab = "x",
       #main = paste("mu = ", muExp))
  #lines(i,nHat,lwd=2)
  #text(max(i),max(nHat), pos = 2,
       #paste("Fit: mu = ",round(muHat*100)/100))
  
  
  #fit = fitdistr(i, "weibull")
  #bHat = fit$estimate[2]  # Estiamted scale parameter
  #cHat = fit$estimate[1]  # Estimated shape parameter
  #nHat = dweibull(i, cHat, bHat)
  
  #win.graph()
  #hist(i, breaks=50, freq = FALSE, col=rgb(0.75,0.75,0.75),
       #ylab = "Probability density",
       #xlab = "x")
  #main = paste("b = ", b,"; c = ", c))
  #lines(i,nHat,lwd=2)
  #text(max(i),max(nHat), pos = 2,
       #paste("Fit: b = ", round(bHat*100)/100, "; c = ", round(cHat*100)/100))
  
  #fit = fitdistr(i, "Poisson")
  #lambdaHat = fit$estimate
  #nHat = dpois(i,lambdaHat)
  
  #win.graph()
  #hist(i, breaks = c(min(i):max(i)), freq = FALSE, col=rgb(0.75,0.75,0.75),
       #ylab = "Probability density",
       #xlab = "x",
       #main = paste("lambda = ", lambda))
  #points(i,nHat,lwd=2)
  #text(min(i),max(nHat), pos = 4,
       #paste("Fit: lambda = ", round(lambdaHat*100)/100))
  

#------------manual coded---------------------------#




