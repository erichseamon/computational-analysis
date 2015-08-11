
rm(list=ls())  # Clear workspace
set.seed(1)
n=1000
randNorm = rnorm(n = n,mean = 0,sd = 1)
randExp = rexp(n = n,rate = 1)  

sumstats = summary(randExp)
hist(randNorm)
win.graph(10,3.75) # creates a window 10 by 3.75)

par(mfrow = c(1,3))
bins  = seq(-5,5,0.25)