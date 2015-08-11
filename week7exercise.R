
#------------------------------------------------------------------------#
# TITLE:        Seamon_Final_Project.R
#
# COURSE:       Computational Data Analysis (FOR504)
#               Final Project
#
# AUTHOR:       Erich Seamon
#
# INSTITUITON:  College of Natural Resources
#               University of Idaho
#
# DATE:         23 Sept. 2014
#
# COMMENTS:     This is the Final Project for FOR 504.
#
#
#--Setting the working directory and clearing the workspace-----------#

#--clear the variable list and set the working directory----#

rm(list = ls()) #--clears all lists
cat("\14")
#----set the working directory
setwd("X:/Dropbox/ES Research/ES Classwork/FOR504/Data")   

#----set packages----#
library("ncdf")
library("raster")
library("sp")
library("rgeos")
library("rgdal")
library("proj4")
library("RNetCDF")
#library("ncdf4")
library("RColorBrewer")
library("raster")
library("rasterVis")
library("latticeExtra")
library("maptools")
library("parallel")
#library("Evapotranspiration")
library("nortest")

memory.size(10000)# Initialize Workspace

graphics.off()
setwd("X:/Dropbox/ES Research/ES Classwork/FOR504/Data/")

# Set random seet for repeatability
set.seed(1)

# Load Required Packages
require(nortest) # Package to conduct normality goodness of fit tests

## Loading required package: nortest

n = 100                           # Sample size.
mu = 10                         # Mean
sigma = 1                                # Standard deviation
randNorm = rnorm(n,mean = mu,sd = sigma)  # Random normal, mean 0, std. 1
randExp = rexp(n,rate = 1)                # Random exponential, mean = 1.

# Tests and plots using random normal data:
x = randNorm

chi2_stats = pearson.test(x)
chi2_p = chi2_stats$p.value # Get p-value for normality test

ks_stats = ks.test((x-mean(x))/sd(x),"pnorm") # When using the KS test you have to 
# specify what distribution you want to test.
# Here it is a standard normal, indicated by
# "pnorm". If you wanted to test if the data
# came from an exponential distribution, you 
# would specify it with "pexp".
ks_p = ks_stats$p.value

lillie_stats = lillie.test(x)
lillie_p = lillie_stats$p.value

# Plots
win.graph()
par(mfcol=c(2,2))
hist(x,
     ylab="Count",
     main=paste("Random normal data, n = ",n,sep=""))

qqnorm(x) # Probability plot comparing data to standard normal distribution
qqline(x) # Add line to evaluate fit of data compared to standard normal
# quantiles.

# Annotate plot
x_loc = -2.25 # x-value location for annotated text on plots (see below)

# Here we are using the concatenate function 'c' to create a set of x and y
# values to designate where annotated text on the plots should go. This 
# is a shortcut in R that can be used instead of writing multiple lines of
# code that use the 'text' function.
text(rep(x_loc,3),c(0.99*max(x),0.96*max(x),0.93*max(x)),
     c(paste("Chi-square p = ", round(chi2_p,digits=2),sep=""),
       paste("K-S p = ", round(ks_p,digits=2), sep=""),
       paste("Lilliefors p = ", round(lillie_p,digits=2),sep="")),
     adj=c(0,NA))

# Tests and plots using random exponential data:
x = randExp

chi2_stats = pearson.test(x)
chi2_p = chi2_stats$p.value

ks_stats = ks.test(x,"pnorm") # Use KS test to evaluate whether data came
# from a standard normal distribution, here
# indicated by the "pnorm" argument.
ks_p = ks_stats$p.value

lillie_stats = lillie.test(x)
lillie_p = lillie_stats$p.value

# Plot
hist(x,
     ylab="Count",
     main=paste("Random exponential data, n = ", n, sep=""))

qqnorm(x)
qqline(x)

# Annotate plot
x_loc = -2.25
text(rep(x_loc,3),c(0.99*max(x),0.90*max(x),0.81*max(x)),
     c(paste("Chi-square p = ", round(chi2_p,digits=2),sep=""),
       paste("K-S p = ", round(ks_p,digits=2), sep=""),
       paste("Lilliefors p = ", round(lillie_p,digits=2),sep="")),
     adj=c(0,NA))