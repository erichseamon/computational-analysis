# 
# Script Title: week3exercise.R
# Description: this is is a script that provides an overview of the datasets to be used as # # part of the overall class project for Forestry 504 Computational Analysis.
# 
# Date: 09.04.2014
# 
# Author: Erich Seamon
#
#
#


#--clear the variable list and set the working directory----#

rm(list = ls()) #--clears all lists
setwd("X:/Dropbox/ES Research/ES Classwork/FOR504/data/")

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


#-----Setting variables that are associated with input datasets------#

aggmet2007.nc = "X:/Dropbox/ES Research/ES Classwork/FOR504/data/_agg_met_2007_WUSA.nc"
MET2007 <- open.ncdf(aggmet2007.nc, write=FALSE)# Open a netcdf file
aggmet2008.nc = "X:/Dropbox/ES Research/ES Classwork/FOR504/data/_agg_met_2008_WUSA.nc"
MET2008 <- open.ncdf(aggmet2008.nc, write=FALSE)# Open a netcdf file
aggmet2009.nc = "X:/Dropbox/ES Research/ES Classwork/FOR504/data/_agg_met_2009_WUSA.nc"
MET2009 <- open.ncdf(aggmet2009.nc, write=FALSE)# Open a netcdf file
aggmet2010.nc = "X:/Dropbox/ES Research/ES Classwork/FOR504/data/_agg_met_2010_WUSA.nc"
MET2010 <- open.ncdf(aggmet2010.nc, write=FALSE)# Open a netcdf file
aggmet2011.nc = "X:/Dropbox/ES Research/ES Classwork/FOR504/data/_agg_met_2011_WUSA.nc"
MET2011 <- open.ncdf(aggmet2011.nc, write=FALSE)# Open a netcdf file

aggmet2007_tmmx_raster <- raster(aggmet2007.nc, varname="max_air_temperature")
aggmet2007_tmmx_matrix <- as.matrix(aggmet2007_tmmx_raster)
aggmet2007_tmmx_matrix <- as.matrix(aggmet2007_tmmx_raster)
aggmet2007_tmmx_matrix_mean <- mean(aggmet2007_tmmx_matrix)
aggmet2007_tmmx_matrix_median <- median(aggmet2007_tmmx_matrix)
aggmet2007_tmmx_matrix_sd <- sd(aggmet2007_tmmx_matrix)
aggmet2007_tmmx_matrix_min <- min(aggmet2007_tmmx_matrix)
aggmet2007_tmmx_matrix_max <- max(aggmet2007_tmmx_matrix)
aggmet2007_tmmx_raster_transposed <- t(aggmet2007_tmmx_raster)
aggmet2007_tmmx_matrix <- as.matrix(aggmet2007_tmmx_raster_transposed)

aggmet2007_tmin_raster <- raster(aggmet2007.nc, varname="min_air_temperature")
aggmet2007_tmin_matrix <- as.matrix(aggmet2007_tmin_raster)
aggmet2007_tmin_matrix <- as.matrix(aggmet2007_tmin_raster)
aggmet2007_tmin_matrix_mean <- mean(aggmet2007_tmin_matrix)
aggmet2007_tmin_matrix_median <- median(aggmet2007_tmin_matrix)
aggmet2007_tmin_matrix_sd <- sd(aggmet2007_tmin_matrix)
aggmet2007_tmin_matrix_min <- min(aggmet2007_tmin_matrix)
aggmet2007_tmin_matrix_max <- max(aggmet2007_tmin_matrix)
aggmet2007_tmin_raster_transposed <- t(aggmet2007_tmin_raster)
aggmet2007_tmin_matrix <- as.matrix(aggmet2007_tmin_raster_transposed)

aggmet2007_minrh_raster <- raster(aggmet2007.nc, varname="min_relative_humidity")
aggmet2007_minrh_matrix <- as.matrix(aggmet2007_minrh_raster)
aggmet2007_minrh_matrix <- as.matrix(aggmet2007_minrh_raster)
aggmet2007_minrh_matrix_mean <- mean(aggmet2007_minrh_matrix)
aggmet2007_minrh_matrix_median <- median(aggmet2007_minrh_matrix)
aggmet2007_minrh_matrix_sd <- sd(aggmet2007_minrh_matrix)
aggmet2007_minrh_matrix_min <- min(aggmet2007_minrh_matrix)
aggmet2007_minrh_matrix_max <- max(aggmet2007_minrh_matrix)
aggmet2007_minrh_raster_transposed <- t(aggmet2007_minrh_raster)
aggmet2007_minrh_matrix <- as.matrix(aggmet2007_minrh_raster_transposed)

aggmet2007_maxrh_raster <- raster(aggmet2007.nc, varname="max_relative_humidity")
aggmet2007_maxrh_matrix <- as.matrix(aggmet2007_maxrh_raster)
aggmet2007_maxrh_matrix <- as.matrix(aggmet2007_maxrh_raster)
aggmet2007_maxrh_matrix_mean <- mean(aggmet2007_maxrh_matrix)
aggmet2007_maxrh_matrix_median <- median(aggmet2007_maxrh_matrix)
aggmet2007_maxrh_matrix_sd <- sd(aggmet2007_maxrh_matrix)
aggmet2007_maxrh_matrix_min <- min(aggmet2007_maxrh_matrix)
aggmet2007_maxrh_matrix_max <- max(aggmet2007_maxrh_matrix)
aggmet2007_maxrh_raster_transposed <- t(aggmet2007_maxrh_raster)
aggmet2007_maxrh_matrix <- as.matrix(aggmet2007_maxrh_raster_transposed)

aggmet2007_windspeed_raster <- raster(aggmet2007.nc, varname="wind_speed")
aggmet2007_windspeed_matrix <- as.matrix(aggmet2007_windspeed_raster)
aggmet2007_windspeed_matrix_mean <- mean(aggmet2007_windspeed_matrix)
aggmet2007_windspeed_matrix_median <- median(aggmet2007_windspeed_matrix)
aggmet2007_windspeed_matrix_sd <- sd(aggmet2007_windspeed_matrix)
aggmet2007_windspeed_matrix_min <- min(aggmet2007_windspeed_matrix)
aggmet2007_windspeed_matrix_max <- max(aggmet2007_windspeed_matrix)
aggmet2007_windspeed_raster_transposed <- t(aggmet2007_windspeed_raster)
aggmet2007_windspeed_matrix <- as.matrix(aggmet2007_windspeed_raster_transposed)

aggmet2007_srad_raster <- raster(aggmet2007.nc, varname="surface_downwelling_shortwave_flux_in_air")
aggmet2007_srad_matrix <- as.matrix(aggmet2007_srad_raster)
aggmet2007_srad_matrix_mean <- mean(aggmet2007_srad_matrix)
aggmet2007_srad_matrix_median <- median(aggmet2007_srad_matrix)
aggmet2007_srad_matrix_sd <- sd(aggmet2007_srad_matrix)
aggmet2007_srad_matrix_min <- min(aggmet2007_srad_matrix)
aggmet2007_srad_matrix_max <- max(aggmet2007_srad_matrix)
aggmet2007_srad_raster_transposed <- t(aggmet2007_srad_raster)
aggmet2007_srad_matrix <- as.matrix(aggmet2007_srad_raster_transposed)


win.graph(10,3.75) 
plot(aggmet2007_tmin_matrix)

#bins = seq(0,20,1)
# Plot a histogram (frequency) distribution
#hist(aggmet2007_windspeed_matrix,      # Data to represented in the distribution
#breaks = bins, # Bins used to create histogram
#xlab = "Mean Max Temperature",    # Axis labels ...
#ylab = "Frequency Counts",
#main = "Frequency Distribution",# ...and title of plot (figure)
#col = "gray")  # color of bars in histogram plot


bins = seq(0,20,1) # As above for randNorm

#par(mfrow=c(4,1))  #---par function sets number or rows and columns for a matrix for plots
plot(aggmet2007_tmin_matrix)  #---plots aggmet2007 windspeed
plot(aggmet2007_tmin_raster_transposed)

hist(aggmet2007_tmin_matrix,breaks=bins,   #---plots aggmet2007 windspeed frequency
     col = "gray",
     xlab = "Wind Speed (unit)",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "2007 Wind Speed Frequency Distribution")

# Add summary statistics to plot
abline(v=aggmet2007_tmin_matrix_mean,lwd=2)
abline(v=aggmet2007_tmin_matrix_median,col = "red",lwd = 2,lty = "dashed")
abline(v=aggmet2007_tmin_matrix_sd,col = "blue", lwd = 2,lty = "dashed")
points(aggmet2007_tmin_matrix_min,100,pch = "*")
points(aggmet2007_tmin_matrix_max,100,pch = "*")

# Add Legend
legend(8.5,1520,
       c("Mean","Median","St.Dev.","Min.","Max."),
       col = c("black","red","blue","black","black"),
       pch = c(NA,NA,NA,"*","*"),
       lwd = c(2,2,2,NA,NA),
       lty = c(1,2,2,NA,NA),
       bty = "n",
       cex = 0.75)

#hist(aggmet2007_windspeed_matrix,
#breaks = bins,
#freq = FALSE,
#xlab="x",
#ylab="Probability",
#main="Probability density distribution",
#col = "gray")

#cat ("Press [enter] to continue")
#line <- readline()


#win.graph(12,7) # New graphing window...
#par(mfrow=c(1,2)) # ...with two panels.

# Use the 'qqnorm' function to produce a plot that compares 50 ordered 
# samples from our randNorm data with a theoretical normal distribution
qqnorm(aggmet2007_tmin_matrix[1:5000],
       ylim = c(-4,4),
       xlim = c(-4,4),
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles",
       main = "Probability plot comparing sampled and theoretical quantiles")

# Add 'qqline' to help diagnose whether the observed quantiles are from a 
# normal distribution. This uses two probability values (default = c(0.25,0.75))
# to define the line for a specified distribution (here a normal disribution).
qqline(aggmet2007_tmin_matrix[1:5000],datax = FALSE,
       distribution = qnorm, 
       probs = c(0.25,0.75))

cat ("Press [enter] to continue")
line <- readline()

win.graph(10,3.75) 
plot(aggmet2007_tmmx_matrix)

#bins = seq(0,20,1)
# Plot a histogram (frequency) distribution
#hist(aggmet2007_windspeed_matrix,      # Data to represented in the distribution
#breaks = bins, # Bins used to create histogram
#xlab = "Mean Max Temperature",    # Axis labels ...
#ylab = "Frequency Counts",
#main = "Frequency Distribution",# ...and title of plot (figure)
#col = "gray")  # color of bars in histogram plot


bins = seq(0,20,1) # As above for randNorm

par(mfrow=c(4,1))  #---par function sets number or rows and columns for a matrix for plots
plot(aggmet2007_tmmx_matrix)  #---plots aggmet2007 windspeed
plot(aggmet2007_tmmx_raster_transposed)

hist(aggmet2007_tmmx_matrix,breaks=bins,   #---plots aggmet2007 windspeed frequency
     col = "gray",
     xlab = "Wind Speed (unit)",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "2007 Wind Speed Frequency Distribution")

# Add summary statistics to plot
abline(v=aggmet2007_tmmx_matrix_mean,lwd=2)
abline(v=aggmet2007_tmmx_matrix_median,col = "red",lwd = 2,lty = "dashed")
abline(v= aggmet2007_tmmx_matrix_sd,col = "blue", lwd = 2,lty = "dashed")
points(aggmet2007_tmmx_matrix_min,100,pch = "*")
points(aggmet2007_tmmx_matrix_max,100,pch = "*")

# Add Legend
legend(8.5,1520,
       c("Mean","Median","St.Dev.","Min.","Max."),
       col = c("black","red","blue","black","black"),
       pch = c(NA,NA,NA,"*","*"),
       lwd = c(2,2,2,NA,NA),
       lty = c(1,2,2,NA,NA),
       bty = "n",
       cex = 0.75)

#hist(aggmet2007_windspeed_matrix,
#breaks = bins,
#freq = FALSE,
#xlab="x",
#ylab="Probability",
#main="Probability density distribution",
#col = "gray")

#cat ("Press [enter] to continue")
#line <- readline()


#win.graph(12,7) # New graphing window...
#par(mfrow=c(1,2)) # ...with two panels.

# Use the 'qqnorm' function to produce a plot that compares 50 ordered 
# samples from our randNorm data with a theoretical normal distribution
qqnorm(aggmet2007_tmmx_matrix[1:5000],
       ylim = c(-4,4),
       xlim = c(-4,4),
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles",
       main = "Probability plot comparing sampled and theoretical quantiles")

# Add 'qqline' to help diagnose whether the observed quantiles are from a 
# normal distribution. This uses two probability values (default = c(0.25,0.75))
# to define the line for a specified distribution (here a normal disribution).
qqline(aggmet2007_tmmx_matrix[1:5000],datax = FALSE,
       distribution = qnorm, 
       probs = c(0.25,0.75))

cat ("Press [enter] to continue")
line <- readline()


win.graph(10,3.75) 
plot(aggmet2007_minrh_matrix)

#bins = seq(0,20,1)
# Plot a histogram (frequency) distribution
#hist(aggmet2007_windspeed_matrix,      # Data to represented in the distribution
#breaks = bins, # Bins used to create histogram
#xlab = "Mean Max Temperature",    # Axis labels ...
#ylab = "Frequency Counts",
#main = "Frequency Distribution",# ...and title of plot (figure)
#col = "gray")  # color of bars in histogram plot


bins = seq(0,20,1) # As above for randNorm

par(mfrow=c(4,1))  #---par function sets number or rows and columns for a matrix for plots
plot(aggmet2007_minrh_matrix)  #---plots aggmet2007 windspeed
plot(aggmet2007_minrh_raster_transposed)

hist(aggmet2007_minrh_matrix,breaks=bins,   #---plots aggmet2007 windspeed frequency
     col = "gray",
     xlab = "Wind Speed (unit)",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "2007 Wind Speed Frequency Distribution")

# Add summary statistics to plot
abline(v=aggmet2007_minrh_matrix_mean,lwd=2)
abline(v=aggmet2007_minrh_matrix_median,col = "red",lwd = 2,lty = "dashed")
abline(v= aggmet2007_minrh_matrix_sd,col = "blue", lwd = 2,lty = "dashed")
points(aggmet2007_minrh_matrix_min,100,pch = "*")
points(aggmet2007_minrh_matrix_max,100,pch = "*")

# Add Legend
legend(8.5,1520,
       c("Mean","Median","St.Dev.","Min.","Max."),
       col = c("black","red","blue","black","black"),
       pch = c(NA,NA,NA,"*","*"),
       lwd = c(2,2,2,NA,NA),
       lty = c(1,2,2,NA,NA),
       bty = "n",
       cex = 0.75)

#hist(aggmet2007_windspeed_matrix,
#breaks = bins,
#freq = FALSE,
#xlab="x",
#ylab="Probability",
#main="Probability density distribution",
#col = "gray")

#cat ("Press [enter] to continue")
#line <- readline()


#win.graph(12,7) # New graphing window...
#par(mfrow=c(1,2)) # ...with two panels.

# Use the 'qqnorm' function to produce a plot that compares 50 ordered 
# samples from our randNorm data with a theoretical normal distribution
qqnorm(aggmet2007_minrh_matrix[1:5000],
       ylim = c(-4,4),
       xlim = c(-4,4),
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles",
       main = "Probability plot comparing sampled and theoretical quantiles")

# Add 'qqline' to help diagnose whether the observed quantiles are from a 
# normal distribution. This uses two probability values (default = c(0.25,0.75))
# to define the line for a specified distribution (here a normal disribution).
qqline(aggmet2007_minrh_matrix[1:5000],datax = FALSE,
       distribution = qnorm, 
       probs = c(0.25,0.75))

cat ("Press [enter] to continue")
line <- readline()

win.graph(10,3.75) 
plot(aggmet2007_maxrh_matrix)

#bins = seq(0,20,1)
# Plot a histogram (frequency) distribution
#hist(aggmet2007_windspeed_matrix,      # Data to represented in the distribution
#breaks = bins, # Bins used to create histogram
#xlab = "Mean Max Temperature",    # Axis labels ...
#ylab = "Frequency Counts",
#main = "Frequency Distribution",# ...and title of plot (figure)
#col = "gray")  # color of bars in histogram plot


bins = seq(0,20,1) # As above for randNorm

par(mfrow=c(4,1))  #---par function sets number or rows and columns for a matrix for plots
plot(aggmet2007_maxrh_matrix)  #---plots aggmet2007 windspeed
plot(aggmet2007_maxrh_raster_transposed)

hist(aggmet2007_maxrh_matrix,breaks=bins,   #---plots aggmet2007 windspeed frequency
     col = "gray",
     xlab = "Wind Speed (unit)",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "2007 Wind Speed Frequency Distribution")

# Add summary statistics to plot
abline(v=aggmet2007_maxrh_matrix_mean,lwd=2)
abline(v=aggmet2007_maxrh_matrix_median,col = "red",lwd = 2,lty = "dashed")
abline(v= aggmet2007_maxrh_matrix_sd,col = "blue", lwd = 2,lty = "dashed")
points(aggmet2007_maxrh_matrix_min,100,pch = "*")
points(aggmet2007_maxrh_matrix_max,100,pch = "*")

# Add Legend
legend(8.5,1520,
       c("Mean","Median","St.Dev.","Min.","Max."),
       col = c("black","red","blue","black","black"),
       pch = c(NA,NA,NA,"*","*"),
       lwd = c(2,2,2,NA,NA),
       lty = c(1,2,2,NA,NA),
       bty = "n",
       cex = 0.75)

#hist(aggmet2007_windspeed_matrix,
#breaks = bins,
#freq = FALSE,
#xlab="x",
#ylab="Probability",
#main="Probability density distribution",
#col = "gray")

#cat ("Press [enter] to continue")
#line <- readline()


#win.graph(12,7) # New graphing window...
#par(mfrow=c(1,2)) # ...with two panels.

# Use the 'qqnorm' function to produce a plot that compares 50 ordered 
# samples from our randNorm data with a theoretical normal distribution
qqnorm(aggmet2007_maxrh_matrix[1:5000],
       ylim = c(-4,4),
       xlim = c(-4,4),
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles",
       main = "Probability plot comparing sampled and theoretical quantiles")

# Add 'qqline' to help diagnose whether the observed quantiles are from a 
# normal distribution. This uses two probability values (default = c(0.25,0.75))
# to define the line for a specified distribution (here a normal disribution).
qqline(aggmet2007_maxrh_matrix[1:5000],datax = FALSE,
       distribution = qnorm, 
       probs = c(0.25,0.75))

cat ("Press [enter] to continue")
line <- readline()

win.graph(10,3.75) 
plot(aggmet2007_srad_matrix)

#bins = seq(0,20,1)
# Plot a histogram (frequency) distribution
#hist(aggmet2007_windspeed_matrix,      # Data to represented in the distribution
#breaks = bins, # Bins used to create histogram
#xlab = "Mean Max Temperature",    # Axis labels ...
#ylab = "Frequency Counts",
#main = "Frequency Distribution",# ...and title of plot (figure)
#col = "gray")  # color of bars in histogram plot


bins = seq(0,20,1) # As above for randNorm

par(mfrow=c(4,1))  #---par function sets number or rows and columns for a matrix for plots
plot(aggmet2007_srad_matrix)  #---plots aggmet2007 windspeed
plot(aggmet2007_srad_raster_transposed)

hist(aggmet2007_srad_matrix,breaks=bins,   #---plots aggmet2007 windspeed frequency
     col = "gray",
     xlab = "Wind Speed (unit)",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "2007 Wind Speed Frequency Distribution")

# Add summary statistics to plot
abline(v=aggmet2007_srad_matrix_mean,lwd=2)
abline(v=aggmet2007_srad_matrix_median,col = "red",lwd = 2,lty = "dashed")
abline(v= aggmet2007_srad_matrix_sd,col = "blue", lwd = 2,lty = "dashed")
points(aggmet2007_srad_matrix_min,100,pch = "*")
points(aggmet2007_srad_matrix_max,100,pch = "*")

# Add Legend
legend(8.5,1520,
       c("Mean","Median","St.Dev.","Min.","Max."),
       col = c("black","red","blue","black","black"),
       pch = c(NA,NA,NA,"*","*"),
       lwd = c(2,2,2,NA,NA),
       lty = c(1,2,2,NA,NA),
       bty = "n",
       cex = 0.75)

#hist(aggmet2007_windspeed_matrix,
#breaks = bins,
#freq = FALSE,
#xlab="x",
#ylab="Probability",
#main="Probability density distribution",
#col = "gray")

#cat ("Press [enter] to continue")
#line <- readline()


#win.graph(12,7) # New graphing window...
#par(mfrow=c(1,2)) # ...with two panels.

# Use the 'qqnorm' function to produce a plot that compares 50 ordered 
# samples from our randNorm data with a theoretical normal distribution
qqnorm(aggmet2007_srad_matrix[1:5000],
       ylim = c(-4,4),
       xlim = c(-4,4),
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles",
       main = "Probability plot comparing sampled and theoretical quantiles")

# Add 'qqline' to help diagnose whether the observed quantiles are from a 
# normal distribution. This uses two probability values (default = c(0.25,0.75))
# to define the line for a specified distribution (here a normal disribution).
qqline(aggmet2007_srad_matrix[1:5000],datax = FALSE,
       distribution = qnorm, 
       probs = c(0.25,0.75))

cat ("Press [enter] to continue")
line <- readline()


win.graph(5,9.75) 
plot(aggmet2007_windspeed_matrix)

#bins = seq(0,20,1)
# Plot a histogram (frequency) distribution
#hist(aggmet2007_windspeed_matrix,      # Data to represented in the distribution
#breaks = bins, # Bins used to create histogram
#xlab = "Mean Max Temperature",    # Axis labels ...
#ylab = "Frequency Counts",
#main = "Frequency Distribution",# ...and title of plot (figure)
#col = "gray")  # color of bars in histogram plot


bins = seq(0,20,1) # As above for randNorm

par(mfrow=c(4,1))  #---par function sets number or rows and columns for a matrix for plots
plot(aggmet2007_windspeed_matrix)  #---plots aggmet2007 windspeed
plot(aggmet2007_windspeed_raster_transposed)

hist(aggmet2007_windspeed_matrix,breaks=bins,   #---plots aggmet2007 windspeed frequency
     col = "gray",
     xlab = "Wind Speed (unit)",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "2007 Wind Speed Frequency Distribution")

# Add summary statistics to plot
abline(v=aggmet2007_windspeed_matrix_mean,lwd=2)
abline(v=aggmet2007_windspeed_matrix_median,col = "red",lwd = 2,lty = "dashed")
abline(v= aggmet2007_windspeed_matrix_sd,col = "blue", lwd = 2,lty = "dashed")
points(aggmet2007_windspeed_matrix_min,100,pch = "*")
points(aggmet2007_windspeed_matrix_max,100,pch = "*")

# Add Legend
legend(8.5,1520,
       c("Mean","Median","St.Dev.","Min.","Max."),
       col = c("black","red","blue","black","black"),
       pch = c(NA,NA,NA,"*","*"),
       lwd = c(2,2,2,NA,NA),
       lty = c(1,2,2,NA,NA),
       bty = "n",
       cex = 0.75)

#hist(aggmet2007_windspeed_matrix,
#breaks = bins,
#freq = FALSE,
#xlab="x",
#ylab="Probability",
#main="Probability density distribution",
#col = "gray")

#cat ("Press [enter] to continue")
#line <- readline()


#win.graph(12,7) # New graphing window...
#par(mfrow=c(1,2)) # ...with two panels.

# Use the 'qqnorm' function to produce a plot that compares 50 ordered 
# samples from our randNorm data with a theoretical normal distribution
qqnorm(aggmet2007_windspeed_matrix[1:5000],
       ylim = c(-4,4),
       xlim = c(-4,4),
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles",
       main = "Probability plot comparing sampled and theoretical quantiles")

# Add 'qqline' to help diagnose whether the observed quantiles are from a 
# normal distribution. This uses two probability values (default = c(0.25,0.75))
# to define the line for a specified distribution (here a normal disribution).
qqline(aggmet2007_windspeed_matrix[1:5000],datax = FALSE,
       distribution = qnorm, 
       probs = c(0.25,0.75))


