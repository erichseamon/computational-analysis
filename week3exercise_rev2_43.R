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


MET2007$dim$lon$vals -> lon
MET2007$dim$lat$vals -> lat
MET2007$dim$day$vals -> day

MET2007$var[[1]] -> maxairtemp

MET2007$var[[2]] -> maxrelhumdity

MET2007$var[[3]] -> minairtemp

MET2007$var[[4]] -> minrelhumidity

MET2007$var[[5]] -> precipamount

MET2007$var[[6]] -> precipduration

MET2007$var[[7]] -> solarradiation

MET2007$var[[8]] -> windspeed

#get.var.ncdf(MET2007, maxairtemp) -> maxat
#get.var.ncdf(MET2007, minairtemp) -> minat
#get.var.ncdf(MET2007, maxrelhumdity) -> maxrh
#get.var.ncdf(MET2007, minrelhumidity) -> minrh
#get.var.ncdf(MET2007, precipamount) -> pramt
#get.var.ncdf(MET2007, precipduration) -> prdur
#get.var.ncdf(MET2007, solarradiation) -> srad
#get.var.ncdf(MET2007, windspeed) -> windspd
#get.var.ncdf(MET2007, lat) -> lat
#get.var.ncdf(MET2007, lon) -> lon
#get.var.ncdf(MET2007, day) -> day

aggmet2007_tmmx_raster <- raster(aggmet2007.nc, varname="max_air_temperature")
aggmet2007_tmmx_matrix <- as.matrix(aggmet2007_tmmx_raster)
aggmet2007_tmmx_raster_transposed <- t(aggmet2007_tmmx_raster)
aggmet2007_tmmx_matrix_mean <- mean(aggmet2007_tmmx_matrix)
aggmet2007_tmmx_matrix_median <- median(aggmet2007_tmmx_matrix)
aggmet2007_tmmx_matrix_sd <- sd(aggmet2007_tmmx_matrix)
aggmet2007_tmmx_matrix_min <- min(aggmet2007_tmmx_matrix)
aggmet2007_tmmx_matrix_max <- max(aggmet2007_tmmx_matrix)

aggmet2007_tmin_raster <- raster(aggmet2007.nc, varname="min_air_temperature")
aggmet2007_tmin_matrix <- as.matrix(aggmet2007_tmin_raster)
aggmet2007_tmin_raster_transposed <- t(aggmet2007_tmin_raster)
aggmet2007_tmin_matrix_mean <- mean(aggmet2007_tmin_matrix)
aggmet2007_tmin_matrix_median <- median(aggmet2007_tmin_matrix)
aggmet2007_tmin_matrix_sd <- sd(aggmet2007_tmin_matrix)
aggmet2007_tmin_matrix_min <- min(aggmet2007_tmin_matrix)
aggmet2007_tmin_matrix_max <- max(aggmet2007_tmin_matrix)

aggmet2007_minrh_raster <- raster(aggmet2007.nc, varname="min_relative_humidity")
aggmet2007_minrh_matrix <- as.matrix(aggmet2007_minrh_raster)
aggmet2007_minrh_raster_transposed <- t(aggmet2007_minrh_raster)
aggmet2007_minrh_matrix_mean <- mean(aggmet2007_minrh_matrix)
aggmet2007_minrh_matrix_median <- median(aggmet2007_minrh_matrix)
aggmet2007_minrh_matrix_sd <- sd(aggmet2007_minrh_matrix)
aggmet2007_minrh_matrix_min <- min(aggmet2007_minrh_matrix)
aggmet2007_minrh_matrix_max <- max(aggmet2007_minrh_matrix)

aggmet2007_maxrh_raster <- raster(aggmet2007.nc, varname="max_relative_humidity")
aggmet2007_maxrh_matrix <- as.matrix(aggmet2007_maxrh_raster)
aggmet2007_maxrh_raster_transposed <- t(aggmet2007_maxrh_raster)
aggmet2007_maxrh_matrix_mean <- mean(aggmet2007_maxrh_matrix)
aggmet2007_maxrh_matrix_median <- median(aggmet2007_maxrh_matrix)
aggmet2007_maxrh_matrix_sd <- sd(aggmet2007_maxrh_matrix)
aggmet2007_maxrh_matrix_min <- min(aggmet2007_maxrh_matrix)
aggmet2007_maxrh_matrix_max <- max(aggmet2007_maxrh_matrix)

aggmet2007_windspeed_raster <- raster(aggmet2007.nc, varname="wind_speed")
aggmet2007_windspeed_matrix <- as.matrix(aggmet2007_windspeed_raster)
aggmet2007_windspeed_raster_transposed <- t(aggmet2007_windspeed_raster)
aggmet2007_windspeed_matrix_mean <- mean(aggmet2007_windspeed_matrix)
aggmet2007_windspeed_matrix_median <- median(aggmet2007_windspeed_matrix)
aggmet2007_windspeed_matrix_sd <- sd(aggmet2007_windspeed_matrix)
aggmet2007_windspeed_matrix_min <- min(aggmet2007_windspeed_matrix)
aggmet2007_windspeed_matrix_max <- max(aggmet2007_windspeed_matrix)

aggmet2007_srad_raster <- raster(aggmet2007.nc, varname="surface_downwelling_shortwave_flux_in_air")
aggmet2007_srad_matrix <- as.matrix(aggmet2007_srad_raster)
aggmet2007_srad_raster_transposed <- t(aggmet2007_srad_raster)
aggmet2007_srad_matrix_mean <- mean(aggmet2007_srad_matrix)
aggmet2007_srad_matrix_median <- median(aggmet2007_srad_matrix)
aggmet2007_srad_matrix_sd <- sd(aggmet2007_srad_matrix)
aggmet2007_srad_matrix_min <- min(aggmet2007_srad_matrix)
aggmet2007_srad_matrix_max <- max(aggmet2007_srad_matrix)




win.graph(10,3.75) 
plot(aggmet2007_tmin_matrix)

cat ("Press [enter] to continue")
line <- readline()

win.graph(10,3.75) 
plot(aggmet2007_tmmx_matrix)

cat ("Press [enter] to continue")
line <- readline()


win.graph(10,3.75) 
plot(aggmet2007_minrh_matrix)

cat ("Press [enter] to continue")
line <- readline()

win.graph(10,3.75) 
plot(aggmet2007_maxrh_matrix)

cat ("Press [enter] to continue")
line <- readline()

#-----------2007 solar radiation -------------------#

win.graph(5,9.75)
#plot(aggmet2007_srad_matrix)

bins = seq(0,100,1) # As above for randNorm

par(mfrow=c(4,1))  #---par function sets number or rows and columns for a matrix for plots
plot(MET2007$dim$day$vals~MET2007$dim$wind_speed$vals,  #---plots aggmet2007 solar radiation
    xlab = "solar radiation (unit)",
    ylab = "Count",
    #ylim = c(0,230),
    #xlim = c(-0.5,7),
    main = "2007 Solar Radiation Over a Daily Timestep")
plot(aggmet2007_srad_raster_transposed,
    xlab = "solar radiation (unit)",
    ylab = "Count",
    #ylim = c(0,230),
    #xlim = c(-0.5,7),
    main = "2007 Solar Radiation Thematic Map")
hist(aggmet2007_srad_matrix,breaks=bins,   #---plots aggmet2007 windspeed frequency
     col = "gray",
     xlab = "solar radiation (unit)",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "2007 Solar Radiation Frequency Distribution")

# Add summary statistics to plot
abline(v=aggmet2007_srad_matrix_mean,lwd=2)
abline(v=aggmet2007_srad_matrix_median,col = "red",lwd = 2,lty = "dashed")
abline(v= aggmet2007_srad_matrix_sd,col = "blue", lwd = 2,lty = "dashed")
points(aggmet2007_srad_matrix_min,100,pch = "*")
points(aggmet2007_srad_matrix_max,100,pch = "*")

# Add Legend
legend(50.5,1000,
       c("Mean","Median","St.Dev.","Min.","Max."),
       col = c("black","red","blue","black","black"),
       pch = c(NA,NA,NA,"*","*"),
       lwd = c(2,2,2,NA,NA),
       lty = c(1,2,2,NA,NA),
       bty = "n",
       cex = 0.75)


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

#-----------2007 solar radiation -------------------#

win.graph(5,9.75) 
#plot(aggmet2007_windspeed_matrix)

bins = seq(0,20,1) # As above for randNorm

par(mfrow=c(4,1))  #---par function sets number or rows and columns for a matrix for plots
plot(aggmet2007_windspeed_matrix,  #---plots aggmet2007 windspeed
    xlab = "wind speed (unit)",
    ylab = "Count",
    #ylim = c(0,230),
    #xlim = c(-0.5,7),
    main = "2007 Wind Speed Over a Daily Timestep")
plot(aggmet2007_windspeed_raster_transposed,
    xlab = "wind speed (unit)",
    ylab = "Count",
    #ylim = c(0,230),
    #xlim = c(-0.5,7),
    main = "2007 Wind Speed Thematic Map")
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
legend(8.5,3500,
       c("Mean","Median","St.Dev.","Min.","Max."),
       col = c("black","red","blue","black","black"),
       pch = c(NA,NA,NA,"*","*"),
       lwd = c(2,2,2,NA,NA),
       lty = c(1,2,2,NA,NA),
       bty = "n",
       cex = 0.75)


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
