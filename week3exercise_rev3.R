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
setwd("X:/Dropbox/ES Research/ES Classwork/FOR504/data/")   #----set the working directory

#----set packages----#

library("ncdf")
library("raster")
library("sp")
library("rgeos")
library("rgdal")
library("proj4")
library("RNetCDF")
library("ncdf4")
library("RColorBrewer")
library("raster")
library("rasterVis")
library("latticeExtra")
library("maptools")


#-----Setting variables that are associated with input datasets 2007-2011------#

yearspan <- c(2007,2008,2009,2010,2011)
variablespan <- c("tmmx", "tmin", "minrh", "maxrh", "pr","pdur", "windspeed", "srad")
rasterspan <- c("raster", "matrix", "raster_transposed", "matrix_mean", "matrix_median", "matrix_sd", "matrix_min", "matrix_max")

for (i in yearspan) {
  for (j in variablespan) {
    dirname <- "X:/Dropbox/ES Research/ES Classwork/FOR504/data"
    aggmetnc <- file.path(paste("aggmet", i, "_", j, ".nc", sep = ""))
    agmetfullname <- file.path(dirname, paste("_agg_met_", j, "_", i, "_WUSA.nc", sep = ""))
    aggmetnc <- agmetfullname 
    metvarname <- file.path(paste("MET", i, "_", j, sep = ""))
    metvar <- open.ncdf(aggmetnc, write=FALSE)# Open a netcdf file 
    names(metvar)<- paste("MET", i, "_", j, sep = "")
      aggmet_raster <- raster(aggmetnc, layer=1)
      aggmet_matrix <- as.matrix(aggmet_raster)
      aggmet_raster_transposed <- t(aggmet_raster)
      aggmet_matrix_mean <- mean(aggmet_matrix)
      aggmet_matrix_median <- median(aggmet_matrix)
      aggmet_matrix_sd <- sd(aggmet_matrix)
      aggmet_matrix_min <- min(aggmet_matrix)
      aggmet_matrix_max <- max(aggmet_matrix)
      assign(paste("aggmet", i, "_", j, "_", "raster", sep = ""),aggmet_raster)
      assign(paste("aggmet", i, "_", j, "_", "matrix",  sep = ""),aggmet_matrix)
      assign(paste("aggmet", i, "_", j, "_", "raster_transposed", sep = ""),aggmet_raster_transposed)
      assign(paste("aggmet", i, "_", j, "_", "matrix_mean", sep = ""),aggmet_matrix_mean)
      assign(paste("aggmet", i, "_", j, "_", "matrix_median", sep = ""),aggmet_matrix_median)
      assign(paste("aggmet", i, "_", j, "_", "matrix_sd", sep = ""),aggmet_matrix_sd)
      assign(paste("aggmet", i, "_", j, "_", "matrix_min", sep = ""),aggmet_matrix_min)
      assign(paste("aggmet", i, "_", j, "_", "matrix_max", sep = ""),aggmet_matrix_max)
    
  
  }
  end
}
end




aggmet2007.nc = "https://www.reacchpna.org/reacchspace/obj1/netcdf/MET/tmmx/tmmx_2007.nc"
MET2007 <- open.ncdf(aggmet2007.nc, write=FALSE)# Open a netcdf file
#aggmet2008.nc = "X:/Dropbox/ES Research/ES Classwork/FOR504/data/_agg_met_2008_WUSA.nc"
#MET2008 <- open.ncdf(aggmet2008.nc, write=FALSE)# Open a netcdf file
#aggmet2009.nc = "X:/Dropbox/ES Research/ES Classwork/FOR504/data/_agg_met_2009_WUSA.nc"
#MET2009 <- open.ncdf(aggmet2009.nc, write=FALSE)# Open a netcdf file
#aggmet2010.nc = "X:/Dropbox/ES Research/ES Classwork/FOR504/data/_agg_met_2010_WUSA.nc"
#MET2010 <- open.ncdf(aggmet2010.nc, write=FALSE)# Open a netcdf file
#aggmet2011.nc = "X:/Dropbox/ES Research/ES Classwork/FOR504/data/_agg_met_2011_WUSA.nc"
#MET2011 <- open.ncdf(aggmet2011.nc, write=FALSE)# Open a netcdf file

#aggmet2007_tmmx.nc <- "X:/Dropbox/ES Research/ES Classwork/FOR504/data/_agg_met_tmmx_2007_WUSA.nc"
#MET2007_tmmx <- open.ncdf(aggmet2007_tmmx.nc, write=FALSE)# Open a netcdf file
#aggmet2007_tmin.nc <- "X:/Dropbox/ES Research/ES Classwork/FOR504/data/_agg_met_tmin_2007_WUSA.nc"
#MET2007_tmin <- open.ncdf(aggmet2007_tmin.nc, write=FALSE)# Open a netcdf file
#aggmet2007_minrh.nc <- "X:/Dropbox/ES Research/ES Classwork/FOR504/data/_agg_met_minrh_2007_WUSA.nc"
#MET2007_minrh <- open.ncdf(aggmet2007_minrh.nc, write=FALSE)# Open a netcdf file
#aggmet2007_maxrh.nc <- "X:/Dropbox/ES Research/ES Classwork/FOR504/data/_agg_met_maxrh_2007_WUSA.nc"
#MET2007_maxrh <- open.ncdf(aggmet2007_maxrh.nc, write=FALSE)# Open a netcdf file
#aggmet2007_pr.nc <- "X:/Dropbox/ES Research/ES Classwork/FOR504/data/_agg_met_pr_2007_WUSA.nc" 
#MET2007_pr <- open.ncdf(aggmet2007_pr.nc, write=FALSE)# Open a netcdf file
#aggmet2007_pdur.nc <- "X:/Dropbox/ES Research/ES Classwork/FOR504/data/_agg_met_pdur_2007_WUSA.nc"
#MET2007_pdur <- open.ncdf(aggmet2007_pdur.nc, write=FALSE)# Open a netcdf file
#aggmet2007_windspeed.nc <- "X:/Dropbox/ES Research/ES Classwork/FOR504/data/_agg_met_windspeed_2007_WUSA.nc"
#MET2007_windspeed <- open.ncdf(aggmet2007_windspeed.nc, write=FALSE)# Open a netcdf file
#aggmet2007_srad.nc <- "X:/Dropbox/ES Research/ES Classwork/FOR504/data/_agg_met_srad_2007_WUSA.nc"
#MET2007_srad <- open.ncdf(aggmet2007_srad.nc, write=FALSE)# Open a netcdf file

#------2007 variables---------------------------------#

#MET2007$dim$lon$vals -> lon
#MET2007$dim$lat$vals -> lat
#MET2007$dim$day$vals -> day

#MET2007$var[[1]] -> maxairtemp
#MET2007$var[[2]] -> maxrelhumdity
#MET2007$var[[3]] -> minairtemp
#MET2007$var[[4]] -> minrelhumidity
#MET2007$var[[5]] -> precipamount
#MET2007$var[[6]] -> precipduration
#MET2007$var[[7]] -> solarradiation
#MET2007$var[[8]] -> windspeed

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

#----------raster generation for 2007------------------#


#aggmet2007_tmmx_raster <- raster(aggmet2007_tmmx.nc, layer=1, varname="max_air_temperature")
#aggmet2007_tmmx_matrix <- as.matrix(aggmet2007_tmmx_raster)
#aggmet2007_tmmx_raster_transposed <- t(aggmet2007_tmmx_raster)
#aggmet2007_tmmx_matrix_mean <- mean(aggmet2007_tmmx_matrix)
#aggmet2007_tmmx_matrix_median <- median(aggmet2007_tmmx_matrix)
#aggmet2007_tmmx_matrix_sd <- sd(aggmet2007_tmmx_matrix)
#aggmet2007_tmmx_matrix_min <- min(aggmet2007_tmmx_matrix)
#aggmet2007_tmmx_matrix_max <- max(aggmet2007_tmmx_matrix)

#aggmet2007_tmin_raster <- raster(aggmet2007_tmin.nc, layer=2, varname="min_air_temperature")
#aggmet2007_tmin_matrix <- as.matrix(aggmet2007_tmin_raster)
#aggmet2007_tmin_raster_transposed <- t(aggmet2007_tmin_raster)
#aggmet2007_tmin_matrix_mean <- mean(aggmet2007_tmin_matrix)
#aggmet2007_tmin_matrix_median <- median(aggmet2007_tmin_matrix)
#aggmet2007_tmin_matrix_sd <- sd(aggmet2007_tmin_matrix)
#aggmet2007_tmin_matrix_min <- min(aggmet2007_tmin_matrix)
#aggmet2007_tmin_matrix_max <- max(aggmet2007_tmin_matrix)

#aggmet2007_minrh_raster <- raster(aggmet2007_minrh.nc, layer=3, varname="min_relative_humidity")
#aggmet2007_minrh_matrix <- as.matrix(aggmet2007_minrh_raster)
#aggmet2007_minrh_raster_transposed <- t(aggmet2007_minrh_raster)
#aggmet2007_minrh_matrix_mean <- mean(aggmet2007_minrh_matrix)
#aggmet2007_minrh_matrix_median <- median(aggmet2007_minrh_matrix)
#aggmet2007_minrh_matrix_sd <- sd(aggmet2007_minrh_matrix)
#aggmet2007_minrh_matrix_min <- min(aggmet2007_minrh_matrix)
#aggmet2007_minrh_matrix_max <- max(aggmet2007_minrh_matrix)

#aggmet2007_maxrh_raster <- raster(aggmet2007_maxrh.nc, layer=4, varname="max_relative_humidity")
#aggmet2007_maxrh_matrix <- as.matrix(aggmet2007_maxrh_raster)
#aggmet2007_maxrh_raster_transposed <- t(aggmet2007_maxrh_raster)
#aggmet2007_maxrh_matrix_mean <- mean(aggmet2007_maxrh_matrix)
#aggmet2007_maxrh_matrix_median <- median(aggmet2007_maxrh_matrix)
#aggmet2007_maxrh_matrix_sd <- sd(aggmet2007_maxrh_matrix)
#aggmet2007_maxrh_matrix_min <- min(aggmet2007_maxrh_matrix)
#aggmet2007_maxrh_matrix_max <- max(aggmet2007_maxrh_matrix)
#
#aggmet2007_windspeed_raster <- raster(aggmet2007_windspeed.nc, layer=5, varname="wind_speed")
#aggmet2007_windspeed_matrix <- as.matrix(aggmet2007_windspeed_raster)
#aggmet2007_windspeed_raster_transposed <- t(aggmet2007_windspeed_raster)
#aggmet2007_windspeed_matrix_mean <- mean(aggmet2007_windspeed_matrix)
#aggmet2007_windspeed_matrix_median <- median(aggmet2007_windspeed_matrix)
#aggmet2007_windspeed_matrix_sd <- sd(aggmet2007_windspeed_matrix)
#aggmet2007_windspeed_matrix_min <- min(aggmet2007_windspeed_matrix)
#aggmet2007_windspeed_matrix_max <- max(aggmet2007_windspeed_matrix)

#aggmet2007_srad_raster <- raster(aggmet2007_srad.nc, layer=6, varname="surface_downwelling_shortwave_flux_in_air")
#aggmet2007_srad_matrix <- as.matrix(aggmet2007_srad_raster)
#aggmet2007_srad_raster_transposed <- t(aggmet2007_srad_raster)
#aggmet2007_srad_matrix_mean <- mean(aggmet2007_srad_matrix)
#aggmet2007_srad_matrix_median <- median(aggmet2007_srad_matrix)
#aggmet2007_srad_matrix_sd <- sd(aggmet2007_srad_matrix)
#aggmet2007_srad_matrix_min <- min(aggmet2007_srad_matrix)
#aggmet2007_srad_matrix_max <- max(aggmet2007_srad_matrix)

#aggmet2007_brick <- brick(aggmet2007_tmmx_raster_transposed,aggmet2007_tmin_raster_transposed,aggmet2007_minrh_raster_transposed,aggmet2007_maxrh_raster_transposed,aggmet2007_srad_raster_transposed,aggmet2007_windspeed_raster_transposed)

#----------------Plotting---------------------#

#-----------2007 Min Air Temperature - Plotting -------------------#

win.graph(7,11) 
#plot(aggmet2007_tmin_matrix)

bins = seq(240,300,1) # defining the bin sequences

par(mfrow=c(3,1))  #---par function sets number or rows and columns for a matrix for plots
boxplot(cbind(aggmet2007_tmin_matrix), #boxplot for min air temperature values across the range of the raster study area.
        ylab = "Min Air Temperature",
        xlab = "raster pixel cell range across study area",
        notch = FALSE,
        main = "2007 Min Air Temperature (Celsius) Boxplot")


#plot(MET2007$dim$day$vals~MET2007$dim$day$vals,  #---TEST plots aggmet2007 min air temperature
      #xlab = "min air temperature (unit)",
      #ylab = "Count",
      #ylim = c(0,230),
      #xlim = c(-0.5,7),
      #main = "2007 Min Air Temperature Over a Daily Timestep")

#plot(aggmet2007_tmin_raster_transposed, #---plots aggmet2007 min air temperature raster: NOTE: needs fixed - ES09102014
     #xlab = "min air temperature (unit)",
     #ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     #main = "2007 Min Air Temperature (Celsius) Thematic Map")
hist(aggmet2007_tmin_matrix,breaks=bins,   #---plots aggmet2007 min air temperature histogram: NOTE: needs fixed - ES09102014
     col = "gray",
     xlab = "min air temperature (Celsius)",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "2007 Min Air Temperature Frequency Distribution")

# Add summary statistics to plot
abline(v=aggmet2007_tmin_matrix_mean,lwd=2)
abline(v=aggmet2007_tmin_matrix_median,col = "red",lwd = 2,lty = "dashed")
abline(v= aggmet2007_tmin_matrix_sd,col = "blue", lwd = 2,lty = "dashed")
points(aggmet2007_tmin_matrix_min,100,pch = "*")
points(aggmet2007_tmin_matrix_max,100,pch = "*")

# Add Legend
legend(290.5,1700,
       c("Mean","Median","St.Dev.","Min.","Max."),
       col = c("black","red","blue","black","black"),
       pch = c(NA,NA,NA,"*","*"),
       lwd = c(2,2,2,NA,NA),
       lty = c(1,2,2,NA,NA),
       bty = "n",
       cex = 0.75)

# Use the 'qqnorm' function to produce a plot that compares 50 ordered 
# samples from our randNorm data with a theoretical normal distribution
qqnorm(aggmet2007_tmin_matrix[1:5000],
       ylim = c(240,300),
       xlim = c(-4,6),
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles",
       main = "2007 Min Air Temperature Probability plot - sampled and theoretical quantiles")

# Add 'qqline' to help diagnose whether the observed quantiles are from a 
# normal distribution. This uses two probability values (default = c(0.25,0.75))
# to define the line for a specified distribution (here a normal disribution).
qqline(aggmet2007_tmin_matrix[1:5000],datax = FALSE,
       distribution = qnorm, 
       probs = c(0.25,0.75))

cat ("Press [enter] to continue")
line <- readline()

#---------------2007 Max Air Temperature - Plotting -------------------#

win.graph(7,11) 
#plot(aggmet2007_tmax_matrix)

bins = seq(230,330,1) # defining the bin sequences

par(mfrow=c(3,1))  #---par function sets number or rows and columns for a matrix for plots
boxplot(cbind(aggmet2007_tmmx_matrix), #boxplot for max air temperature values across the range of the raster study area.
        ylab = "Max Air Temperature (Celsius)",
        xlab = "raster pixel cell range across study area",
        notch = FALSE,
        main = "2007 Max Air Temperature (Celsius) Boxplot")


#plot(MET2007$dim$day$vals~MET2007$dim$day$vals,  #---plots aggmet2007 max air temperature
#xlab = "max air temperature (unit)",
#ylab = "Count",
#ylim = c(0,230),
#xlim = c(-0.5,7),
#main = "2007 Max Air Temperature Over a Daily Timestep")
#plot(aggmet2007_tmmx_raster_transposed,  #---plots aggmet2007 max air temperature raster: NOTE: needs fixed - ES09102014
     #xlab = "max air temperature (unit)",
     #ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     #main = "2007 Max Air Temperature Thematic Map")
hist(aggmet2007_tmmx_matrix,breaks=bins,   #---plots aggmet2007 max air temperature
     col = "gray",
     xlab = "max air temperature (Celsius)",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "2007 Max Air Temperature (Celsious) Frequency Distribution")

# Add summary statistics to plot
abline(v=aggmet2007_tmmx_matrix_mean,lwd=2)
abline(v=aggmet2007_tmmx_matrix_median,col = "red",lwd = 2,lty = "dashed")
abline(v= aggmet2007_tmmx_matrix_sd,col = "blue", lwd = 2,lty = "dashed")
points(aggmet2007_tmmx_matrix_min,100,pch = "*")
points(aggmet2007_tmmx_matrix_max,100,pch = "*")

# Add Legend
legend(315.5,1700,
       c("Mean","Median","St.Dev.","Min.","Max."),
       col = c("black","red","blue","black","black"),
       pch = c(NA,NA,NA,"*","*"),
       lwd = c(2,2,2,NA,NA),
       lty = c(1,2,2,NA,NA),
       bty = "n",
       cex = 0.75)

# Use the 'qqnorm' function to produce a plot that compares 50 ordered 
# samples from our randNorm data with a theoretical normal distribution
qqnorm(aggmet2007_tmmx_matrix[1:5000],
       ylim = c(240,300),
       xlim = c(-4,6),
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles",
       main = "2007 Max Air Temperature Probability plot - sampled and theoretical quantiles")

# Add 'qqline' to help diagnose whether the observed quantiles are from a 
# normal distribution. This uses two probability values (default = c(0.25,0.75))
# to define the line for a specified distribution (here a normal disribution).
qqline(aggmet2007_tmmx_matrix[1:5000],datax = FALSE,
       distribution = qnorm, 
       probs = c(0.25,0.75))

cat ("Press [enter] to continue")
line <- readline()

#-----------2007 Min Relative Humidity - Plotting -------------------#

win.graph(7,11) 
plot(aggmet2007_minrh_matrix)

bins = seq(0,120,1) # defining the bin sequences

par(mfrow=c(4,1))  #---par function sets number or rows and columns for a matrix for plots
boxplot(cbind(aggmet2007_minrh_matrix), #boxplot for min rel humidity values across the range of the raster study area.
        ylab = "Min Relative Humidity (%)",
        xlab = "raster pixel cell range across study area",
        notch = FALSE,
        main = "2007 Relative Humidity Boxplot")


#plot(MET2007$dim$day$vals~MET2007$dim$day$vals,  #---plots aggmet2007 min rel humidity
#xlab = "min relative humidity (%)",
#ylab = "Count",
#ylim = c(0,230),
#xlim = c(-0.5,7),
#main = "2007 Min Relative Humidity Over a Daily Timestep")
plot(aggmet2007_minrh_raster_transposed,
     xlab = "min relative humidity (%)",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "2007 Min Relative Humidity Thematic Map")
hist(aggmet2007_minrh_matrix,breaks=bins,   #---plots aggmet2007 min rel humidity  frequency
     col = "gray",
     xlab = "min relative humidity (%)",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "2007 Min Relative Humidity Frequency Distribution")

# Add summary statistics to plot
abline(v=aggmet2007_minrh_matrix_mean,lwd=2)
abline(v=aggmet2007_minrh_matrix_median,col = "red",lwd = 2,lty = "dashed")
abline(v= aggmet2007_minrh_matrix_sd,col = "blue", lwd = 2,lty = "dashed")
points(aggmet2007_minrh_matrix_min,100,pch = "*")
points(aggmet2007_minrh_matrix_max,100,pch = "*")

# Add Legend
legend(80.5,600,
       c("Mean","Median","St.Dev.","Min.","Max."),
       col = c("black","red","blue","black","black"),
       pch = c(NA,NA,NA,"*","*"),
       lwd = c(2,2,2,NA,NA),
       lty = c(1,2,2,NA,NA),
       bty = "n",
       cex = 0.75)

# Use the 'qqnorm' function to produce a plot that compares 50 ordered 
# samples from our randNorm data with a theoretical normal distribution
qqnorm(aggmet2007_minrh_matrix[1:5000],
       ylim = c(-0,100),
       xlim = c(-4,6),
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles",
       main = "2007 Min Relative Humidity Probability plot - sampled and theoretical quantiles")

# Add 'qqline' to help diagnose whether the observed quantiles are from a 
# normal distribution. This uses two probability values (default = c(0.25,0.75))
# to define the line for a specified distribution (here a normal disribution).
qqline(aggmet2007_minrh_matrix[1:5000],datax = FALSE,
       distribution = qnorm, 
       probs = c(0.25,0.75))

cat ("Press [enter] to continue")
line <- readline()

#-----------2007 Max Relative Humidity - Plotting -------------------#

win.graph(7,11) 
#plot(aggmet2007_maxrh_matrix)

bins = seq(20,120,1) # defining the bin sequences

par(mfrow=c(4,1))  #---par function sets number or rows and columns for a matrix for plots
boxplot(cbind(aggmet2007_maxrh_matrix),  #boxplot for max rel humidity values across the range of the raster study area.
        ylab = "Max Relative Humidity (%)",
        xlab = "raster pixel cell range across study area",
        notch = FALSE,
        main = "2007 Max Relative Humidity Boxplot")


#plot(MET2007$dim$day$vals~MET2007$dim$day$vals,  #---plots aggmet2007 max rel humidity
#xlab = "max relative humidity (%)",
#ylab = "Count",
#ylim = c(0,230),
#xlim = c(-0.5,7),
#main = "2007 Max Relative Humidity Over a Daily Timestep")
plot(aggmet2007_maxrh_raster_transposed,
     xlab = "max relative humidity (%)",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "2007 Max Relative Humidity Thematic Map")
hist(aggmet2007_maxrh_matrix,breaks=bins,   #---plots aggmet2007 windspeed frequency
     col = "gray",
     xlab = "max relative humidity (%)",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "2007 Max Relative Humidity Frequency Distribution")

# Add summary statistics to plot
abline(v=aggmet2007_maxrh_matrix_mean,lwd=2)
abline(v=aggmet2007_maxrh_matrix_median,col = "red",lwd = 2,lty = "dashed")
abline(v= aggmet2007_maxrh_matrix_sd,col = "blue", lwd = 2,lty = "dashed")
points(aggmet2007_maxrh_matrix_min,100,pch = "*")
points(aggmet2007_maxrh_matrix_max,100,pch = "*")

# Add Legend
legend(70.5,2500,
       c("Mean","Median","St.Dev.","Min.","Max."),
       col = c("black","red","blue","black","black"),
       pch = c(NA,NA,NA,"*","*"),
       lwd = c(2,2,2,NA,NA),
       lty = c(1,2,2,NA,NA),
       bty = "n",
       cex = 0.75)

# Use the 'qqnorm' function to produce a plot that compares 5000 ordered 
# samples from the Max Rel Humidity matrix with a theoretical normal distribution
qqnorm(aggmet2007_maxrh_matrix[1:5000],
       ylim = c(-0,200),
       xlim = c(-4,5),
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles",
       main = "2007 Max Relative Humidity Probability plot - sampled and theoretical quantiles")

# Add 'qqline' to help diagnose whether the observed quantiles are from a 
# normal distribution. This uses two probability values (default = c(0.25,0.75))
# to define the line for a specified distribution (here a normal disribution).
qqline(aggmet2007_maxrh_matrix[1:5000],datax = FALSE,
       distribution = qnorm, 
       probs = c(0.25,0.75))

cat ("Press [enter] to continue")
line <- readline()

#-----------2007 Solar Radiation -------------------#

win.graph(7,11)
#plot(aggmet2007_srad_matrix)

bins = seq(0,100,1) # defining the bin sequences

par(mfrow=c(4,1))  #---par function sets number or rows and columns for a matrix for plots
boxplot(cbind(aggmet2007_srad_matrix), #boxplot for solar radiation values across the range of the raster study area.
        ylab = "Solar Radiation (w/m2)",
        xlab = "raster pixel cell range across study area",
        notch = FALSE,
        main = "2007 Solar Radiation Boxplot")


#plot(MET2007$dim$day$vals~MET2007$dim$day$vals,  #---plots aggmet2007 solar radiation
    #xlab = "solar radiation (W/m2)",
    #ylab = "Count",
    #ylim = c(0,230),
    #xlim = c(-0.5,7),
    #main = "2007 Solar Radiation Over a Daily Timestep")
plot(aggmet2007_srad_raster_transposed,
    xlab = "solar radiation (W/m2)",
    ylab = "Count",
    #ylim = c(0,230),
    #xlim = c(-0.5,7),
    main = "2007 Solar Radiation Thematic Map")
hist(aggmet2007_srad_matrix,breaks=bins,   #---plots aggmet2007 windspeed frequency
     col = "gray",
     xlab = "solar radiation (W/m2)",
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
       ylim = c(-4,100),
       xlim = c(-4,5),
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles",
       main = "2007 Solar Radiation Probability plot - sampled and theoretical quantiles")

# Add 'qqline' to help diagnose whether the observed quantiles are from a 
# normal distribution. This uses two probability values (default = c(0.25,0.75))
# to define the line for a specified distribution (here a normal disribution).
qqline(aggmet2007_srad_matrix[1:5000],datax = FALSE,
       distribution = qnorm, 
       probs = c(0.25,0.75))


cat ("Press [enter] to continue")
line <- readline()

#-----------2007 Wind Speed-------------------#

win.graph(7,11) 
#plot(aggmet2007_windspeed_matrix)

bins = seq(0,12,1) # defining the bin sequences

par(mfrow=c(4,1))  #---par function sets number or rows and columns for a matrix for plots
boxplot(cbind(aggmet2007_windspeed_matrix),  #boxplot for wind speed values across the range of the raster study area.
        ylab = "Wind Speed (m-s-1)",
        xlab = "raster pixel cell range across study area",
        notch = FALSE,
        main = "2007 Wind Speed at 10m Boxplot")

#plot(aggmet2007_windspeed_matrix,  #---plots aggmet2007 windspeed
    #xlab = "wind speed (m-s-1)",
    #ylab = "Count",
    #ylim = c(0,230),
    #xlim = c(-0.5,7),
    #main = "2007 Wind Speed Over a Daily Timestep")
plot(aggmet2007_windspeed_raster_transposed,
    xlab = "wind speed (m-s-1)",
    ylab = "Count",
    #ylim = c(0,230),
    #xlim = c(-0.5,7),
    main = "2007 Wind Speed at 10m Thematic Map")
hist(aggmet2007_windspeed_matrix,breaks=bins,   #---plots aggmet2007 windspeed frequency
     col = "gray",
     xlab = "Wind Speed (m-s-1)",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "2007 Wind Speed at 10m - Frequency Distribution")

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
       ylim = c(0,15),
       xlim = c(-4,5),
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles",
       main = "2007 Wind Speed Probability plot - sampled and theoretical quantiles")

# Add 'qqline' to help diagnose whether the observed quantiles are from a 
# normal distribution. This uses two probability values (default = c(0.25,0.75))
# to define the line for a specified distribution (here a normal disribution).
qqline(aggmet2007_windspeed_matrix[1:5000],datax = FALSE,
       distribution = qnorm, 
       probs = c(0.25,0.75))



