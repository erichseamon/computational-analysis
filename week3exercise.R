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

r <- raster(aggmet2007.nc)



#-----------List a summary of input files----#

summary(aggmet2011.nc)



lon <- get.var.ncdf(MET2007, "lat")
nlon <- dim(lon)
#head(lon)

lat <- get.var.ncdf(MET2007, "lon")
nlat <- dim(lat)
#head(lat)

day <- get.var.ncdf(MET2007, "day")
nday <- dim(day)


MET2007$dim$lon$vals -> lon
MET2007$dim$lat$vals -> lat
MET2007$dim$day$vals -> day
#MET2007$dim$min_air_temperature$vals -> minairtemp
#MET2007$dim$max_air_temperature$vals -> maxairtemp
#MET2007$dim$solar_radiation$vals -> srad

names(MET2007$var)

MyVariables<-objects()    #Lists my variables
for (i in MyVariables) {
  print(i)
}

names(MET2007$var)

cat ("Press [enter] to continue")
line <- readline()


MET2007$var[[1]] -> maxairtemp
MET2007$var[[2]] -> maxrelhumdity
MET2007$var[[3]] -> minairtemp
MET2007$var[[4]] -> minrelhumidity
MET2007$var[[5]] -> precipamount
MET2007$var[[6]] -> precipduration
MET2007$var[[7]] -> solarradiation
MET2007$var[[8]] -> windspeed

get.var.ncdf(MET2007, maxairtemp) -> maxat
get.var.ncdf(MET2007, minairtemp) -> minat
get.var.ncdf(MET2007, maxrelhumdity) -> maxrh
get.var.ncdf(MET2007, minrelhumidity) -> minrh
get.var.ncdf(MET2007, precipamount) -> pramt
get.var.ncdf(MET2007, precipduration) -> prdur
get.var.ncdf(MET2007, solarradiation) -> srad
get.var.ncdf(MET2007, windspeed) -> windspd
#get.var.ncdf(MET2007, lat) -> latitude
#get.var.ncdf(MET2007, lon) -> longitude
#get.var.ncdf(MET2007, day) -> daytime

#-----Reading Tiff into a raster and plotting----------------#

#tmmx2011 <- readGDAL(fname = "X:/Dropbox/ES Research/ES Classwork/FOR504/data/tmmx_2011.tif")
#tmin2011 <- readGDAL(fname = "X:/Dropbox/ES Research/ES Classwork/FOR504/data/tmin_2011.tif")
#minrh2011 <- readGDAL(fname = "X:/Dropbox/ES Research/ES Classwork/FOR504/data/minrh_2011.tif")
#maxrh2011 <- readGDAL(fname = "X:/Dropbox/ES Research/ES Classwork/FOR504/data/maxrh_2011.tif")
#srad2011 <- readGDAL(fname = "X:/Dropbox/ES Research/ES Classwork/FOR504/data/srad_2011.tif")
#windspeed2011 <- readGDAL(fname = "X:/Dropbox/ES Research/ES Classwork/FOR504/data/windspeed_2011.tif")


#------------Create rasters from TIFFs, then transpose due to lat/lon reversal-----#

#tmmx2011 <- raster(tmmx2011)
#tmin2011 <- raster(tmin2011)
#minrh2011 <- raster(minrh2011)
#maxrh2011 <- raster(maxrh2011)
#srad2011 <- raster(srad2011)
#windspeed2011 <- raster(windspeed2011)

#tmmx2011 <- t(tmmx2011)
#tmin2011 <- t(tmin2011)
#minrh2011 <- t(minrh2011)
#maxrh2011 <- t(maxrh2011)
#srad2011 <- t(srad2011)
#windspeed2011 <- t(windspeed2011)

#tmmx2011projection <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#tmmx2011r <- projectraster(tmmx2011, crs=tmmx2011projection)


#-----------Create Raster Brick From Meteorological rasters---------------#

b <- brick(tmmx2011,tmin2011,minrh2011,maxrh2011,srad2011,windspeed2011)


#plot (as.numeric(maxat),as.numeric(day),type = "l")

#obsdatadates = as.Date(MET2007$time$vals, origin = "1")
#Get the whole data first
#obsoutput = get.var.ncdf(MET2007, varid = 'min_air_temperature',verbose=TRUE)
#Prepare your points of interest
#points_of_interest = data.frame(day,minat)
#Subset your data accordingly
#data_at_point = apply(points_of_interest,1,function(x)obsoutput[x[1],x[2],])
#Turn it into a dataframe
#data_at_point = as.data.frame(data_at_point)
#Add the dates to the dataframe
#data_at_point$Date = obsdatadates



#-----------Setup Study Area Shapefile-----------#

setwd("X:/Dropbox/ES Research/ES Classwork/FOR504/data/")
ogrListLayers("Studyarea.shp") #will show you available layers for the above dataset
ogrInfo(dsn="Studyarea.shp", layer="Studyarea")
studyareashape <- readOGR(dsn="Studyarea.shp",layer="Studyarea")



#-------------Plot outputs----------------#


#plot(tmmx2011)
#plot(tmin2011)
#plot(minrh2011)
#plot(maxrh2011)
#plot(srad2011)
#plot(windspeed2011)


#plot(b)
#win.graph()
#plot(tmmx2011r)
#plot(studyareashape, add=TRUE) #to get an overview

cat ("Press [enter] to continue")
line <- readline()


#------------NetCDF info-----------#

#------------Plot Histograms of Climate Data--------------#

win.graph(10,3.75) 
plot(tmmx2011)

cat ("Press [enter] to continue")
line <- readline()

bins = seq(240,300,1)
# Plot a histogram (frequency) distribution
hist(tmmx2011,      # Data to represented in the distribution
     breaks = bins, # Bins used to create histogram
     xlab = "Mean Max Temperature",    # Axis labels ...
     ylab = "Frequency Counts",
     main = "Frequency Distribution",# ...and title of plot (figure)
     col = "gray")  # color of bars in histogram plot

#print(fid)# Get a netcdf file info
#var_id <- varid.inq.ncdf(fid, var_name)# Get the variable identifier
#print(var_id)

#my_var_name <- varname.inq.ncdf(fid, var_id)# Get the name of a variable id 
#print(my_var_name)

#attr <- att.get.ncdf(fid, 0,"creation_date")# Get a file attribute
#print(attr)

#attr_value <- att.get.ncdf(fid, var_name, attr_name)# Get a variable attribute
#print(attr_value)

#size <- varsize.ncdf(fid, var_id)# Get the size of a variable
#print (size)

#ndims <- varndims.ncdf(fid, var_id)# Get dimensions of a variable
#print (ndims)

#data <- get.var.ncdf(fid, var_name)# Read data from a netcdf file
#print(data)

#var_object <- var.inq.ncdf(fid,var_id )# Get the variable objet
#print(var_object)

#name <- var_object$name# Get the name from the variable object
#print (name)

#n_attr <- var_object[["units"]]# Get variable object units
#print(n_attr)



#------Close NetCDF----------------#

#close.ncdf(MET2007)# Close a netcdf file
