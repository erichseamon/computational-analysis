#---------------------------------------------------------------------------------------------------------------------#
# TITLE:        Week6exercise_2.R
#
# COURSE:       Computational Data Analysis (FOR504)
#               Assignment #4
#
# AUTHOR:       Erich Seamon
#
# INSTITUITON:  College of Natural Resources
#               University of Idaho
#
# DATE:         29 Sept. 2014
#
# COMMENTS:     This file contains week 6 exercise - part 2 - cross-correlations 
#
#
#---------------Setting the working directory and clearing the workspace----------------------------------------------#

setwd("X:/Dropbox/ES Research/ES Classwork/FOR504/data") # Set working directory
rm(list=ls())   # Clear workspace (remove varialbes)
graphics.off()  # Close current graphing windows
cat("\14")      # Clear command prompt

library(MASS)
library(scatterplot3d)


gridLoc = read.table("dai-namerica-grid.txt", 
                   header = F) # [decimal degrees] Lat. and lon. for each grid 
# point in the dataset.
data = read.csv('namerica-pdsi-recs.csv',header=TRUE)   # [PDSI] Each row is a 
# year and each column is a different grid point. 



yr = data[,1]                 # [yr CE]
PDSI = data[,2:ncol(data)]    # [PDSI] Reconstructed PDSI
PDSI[PDSI == -99.999] = NA    # Replace missing values with NaN.
gridId = 1:length(gridLoc)    # Grid point ID



# Create some index values for relevant geographic locations. You can find 
# the grid point for any location by following the link above, or by 
# plotting the grid points, below.
# 
# **Plot the PDSI grid and select relevant locations for analysis**

#win.graph(9,7)  # Make a figure that is x, y inches wide, tall.
#index = 1:length(t(gridLoc))
#plot(gridLoc[,1], gridLoc[,2], pch = 22, cex = 3.0, xlab="Longitude", ylab="Latitude")
#for (i in 1:length(index)){
  #text(gridLoc[index[i],1],gridLoc[index[i],2],paste(index[i]),cex = 0.5)
#}


# Based on this plot, or maybe better yet the plot from the web, you can
# select some relevant locations. The locations below were picked by
# copying the values from the website linked to above. 

Moscow_in = c(55)          # Moscow, ID
Idaho_in = c(70,85,69,68)  # Idaho and NW Montana
PNW_in = c(25,32,43,33,34,45,57,56,44,55,68,69,70,85) # PNW: WA, OR, ID, NW MT
WesternUS_in = c(55,68,56,69,57,70,85,84,83,25,32,33,34,35,36,43,44,45,46,47,
                 48,55,56,57,58,59,60,61,68,69,70,71,72,73,74,83,84,85,86,87,88,89,99,
                 100,101,102,103,104,105,114,115,116,117,118,119,120,128,129,130,131,
                 132,133,134,135,143,144,145,146,147,148,149,150,159,160,161,162,163,
                 164,165,166,167)
All = c(1:286)

droughtrandom <- c(1:20)

#win.graph(20,20)
#par(mfrow=c(10,10))
win.graph(15,13)

lyt = c(1, 2, 9, 10, 17,18,25,26,33,34,
        3, 4, 11, 12, 19,20,27,28,35,36,
        5, 6, 13, 14, 21,22,29,30,37,38,
        7, 8, 15, 16, 23,24,31,32,39,40)

#lytmtx = matrix(lyt,nrow=4,ncol=10)
lytmtx = matrix(lyt,nrow=4,ncol=10,byrow=T)
layout(lytmtx) # Set the layout of the figure using the 'layout' function

for (i in droughtrandom){

randomsample <- NULL  
randomsample <- sample(All,1)

alpha = 0.05

pt = randomsample                   # Set the single point here. 
# pt = unidrnd(length(gridId),1,1) # Use this code for a random point. 

index = pt                             # Index for location of point.
x = PDSI[,index]                       # x = PDSI values at point pt.
r_pt = numeric(nrow(gridLoc))        # Space for correlation coefficients for
# comparisons between all grid points
# and point pt.
p_pt = numeric(nrow(gridLoc))        # Space for p-values, for same comparisons. 

for (i in 1:nrow(gridLoc)){ # For each grid point.
  y = PDSI[,i] # y = PDSI for grid point i.
  index2 = which(as.logical(((x > -999) * (y > -999)))) # Index for all non-NaN values.
  stats = cor.test(x[index2],y[index2]);    # Correlations statistics.
  r_pt[i] = as.numeric(stats$estimate) # Store correlation coefficient.
  p_pt[i] = as.numeric(stats$p.value)  # Store associated p-value. 
}

# These will be the variables used in all the plots below:
x = gridLoc[,1]       # [longitude]
y = gridLoc[,2]       # [latitude]
z = r_pt              # [correlation coefficient]
index3 = which(as.logical(as.numeric(p_pt <= alpha) * as.numeric(r_pt > 0))) # Find positive correlations
# with a p-value <= alpha and with correlations > 0.2.
index4 = which(as.logical(as.numeric(p_pt <= alpha) * as.numeric(r_pt < -0))) # Same, for negative 
# correlations.


#win.graph()
#par(mfrow=c(4,5))
plot(x[index3],y[index3],
     col="red",
     pch=19,
     cex=3*z[index3]^2,
     xlab="Longitude",
     ylab="Latitude")
points(x[index4],y[index4],
       col="blue",
       pch=19,
       cex=100*abs(z[index4]^2))

# Finally, plot the location of the grid point of comparison. 
points(gridLoc[-c(index3,index4),1],gridLoc[-c(index3,index4),2],pch='.')


#win.graph()
plot3d <- scatterplot3d(x[index3],y[index3],z[index3]^2,
                        pch = 19,
                        color="red",
                        type="h",
                        ylab="Longitude",
                        xlab="Latitude",
                        zlab="r^2")
# Add points to show magnitude of significantly negative correlated points
plot3d$points3d(x[index4],y[index4],z[index4]^2,
                pch=19,
                col="blue",
                type="h")






}