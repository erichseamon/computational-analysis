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

randomsample <- sample(All,10)

# *Create scatter plots among PDSI series from multiple points*


alpha = 0.05 # If the probability of an observed correlation is 
# less than alpha, then a regression line will be
# fit to the data. 

index = randomsample  # Select the index values to plot

# This code, if uncommented, will select random locations:
# index = unidrnd(length(gridId),4,1);     

varName = index           # varName will be used to label the plots. 
X = PDSI[,index]          # Matrix of data. Columns will be compares in the 
# scatter plots.
n = length(index)         # Number of columns in X, or number of variables.
rowIn = seq(1,n^2,n)    # Index for subplots in the figures. 

win.graph()

# Create layout for figure panels. Panels will be added to figure in the
# order of the numbers in the matrix.
lyt = c(1, 0, 11, 11,
        2, 3, 11, 11,
        4, 5,  6,  0,
        7, 8,  9, 10)

lytmtx = matrix(lyt,nrow=4,ncol=4,byrow=T)
layout(lytmtx) # Set the layout of the figure using the 'layout' function

for (j in 1:n){ # For each variable
  x = X[,j] # Define x as the values in column j.
  for (i in 1:n){ # For each variable
    if (i <= j){ # If i is less than j...
      y = X[,i]; # Define y as the values in column i
      
      if (i != j){
        plot(x,y,col=rgb(0.5,0.5,0.5),
             main="",
             xlab="",
             ylab="") # Plot x vs. y.
        index2 = which(as.logical((x > -999) * (y > -999))) # Index for non-NaN values.
        stats = cor.test(x[index2],y[index2]) # Correlate x and y, and return
        # correlation coefficient, r,
        # and the probability of Type I
        # error, p.
        
        r = as.numeric(stats$estimate) # Correlation
        p = as.numeric(stats$p.value) #probability of Type I error, p
      }
      if (j == i){       # If i and j are the same, then make a 
        # histogram instead of a scatter plot. 
        hist(x,     # Histogram variables.
             col=rgb(0.5,0.5,0.5),
             main = "",
             ylab="",
             xlab="")
        
        
      } else if (p < alpha){  # If the probability of Type-I error is
        # less than alpha, then plot r and the
        # least-squares line of best fit. 
        text(min(x)+2*(range(x,na.rm=T)[2] - range(x,na.rm=T)[1])/10, # Use range function to dynamically
             max(y)-2*(range(y,na.rm=T)[2] - range(y,na.rm=T)[1])/10,   # place text in each figure.
             paste("r = ",round(r,digits = 2),sep=""),
             cex=0.9)
        abline(lm(y~x)) # Add least squares line using 'abline' and 'lm' functions
      }
      if (i == 1 & j != 1){       # If i == 1 (first column), add y-axis label.
        title(ylab = paste(varName[j]))
      }
    }
    
  }
  title(paste(varName[j]))   # Add a title for each column. 
}

# Add the PDSI grid map, to be able to reference locations:
# Plot grid locations.

# Here, the negative index (e.g. gridLoc[-index,1]) indicates that those are 
# indices you do not want to inlcude. 
# For example try the code:
# a = c(1,2,3)
# a[-1] # keep the 2nd and 3rd indices but not the first ...
# [1] 2 3 # returned values
# 
# So here we want to plot grid points that were not included in the Idaho index
# (Idaho_in)
plot(gridLoc[-index,1],gridLoc[-index,2],pch='.',
     xlab="Longitude",
     ylab="Latitude")    

for (i in 1:length(index)){    # For each grid location in the index used, add 
  # text to the plot that shows the grid number. 
  text(gridLoc[index[i],1],gridLoc[index[i],2],paste(index[i]),cex=0.8)
}







