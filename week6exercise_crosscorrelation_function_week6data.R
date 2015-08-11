
#---------------------------------------------------------------------#
# TITLE:        Week6exercise_crosscorrelation_function_week6data.R
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
# COMMENTS:     This file contains week 6 exercise - the cross-correlation function
#
#
#---Setting the working directory and clearing the workspace----------#

setwd("X:/Dropbox/ES Research/ES Classwork/FOR504/Data/")
#-------Loading the dataset to use for analysis-----------------------#

data = read.csv("Week6Data.csv", # Read in random seedling data
                     header=TRUE, 
                     skip=1, 
                     col.names=c("var1", "var2", "var3", "var4"),)   

#-------Creating variables for each column in dataset-----------------#

vars = data[,1:ncol(data)] 

datain = c("var1", "var2", "var3", "var4")



alpha = 1 # If the probability of an observed correlation is 
# less than alpha, then a regression line will be
# fit to the data. 

index = datain  # Select the index values to plot

# This code, if uncommented, will select random locations:
# index = unidrnd(length(gridId),4,1);     

varName = index       # varName will be used to label the plots. 
X = vars[,index]      # Matrix of data. Columns will be compares in the 
# scatter plots.
n = length(index)     # Number of columns in X, or number of variables.
rowIn = seq(1,n^2,n)  # Index for subplots in the figures. 

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
        # Index for non-NaN values.
        index2 = which(as.logical((x > -999) * (y > -999))) 
        # Correlate x and y, and return
        stats = cor.test(x[index2],y[index2]) 
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
        # Use range function to dynamically
        text(min(x)+2*(range(x,na.rm=T)[2] - range(x,na.rm=T)[1])/3, 
             # place text in each figure.
             max(y)-2*(range(y,na.rm=T)[2] - range(y,na.rm=T)[1])/50,   
             paste("r = ",round(r,digits = 2),sep=""),
             cex=0.9)
        # Add least squares line using 'abline' and 'lm' functions
        abline(lm(y~x)) 
      }
      # If i == 1 (first column), add y-axis label.
      if (i == 1 & j != 1){       
        title(ylab = paste(varName[j]))
      }
    }
    
  }
  title(paste(varName[j]))   # Add a title for each column. 
}

# Add the PDSI grid map, to be able to reference locations:
# Plot grid locations.

# Here, the negative index (e.g. gridLoc[-index,1]) indicates that 
# those are indices you do not want to inlcude. 
# For example try the code:
# a = c(1,2,3)
# a[-1] # keep the 2nd and 3rd indices but not the first ...
# [1] 2 3 # returned values
# 
# So here we want to plot grid points that were not included 
# in the Idaho index (Idaho_in)

#plot(gridLoc[-index,1],gridLoc[-index,2],pch='.',
     #xlab="Longitude",
     #ylab="Latitude")    

#for (i in 1:length(index)){    # For each grid location 
  # in the index used, add text to the plot that shows the grid number. 
  #text(gridLoc[index[i],1],gridLoc[index[i],2],paste(index[i]),cex=0.8)
#}

