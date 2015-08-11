#---------------------------------------------------------------------#
# TITLE:        week6exercise_crosscorrelation_function_invoke.R
#
# COURSE:       Computational Data Analysis (FOR504)
#               Assignment #4
#
# AUTHOR:       Erich Seamon
#
# INSTITUITON:  College of Natural Resources
#               University of Idaho
#
# DATE:         8 Oct. 2014
#
# COMMENTS:     This file invokes the crosscorrelation function for a 
#               specified dataset
#
#
#-------Setting the working directory and clearing the workspace------#


rm(list=ls())   # Clear workspace (remove variables)
graphics.off()  # Close current graphing windows
cat("\14")      # Clear command prompt

#--------Set working directory--------------#
setwd("X:/Dropbox/ES Research/ES Classwork/FOR504/data")


#-------Loading the dataset to use for analysis --------------------#

data = read.csv("Week6Data.csv", # Read in random seedling data
                     header=TRUE, 
                     skip=1, 
                     col.names=c("var1", "var2", "var3", "var4"),)   

#----------funs cross correlations function for a dataset----------#
#-------------- Set script location -------------------#
scriptdirname <- setwd("X:/Dropbox/ES Research/ES Classwork/FOR504/scripts/") 
source ("X:/Dropbox/ES Research/ES Classwork/FOR504/scripts/week6exercise_crosscorrelation_function.R")
x <- CrossCorrelationFunction(data)
