#------------------------------------------------------------------------#
# TITLE:        Seamon_Week9exercise_q13function.R
#
# COURSE:       Computational Data Analysis (FOR504)
#               week 9 exercise - question 13 function
#
# AUTHOR:       Erich Seamon
#
# INSTITUITON:  College of Natural Resources
#               University of Idaho
#
# DATE:         October 17. 2014
#
# COMMENTS:     
#
#
#--Setting the working directory and clearing the workspace-----------#

#----SETUP SECTION----------------------------------------------------#

#--clear the variable list and set the working directory----#

rm(list = ls()) #--clears all lists------#

#----set the working directory.  In order to run this script from any UI--#
#----network location - mount \\CALS-DDVJ9YR1\climatevariables as --------#
#----your Z: drive.-------------------------------------------------------#

setwd("D:/Dropbox/ES Research/ES Classwork/FOR504/data") 
options(warn=0)

#----set packages---------------------------------------------------------#

setwd("D:/Dropbox/ES Research/ES Classwork/FOR504/data") # Set working directory
rm(list=ls())   # Clear workspace (remove varialbes)
graphics.off()  # Close current graphing windows
cat("\14")      # Clear command prompt

memory.size(10000)

#------------Question 13 begin function content-----------#

q14function <- function(){

x = sample(1:1000, 1)
print(x)
isiteven <- !(x%%2)
print(paste("Is it even? --->", isiteven)) 
}
#to invoke the function - run this command - "q13function()"




