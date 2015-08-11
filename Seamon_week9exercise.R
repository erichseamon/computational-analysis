#------------------------------------------------------------------------#
# TITLE:        Seamon_Week9exercise.R
#
# COURSE:       Computational Data Analysis (FOR504)
#               Final Project
#
# AUTHOR:       Erich Seamon
#
# INSTITUITON:  College of Natural Resources
#               University of Idaho
#
# DATE:         October 17. 2014
#
# COMMENTS:     This is Erich Seamon's Final Project for FOR 504, 
#               which uses weather parameters for the inland pacific 
#               northwest from 2007-2011 to calculate evapotranspiration 
#               on a daily timestep, as well as to compare crop yield to
#               across the study area to the aforementioned ET values. 
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

#--SECTION 1: Load dataset, remove missing values, and extract data---- #

#-----loading shapefiles for study region - tri-state shapefile
#-----and the crop yield point data for 2011-2013------#

data = read.csv('practice_data.csv',header=TRUE)   # [PDSI] Each row is a 
# year and each column is a different grid point. 


dataV1 = data$V1 == -999
dataV2 = data$V2 == -999
dataV3 = data$V3 == -999

data$V3[data$V3==-999] <- NaN
data$V2[data$V2==-999] <- NaN
data$V1[data$V1==-999] <- NaN

col1 = data$V1
col2 = data$V2
col3 = data$V3

data <- data.frame(col1, col2, col3)
lapply(data, is.numeric)

index1 = data[,1] != c('NaN')
index2 = data[,2] != c('NaN')
index3 = data[,3] != c('NaN')

table(index1)["FALSE"]
table(index2)["FALSE"]
table(index3)["FALSE"]

#-----question 4-------#

dataV3 <- subset(data, col3>= 1)

colMeans(dataV3, na.rm = TRUE)

#---Question 5

dataV4 <- subset(data, col3= 0)

colMeans(dataV4, na.rm = TRUE)

#---Question 6

dataV5 <- subset(data, col1 > 13 & col2 < 10 & col3 < 3)


#----Question 7-----#

q7 <- c(1:10)

for (i in q7) {
  print (i)
}

#----Question 8----#

x = 0
y <- matrix(data=NaN, c(1000))
outloop <- c(0:9)
innerloop <- c(1:100)

for (i in outloop ) {
  for (j in innerloop) {
    xnew <- j + (i * 100) 
    print (xnew)
    xnew = 0
  }
}
    
#-----question 9 ----------------#

vector <- c(1.5,2.6,3.3,4.4,5)
vectorloop <- c(1:length(vector))


for (i in vectorloop) {
  print(paste("element ", i, " is ", vector[i], sep=""))
}

#-----question 10----------------#

#answer 1.  use for i = 1:4 when iterating thru a vector where you want 
#the indexing to be done sequentially.  If you want to iterate thru 
#the vector but index on particular values - then the latter statement,
#for i = [x,x,x,x] woud be more useful.

#answer 2. 

#-----question 11-----------------#

q11matrix <- matrix(data=NaN, 5,5)
NaNspan <- c(1:5)

for (i in 1:ncol(q11matrix)) {
  for (j in 1:nrow(q11matrix)) {
    q11matrix[j,i] <- j * i
    q11matrix[1:4,5] <- NaN 
    q11matrix[1:3,4] <- NaN 
    q11matrix[1:2,3] <- NaN 
    q11matrix[1:1,2] <- NaN 
  }
}

#--------Question 12 -----------#

x <- c(1:10)
y <- x

plot(x,y)


y <- jitter(y, 5)

plot(x,y)











