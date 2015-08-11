#------------------------------------------------------------------------#
# TITLE:        Week9exercise.R
#
# COURSE:       Computational Data Analysis (FOR504)
#               Week 9 exercise questions and answers
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
#--Setting the working directory and clearing the workspace-----------#

#----SETUP SECTION----------------------------------------------------#

#--clear the variable list and set the working directory----#

rm(list = ls()) #--clears all lists------#

#----set the working directory.  In order to run this script from any UI--#
#----network location - mount \\CALS-DDVJ9YR1\climatevariables as --------#
#----your Z: drive.-------------------------------------------------------#

setwd("Y:/Dropbox/ES Research/ES Classwork/FOR504/data") 
options(warn=0)

#----set packages---------------------------------------------------------#

setwd("Y:/Dropbox/ES Research/ES Classwork/FOR504/data") # Set working directory
rm(list=ls())   # Clear workspace (remove varialbes)
graphics.off()  # Close current graphing windows
cat("\14")      # Clear command prompt

memory.size(10000)

#--SECTION 1: Load dataset, remove missing values, and extract data---- #

#-----loading shapefiles for study region - tri-state shapefile
#-----and the crop yield point data for 2011-2013------#

data = read.csv('practice_data.csv',header=TRUE)   # [PDSI] Each row is a 
# year and each column is a different grid point. 

#---Question 1--------#

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

#------Question 2-------#

index1 = data[,1] != c('NaN')
index2 = data[,2] != c('NaN')
index3 = data[,3] != c('NaN')

 #----Question 3-----#

table(index1)["FALSE"]
table(index2)["FALSE"]
table(index3)["FALSE"]

cat ("Press [enter] to continue")
line <- readline()

#-----question 4-------#

dataV3 <- subset(data, col3>= 1)

colMeans(dataV3, na.rm = TRUE)

#---Question 5

dataV4 <- subset(data, col3 == 0)

colMeans(dataV4, na.rm = TRUE)

#---Question 6

dataV5 <- subset(data, col1 > 13 & col2 < 10 & col3 < 3)


#----Question 7-----#

q7 <- c(1:10)

for (i in q7) {
  print(1:i)
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

#answer 1.  use for i in c(1:4) when iterating thru a vector where you want 
#the indexing to be done sequentially.  If you want to iterate thru 
#the vector but index on particular values - then the latter statement,
#for i in c(x, x, x, x) woud be more useful.

#answer 2. 

#-----question 11-----------------#

q11matrix <- matrix(data=NaN, 5,5)


for (i in 1:ncol(q11matrix)) {
  for (j in 1:nrow(q11matrix)) {
    q11matrix[j,i] <- j * i
    if(j<i) { q11matrix[j,i] <- NaN}
  }
}

q11matrix

#--------Question 12 -----------#

x <- c(1:10)
y <- x

plot(x,y)
lines(x,y)


y2 <- jitter(y, 5)

plot(x,y)
points(x,y2, col = "red")



#---------Question 13-------------#

#done - week9exercise_q13function.R

#---------Question 14--------------#

x = sample(1:1000, 1)
print(x)
isiteven <- !(x%%2)
print(paste("Is it even? --->", isiteven)) 











