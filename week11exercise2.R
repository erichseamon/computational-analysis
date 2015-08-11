#------------------------------------------------------------------------#
# TITLE:        Week11exercise.R
#
# COURSE:       Computational Data Analysis (FOR504)
#               Week 11 exercise questions and answers - Monte Carlo Obs.
#
# AUTHOR:       Erich Seamon
#
# INSTITUITON:  College of Natural Resources
#               University of Idaho
#
# DATE:         Nov 7. 2014
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

set.seed(1)
cat("\14")
graphics.off()

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
library("parallel")
#library("Evapotranspiration")
library("plyr")
#library("data.table")
library("sirad")


setwd("D:/Dropbox/ES Research/ES Classwork/FOR504/data/Week11/")
GISS_globalLandOceTemp = read.table("GISS_globalLandOceTemp.txt",
                                    header=F,
                                    skip=5) # Load data. 

yr = GISS_globalLandOceTemp[,1]           # [yr CE]
globalT = GISS_globalLandOceTemp[,2]      # [C] temp. anomaly

setwd("D:/Dropbox/ES Research/ES Classwork/FOR504/data/climatevariables/")

cropyield <- readShapePoints('REACCHcropyield_nonulls.shp', 
                             proj4string=CRS
                             ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
yield12ww_df <- data.frame(cropyield[,8])
yield13ww_df <- data.frame(cropyield[,9])

yield12ww <- cropyield[,8]
yield13ww <- cropyield[,9]
yield12s <- cropyield[,11]
yield13s <- cropyield[,12]

alpha = 0.05

n = length(yield12ww_df[,1]) # Sample size.

#a_rotate <- c(0, .25, .5, .75)

#for (l in a_rotate) {

#a = (l) # Lag-1 autocorrelation.
#redNoise = matrix(NA,nrow=n,ncol=2) # Create space for two red noise time
# series (independent).
#redNoise[1,] = rnorm(2)  # Fill in first value. 

#for (j in 1:2){    # For each random time series
#  for (i in 2:n){    # For each sample, n
#    error = rnorm(1)  # Random error
#    redNoise[i,j] = a * redNoise[i-1,j] + sqrt(1-a^2) * error
#  }
#}



yieldww_cor_stats = cor.test(yield12ww_df[,1],yield13ww_df[,1])  # Correlation, and p-value.
yieldww_r_o = yieldww_cor_stats$estimate # Observed test statistic, r.
yieldww_p_o = yieldww_cor_stats$p.value  # Observed test statistic, p.




M = 1000 # Number of Monte Carlo trials.
yieldww_r_i = matrix(NA,nrow=M,ncol=1) # Space to record value of test statistic. 
yieldww_p_i = matrix(NA,nrow=M,ncol=1) # Space to record value of test statistic. 

#yieldmean12 = matrix(NA,nrow=M,ncol=1)
#yieldmean13 = matrix(NA,nrow=M,ncol=1)

for (k in 1:M){
  
  #index = sample(1:n, # vector of integers (1, 2, ???,n) from which to sample from
  #               n, # sample size
  #               replace=T)  # sampling should be done with replacement
  
  #yieldmean12[k] = mean(yield12ww_df[index]) 
  #yieldmean13[k] = mean(yield13ww_df[index]) 

  
  cropyield12ww_i <- as.matrix(sample(yield12ww_df[,1], n, replace = TRUE))
  cropyield13ww_i <- as.matrix(sample(yield12ww_df[,1], n, replace = TRUE))
  # Determine the pseudo-population when the null hypothesis is true, and
  # generate a random sample of size n:
  #cropyield_i = matrix(NA,nrow=n,ncol=2) # Create space for two red noise time
  # series (independent).
  #cropyield_i[1,] = rnorm(2) # Fill in first value. 
  #for (j in 1:2){                  # For each random time series
  #  for (i in 2:n){                # For each sample, n
  #    error = rnorm(1)             # Random error
  #    cropyield_i[i,j] = a * cropyield_i[i-1,j] + sqrt(1-a^2) * error
  #  }
  #}
  # Perform hypothesis tests (i.e., evaluate p, with alpha = 0.05), and 
  # record test statistic, r.
  yieldww_cor_stats_i = cor.test(cropyield12ww_i[,1],cropyield13ww_i[,1])  # Correlation, and p-value.
  yieldww_r_i[k] = yieldww_cor_stats_i$estimate # MC test statistic, r.
  yieldww_p_i[k] = yieldww_cor_stats_i$p.value  # MC test statistic, p.
}

alpha_hat = sum(as.numeric(yieldww_p_i<=alpha))/M # Monte-Carlo estimated alpha

p_hat = sum(as.numeric(yieldww_r_i^2 >= yieldww_r_o^2))/M # Monte-Carlo estimated p-value

alpha_orig = sum(as.numeric(yieldww_p_o<=alpha))/M # True probability of Type I error

bootciyieldww = quantile(yieldww_r_i,probs=c(0.025,0.975))
bootciyieldww



win.graph(11,7) #---Creates the window size---------------------------------------------------------------------------#
bins = seq(0,6000,500) #-----defining the bin sequences----------------------------------------------------------------#
par(mfrow=c(1,2))  #---par function sets number or rows and columns for plotting---------------------------------------#


#win.graph()
fn = ecdf(yieldww_r_i^2)
plot(fn,
     main=paste("r^2 betwen two random time series: alpha = ", alpha, "; alpha_hat = ", alpha_hat,
                "; p = ", round(yieldww_p_o,digits=2), "; p_hat = ", round(p_hat,digits=2),sep=""),
     xlab="yieldww_r^2_i",
     #breaks = bins,
     ylab="Frequency",
     cex.main=0.75)
abline(v=yieldww_r_o^2,col="red") # Vertical line indicating observed r-squared value
legend(0.1,0.4,c('CDF of r_i^2','r_o^2'),
       col=c("black","red"),
       lty=c(1,1))

#---comparing estimated pvalues to monte carlo generated pvalues---#



#win.graph()
fn = ecdf(yieldww_p_i)
plot(fn,
     main=paste("p_i betwen two random time series: alpha = ", alpha, "; alpha_hat = ", alpha_hat,
                "; p = ", round(yieldww_p_o,digits=2), "; p_hat = ", round(p_hat,digits=2),sep=""),
     xlab="p_i",
     #breaks = bins,
     ylab="Frequency",
     cex.main=0.75)
abline(v=yieldww_p_o,col="red") # Vertical line indicating observed p-values
legend(0.1,0.4,c('CDF of p_i','p_o'),
       col=c("black","red"),
       lty=c(1,1))

#}


win.graph()
plot(yield12ww_df[,1],yield13ww_df[,1],
     #xlab="2012 crop yield",
     #ylab="2013 crop yield",
     main="Correlation of 2012 and 2013 crop yield")
lsline = lm(yield12ww_df[,1]~yield13ww_df[,1]) # Least-squares line
abline(lsline)
#lines(yield12ww_df[,1],yield13ww_df[,1], k, type="l")
legend(1890,0.6,
       paste('Departure from 1951-1980 mean, Trend: r^2 = ',round(yieldww_r_o^2,digits=2),sep=""))



win.graph()
hist(yieldww_r_i,breaks=50,col="blue",
     xlab="yieldww_r_i",
     ylab="Frequency",
     main=paste("Linear trend in random time series: alpha = ", round(alpha,digits=2), "; alpha_hat = ",
                round(alpha_hat,digits=2), "; p = ", yieldww_p_o, "; p_hat = ", 
                round(p_hat,digits=2)),
     cex.main=0.75)
abline(v=yieldww_r_o,col="red")
abline(v=bootciyieldww[1],col="green")
abline(v=bootciyieldww[2],col="green")
legend(-0.8,50,c('yieldww_r_i','yieldww_r_o'),
       col=c("blue","red"),
       lty=c(NA,1),
       pch=c(15,NA))
