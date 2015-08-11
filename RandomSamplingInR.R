# Initialize workspace
rm(list=ls())
cat("\14")

# A key function to know in R is the 'sample' function
sample(10) # will produce a random permutation of 10 numbers
sample(1:10,7) # will randomly sample 7 observations from the values 1:10
               # without replacement
sample(1:10,7,replace=TRUE) # will randomly sample 7 observations from the 
                            # values 1:10 with replacement
sample(1:10,3,prob=c(0.5,rep(0.5/9,9))) # will randomly sample 3 observations from the 
                                        # values 1:10 using weighted probabilities of 
                                        # assigned to each value

# See ?sample for more examples and details on how to use this function. We will now
# demonstrate how the sample function can be applied to a bootstrapping. 

# Generate some random data
n = 100 # number of observations in dataset
x = rnorm(n) # standard normal distribution of size n

# In Matlab you can use the ???bootstrp??? function, but in R you generally have to do it
# manually. Here is an example
B = 2000 # number of bootstrap samples to simulate
x_mean = matrix(NA,nrow=B,ncol=1) # empty vector to store bootstrap sample statistic
for (i in 1:B){
  # randomly sampled index for iteration i. In bootstrapping sampling is done with
  # replacement.
  index = sample(1:n, # vector of integers (1, 2, ???,n) from which to sample from
               n, # sample size
               replace=T)  # sampling should be done with replacement
  # calculate mean for bootstrapped sample and store it in pre-allocated space
  x_mean[i] = mean(x[index]) 
}

# From the bootstrapped samples calculate the 95% confidence interval of the test
# statistic
bootci = quantile(x_mean,probs=c(0.025,0.975))
bootci

# compare the bootstrapped confidence intervals with standard confidence intervals
c(mean(x)-1.96*sqrt(var(x)/n), mean(x)+1.96*sqrt(var(x)/n))

# You can also use the 'bootstrap' package in R.
library(bootstrap) # load package
theta = function(x) mean(x) # Define a function that will calculate the statistic
                            # to be estimated from bootstrapping

# Define a function that will calculate confidence intervals for bootstrapped test 
# statistic
ci_func = function(x,ci=c(0.025,0.975)) quantile(x,probs = ci) 

# conduct bootstrapping using 'bootstrap' function.
bootResults = bootstrap(x = x,nboot = B,theta = theta, func = ci_func)

thetaStar = bootResults$thetastar
hist(thetaStar)
