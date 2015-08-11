rm(list = ls()) #--clears all lists------#
cat("\14")

library(bootstrap)
n = 10
x = rnorm(n)
theta = mean(x)

jack.T = matrix(NA,n,1)
jack.sample = matrix(NA,n-1,n)

for (i in 1:n) {
  jack.sample[,i] = x[-i]
  jack.T[i] = mean(jack.sample[,i])

}
jack.bias = (n-1) * (mean(jack.T) - mean(x)) #--page 282

jack.se = sqrt((n-1)/n * sum((jack.T - mean(jack.T))^2))

jack.CI = c(mean(x)-2*jack.se, mean(x)+2*jack.se)

Theta = function(x)(mean(x))
results = jackknife(x,Theta)

results$jack.se
jack.se

B = 1000  # Number of bootstrapp iterations
x_mean = matrix(NA,nrow=B,ncol=1) # empty vector to store bootstrap sample statistic
for (i in 1:B){
  # Randomly sampled index for iteration i. In bootstrapping, sampling is done
  # with replacement.
  index = sample(1:n, # vector of integers (1, 2, ???,n) from which to sample from
                 n, # sample size
                 replace=T)  # sampling should be done with replacement
  # calculate mean for bootstrapped sample and store it in pre-allocated space
  x_mean[i] = mean(x[index]) 
}
boot.CI = quantile(x_mean,probs=c(0.025,0.975))
boot.CI


n = 100  # Sample size
b = 2  # Regression parameter (slope)

x = rnorm(n)  # Random, normal x data
y = b*x + rnorm(n)  # y data, as a function of x, plus random noise

#### Function to evaluate polynomial equations, equivalent to polyval in Matlab
polyval <- function(c,x){
  n = length(c)
  y = x*0 + c[1]
  for (i in 2:n){
    y = c[i] + x*y
  }
  y
}
#### End function


K = .20

B = 1000

#  c(sample(n, 20, replace = TRUE))  # When K = n, this is leave-one-out cross-validation

#r1 = matrix(NA,B,1)
#r2 = matrix(NA,B,1)
#r3 = matrix(NA,B,1)

r1 = matrix(NA,K,1)  # Store error from linear fit
r2 = matrix(NA,K,1)  # Store error from quadratic fit
r3 = matrix(NA,K,1)  # Store error from cubic fit

for (i in 1:B){
  # Training data
  
 trainIndex = sample(1:(n-(K*n)),n-(K*n))
 
  x.train = x[trainIndex]
  y.train = y[trainIndex]
  
  # Testing data
# testindex = sample(1:(n-(K2*n)),n-(K2*n))
  x.test = (x[-trainIndex])
  y.test = (y[-trainIndex])
  
  # Fit models
  p1 = lm(y.train ~ poly(x.train,1, raw = T))
  p2 = lm(y.train ~ poly(x.train,2, raw = T))
  p3 = lm(y.train ~ poly(x.train,3, raw = T))
  
  # Get errors
  r1[i] = sum((y.test - polyval(rev(p1$coefficients), x.test))^2)
  r2[i] = sum((y.test - polyval(rev(p2$coefficients), x.test))^2)
  r3[i] = sum((y.test - polyval(rev(p3$coefficients), x.test))^2)
}

pe1 = mean(r1)
pe2 = mean(r2)
pe3 = mean(r3)

finalerror <- c(pe1, pe2, pe3)

# Polynomial fits for full models
p1 = lm(y ~ poly(x,1))
p2 = lm(y ~ poly(x,2))
p3 = lm(y ~ poly(x,3))

# Predicted y values
yHat1 = predict(p1)
yHat2 = predict(p2)
yHat3 = predict(p3)

# Sort by x to be able to plot reasonably
sortResults = sort(x,index.return = T)
index = sortResults$ix

# Plot
plot(x,y, main = 'Random correlated data')
lines(x[index],yHat1[index],col = 'blue')
lines(x[index],yHat2[index],col = 'red', lty = 2)
lines(x[index],yHat3[index],col = 'red', lty = 2)