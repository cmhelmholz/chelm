## Problem Set 1
## Carolin Helmholz

## 1
# Poisson:
# Natural numbers beginning at 0.
# mu: the mean number of successes in a time interval.
# Discrete.
# Events that occur in a fixed period of time or space. Ex: rolling a die over the
  #course of a fixed 2 minute period.  

# Negative binomial:
# 0 and all positive integers.
# r > 0, used to denote the number of attempts before success (integer/real(?))
  # p: probability of success in each experiment (real)
# Discrete.
# Data (independent) from a non-fixed number of trials (n), where there are only 
  #two possible outcomes, and the probability of success for each trial is constant. 
  #The number of trials needed for r successes is denoted by the variable Y. 
  #Ex: choosing a card from a deck (with replacement) until you have drawn two aces. 

# T:
# Real numbers from negative infinity to positive infinity.
# Nu (degrees of freedom, expressed as real numbers) greater than 0.
# Continuous
# Three main options: data from two independent groups, data from a single group at 
  #two different instances, or data from a single group and a known comparable mean.
  # Used when population variance is unknown and/or sample size is small.
  # Ex: costs of a procedure at two different hospitals. 

## 2
# (a)
# When the measured variance of our random variable is larger than the expected value. 

# (b)
# Using the Poisson models assumes that for a given covariate profile T, the mean and 
  #variance of T are equal. The PMF function for the Poisson distribution only takes 
  #into account a single variable intending to relfect both the mean and variance. 
  #Heterogeneity in the data (where now the variance is not accurately represented) 
  #can render this model ill-suited.

# (c)
# In the Negative Binomial Distribution, the equation for variance includes the extra
  #parameter r, with the result being that the mean and variance are no longer set to 
  #be equal, allowing for a more accurate representation of certain data. 

## 3
# (a)
x <- rnorm(10000, 3, 1.224744871) #rnorm takes sd, we were provided the variance.
kurtosis <- function(x){
  mu <- mean(x)                 #calculates mean
  var <- var(x)                 #calculates variance
  N <- length(x)                #calculates N (samples)
  numer <- (x-mu)^4             #calculates numerator of equation
  denom <- var^2                #calculates denominator of equation
  kurt <- (1/N)*sum(numer/denom)  #kurtosis eqation
  return(kurt)
}
                  # kurt = 3.0541

# (b)
secondx <- rnorm(10000, 12, 7.0710678)
kurtosis2 <- function(secondx){     #variable names changed for new data.
  mu2 <- mean(secondx)              
  var2 <- var(secondx)              
  N2 <- length(secondx)             
  numer2 <- (secondx-mu2)^4         
  denom2 <- var2^2                  
  kurt2 <- (1/N2)*sum(numer2/denom2)
  return(kurt2)
}
                  # kurt2 = 2.9356

# (c) zero

# (d)(e) 
nu <- 10
tvar <- function(nu){   #create a function that takes nu and returns the variance (formula found online).
  var3 <-(nu/(nu-2))
  return(var3)
}                 # var3 = 1.25

# (f) 
newnorm <- rnorm(100000, 0, 1.11803398875)

# (g)
newtdist <- rt(100000, 10)
sorted <- sort(newtdist)
plot(sorted, dt(sorted, 10), type = 'l') #plot the sorted data as a line graph.

# (h)
sorted2 <- sort(newnorm)  
lines(sorted2, type="l", col="red") #graph isn't looking correct - not sure why.

# (i)
# Unable to visualize with graph, but from what I understand (and can see form visualizations), the 
  #t-test is better for outliers because it uses group means, which according to the CLT, essentially 
  #reach a normal distribution after repeated sampling (irregardless of outliers). Also, the t-test 
  #uses a standard error of the sample means that is not affected by the distribution of X. The 95% 
  #CI should include the difference in the group means even when x is not normal (as long as N is 
  #large enough). The tails in a t-distribution represent the leftover 5%. 


## 4
# (a) #had trouble with load.csv, so I loaded ducks.csv with the import dataset button
ducksneeded <- (ducks$x) # pulling out the second column

# (b) 
# Normal: Does not appear normal.
# Gamma: Parameters are positive real numbers.
# Beta: Beta dist is defined on the interval [0,1].
# Poisson: Support is natural numbers starting at zero
# Negative binomial: Support is 0 and all positive integers.
# t: Not testing between two means, N too large.

# (c)
duckdensity <- density(ducksneeded, n=10000)  #produces density vector
plot(duckdensity) #visualizes density vector

# (d)
ducktest <- duckdensity$x
duckup <- which(duckdensity$x >= 7)[1] #result = 7063 (?)
ducklow <- which(duckdensity$x <= 7)[1] #result = 1 (?) this doesn't make sense

# These vector indexes would've provided me the bounds from which to estimate my rectangle

#integrate(dnorm, mean=(mean(duckdensity$x), sd= __, lower = __, upper = __)) 


# (e)

