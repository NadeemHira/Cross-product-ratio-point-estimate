# Determining the mean value of the estimator rho,
# The actual value of rho 
# Bias of the estimator
# Mean squared errors
# Variance of the estimator


# Varying values of p1, p2, q1, q2, n1 and n2
p1=0.2
p2=0.2
q1=1-p1
q2=1-p2
n1=50
n2=50 

# Number of simulations
ns=10^5 
set.seed(50)

# Exact value of the estimator
rho= p1*q2/(q1*p2)

# Creating empty vectors
rhohat=rep(0, ns)
rhat =0 

for (i in 1:ns) {
  t1= rbinom(1, n1, p1) # Direct Binomial Scheme for first sample
  t2= rbinom(1, n2, p2) # Direct Binomial Scheme for second sample
  rhohat[i]  = (t1/(n1+1-t1))*((n2+1)/(t2+1)-1) # Estimate for rhohat
  rhat  = rhat   + rhohat[i]  
  # variance for rhohat as mentioned in the paper
  s2 <- 1/(n1*(1-(t1/n1))*(t1/n1)) + 1/(n2*(1-(t2/n2))*(t2/n2))
}

# Lower and upper limits for 95% confidence
lower <- rhohat - qnorm(0.975)*sqrt(s2)
upper <- rhohat + qnorm(0.975)*sqrt(s2)

# To check for the following condition
cond <- numeric(ns)
for(i in 1:ns){
  if(rho >= lower[i] & rho <= upper[i]) cond[i]=1
}

# Mean for rhohat values
mean(cond)

# Width of the interval
width <- upper - lower
mean(width)

# Standard error
se <- sqrt(s2) 
mean(se)

# Exact rho values
rho

# Bias calculation
b <- rhat - rho
b

# Mean-squared error for rho-hat
mse <- mean((rhohat - rho)^2)
mse

# Variance for rho hat
var <- mean((rhohat-rhat)^2)
var

