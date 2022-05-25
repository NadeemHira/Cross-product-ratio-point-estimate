# Determining the estimates for Inverse-Direct sampling scheme

# Determining the mean value of the estimator rho,
# The actual value of rho 
# Bias of the estimator
# Mean squared errors
# Variance of the estimator


# Varying values of p1, p2, q1, q2, n1 and n2


p1=0.05
p2=0.05
q1=1-p1
q2=1-p2
n1=200
n2=200

# Rounding the value of trials for the first sample
m <- round(n1*p1)

# Number of simulations
ns=10^5 

# Setting seed
set.seed(100)

# Cross-product ratio estimator, rho
rho= p1*q2/(q1*p2)

# Creating empty vectors
rhat=0
rhohat=rep(0,ns)


for (i in 1:ns) {
  t1= rnbinom(1, m, p1) + m # generating pascal dist for first sample
  t2= rbinom(1, n2, p2) # generating binomial scheme for second sample
  rhohat[i]  = ((m-1)/(t1-m+1))*((n2+1)/(t2+1)-1) # Estimate for rhohat
  rhat  = rhat   + rhohat[i]
}

# Estimate for rho-hat
rhat=rhat/ns 
rhat 

# Exact value for rho
rho

# Bias of the estimator
b <- rhat - rho
b

# Mean-squared errors for rho-hat
mse <- mean((rhohat - rho)^2)
mse

# Variance of rho-hat
var <- mean((rhohat-rhat)^2)
var

