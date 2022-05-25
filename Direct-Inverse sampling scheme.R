# Determining the mean value of the estimator rho,
# The actual value of rho 
# Bias of the estimator
# Mean squared errors
# Variance of the estimator


# Varying values of p1, p2, q1, q2, n1 and n2

p1=0.9
p2=0.9
q1=1-p1
q2=1-p2
n1=200
n2=200

# Rounding the value of trials for the second sample
m <- round(n2*p2)

# Number of simulations
ns=10^5 

# Setting seed
set.seed(100)

# Cross-product ratio estimator, rho
rho= p1*q2/(q1*p2)

# Creating empty vectors
rhohat=rep(0, ns)
rhat =0

for (i in 1:ns) {
  t1= rbinom(1, n1, p1) # generating binomial scheme for first sample
  t2= rnbinom(1, m, p2) + m # generating pascal dist for second sample
  rhohat[i]  = (t1/(n1+1-t1))*((sum(t2)/m)-1) # Estimate for rhohat
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

