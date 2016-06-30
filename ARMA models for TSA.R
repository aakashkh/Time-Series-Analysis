# The preferred model, from a selection of models, has the minium AIC of the group
# Akaike Information Criterion
# AR(p) to stationary, characteristic equation roots > 1

# Simulating AR(1)
set.seed(1)
x <- w <- rnorm(100)
for (t in 2:100) 
  x[t] <- 0.6*x[t-1] + w[t] # t must range from to 100 in this loop.

layout(1:2)
plot(x, type="l")
acf(x)

# Fitting AR model to our simulated AR(1)
x.ar <- ar(x, method = "mle")
x.ar$order
x.ar$ar
# confidence interval for parameter
x.ar$ar + c(-1.96, 1.96)*sqrt(x.ar$asy.var) #asy.var = asymptotic variance 
# True parameter = 6, falls between t C.I.
# Changing parameter to -0.6
set.seed(1)
x2 <- w <- rnorm(100)
for (t in 2:100) 
  x2[t] <- -0.6*x2[t-1] + w[t]
layout(1:2)
plot(x2, type="l")
acf(x2)

# Fitting AR model to our simulated AR(1) above
x2.ar <- ar(x2, method = "mle")
x2.ar$order
x2.ar$ar
# confidence interval for parameter
x2.ar$ar + c(-1.96, 1.96)*sqrt(x2.ar$asy.var) #asy.var = asymptotic variance 

#AR (2)
