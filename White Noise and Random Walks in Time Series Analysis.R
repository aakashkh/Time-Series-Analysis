# White noise
set.seed(1)
acf(rnorm(1000))
set.seed(1)
var(rnorm(1000, mean = 0, sd = 1))

# Random Walk
set.seed(4)
x <- w <- rnorm(1000)
for(t in 2:1000)
  x[t] <- x[t-1] + w[t]
plot(x,type = "l")
acf(x)

# Fitting random walks to financial data