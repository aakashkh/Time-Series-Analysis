set.seed(1)
x = seq(1,100)+20.0*rnorm(1:100)
set.seed(2)
y <- seq(1,100)+20.0*rnorm(1:100)
plot(x,y)
cov(x,y)

#Correlogram showing no trend and cycle
set.seed(1)
w <- rnorm(100)
acf(w)

#Correlogram showing trend
w <- seq(1,100)
acf(w)

#Correlogram showing cycle
w <- rep(1:10,10)
acf(w)
