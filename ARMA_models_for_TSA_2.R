# Moving Average Proces (MA(1), parameter = 0.6)
set.seed(1)
x <- w <- rnorm(100)
for (t in 2:100) 
  x[t] <- w[t] + 0.6*w[t-1]
layout(1:2)
plot(x, type="l")
acf(x) # for k(lag) > q(number of parameters), acf = 0

# Simulate moving average to fit the above generated series
# Arima function, setting AR and Integrated paramter = 0
x.ma <- arima(x, order=c(0, 0, 1)) 
x.ma
# Ouput shows Standard erord, estimated variance
# loglikelihood and Akaike Information Criterion also
# Arima also intercept intercept term as it do no subtract mean value of series
# CI for parameter
0.6023 + c(-1.96, 1.96)*0.0827

# Moving Average Proces (MA(1), parameter = -0.6)
set.seed(1)
x2 <- w <- rnorm(100)
for (t in 2:100) 
  x2[t] <- w[t] -0.6*w[t-1]
layout(1:2)
plot(x2, type="l")
acf(x2) # for k(lag) > q(number of parameters), acf = 0

# Simulate moving average to fit the above generated series
# Arima function, setting AR and Integrated paramter = 0
x2.ma <- arima(x2, order=c(0, 0, 1)) 
x2.ma
# CI for parameter
-0.7298 + c(-1.96, 1.96)* 0.1008 

# MA(3) - significant peaks at k =1,2,3 and insignificant peaks for k > 3
