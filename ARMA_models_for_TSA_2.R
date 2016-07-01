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
