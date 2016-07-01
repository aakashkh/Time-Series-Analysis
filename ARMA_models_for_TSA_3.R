# Critertion to see that a model is good fit are - 
# 1. AIC 2. BIC 3. Ljung-Box Test
# Autoregressive Moving Average of order p, q, or ARMA(p,q)
# p=1, q=1
set.seed(1)
x <- arima.sim(n=1000, model=list(ar=0.5, ma=-0.5)) # simulate ARMA(1,1)
plot(x)
acf(x)
# Fitting ARMA
arima(x, order=c(1, 0, 1))
0.6785 + c(-1.96, 1.96)*0.1673 # CI for AR parameter
-0.7311 + c(-1.96, 1.96)*0.1550 # CI for MA parameter
# CI contain both of our parameters

# p=2, q=2
set.seed(1)
x2 <- arima.sim(n=1000, model=list(ar=c(0.5, -0.25), ma=c(0.5, -0.3)))
plot(x2)
acf(x2)
arima(x2, order=c(2, 0, 2))
# Fitting ARMA
0.653 + c(-1.96, 1.96)*0.0802
-0.229 + c(-1.96, 1.96)*0.0346
0.319 + c(-1.96, 1.96)*0.0792
-0.552 + c(-1.96, 1.96)*0.0771
# CI do not involves the values of the parameter of MA