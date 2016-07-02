# Stochastic Trends can be handled by ARIMA
# Seasonal Effects - Seasonal ARIMA
# Conditional Heteroscedastic effects - ARCH/GARCh
# diff(z, d=3) for repeated difference
# Simulate ARIMA(1,1,1)
set.seed(2)
x <- arima.sim(list(order = c(1,1,1), ar = 0.6, ma=-0.5), n = 1000)
# Non stationary time series with stochastic trending component
plot(x)

# Fit ARIMA
x.arima <- arima(x, order = c(1,1,1))
x.arima
# CI for AR and MA parameters
0.6470 + c(-1.96, 1.96)*0.1065
-0.5165 + c(-1.96, 1.96)*0.1189
# Both parameter estimates fall within CI
# Residuals as the Discrete White Noise (DWN)
acf(resid(x.arima))
# Box-Ljung test to check the model
Box.test(resid(x.arima), lag=20, type = "Ljung-Box")
# p-value = .5191 is > 0.05 and therefore DWN is a godd fit to residuals
