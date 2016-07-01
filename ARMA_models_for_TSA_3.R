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
"In order to determine which order of the ARMA model is appropriate 
for a series, we needto use the AIC (or BIC) across a subset of values 
for , and then apply the Ljung-Box test todetermine if a good fit has 
been achieved, for particular values of"

# ARMA (3,2), best ARMA(p,q) model
set.seed(3)
x3 <- arima.sim(n=1000, model=list(ar=c(0.5, -0.25, 0.4), ma=c(0.5, -0.3)))
# Check which is best fit
final.aic <- Inf
final.order <- c(0,0,0)
for (i in 0:4) 
  for (j in 0:4) {
    current.aic <- AIC(arima(x3, order=c(i, 0, j)))
    if (current.aic < final.aic) {
      final.aic <- current.aic
      final.order <- c(i, 0, j)
      final.arma <- arima(x3, order=final.order)
  }
  }
# final.aic = 2863.365
# final.order = 3 0 2
# Check if residuals of the model are Discrete White Noise (DWN) or not
acf(resid(final.arma)) # actually a DWN
# Ljung-Box test for 20 lags to confirm this
Box.test(resid(final.arma), lag=20, type="Ljung‐Box")
# p-value = 0.869 > 0.05 and hence model is good fit

# ARMA(p,q) on financial data -  S&P500 US Equity Index
library(quantmod)
getSymbols("^GSPC")
sp = diff(log(Cl(GSPC)))
# check which model fit best
spfinal.aic <- Inf
spfinal.order <- c(0,0,0)
for (i in 0:4) 
  for (j in 0:4) {
    spcurrent.aic <- AIC(arima(sp, order=c(i, 0, j)))
    if (spcurrent.aic < spfinal.aic) {
      spfinal.aic <- spcurrent.aic
      spfinal.order <- c(i, 0, j)
      spfinal.arma <- arima(sp, order=spfinal.order)
    }
  }

spfinal.order
acf(resid(spfinal.arma), na.action=na.omit) # residuals of the fitted model
# Sort of poor fit, but better than previous
# Ljung-Box test to see whether ARMA(3,3) a good fit or not
Box.test(resid(spfinal.arma), lag=20, type="Ljung‐Box")
# p-value = 0.01034 is less than .05 and hence not a good model
