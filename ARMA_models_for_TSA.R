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

#AR (2) Parameters -a1 = 0.666 and a2 = -0.333
set.seed(1)
x3 <- w <- rnorm(100)
for (t in 3:100) 
  x3[t] <- 0.666*x3[t-1]-0.333*x3[t-2] + w[t]
layout(1:2)
plot(x3, type="l")
acf(x3)

#Fit ar2 
x3.ar <- ar(x3, method = "mle")
x3.ar$ar

# AR(p) model to financial data
# on Amazon
library(quantmod)
getSymbols('AMZN')
AMZN
str(AMZN)
summary(AMZN)
layout(1:1)
plot(Cl(AMZN))

# Logarithmic returns and first order difference for AMZN
amznrt = diff(log(Cl(AMZN)))
plot(amznrt)
#Checked that differenced series is white noise or not
acf(amznrt, na.action=na.omit) # not a white series
# Fit AR (p) model
amznrt.ar <- ar(amznrt, na.action=na.omit)
amznrt.ar$order
amznrt.ar$ar
amznrt.ar$asy.var
# we see that p = 2, we`ll check CI for both parameters and if they consists zero 
# then that parameter is not useful, it reduces our confidence
-0.02161211 + c(-1.96, 1.96)*sqrt(0.0004175039) #  -0.06166062  0.01843640
-0.06193300 + c(-1.96, 1.96)*sqrt(0.0004175039) #  -0.10198151 -0.02188449
# Parameter 1  contain 0
# Parametr 2 do not contain 0 and hence be careful with using AR(2)
# Also AR ignores volatility clustering

# On S&P 500
getSymbols("^GSPC")
GSPC
str(GSPC)
summary(GSPC)
plot(Cl(GSPC))
# first order differences of log prices
gspcrt = diff(log(Cl(GSPC)))
plot(gspcrt)
# Volatility clustering visible
acf(gspcrt, na.action=na.omit) # Not white noise
# Long memory processes significant correlation at higher lag  
# Try to fit AR
gspcrt.ar <- ar(gspcrt, na.action=na.omit)
gspcrt.ar$order
gspcrt.ar$ar # 20 parameters
# hence complex model and AR is not suitable

