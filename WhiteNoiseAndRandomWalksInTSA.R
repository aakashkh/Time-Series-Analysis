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
# Check that simulated random walk is good
" definition of a random walk, which is simply that the difference between two 
neighbouring values is equal to a realisation from a discrete white noise process.
Hence, if we create a series of the differences of elements from our simulated series, 
we should have a series that resembles discrete white noise "
acf(diff(x)) # output shows that it is a white noise and hence good fit

#Random walk model on Microsfot (MSFT) data
library(quantmod)
getSymbols('MSFT', src = 'yahoo')
MSFT # microsoft data
# Op(MSFT) , Hi(MSFT) , Lo(MSFT) , Cl(MSFT) , Vo(MSFT) , Ad(MSFT) .
# The above commands can be used to obtain the Open, High, Low, Close, Volume and Adjusted Close prices for the Microsoft stock

acf(diff(Ad(MSFT)), na.action = na.omit)
#na.action = na.omit ignores missing values by omitting them
#Output suggest that adjusting close price of Microsoft is a random walk

# For S&P Index
getSymbols('^GSPC', src='yahoo')
acf(diff(Ad(GSPC)), na.action = na.omit)
#Output suggest that there are many peaks at different levels and hence random walk is not a good fit.
