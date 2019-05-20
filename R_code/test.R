
library(readxl)
require(rugarch)
library(ggplot2)
require(graphics)

library("dplyr")
library("ggpubr")

btc <- na.omit(read.csv("C:/Users/matte/Desktop/Master-Thesis/R_code/BTC-USD.csv"))
spx  <- na.omit(read.csv("C:/Users/matte/Desktop/Master-Thesis/R_code/spx.csv"))

btc$returns = c(0,diff(as.matrix(log(prices['Close'])), lag =1)*100)
spx$returns = c(0,diff(as.matrix(log(spx['Close'])), lag =1)*100)

# qqplot btc
qqnorm(btc$returns)
qqline(btc$returns, col='red')

# qqplot spx
qqnorm(spx$returns)
qqline(spx$returns, col='red')


#### AUTOCORRELATION ####
acf(btc$returns, lag.max = NULL,
    type = c("correlation", "covariance", "partial"),
    plot = TRUE, na.action = na.fail, demean = TRUE)

acf(spx$returns, lag.max = NULL,
    type = c("correlation", "covariance", "partial"),
    plot = TRUE, na.action = na.fail, demean = TRUE)


### Ljung-Box test ###
lag.length = 25
Box.test(btc$returns, lag=lag.length, type="Ljung-Box") # test stationary signal
Box.test(spx$returns, lag=lag.length, type="Ljung-Box") # test stationary signal

### Augmented Dickey-Fuller (ADF) t-statistic test ###
options(warn=-1)
library(tseries)

adf.test(btc$returns)
adf.test(spx$returns)



