
library(readxl)
require(rugarch)
library(ggplot2)
library(forecast)
theme_set(theme_minimal())
require(graphics)

library("dplyr")
library("ggpubr")

btc <- na.omit(read.csv("C:/Users/matte/Desktop/Master-Thesis/Master-Thesis/R_code/BTC-USD.csv"))
spx  <- na.omit(read.csv("C:/Users/matte/Desktop/Master-Thesis/Master-Thesis/R_code/spx.csv"))

btc$returns = c(0,diff(as.matrix(log(btc['Close'])), lag =1))
btc$square_res = c(btc$returns**2)
btc$Date = as.Date(btc$Date)
spx$returns = c(0,diff(as.matrix(log(spx['Close'])), lag =1)*100)


# BTC returns
ggplot(data = btc, mapping = aes(x = Date, y = returns, group=1)) +
  geom_line(color = "#00AFBB", size = 1) +
  ggtitle('BTC returns') 



# qqplot btc
qqnorm(btc$returns)
qqline(btc$returns, col='red')

# qqplot residuals squares
qqnorm(btc$square_res)
qqline(btc$square_res, col='red')


# qqplot spx
qqnorm(spx$returns)
qqline(spx$returns, col='red')


#### AUTOCORRELATION ####
# btc returns
acf(abs(btc$returns), lag.max = NULL,
    type = c("correlation", "covariance", "partial"),
    plot = TRUE, na.action = na.fail, demean = TRUE)

# btc returns square
acf(btc$square_res, lag.max = NULL,
    type = c("correlation", "covariance", "partial"),
    plot = TRUE, na.action = na.fail, demean = TRUE)

# acf(spx$returns, lag.max = NULL,
#     type = c("correlation", "covariance", "partial"),
#     plot = TRUE, na.action = na.fail, demean = TRUE)

### Lag Plot ###
gglagchull(btc$returns)

### Ljung-Box test ###
lag.length = 25
Box.test(btc$returns, lag=lag.length, type="Ljung-Box") # test stationary signal
# Box.test(spx$returns, lag=lag.length, type="Ljung-Box") # test stationary signal

### Augmented Dickey-Fuller (ADF) t-statistic test ###
options(warn=-1)
library(tseries)

adf.test(btc$returns)
adf.test(spx$returns)

density(btc$returns)
curve(dnorm(x,mean=m, sd = varia), add=TRUE)
