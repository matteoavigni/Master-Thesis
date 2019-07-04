setwd("C:/Users/matte/Desktop/MasterThesis-vianello/R_code")
setwd("C:/Users/matte/Desktop/Master-Thesis/Master-Thesis/R_code")

load("crypto_data.rda")
load("crypto_returns.rda")

library(readxl)
require(rugarch)
library(ggplot2)
library(forecast)
require(graphics)

library("dplyr")
library("ggpubr")


n = length(crypto_data$btc_date)
N = 200
x11()
par(mfrow = c(2,2))
plot(crypto_data$btc_date[(n-N):n],crypto_data$btc[(n-N):n],type='l')
plot(crypto_data$eth_date[(n-N):n],crypto_data$eth[(n-N):n],type='l')
plot(crypto_data$xrp_date[(n-N):n],crypto_data$xrp[(n-N):n],type='l')
plot(crypto_data$ltc_date[(n-N):n],crypto_data$ltc[(n-N):n],type='l')

graphics.off()



corr_crypto = cor(crypto_returns[1:n,2*1:4])*100
corr_prices = cor(crypto_data[,2*1:4])




