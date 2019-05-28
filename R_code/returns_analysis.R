library(readxl)
require(rugarch)
library(ggplot2)
library(forecast)
theme_set(theme_minimal())
require(graphics)

library("dplyr")
library("ggpubr")


prices = read_excel('C:/Users/matte/Desktop/Master-Thesis/Master-Thesis/Excelsheets/prices.xlsx')
# bitcoin prices
postscript(paste("C:/Users/matte/Desktop/Master-Thesis/Master-Thesis/20190529/BTCprices.eps", sep =""), horizontal = FALSE, onefile = FALSE, width = 4.0, height = 3.0)
ggplot(data = prices, mapping = aes(x = date, y = BITCOIN, group=1)) +
  geom_line(color = "#00AFBB", size = 1) +
  ggtitle('BTC returns')
dev.off()

# reutrns
returns = data.frame(matrix(ncol = length(c(names(prices))) , nrow = 2165))
colnames(returns) <- x <- c(names(prices))


for(n in x[2:18]){
  returns[n] =  c(0,diff(as.matrix(log(prices[n])), lag =1))
}
returns['date'] <- prices['date']
postscript(paste("C:/Users/matte/Desktop/Master-Thesis/Master-Thesis/20190529/BTCreturns.eps", sep =""), horizontal = FALSE, onefile = FALSE, width = 4.0, height = 3.0)
ggplot(data = prices, mapping = aes(x = date, y = BITCOIN, group=1)) +
  geom_line(color = "#00AFBB", size = 1) +
  ggtitle('BTC returns')
dev.off()

ggplot(data = returns, mapping = aes(x = date, y = BITCOIN, group=1)) +
  geom_line(color = "#00AFBB", size = 1) +
  ggtitle('BTC returns') 


# qqplot
for(n in x[2:18]){  
  postscript(paste("C:/Users/matte/Desktop/Master-Thesis/Master-Thesis/20190529/",n,"qqplot.eps", sep =""), horizontal = FALSE, onefile = FALSE, width = 4.0, height = 3.0)
  qqnorm(as.numeric(unlist(returns[n])), main = NA, xlab = NA, ylab = NA)
  qqline(as.numeric(unlist(returns[n])), col='red', xlab = NA, ylab = NA)
  dev.off()
}


### Lag Plot ###
for(n in x[2:18]){  
  postscript(paste("C:/Users/matte/Desktop/Master-Thesis/Master-Thesis/20190529/",n,"lagplot.eps", sep =""), horizontal = FALSE, onefile = FALSE, width = 4.0, height = 3.0)
  lag.plot(as.numeric(unlist(returns[n])), lag=1, diag = FALSE, do.lines = FALSE) 
  dev.off()
}

postscript(paste("C:/Users/matte/Desktop/Master-Thesis/Master-Thesis/20190529/BTClagplot2.eps", sep =""), horizontal = FALSE, onefile = FALSE, width = 4.0, height = 3.0)
lag.plot(as.numeric(unlist(returns[n])), lag=2, diag = FALSE, do.lines = FALSE) 
dev.off()

#### AUTOCORRELATION returns####
# btc returns
postscript(paste("C:/Users/matte/Desktop/Master-Thesis/Master-Thesis/20190529/BTCacf.eps", sep =""), horizontal = FALSE, onefile = FALSE, width = 4.0, height = 3.0)
acf(returns['BITCOIN'], lag.max = NULL,
    type = c("correlation", "covariance", "partial"),
    plot = TRUE, na.action = na.fail, demean = TRUE)
dev.off()
postscript(paste("C:/Users/matte/Desktop/Master-Thesis/Master-Thesis/20190529/SPacf.eps", sep =""), horizontal = FALSE, onefile = FALSE, width = 4.0, height = 3.0)
acf(returns['S&P500'], lag.max = NULL,
    type = c("correlation", "covariance", "partial"),
    plot = TRUE, na.action = na.fail, demean = TRUE)
dev.off()

#### AUTOCORRELATION abs returns####
# btc abs returns
postscript(paste("C:/Users/matte/Desktop/Master-Thesis/Master-Thesis/20190529/BTCabsacf.eps", sep =""), horizontal = FALSE, onefile = FALSE, width = 4.0, height = 3.0)
acf(abs(returns['BITCOIN']), lag.max = NULL,
    type = c("correlation", "covariance", "partial"),
    plot = TRUE, na.action = na.fail, demean = TRUE)
dev.off()
postscript(paste("C:/Users/matte/Desktop/Master-Thesis/Master-Thesis/20190529/SPabsacf.eps", sep =""), horizontal = FALSE, onefile = FALSE, width = 4.0, height = 3.0)
acf(abs(returns['S&P500']), lag.max = NULL,
    type = c("correlation", "covariance", "partial"),
    plot = TRUE, na.action = na.fail, demean = TRUE)
dev.off()


