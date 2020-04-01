
setwd("C:/Users/matte/Desktop/codici")
library(readxl)


returns = read_excel('C:/Users/matte/Desktop/papers/dataset/dataset.xlsx', sheet = 'Rendimenti log')
attach(returns)

library(readxl)
library(readxl)
library(ggplot2)
library(tidyverse)
library(reshape2)
library("GmAMisc")




cor.perm <- function (x, y, nperm = 499)
{
  r.obs <- cor (x = x, y = y)
  P.par <- cor.test (x = x, y = y)$p.value
  r.per <- replicate (nperm, expr = cor (x = x, y = sample (y)))
  r.per <- sapply (1:nperm, FUN = function (i) cor (x = x, y = sample (y)))
  # r.per <- c(r.per, r.obs)
  hist (r.per, xlim = c(-1,1))
  abline (v = r.obs, col = 'red')
  P.per <- sum (abs (r.per) >= abs (r.obs))/(nperm + 1) 
  return (list (r.obs = r.obs, P.par = P.par, P.per = P.per))
}


##-------------------------------------------------------------------------
#  Correlazioni intero dataset
##-------------------------------------------------------------------------

nomi <- c("btc","sp500")
start_date <- "2010-03-31"
end_date <- "2019-06-30"
idx = which(btc_date >= start_date & btc_date <= end_date) # & btc != 0

cor(returns[idx,nomi])
nomicolonne <- c("bric","sp500","eurostoxx","nasdaq","bond_europe","bond_us","bond_eur",
                 "eur","gbp","chf","jpy","gold","wti","grain","metal","vix")


corr_val <- vector(mode="list", length=length(nomicolonne))
perm_pval <- vector(mode="list", length=length(nomicolonne))
ttest_pval <- vector(mode="list", length=length(nomicolonne))

names(corr_val) <- nomicolonne
names(perm_pval) <- nomicolonne
names(ttest_pval) <- nomicolonne


##-------------------------------------------------------------------------
#  significatività permutation
##-------------------------------------------------------------------------
for (x in nomicolonne){
  nomi <- c("eth",x)
  vec1 <- as.numeric(unlist(returns[,nomi[1]]))
  vec2 <- as.numeric(unlist(returns[,nomi[2]]))
  p_values <- cor.perm(vec1, vec2, nperm = 499)
  corr_val[x] <- p_values$r.obs
  perm_pval[x] <- p_values$P.per
  ttest_pval[x] <- p_values$P.par
  

}



