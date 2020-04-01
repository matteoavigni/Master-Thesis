library(readxl)

returns = read_excel('dati_corr_crypto.xlsx')
# returns = read_excel('dataset.xlsx', sheet = 'Rendimenti log')
attach(returns)

library(readxl)
library(readxl)
library(ggplot2)
library(tidyverse)
library(reshape2)
library("GmAMisc")


##-------------------------------------------------------------------------
#  Correlazioni rolling
##-------------------------------------------------------------------------
nomicolonne <- c("ltc","xrp")
for(x in nomicolonne){
  nomi <- c("eth",x)
  sample_length <- 90  # 3 mesi
  idx = which(btc_date >= btc_date[1] & btc_date <= btc_date[1+sample_length])
  correl <- (cor(returns[idx,nomi]))[nomi[1], nomi[2]]
  
  for(i in 2:(length(btc)-sample_length)){
    idx = which(btc_date >= btc_date[i] & btc_date <= btc_date[sample_length+i] & btc != 0)
    correl <- append(correl, (cor(returns[idx,nomi]))[nomi[1], nomi[2]])
  }
  corr_plot_12 <- data.frame("date" = btc_date[(1+ sample_length):length(btc_date)])
  corr_plot_12$corr <- correl
  
  sample_length <- 365  #12 mesi
  idx = which(btc_date >= btc_date[1] & btc_date <= btc_date[1+sample_length])
  correl <- (cor(returns[idx,nomi]))[nomi[1], nomi[2]]
  
  for(i in 2:(length(btc)-sample_length)){
    idx = which(btc_date >= btc_date[i] & btc_date <= btc_date[sample_length+i] & btc != 0)
    correl <- append(correl, (cor(returns[idx,nomi]))[nomi[1], nomi[2]])
  }
  corr_plot_18 <- data.frame("date" = btc_date[(1+ sample_length):length(btc_date)])
  corr_plot_18$corr <- correl
  
  
  ##-------------------------------------------------------------------------
  #  Plot corr
  ##-------------------------------------------------------------------------
  result <- paste (nomi[1], nomi[2], sep=" - ") 
  theme_update(plot.title = element_text(hjust = 0.5, size =20))
  ggplot()+
    geom_line(data=corr_plot_12,aes(y=corr,x= date),size=0.7) + 
    geom_line(data=corr_plot_18,aes(y=corr,x= date),size=0.7, color="blue") +
    theme(legend.position = "none", panel.background = element_blank(),
          panel.grid.major = element_line(colour = "grey")) +
    ylim(-0.6,1) +
    labs(title = result, x = "end_date", y = "corr")
  
  # filename <- paste("C:/Users/matte/Desktop/papers/tesi/grafici cap 3/corr crypto",result, sep="/")
  # filename <- paste(filename,"png",sep = ".")
  # ggsave(filename, plot = last_plot(), device = NULL, path = NULL,
  #        scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"),
  #        dpi = 100, limitsize = TRUE)
  
  ##-------------------------------------------------------------------------
  #  significatività
  ##-------------------------------------------------------------------------
  
  sample_length <- 90  # 3 mesi
  idx = which(btc_date >= btc_date[1] & btc_date <= btc_date[1+sample_length])
  vec1 <- as.numeric(unlist(returns[idx,nomi[1]]))
  vec2 <- as.numeric(unlist(returns[idx,nomi[2]]))
  p_values <- cor.test(vec1, vec2)$p.value
  
  for(i in 2:(length(btc_date)-sample_length)){
    idx = which(btc_date >= btc_date[i] & btc_date <= btc_date[sample_length+i] & btc != 0)
    vec1 <- as.numeric(unlist(returns[idx,nomi[1]]))
    vec2 <- as.numeric(unlist(returns[idx,nomi[2]]))
    p_values <- append(p_values, cor.test(vec1, vec2)$p.value)
  }
  
  pval_plot_12 <- data.frame("date" = btc_date[(1+ sample_length):length(btc_date)])
  pval_plot_12$pval <- p_values
  
  sample_length <- 365  #12 mesi
  idx = which(btc_date >= btc_date[1] & btc_date <= btc_date[1+sample_length])
  vec1 <- as.numeric(unlist(returns[idx,nomi[1]]))
  vec2 <- as.numeric(unlist(returns[idx,nomi[2]]))
  p_values <- cor.test(vec1, vec2)$p.value
  
  for(i in 2:(length(btc)-sample_length)){
    idx = which(btc_date >= btc_date[i] & btc_date <= btc_date[sample_length+i] & btc != 0)
    vec1 <- as.numeric(unlist(returns[idx,nomi[1]]))
    vec2 <- as.numeric(unlist(returns[idx,nomi[2]]))
    p_values <- append(p_values, cor.test(vec1, vec2)$p.value)
  }
  
  pval_plot_18 <- data.frame("date" = btc_date[(1+ sample_length):length(btc_date)])
  pval_plot_18$pval <- p_values
  
  
  ##-------------------------------------------------------------------------
  #  Plot p-values
  ##-------------------------------------------------------------------------
  
  
  theme_update(plot.title = element_blank())#element_text(hjust = 0.5, size = 20))
  ggplot()+
    geom_line(data=pval_plot_12,aes(y=pval,x= date),size=0.7) + 
    geom_point(data=pval_plot_12,aes(y=pval,x= date),size=1.5) + 
    geom_line(data=pval_plot_18,aes(y=pval,x= date),size=0.7, color = "blue") +
    geom_point(data=pval_plot_18,aes(y=pval,x= date),size=1.5, color = "blue") +
    geom_line(data=pval_plot_12,aes(y=0.05,x= date),size=0.7, color = "red") +
    theme(legend.position = "none", panel.background = element_blank(),
          panel.grid.major = element_line(colour = "grey")) +
    ylim(0,1) +
    labs(title = result, x = "end_date", y = "p_val")
  
  result <- paste(result,"pval",sep = "-")
  # filename <- paste("C:/Users/matte/Desktop/papers/tesi/grafici cap 3/corr crypto",result, sep="/")
  # filename <- paste(filename,"png",sep = ".")
 # ggsave(filename, plot = last_plot(), device = NULL, path = NULL,
 #        scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"),
 #        dpi = 100, limitsize = TRUE)
  
} 




