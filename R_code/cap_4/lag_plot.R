library
library(readxl)
library(ggplot2)
library(tidyverse)
library(reshape2)
library("GmAMisc")
library(ggpubr)


root <- "C:/Users/matte/Desktop/papers/tesi/grafici cap 4/daily/"

# returns_i <- read_excel('dataset.xlsx', sheet = 'Rendimenti log')
returns_c <- read_excel('dati_crypto.xlsx')
start_date <- max(min(returns_i$btc_date), min(returns_c$btc_date))
end_date <- min(max(returns_i$btc_date), max(returns_c$btc_date))


nomi <- c("bric_date","bric","sp500_date","sp500","eurostoxx_date","eurostoxx","nasdaq_date","nasdaq",
          "bond_europe_date","bond_europe","bond_us_date","bond_us","bond_eur_date","bond_eur","eur_date",
          "eur","gbp_date","gbp","chf_date","chf","jpy_date","jpy","gold_date","gold","wti_date","wti",
          "grain_date","grain","metal_date","metal")
returns_inv <- returns_i[order(nrow(returns_i):1),nomi]
idx_ds <- which(returns_inv$bric_date >= start_date & returns_inv$bric_date <= end_date)
returns_ds <- returns_inv[idx_ds,]

idx_ds <- which(returns_c$btc_date >= start_date & returns_c$btc_date <= end_date)
returns_cripto <- returns_c[idx_ds,]

ds = returns_ds
instr <- c("bric","sp500","eurostoxx","nasdaq","bond_europe","bond_us","bond_eur","eur","gbp","chf",
          "jpy","gold","wti","grain","metal")
# 
# ds = returns_cripto
# instr <- c("eth","xrp","btc","ltc")

##--------------------------------------------------------------------------------
#   hist
##--------------------------------------------------------------------------------
for(x in instr){
  nome = x
  
  medium_el <- round(length(ds[[nome]])/2,0)
  
  # hi1 <- ggplot(data = ds[1:medium_el,], aes(ds[[nome]][1:medium_el])) + 
  #   geom_histogram(binwidth = 0.001) +
  #   # ylim(0,250) +
  #   # xlim(-0.4,0.4) +
  #   labs(title = paste(nome," 1st part"), x = "returns", y = "freq")
  # 
  # hi2 <- ggplot(data = ds[medium_el:length(ds[[nome]]),], aes(ds[[nome]][medium_el:length(ds[[nome]])])) + 
  #   geom_histogram(binwidth = 0.001) +
  #   # ylim(0,250) +
  #   # xlim(-0.4,0.4) +
  #   labs(title = paste(nome," 2nd part"), x = "returns", y = "freq")
  # 
  # ggarrange(hi1, hi2,
  #           ncol = 2, nrow = 1)
  # ggsave(paste(root, nome, "_hist.png"), plot = last_plot(), device = NULL, path = NULL,
  #        scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"),
  #        dpi = 300, limitsize = TRUE)
  # 
  ##--------------------------------------------------------------------------------
  #   lagplot
  ##--------------------------------------------------------------------------------
  limit <- max(abs(ds[[nome]]))
  lim <- 0.06
  ggplot(data = ds, aes(x = ds[[nome]], y = c(tail(ds[[nome]],-1),0))) +
    geom_point() +
    ylim(-lim,lim) +
    xlim(-lim,lim) +
    theme_minimal() +
    # theme(panel.background = element_blank(),
    #       panel.grid.major = element_blank(), 
    #       panel.grid.minor = element_blank(),
    #       axis.line = element_segment(),
    # ) +
    labs(title = paste(nome), x = "returns t", y = "returns t+1") +
    theme(plot.title = element_text(hjust = 0.5,size=24,face="bold"),
          axis.text=element_text(size=17),
          axis.title=element_text(size=17))
  
  
  # ggsave(paste(root, nome, "_lag1d.png"), plot = last_plot(), device = NULL, path = NULL,
  #        scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"),
  #        dpi = 100, limitsize = TRUE)
}






