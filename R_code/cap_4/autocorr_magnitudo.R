library
library(readxl)
library(ggplot2)
library(tidyverse)
library(reshape2)
library("GmAMisc")
library(ggpubr)



ggacf <- function(series) {
  significance_level <- qnorm((1 + 0.95)/2)/sqrt(sum(!is.na(series)))  
  a<-acf(series, lag.max= 15, plot=F)
  a.2<-with(a, data.frame(lag, acf))
  g<- ggplot(a.2[-1,], aes(x=lag,y=acf)) + 
    geom_bar(stat = "identity", position = "identity") + xlab('Lag') + ylab('ACF') +
    geom_hline(yintercept=c(significance_level), lty=3) +
    ylim(-0.5,0.5)
  
  # fix scale for integer lags
  if (all(a.2$lag%%1 == 0)) {
    g<- g + scale_x_discrete(limits = seq(1, max(a.2$lag)));
  }
  return(g);
}


root <- "C:/Users/matte/Desktop/papers/tesi/grafici cap 4/autocorr magnitudo/magnitudo/"

returns_i <- read_excel('dataset.xlsx', sheet = 'Rendimenti log')
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

instr <- c("bric","sp500","eurostoxx","nasdaq","bond_europe","bond_us","bond_eur","eur","gbp","chf",
           "jpy","gold","wti","grain","metal")


# ds = returns_ds
instr <- c("btc","eth","ltc","xrp")
ds = returns_cripto
##--------------------------------------------------------------------------------
#   hist
##--------------------------------------------------------------------------------
for(x in instr){
  nome = x
  
  ##--------------------------------------------------------------------------------
  #   lagplot
  ##--------------------------------------------------------------------------------
  p <- ggacf(abs(ds[[nome]]))
  p +
    labs(title = nome) +
    theme_bw() +
    ylab("Autocorrelation") +
    ylim(0,0.5) +
    theme(plot.title = element_text(hjust = 0.5,size=20,face="bold"),
          axis.title = element_text(size=12),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          legend.spacing.y = unit(0, "mm"), 
          aspect.ratio = 1,
          axis.text = element_text(colour = 1, size = 8),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black")) +
    
  

  ggsave(paste(root, nome, "_abs_autocor.png"), plot = last_plot(), device = NULL, path = NULL,
         scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"),
         dpi = 300, limitsize = TRUE)
}
  
