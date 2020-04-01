setwd("C:/Users/matte/Desktop/codici")
library
library(readxl)
library(ggplot2)
library(tidyverse)
library(reshape2)
library("GmAMisc")
library(ggpubr)
library(scales)
library(moments)


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
idx_cr <- which(returns_c$btc_date >= start_date & returns_c$btc_date <= end_date)
instr <- c("bric","sp500","eurostoxx","nasdaq","bond_europe","bond_us","bond_eur","eur","gbp","chf",
           "jpy","gold","wti","grain","metal")



##----------------------------------------------------------------------------------------
#    DATASET
##----------------------------------------------------------------------------------------
#          dataset indici
##----------------------------------------------------------------------------------------
returns_ds <- returns_inv[idx_ds,]
#         dataset crypto
##----------------------------------------------------------------------------------------
returns_cripto <- returns_c[idx_cr,]


##----------------------------------------------------------------------------------------
#  Sharp rolling
##----------------------------------------------------------------------------------------

# annualize_factor <- 365
annualize_factor <- 252
sample_length <- annualize_factor*2
# nomicolonne <- c("btc","eth","ltc","xrp")
nomicolonne <- c("bric","sp500","eurostoxx","nasdaq","bond_europe","bond_us","bond_eur","eur","gbp","chf",
            "jpy","gold","wti","grain","metal")

df <- returns_ds
# df <- returns_cripto

attach(df)
reference_dates = bric_date
# reference_dates = btc_date
rets <- data.frame("date" = as.Date(reference_dates[(1+ sample_length):length(reference_dates)]))
stddev <- data.frame("date" = as.Date(reference_dates[(1+ sample_length):length(reference_dates)]))
skew <- data.frame("date" = as.Date(reference_dates[(1+ sample_length):length(reference_dates)]))
kurt <- data.frame("date" = as.Date(reference_dates[(1+ sample_length):length(reference_dates)]))

for(x in nomicolonne){
  idx <- which(reference_dates >= reference_dates[1] & reference_dates <= reference_dates[1+sample_length])
  er <- mean(as.double(unlist(df[idx,x]))) *100 #*annualize_factor
  dev <- sd(as.double(unlist(df[idx,x])))*10#*sqrt(annualize_factor)
  skewnnn <- skewness(as.double(unlist(df[idx,x])))
  kurttt <- kurtosis(as.double(unlist(df[idx,x])))
  
  for(i in 2:(length(reference_dates)-sample_length)){
    idx <- which(reference_dates >= reference_dates[i] & reference_dates <= reference_dates[sample_length+i])# & btc != 0)
    er <- append(er, mean(as.double(unlist(df[idx,x])))*100 )#*annualize_factor
    dev <- append(dev, sd(as.double(unlist(df[idx,x])))*10 )#*sqrt(annualize_factor)
    skewnnn <- append(skewnnn, skewness(as.double(unlist(df[idx,x]))) )
    kurttt <- append(kurttt, kurtosis(as.double(unlist(df[idx,x]))))
  }
  rets[x] <- er
  stddev[x] <- dev
  skew[x] <- skewnnn
  kurt[x] <- kurttt
}


nome = 'sp500'
ggplot() +
  geom_line(data = rets, mapping = aes(x=date, y=sp500, color="avg")) +
  geom_line(data = stddev, mapping = aes(x=date, y=sp500, color="std")) +
  geom_line(data = skew, mapping = aes(x=date, y=sp500, color="skewness")) +
  geom_line(data = kurt, mapping = aes(x=date, y=sp500, color="kurtosis")) +
  ggtitle(nome) +
  # ylab("Prameters") +
  scale_color_manual(values = c("#FF7558","#80C4FF","#FFE562","#90FFA4")) + #
  theme_bw() +
  ylim(-2,50) +
  scale_x_date(labels = date_format("%m-%Y")) +
  theme(plot.title = element_text(hjust = 0.5,size=22,face="bold"),
        # panel.border = element_blank(),
        # panel.grid.major = element_blank(),
        axis.title.y=element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.spacing.y = unit(0, "mm"), 
        aspect.ratio = 1, axis.text = element_text(colour = 1, size = 10),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        axis.title=element_text(size=14),
        axis.text.x = element_text(angle = 45, hjust = 1)) 

# root = 'C:/Users/matte/Desktop/papers/tesi/grafici cap 4/id/ret/'
# ggsave(paste(root, nome, "_lag1d_200.png"), plot = last_plot(), device = NULL, path = NULL,
#        scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"),
#        dpi = 100, limitsize = TRUE)

















