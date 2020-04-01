library
library(readxl)
library(ggplot2)
library(tidyverse)
library(reshape2)
library("GmAMisc")
library(ggpubr)


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


library(fitdistrplus)
library(logspline)

x <- as.numeric(unlist(returns_ds[,"sp500"]))
ggplot(data=returns_ds, aes(sp500)) + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.spacing.y = unit(0, "mm"), 
        aspect.ratio = 1, axis.text = element_text(colour = 1, size = 12),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  geom_histogram(aes(y=..density..), bins = 35, fill="white", col="black") + 
  geom_density(data=returns_ds, aes(sp500,color="Empirical Distribution")) +
  stat_function(fun = dnorm, args = list(mean = mean(x), sd = sqrt(var(x))), aes(color="Gaussian Distribution")) + 
  geom_hline(color = "black", yintercept = 0) +
  ylab("Density") +
  xlab("returns") +
  xlim(-0.4,0.4)+
  ggtitle("sp500") +
  scale_color_manual(values = c("blue", "red")) +
  labs(color='Density',shape = "", linetype = "")
  


cripto = "btc"
x <- as.numeric(unlist(returns_cripto[,cripto]))
ggplot(data=returns_cripto, aes(btc)) + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5,size=20,face="bold"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.spacing.y = unit(0, "mm"), 
        aspect.ratio = 1, axis.text = element_text(colour = 1, size = 12),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  geom_histogram(aes(y=..density..), bins = 35, fill="white", col="black") + 
  geom_density(aes(color="Empirical Distribution")) +
  stat_function(fun = dnorm, args = list(mean = mean(x), sd = sqrt(var(x))), aes(color="Gaussian Distribution")) + 
  geom_hline(color = "black", yintercept = 0) +
  ylab("Density") +
  xlab("returns") +
  xlim(-0.4,0.4)+
  ylim(0,25) +
  ggtitle(cripto) +
  scale_color_manual(values = c("blue", "red")) +
  labs(color='Density',shape = "", linetype = "")

# ggsave(paste("C:/Users/matte/Desktop/papers/tesi/grafici cap 4/pdf/distr/btcdist.png"), plot = last_plot(), device = NULL, path = NULL,
#        scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"),
#        dpi = 100, limitsize = TRUE)







# descdist(x, boot = 1000, discrete = FALSE)
# 
# plotdist(x, histo = TRUE, demp = TRUE)
# fit <- fitdist(x, "norm")
# plot(fit)
# 
# 
# fg <- fitdist(x, "exp")
# par(mfrow = c(2, 2))
# plot.legend <- c("Weibull", "lognormal", "gamma")
# 
# denscomp(list(fw, fln, fg), legendtext = plot.legend)
# qqcomp(list(fw, fln, fg), legendtext = plot.legend)
# cdfcomp(list(fw, fln, fg), legendtext = plot.legend)
# ppcomp(list(fw, fln, fg), legendtext = plot.legend)
# 
# 
# 
# 
# library(gamlss)
# library(gamlss.dist)
# library(gamlss.add)
# fit <- fitDist(x, k = 2, type = "realplus", trace = FALSE, try.gamlss = TRUE)
# 
# summary(fit)