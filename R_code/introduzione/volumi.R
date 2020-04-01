setwd("C:/Users/matte/Desktop/papers/tesi/introduzione volumi")
library(readxl)
library(ggplot2)
library(tidyverse)
library(reshape2)
library("GmAMisc")
library(ggpubr)


volumi <- read_excel('C:/Users/matte/Desktop/papers/tesi/introduzione volumi/volumi.xlsx')
df <- as.data.frame(volumi)

nomi <- c("BTC","ETH","XRP","LTC","BCH","EOS","ETC","ZEC","ADA","XLM","XMR","BSV")

tot <- list()
perc_btc <- list()
perc_ltc <- list()
perc_xrp <- list()
perc_eth <- list()
sample_length = 30
for(i in (1+sample_length):length(df[,"DATA"])){
  tot <- append(tot,sum(mean(df[i,nomi])))
  perc_btc <- append(perc_btc,df[i,"BTC"]/sum(df[i,nomi])*100)
  perc_ltc <- append(perc_ltc,df[i,"LTC"]/sum(df[i,nomi])*100)
  perc_xrp <- append(perc_xrp,df[i,"XRP"]/sum(df[i,nomi])*100)
  perc_eth <- append(perc_eth,df[i,"ETH"]/sum(df[i,nomi])*100)
}

nomi <- c("BTC","ETH","XRP","LTC","BCH","EOS","ETC","ZEC","ADA","XLM","XMR","BSV")
volumes <- data.frame("DATA" = df[(sample_length+1):length(df[,"DATA"]),"DATA"])
for(x in nomi){
  tot = mean(as.double(rowSums(df[1:sample_length,nomi])))
  vol <- mean(as.double(df[1:sample_length,x]))/tot*100
  for(i in (2+sample_length):length(df[,"DATA"])){
    tot = mean(as.double(rowSums(df[(i-sample_length):(i-1),nomi])))
    vol <- append(vol,mean( as.double( df[(i-sample_length):(i-1),x]) )/tot *100)
  }
  volumes[x] <- vol
}
# volumes$BTC_LTC_XRP_ETH = rowSums(volumes[,c("ETH","BTC","XRP","LTC")])
sum_4 <- data.frame("DATA" = df[(sample_length+1):length(df[,"DATA"]),"DATA"])
sum_4["variable"] <- rowSums(volumes[,c("BTC","ETH","XRP","LTC")])
volumes_plot <- melt(volumes, id="DATA")

# rapresented <- unlist(perc_btc) + unlist(perc_ltc) + unlist(perc_xrp) + unlist(perc_eth)
# df$perc_rapr <- rapresented
# df$perc_btc <- unlist(perc_btc)
# df$perc_ltc <- unlist(perc_ltc)
# df$perc_xrp <- unlist(perc_xrp)
# df$perc_eth <- unlist(perc_eth)


ggplot(data = volumes_plot,aes(x=DATA,y=value,color=variable)) +
  geom_line() +
  # scale_color_manual(values = c("#FF7558","#FFE562","#80C4FF","#90FFA4","#A4007C","#000000")) +
  ylim(0,100) +
  xlab("dates") +
  ylab("% of Total Volume Exchanged") +
  # ggtitle("Daily",subtitle = "Considero il totale come somma di tutte") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())


# stacked area chart
ggplot() + 
  geom_area(data = volumes_plot,aes(x=DATA,y=value,fill=variable)) +
  geom_line(data=sum_4,aes(x=DATA,y=variable),color='black') + #,color="BTC + ETH + XRP + LTC"
  xlab("dates") +
  ylab("% of Total Volume Exchanged") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())



ggplot(data = df) +
  geom_line(aes(x = DATA, y = BTC, color="BTC"), color="blue") +
  geom_line(aes(x = DATA, y = ETH, color="ETH"), color="red") +
  geom_line(aes(x = DATA, y = XRP, color="XRP"), color="green") +
  geom_line(aes(x = DATA, y = LTC, color="LTC"), color="orange") +
  geom_line(aes(x = DATA, y = BCH, color="BCH")) +
  geom_line(aes(x = DATA, y = EOS, color="EOS")) +
  geom_line(aes(x = DATA, y = ETC, color="ETC")) +
  geom_line(aes(x = DATA, y = ZEC, color="ZEC")) +
  geom_line(aes(x = DATA, y = ADA, color="ADA")) +
  geom_line(aes(x = DATA, y = XLM, color="XLM")) +
  geom_line(aes(x = DATA, y = XMR, color="XMR")) +
  geom_line(aes(x = DATA, y = BSV, color="BSV")) +
  ggtitle("Exchanged volumes") +
  ylab("Volumes") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

  
