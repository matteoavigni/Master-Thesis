library(mondate)
setwd("C:/Users/matte/Desktop/MasterThesis-vianello/R_code")
setwd("C:/Users/matte/Desktop/Master-Thesis/Master-Thesis/R_code")
load("returns.rda")
load("data.rda")
load("my_data.rda")
load("my_returns.rda")
load("crypto_data.rda")
load("crypto_returns_w.rda")
load("crypto_returns_m.rda")
load("crypto_returns.rda")
load("my_data_final.rda")
load("my_returns_final.rda")
attach(my_returns_final)
attach(crypto_returns)

dates = as.Date(btc_date, origin="1899-12-30")


earliest = "2018-01-01"
# latest = max(dates)
latest = "2020-02-02"

w = 46 # rolling window in months
step = 1 # moving step in months

d = as.Date(mondate(latest, timeunits = )-step,origin="1899-12-30")
beg_times = as.Date(mondate(latest)-w)
end_times = latest
beg_times = earliest


n_assets = (dim(crypto_returns)[2]%/%2)
idx = which(dates >= beg_times & dates<=end_times)
correlations = cor(btc[idx],my_returns[idx,2*(1:n_assets)])
returns_2015 = my_returns[idx,2*(1:n_assets)]

corr2015 = cor(returns_2015)*100
corr_tot = cor(crypto_returns[idx,2*(1:n_assets)])*100
corr_final = cor(my_returns_final[idx,2*(1:n_assets)])*100

my_returns = my_returns[idx,]
n_assets = (dim(my_returns)[2]%/%2)

corr = cor(my_returns[,2*(1:n_assets)])*100



