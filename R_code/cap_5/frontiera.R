library(readxl)
library(quadprog)

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
#    Funzione per calcolo frontiera
##----------------------------------------------------------------------------------------
EfficientFrontier = function(r,S,full=FALSE,plot=FALSE, N=100, no_short_sales, max_r=NA, min_r =NA){
  # INPUT
  # r: expected returns of the assets
  # S: covariance matrix of the asset returns
  # full: computes only efficient frontier if FALSE
  # plot: if TRUE the frontier is plotted
  # N: how many expected returns to take into consideration to plot frontier
  # print(r)
  # print(S)
  library(quadprog)
  
  if (is.na(max_r)){
    max_r = max(r)
  }
  if (is.na(min_r)){
    min_r = min(r)
  }
  
  # number of assets
  n= length(r)
  
  D = 2*S       #times 2 because there is a 1/2 in the implicit formulation
  d = rep(0,n)  # zeros
  
  
  # Constraint on returns
  A = t(r)
  
  b = 0.0 # expected return that will be iterated to compute frontier
  
  # Constraint on weights: sum(w)=1
  A = rbind(A,rep(1,n))
  b = rbind(b,1)
  
  # Constraint on short selling on given assets
  for (i in no_short_sales){
    A_i = matrix(0,nrow=1,ncol = n)
    A_i[1,i]=1
    A = rbind(A,A_i)
    b = rbind(b,0)
  }
  
  # yy = set of returns
  # xx = set of corresponding volatility
  yy= seq(from = min(r), to = max_r,length.out = N+1)
  yy[1] = yy[1]+ 1e-5
  yy[N+1] = yy[N+1] -1e-5
  
  b[1]= yy[1]
  # print(yy)
  
  xx = rep(0,length(yy))
  for (i in 1:(N+1)) {
    # print(i)
    # print(yy[i])
    b[1] = yy[i]
    # print(b)
    sol = solve.QP(Dmat = D, dvec = (d), Amat = t(A), bvec = t(b), meq = 2)
    xx[i] = sqrt(sol$value)
    
    # sigma = sqrt(t(sol$solution)%*%S%*%sol$solution)
    # print(paste(xx[i], sigma))
  }
  
  if (!full){
    min_sigma = min(xx)
    # print(min_sigma)
    idx = which(yy>=yy[which(xx==min_sigma)])
    # print(idx)
    xx= xx[idx]
    yy= yy[idx]
  }
  
  res = list(sigma = xx, expected_return=yy)
  return(res)
}
OptimalAllocation_constr_return= function(r,S, target_return = NA, no_short_sales){
  if(is.na(target_return) ){
    stop("Please specify expected return.")
  }
  
  if(target_return > max(r)){
    # print(max(r))
    warning("Cannot produce such a high return without short-selling.")
    target_return=max(r)-1e-5
  }
  
  library(quadprog)
  
  # number of assets
  n= length(r)
  
  D = 2*S       #times 2 because there is a 1/2 in the implicit formulation
  d = rep(0,n)  # zeros
  
  # Constraint on returns
  A = t(r)
  b = target_return
  
  # Constraint on weights: sum(w)=1
  A = rbind(A,rep(1,n))
  b= rbind(b,1)
  
  # Constraints on short selling ony on given assets
  for (i in no_short_sales){
    A_i = rep(0,n)
    A_i[i]=1
    A = rbind(A,A_i)
    b = rbind(b,0)
  }
  
  sol = solve.QP(Dmat = D, dvec = (d), Amat = t(A), bvec = t(b), meq = 2)
  w_opt= sol$solution
  rownames(w_opt)=rownames(r)
  return(w_opt)
}
get_target_ret = function(df_ret){
  sharp <- df_ret$expected_return[1]/df_ret$sigma[1]
  target_return <- df_ret$expected_return[1]
  for(i in 2:length(df_ret$sigma)){
    if(df_ret$expected_return[i]/df_ret$sigma[i]>sharp){
      sharp <- df_ret$expected_return[i]/df_ret$sigma[i]
      target_return <- df_ret$expected_return[i]
      print(i)
    }
  }
  return(target_return)
}

##----------------------------------------------------------------------------------------
#    DATASET
##----------------------------------------------------------------------------------------

#          dataset indici
returns_ds <- returns_inv[idx_ds,]

#         dataset crypto
returns_cripto <- returns_c[idx_cr,]

##----------------------------------------------------------------------------------------
##----------------------------------------------------------------------------------------


all_common_dates <- intersect(returns_ds$bric_date, returns_cripto$btc_date)
common_dates <- all_common_dates[400:length(all_common_dates)]

indexes <- returns_ds[which(returns_ds$bric_date %in% common_dates),instr]
cryptos <- returns_cripto[which(returns_cripto$btc_date %in% common_dates),c("btc","eth","ltc","xrp")]

r <- append(colMeans(indexes)*250,colMeans(cryptos)*365)
S <- cov(cbind(indexes, cryptos))*250



##----------------------------------------------------------------------------------------
#    Calcolo frontiera
##----------------------------------------------------------------------------------------
max_r=0.4
min_r=0

max_r=NA
min_r=NA

no_short_sales <- 1:length(r)
front_no_short <- EfficientFrontier(r,S,full=FALSE,plot=FALSE, N=100, no_short_sales=no_short_sales, max_r=max_r, min_r=min_r)
target_return <- get_target_ret(front_no_short)
opt_w <- OptimalAllocation_constr_return(r,S, target_return = target_return, no_short_sales=no_short_sales)

ordered_list <- c("bric","sp500","eurostoxx","nasdaq","bond_europe",
                  "bond_us","bond_eur","eur","gbp","chf","jpy","gold",
                  "wti","grain","metal","btc","eth","ltc","xrp") 

no_short_sales <- 1:16
front_no_short_BTC_only <- EfficientFrontier(r[1:16],S[1:16,1:16],full=FALSE,plot=FALSE, N=100, no_short_sales=no_short_sales, max_r=max_r, min_r =min_r)
target_return <- get_target_ret(front_no_short_BTC_only)
opt_w_BTC_only <- OptimalAllocation_constr_return(r[1:16],S[1:16,1:16], target_return = target_return, no_short_sales=no_short_sales)


no_short_sales <- 1:15
front_no_short_no_crypto <- EfficientFrontier(r[1:15],S[1:15,1:15],full=FALSE,plot=FALSE, N=100, no_short_sales=no_short_sales, max_r=max_r, min_r =min_r)
target_return <- get_target_ret(front_no_short_no_crypto)
opt_w_no_crypto <- OptimalAllocation_constr_return(r[1:15],S[1:15,1:15], target_return = target_return, no_short_sales=no_short_sales)


# xlimss <- c(min(front_no_short$sigma,front_no_short_BTC_only$sigma,front_no_short_no_crypto$sigma),
#             min(max(front_no_short$sigma),max(front_no_short_BTC_only$sigma),max(front_no_short_no_crypto$sigma)))
# ylimss <- c(min(front_no_short$expected_return,front_no_short_BTC_only$expected_return,front_no_short_no_crypto$expected_return),
#             0.7)


plot(front_no_short$sigma,front_no_short$expected_return,col='red',type = 'l',
     xlab="Annualized Volatility", ylab="Annualized Returns",
     xlim=xlimss,ylim=ylimss,
     main="Efficient Markowitz Mean???Variance Frontier")
lines(front_no_short_BTC_only$sigma,front_no_short_BTC_only$expected_return,col='blue',type='l',xlim=xlimss)
lines(front_no_short_no_crypto$sigma,front_no_short_no_crypto$expected_return,col='black',type='l',xlim=xlimss)
legend("topleft", legend=c("All","BTC only","no crypto"), col=c("red","blue","black"),lty=1:3,cex=0.5)


plot(front_no_short_BTC_only$sigma,front_no_short_BTC_only$expected_return,col='red',type = 'l',
     xlab="Annualized Volatility", ylab="Annualized Returns",
     main="Efficient Markowitz Mean???Variance Frontier")









