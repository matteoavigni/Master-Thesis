#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Mar 24 11:02:10 2019

@author: matteo
"""


import fix_yahoo_finance as fy
import pandas as pd
import matplotlib.pyplot as plt
import scipy.stats  as stats

#%matplotlib inline



def create_df(tickers, date1, date2, flag=0):
    
    a = {}
    for n in tickers:
        df = fy.download(n,date1,date2)
        a[n] = df
    
    df = pd.concat(a, axis = 1, keys = tickers).dropna()
    
    if flag !=0:
        for n in tickers:
            plt.plot(df[n]['Close'])
        plt.legend(tickers)
    return df




def rem_dup_ordered(lista):
    a = set(lista)
    list_new = []
    for n in lista:
        if n in a:
            a.remove(n)
            list_new.append(n)
    return list_new

def my_returns(tickers, df):
    a = {}
    for n in tickers:
        a[n] = df[n]['Close'].shift(1)/df[n]['Close'] -1
    daf = pd.concat(a, axis = 1, keys = tickers).dropna()
    return daf


if __name__ == '__main__':
    #'^GSPC','^NDX','^STOXX50E','IBM'
    # criptovalute 'BTC-USD','ETH-USD','XRP-USD', ''
    #tickers = ['BTC-USD','ETH-USD','XRP-USD']
    tickers = [ 'AAPL','goog']#, 'JNJ', 'GE', 'GOOG', 'CVX', 'PG', 'WFC']
    date1 = '2014-01-01'
    date2 = '2018-10-20'
    

    df = create_df(tickers, date1, date2)
    rendimenti = my_returns(tickers,df)
    aa = rendimenti.corr()
    
## PEARSONR PVALUES    
    a= {}
    for n in tickers: # rows are the number of rows in the matrix.
        for m in tickers:
            a[n,m] = stats.pearsonr(df[n]['Close'],df[m]['Close'])[1]

## PERMUTATION PVALUES
#    b= {}
#    for n in tickers: # rows are the number of rows in the matrix.
#        for m in tickers:
#            b[n,m] = permutation_test(df[n]['Close'],df[m]['Close'], method='approximate',num_rounds=10000, seed=0) 
    
    ## test rimozione duplicati







