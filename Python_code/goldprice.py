#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Mar 31 12:11:10 2019

@author: matteo
"""


import pandas as pd
import datetime
import scraping as scr
from time import strptime
import datetime

#%matplotlib inline



def convdata(data):
    yyyy = data.split()[1].split('-')[2]
    mm = str(strptime(data.split()[1].split('-')[1],'%b').tm_mon) if  strptime(data.split()[1].split('-')[1],'%b').tm_mon > 9 else '0'+str(strptime(data.split()[1].split('-')[1],'%b').tm_mon)  
    dd = data.split()[1].split('-')[0] if int(data.split()[1].split('-')[0]) > 9 else '0'+data.split()[1].split('-')[0]
    return '-'.join([str(yyyy),str(mm),str(dd)])

#==========================================
filename = 'goldPrice.csv'
PriceGold_prov = pd.read_csv(filename)[['Date','Close (kg)']]
PriceGold_prov['Date'] = [datetime.datetime.strptime(PriceGold_prov['Date'].iloc[i], "%H:%M:%S %d-%b-%Y") for i in range(len(PriceGold_prov)) ]
PriceGold_prov = PriceGold_prov.set_index(['Date']).iloc[::-1]
reversed_df = PriceGold_prov.iloc[::-1]

tickers = ['BTC-USD']

date11 = datetime.datetime.strptime(PriceGold_prov.index[0], "%H:%M:%S %d-%b-%Y")
date22 = datetime.datetime.strptime(PriceGold_prov.index[-1], "%H:%M:%S %d-%b-%Y")
df_prov = scr.create_df(tickers,PriceGold_prov.index[0],PriceGold_prov.index[-1])
date1 = max(df_prov.index[0],date11)
date2 = min(df_prov.index[-1],date22)

df = df_prov[df_prov.index>=date1][df_prov.index<=date2]
PriceGold = PriceGold_prov[PriceGold_prov.index>=date1][df_prov.index<=date2]

