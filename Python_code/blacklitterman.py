#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Apr 10 21:41:45 2019

@author: matteo

DESCRIPTION:
    This script uses both mean-variance and Black-Litterman approach to compute the optimal portfolio given
    a list of risky assets and the risk-free rate.
"""


from numpy import matrix, array, zeros, empty, sqrt, ones, dot, append, mean, cov, transpose, linspace
from numpy.linalg import inv, pinv
from pylab import *
#from structures.quote import QuoteSeries
import scipy.optimize
import random

####################################
# Helper Functions
####################################

def load_data_net():
        """ 
        DESCRIPTION:
            This function loads historical stock prices of the assets given in "symbols" from Yahoo Finance 
            database. Both symbols and caps must be manually updated
            INPUT:
                ------
            OUTPUT:
                symbols = list of tickers of the investable universe
                prices_out = list of historical prices of the assets in "symbols"
                caps_out = market caps of the assets in "symbols"
        """
        symbols = [ 'XOM','AAPL', 'MSFT', 'JNJ', 'GE', 'GOOG', 'CVX', 'PG', 'WFC','BTC-USD']
        cap = {'BTC-USD':349.300E9,'^GSPC':23.77e12, 'XOM':349.329e9, 'AAPL':928.91e9, 'MSFT':919.821e9, 'JNJ':362.61e9, 'GE':87.183e9, 'GOOG':840.518e9,
               'CVX':240.206e9, 'PG':256.289e9, 'WFC':221.556e9, 'BTC-USD':93.795e9} #
        n = len(symbols)
        import scraping                                                         # module to download prices from Yahoo Finance database
        prices_out, caps_out = [], []                                           
        prices = scraping.create_df(symbols,'2015-01-01','2019-04-06')          # prices taken from Yahoo Finance
        for s in symbols:
                prices_out.append(prices[s]['Close'])
                caps_out.append(cap[s])
        
        return symbols, prices_out, caps_out

#==========================================================================================================================================================#


def assets_meanvar(names, prices, caps):
        """ 
        DESCRIPTION:
            This function computes the market-equilibrium portfolio and its assets statistics
        INPUT:
            names = names of the assets under analisys [list of strings]
            prices = list of historical prices of the assets in "names" [list of series of prices]
            caps = market caps of the assets in "names" [list of float]
        OUTPUT:
            names = names of the assets under analisys [list of strings]
            weights = weights of the market equilibrium portfolio [array]
            expreturns = expected-returns of the assets in "names" [array]
            covars = variance-covariance matrix of the assets in "names" [array]
        """
        prices = matrix(prices)                         # create numpy matrix from prices
        weights = array(caps) / sum(caps)               # create weights

        # create matrix of historical returns
        rows, cols = prices.shape
        returns = empty([rows, cols-1])
        for r in range(rows):
                for c in range(cols-1):
                        p0, p1 = prices[r,c], prices[r,c+1]
                        returns[r,c] = (p1/p0)-1

        # calculate expected returns
        expreturns = array([])
        for r in range(rows):
                expreturns = append(expreturns, mean(returns[r]))
        # calculate covariances
        covars = cov(returns)

        expreturns = (1+expreturns)**250-1              # Annualize expected returns
        covars = covars * 250                           # Annualize covariances
        return names, weights, expreturns, covars


#==========================================================================================================================================================#


def port_mean(W, R):
        """ 
        DESCRIPTION:
            This function computes the expected return of the portfolio W
        INPUT:
            W = portfolio [array]
            R = expected-returns of the assets in "names" [array]
        OUTPUT:
            sum(R*W) = expected return of the portfolio W [float]
        """
        return sum(R*W)

#==========================================================================================================================================================#


def port_var(W, C):
        """ 
        DESCRIPTION:
            This function computes the variance of the portfolio W returns
        INPUT:
            W = portfolio [array]
            C = variance-covariance matrix of the assets in "W" [array of arrays]
        OUTPUT:
            dot(dot(W, C), W) = variance of the portfolio W [float]
        """
        return dot(dot(W, C), W)

#==========================================================================================================================================================#

def port_mean_var(W, R, C):
        """ 
        DESCRIPTION:
            This function reports the exp-return and the variance of the portfolio W
        INPUT:
            W = portfolio [array]
            R = expected-returns of the assets in "names" [array]
            C = variance-covariance matrix of the assets in "W" [array of arrays]
        OUTPUT:
            port_mean(W, R) =  expected return of the portfolio W [float]
            port_var(W, C) = variance of the portfolio W [float]
        """
        return port_mean(W, R), port_var(W, C)
#==========================================================================================================================================================#


def solve_frontier(R, C):
        """ 
        DESCRIPTION:
            This function calculates mean-variance frontier and returns its [x,y] points in two arrays
        INPUT:
            R = expected-returns of the assets in "names" [array]
            C = variance-covariance matrix of the assets in "W" [array of arrays]
        OUTPUT:
            array(frontier_mean) = exp-returns of the frontier's portfolios [array]
            array(frontier_var) = variances of the frontier's portfolios [array]
            frontier_weights = list of frontier's portfolios weights [list  of lists of weights]
        """
        def fitness(W, R, C, r):
                # For given level of return r, find weights which minimizes
                # portfolio variance.
                mean, var = port_mean_var(W, R, C)
                # Big penalty for not meeting stated portfolio return effectively serves as optimization constraint
                penalty = 50*abs(mean-r)
                return var + penalty
        frontier_mean, frontier_var, frontier_weights = [], [], []
        n = len(R)                                                       # Number of assets in the portfolio
        for r in linspace(min(R), max(R), num=20):                       # Iterate through the range of returns on Y axis
                W = ones([n])/n                                          # start optimization with equal weights
                b_ = [(0,1) for i in range(n)]                           # pesi compresi tra 0 e 1
                c_ = ({'type':'eq', 'fun': lambda W: sum(W)-1. })        # full invested
                optimized = scipy.optimize.minimize(fitness, W, (R, C, r), method='SLSQP', constraints=c_, bounds=b_)
                if not optimized.success:
                        raise BaseException(optimized.message)
                # add point to the min-var frontier [x,y] = [optimized.x, r]
                frontier_mean.append(r)                                  # return
                frontier_var.append(port_var(optimized.x, C))            # min-variance based on optimized weights
                frontier_weights.append(optimized.x)
        return array(frontier_mean), array(frontier_var), frontier_weights
#==========================================================================================================================================================#


def solve_weights(R, C, rf):
        """ 
        DESCRIPTION:
            solve_weights finds the optimal weights in the sense of mean-variance minimization (sharpe ratio maximization)
        INPUT:
            R = expected returns of the available risky assets [array]
            C = variance-covariance matrix for the available risky assets [array of arrays]
            rf = risk free return [float]
        OUTPUT:
            optimized.x = optimal weights [array]
        """
        def fitness(W, R, C, rf):                               # function to be minimized
                mean, var = port_mean_var(W, R, C)              # calculate mean/ of the portfolio
                util = (mean - rf) / sqrt(var)                  # utility = Sharpe ratio
                return 1/util                                   # maximize the utility, minimize its inverse value
        n = len(R)
        W = ones([n])/n                                         # start optimization with equal weights
        b_ = [(0.,1.) for i in range(n)]                        # weights for boundaries between 0%..100%. No leverage, no shorting
        c_ = ({'type':'eq', 'fun': lambda W: sum(W)-1. })       # Sum of weights must be 100%
        optimized = scipy.optimize.minimize(fitness, W, (R, C, rf), method='SLSQP', constraints=c_, bounds=b_) #Sequential Least Square Programming method
        if not optimized.success:
                raise BaseException(optimized.message)
        return optimized.x

#==========================================================================================================================================================#

def print_assets(names, W, R, C):
        """ 
        DESCRIPTION:
            Prints the portfolio data and statistics
        INPUT:
            names = names of the assets under analisys [list of strings]
            W = portfolio [array]
            R = expected returns of the available risky assets [array]
            C = variance-covariance matrix for the available risky assets [array of arrays]
        OUTPUT:
            -------
        """
        print("%-10s %6s %6s %6s %s" % ("Name", "Weight", "Return", "Dev", "   Correlations"))
        for i in range(len(names)):
                print("%-10s %5.1f%% %5.1f%% %5.1f%%    " % (names[i], 100*W[i], 100*R[i], 100*C[i,i]**.5), end='')
                for j in range(i+1):
                        corr = C[i,j] / (sqrt(C[i,i]) * (sqrt(C[j,j]))) # calculate correlation from covariance
                        print("%.3f " % corr, end='')
                print()

#==========================================================================================================================================================#


def optimize_and_display(title, names, R, C, rf, color='black'):
        """ 
        DESCRIPTION:
            Computes and plots the frontier
        INPUT:
            title = title of the plot [string]
            names = names of the assets under analisys [list of strings]
            R = expected returns of the available risky assets [array]
            C = variance-covariance matrix for the available risky assets [array of arrays]
            rf = risk free return [float]
            color = color of the graph [string]
        OUTPUT:
            -------
        """
        # optimize
        W = solve_weights(R, C, rf)
        mean, var = port_mean_var(W, R, C)                                              # calculate tangency portfolio
        f_mean, f_var, f_weights = solve_frontier(R, C)                                 # calculate min-var frontier

        # display min-var frontier
        print(title)
        print_assets(names, W, R, C)
        n = len(names)
        scatter([C[i,i]**.5 for i in range(n)], R, marker='x',color=color)              # draw assets
        for i in range(n):                                                              # draw labels
                text(C[i,i]**.5, R[i], '  %s'%names[i], verticalalignment='center', color=color)
        scatter(var**.5, mean, marker='o', color=color)                                 # draw tangency portfolio
        plot(f_var**.5, f_mean, color=color)                                            # draw min-var frontier
        xlabel('$\sigma$'), ylabel('$r$')
        grid(True)
#       show()

        # Display weights
        #m = empty([n, len(f_weights)])
        #for i in range(n):
        #       for j in range(m.shape[1]):
        #               m[i,j] = f_weights[j][i]
        #stackplot(f_mean, m)
        #show()

#==========================================================================================================================================================#


def prepare_views_and_link_matrix(names, views):
        """ 
        DESCRIPTION:
            Given the pairs of assets, prepare the views and link matrices. This function is created just for users' convenience
        INPUT:
            names = names of the assets under analisys [list of strings]
            views = expected returns of the available risky assets [list of tuples: [  [('MSFT', '>', 'GE', 0.02),
                                                                                        ('AAPL', '<', 'JNJ', 0.02)] ]
        OUTPUT:
            array(Q) = views values [array]
            P = views matrix [array of lists]
        """
        r, c = len(views), len(names)
        Q = [views[i][3] for i in range(r)]     # view matrix
        P = zeros([r, c])                                       # link matrix
        nameToIndex = dict()
        for i, n in enumerate(names):
                nameToIndex[n] = i
        for i, v in enumerate(views):
                name1, name2 = views[i][0], views[i][2]
                P[i, nameToIndex[name1]] = +1 if views[i][1]=='>' else -1
                P[i, nameToIndex[name2]] = -1 if views[i][1]=='>' else +1
        return array(Q), P
#%%  MAIN
####################################
# Main
####################################



#       lmb             lambda - risk aversion coefficient
#       V               assets variances (diagonal in covariance matrix)
#       Pi              portfolio equilibrium excess returns
#       tau     scaling factor for Black-litterman

# Load names, prices, capitalizations from the data source(yahoo finance)
names, prices, caps = load_data_net()
n = len(names)

# Estimate assets's expected return and covariances
names, W, R, C = assets_meanvar(names, prices, caps)
rf = .015       # Risk-free rate

print("Historical Weights")
print_assets(names, W, R, C)

# Calculate portfolio historical return and variance
mean, var = port_mean_var(W, R, C)

# Mean-Variance Optimization (based on historical returns)
optimize_and_display('Optimization based on Historical returns', names, R, C, rf, color='red')
show()

# Black-litterman reverse optimization
lmb = (mean - rf) / var                         # Calculate return/risk trade-off
Pi = dot(dot(lmb, C), W)                        # Calculate equilibrium excess returns

# Mean-variance Optimization (based on equilibrium returns)
optimize_and_display('Optimization based on Equilibrium returns', names, Pi+rf, C, rf, color='green')
show()

# Determine views to the equilibrium returns and prepare views (Q) and link (P) matrices
views = [
        ('MSFT', '>', 'GE', 0.02),
        ('AAPL', '<', 'JNJ', 0.02)
        ]

Q, P = prepare_views_and_link_matrix(names, views)
print('Views Matrix')
print(Q)
print('Link Matrix')
print(P)

tau = .025 # scaling factor

# Calculate omega - uncertainty matrix about views
omega = dot(dot(dot(tau, P), C), transpose(P)) # 0.025 * P * C * transpose(P)
# Calculate equilibrium excess returns with views incorporated
sub_a = inv(dot(tau, C))
sub_b = dot(dot(transpose(P), inv(omega)), P)
sub_c = dot(inv(dot(tau, C)), Pi)
sub_d = dot(dot(transpose(P), inv(omega)), Q)
Pi = dot(inv(sub_a + sub_b), (sub_c + sub_d))

# Mean-variance Optimization (based on equilibrium returns)
optimize_and_display('Optimization based on Equilibrium returns with adjusted views', names, Pi+rf, C, rf, color='blue')
show()


print('Mean: '+str(mean))
print('Var: '+str(var))