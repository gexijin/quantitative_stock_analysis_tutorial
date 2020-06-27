# R Code for S&P500 Stock Analysis
# Author: Matt Dancho
# Date: 2016-10-23


# Prerequisites ----------------------------------------------------------------
library(quantmod)   # get stock prices; useful stock analysis functions
library(xts)        # working with extensible time series
library(rvest)      # web scraping
library(tidyverse)  # ggplot2, purrr, dplyr, tidyr, readr, tibble
library(stringr)    # working with strings
library(forcats)    # working with factors
library(lubridate)  # working with dates in tibbles / data frames
library(plotly)     # Interactive plots
library(corrplot)   # Visuazlize correlation plots


# Web Scraping: Get the List of S&P500 Stocks ----------------------------------

# Web-scrape S&P500 stock list
sp_500 <- read_html("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies") %>%
    html_node("table.wikitable") %>%
    html_table() %>%
    select(`Symbol`, Security, `GICS Sector`, `GICS Sub Industry`) %>%
    as_tibble()
# Format names
names(sp_500) <- sp_500 %>%
    names() %>%
    str_to_lower() %>%
    make.names()

# remove BRK.B
sp_500 <- sp_500 %>% 
    filter(symbol != "BRK.B") %>%
    filter(symbol != "BF.B") %>%
    filter(symbol != "ALLE")

sp_500 <- sp_500[1:400, ]

# Creating Functions to Map ----------------------------------------------------

get_stock_prices <- function(ticker, return_format = "tibble", ...) {
    # Get stock prices
    stock_prices_xts <- getSymbols(Symbols = ticker, auto.assign = FALSE, ...)
    # Rename
    names(stock_prices_xts) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
    # Return in xts format if tibble is not specified
    if (return_format == "tibble") {
        stock_prices <- stock_prices_xts %>%
            as_tibble() %>%
            mutate(Date = ymd(index(stock_prices_xts))) # index returns the date from xts object
        
    } else {
        stock_prices <- stock_prices_xts
    }
    stock_prices
}

get_log_returns <- function(x, return_format = "tibble", period = 'daily', ...) {
    # Convert tibble to xts
    if (!is.xts(x)) {
        x <- xts(x[,-1], order.by = x$Date)
    }
    # Get stock prices
    log_returns_xts <- periodReturn(x = x$Adjusted, type = 'log', period = period, ...)
    # Rename
    names(log_returns_xts) <- "Log.Returns"
    # Return in xts format if tibble is not specified
    if (return_format == "tibble") {
        log_returns <- log_returns_xts %>%
            as_tibble() %>%
            rownames_to_column(var = "Date") %>%
            mutate(Date = ymd(Date))
    } else {
        log_returns <- log_returns_xts
    }
    log_returns
}

get_sma <- function(x, n=10) {
    # Convert tibble to xts
    if (!is.xts(x)) {
        x <- xts(x[,-1], order.by = x$Date)
    }
    # Get stock prices
    SMA10 <- SMA(Cl(x), n = n)
    SMA10[length(SMA10)]
}

get_RSI <- function(x, n=14) {
    Date1 = x$Date
    x <- x %>%  # remove Date column as it is used as stock price
        select( -c(Date) )
    
    # Convert tibble to xts
    if (!is.xts(x)) {
        x <- xts(x[,-1], order.by = Date1)
    }
    # Calculate RSI
    tem <- RSI(Cl(x), n = 14)
    tail(tem, 1)
}

get_MACD <- function(x, ...) {
    Date1 = x$Date
    x <- x %>%  # remove Date column as it is used as stock price
        select( -c(Date) )
    
    # Convert tibble to xts
    if (!is.xts(x)) {
        x <- xts(x[,-1], order.by = Date1)
    }
    # Calculate RSI
    tem <- MACD(Cl(x), nFast = 12, nSlow = 26, nSig = 9)
    tail(tem, 1)
}

get_ADX <- function(x, ...) {
    Date1 = x$Date
    x <- x %>%  # remove Date column as it is used as stock price
        select( -c(Date) )
    
    # Convert tibble to xts
    if (!is.xts(x)) {
        x <- xts(x[,-1], order.by = Date1)
    }
    # Calculate RSI
    tem2 <- ADX(x, n = 14)
    tem <- tail(tem2, 1)
   # names(tem) <- colnames(tem2)
}

# Mapping the Functions --------------------------------------------------------
from <- "2020-01-01"
to   <- today()
sp_500 <- sp_500 %>%
    mutate(
        stock.prices = map(symbol,
                           function(.x) get_stock_prices(.x,
                                                         return_format = "tibble",
                                                         from = from,
                                                         to   = to)
        ),
        SMA3 = map(stock.prices, function(.x) get_sma(.x, n=3 )   ),
        SMA10 = map(stock.prices, function(.x) get_sma(.x, n=10 )   ),
        SMA20 = map(stock.prices, function(.x) get_sma(.x, n=20 )   ),
        SMA50 = map(stock.prices, function(.x) get_sma(.x, n=50 )   ),
        #SMA100 = map(stock.prices, function(.x) get_sma(.x, n=100 )   ),
        RSI = map(stock.prices, function(.x) get_RSI(.x)   ),    
        MACD = map(stock.prices, function(.x) get_MACD(.x)   ),
        ADX = map(stock.prices, function(.x) get_ADX(.x)   )
    )


# Split columns as some indicators  return multiple values-----------------------------
sp_500$DIp = 0
for( i in 1:nrow(sp_500))
    sp_500$DIp[i] <- as.vector(sp_500$ADX[[i]])[1]
sp_500$DIn = 0
for( i in 1:nrow(sp_500))
    sp_500$DIn[i] <- as.vector(sp_500$ADX[[i]])[2]
sp_500$DX = 0
for( i in 1:nrow(sp_500))
    sp_500$DX[i] <- as.vector(sp_500$ADX[[i]])[3]

for( i in 1:nrow(sp_500))
    sp_500$ADX[i] <- as.vector(sp_500$ADX[[i]])[4]



sp_500$macd = 0
for( i in 1:nrow(sp_500))
    sp_500$macd[i] <- as.vector(sp_500$MACD[[i]])[1]

sp_500$macd.signal = 0
for( i in 1:nrow(sp_500))
    sp_500$macd.signal[i] <- as.vector(sp_500$MACD[[i]])[2]

sp_500$macd.diff = sp_500$macd - sp_500$macd.signal


sp500 <- sp_500 %>%
    select( -c(MACD)) %>%
    as.data.frame() %>%
    mutate(RSI = as.numeric(RSI)) %>%
    mutate(macd = as.numeric(macd))
# Visualization

ggplot(sp500, aes(gics.sector, RSI)) + 
    geom_boxplot() + 
    coord_flip()
    
ggplot(sp500, aes(gics.sector, macd)) + 
    geom_boxplot() + 
    coord_flip()

ggplot(sp500, aes(gics.sector, macd.diff)) + 
    geom_boxplot() + 
    coord_flip()



top <- sp500 %>%
    filter(macd.diff > 0, RSI < 80, ADX > 25) 


selected = 2



stock <- getSymbols(top$symbol[selected], from=today()-90, to=today(), auto.assign = FALSE,)

chartSeries(stock,
            name = paste(top$symbol[selected], top$security[selected]),
            type="candlesticks",
            #subset='2007',
            theme=chartTheme('white'))
addSMA(n=3,on=1,col = "blue")
addSMA(n=10,on=1,col = "red")
addRSI(n=14)
addMACD(fast = 3, slow = 10, signal = 5)

















# Visualizing the Results with Plotly ------------------------------------------

plot_ly(data   = sp_500,
        type   = "scatter",
        mode   = "markers",
        x      = ~ sd.log.returns,
        y      = ~ mean.log.returns,
        color  = ~ n.trade.days,
        colors = "Blues",
        size   = ~ n.trade.days,
        text   = ~ str_c("<em>", security, "</em><br>",
                         "Ticker: ", symbol, "<br>",
                         "Sector: ", gics.sector, "<br>",
                         "Sub Sector: ", gics.sub.industry, "<br>",
                         "No. of Trading Days: ", n.trade.days),
        marker = list(opacity = 0.8,
                      symbol = 'circle',
                      sizemode = 'diameter',
                      sizeref = 4.0,
                      line = list(width = 2, color = '#FFFFFF'))
        ) %>%
    layout(title   = 'S&P500 Analysis: Stock Risk vs Reward',
           xaxis   = list(title = 'Risk: StDev Log Returns',
                          gridcolor = 'rgb(255, 255, 255)',
                          zerolinewidth = 1,
                          ticklen = 5,
                          gridwidth = 2),
           yaxis   = list(title = 'Reward: Mean Log Returns',
                          gridcolor = 'rgb(255, 255, 255)',
                          zerolinewidth = 1,
                          ticklen = 5,
                          gridwith = 2),
           margin = list(l = 100,
                         t = 100,
                         b = 100),
           font   = list(color = '#FFFFFF'),
           paper_bgcolor = 'rgb(0, 0, 0)',
           plot_bgcolor = 'rgb(0, 0, 0)')


# Bonus: Computing Correlations ------------------------------------------------

# Filter high performing stocks
limit <- 30
sp_500_hp <- sp_500 %>%
    filter(n.trade.days > 1000) %>%
    filter(sd.log.returns < 0.0315) %>%
    mutate(rank = mean.log.returns %>% desc() %>% min_rank()) %>%
    filter(rank <= limit) %>%
    arrange(rank) %>%
    select(symbol, rank, mean.log.returns, sd.log.returns, log.returns)
sp_500_hp

# Unnest high performing stocks
sp_500_hp_unnest <- sp_500_hp %>%
    select(symbol, log.returns) %>%
    unnest()
sp_500_hp_unnest

# Spread format conducive to cor()
sp_500_hp_spread <- sp_500_hp_unnest %>%
    spread(key = symbol, value = Log.Returns) %>%
    na.omit()
sp_500_hp_spread

# Correlation plot
sp_500_hp_spread %>%
    select(-Date) %>%
    cor() %>%
    corrplot(order   = "hclust",
             addrect = 6)