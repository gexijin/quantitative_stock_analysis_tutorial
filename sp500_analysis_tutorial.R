# R Code for S&P500 Stock Analysis
# Author: Matt Dancho
# revised Xijin Ge
# Date: 2020-6-26


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
library(ggridges)   # Ridge plots
library(ggrepel)    # label data points

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
    filter(symbol != "HWM")

#sp_500 <- sp_500[1:300, ]

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
        Sys.sleep(0.3)
        stock_prices 
}

get_stock_prices_back <- function(ticker, return_format = "tibble", ...) {
    # Get stock prices
     tryCatch({ stock_prices_xts <- getSymbols(Symbols = ticker, auto.assign = FALSE, ...) 
    }, error = function(err) {
        tryCatch({ stock_prices_xts <- getSymbols(Symbols = ticker, auto.assign = FALSE, ...) 
        }, error = function(err) {
            getSymbols(Symbols = ticker, auto.assign = FALSE, ...)
        })
    })
    
    if(is.null(stock_prices_xts)) { 
        return(NULL) 
        } else { 
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
    Sys.sleep(0.3)
    return( stock_prices )
        }
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
#sp100 <- sp_500[1:100, ] %>%
#sp200 <- sp_500[101:200, ] %>%
#sp300 <- sp_500[201:300, ] %>%
#sp400 <- sp_500[301:400, ] %>%
sp500 <- sp_500[401:500, ] %>%
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

sp_500 = rbind(sp100, sp200, sp300, sp400)



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
    mutate(macd = as.numeric(macd)) %>%
    mutate(macd.diff = as.numeric(macd.diff)) %>%
    mutate(ADX = as.numeric(ADX))
# Visualization -----------------------------------------------------

ggplot(sp500, aes(gics.sector, RSI)) + 
    geom_boxplot() + 
    coord_flip()
    
ggplot(sp500, aes(gics.sector, macd)) + 
    geom_boxplot() + 
    coord_flip()

ggplot(sp500, aes(gics.sector, macd.diff)) + 
    geom_boxplot() + 
    coord_flip()

# ridge plots
ggplot(sp500, aes(RSI, gics.sector, fill = gics.sector)) + 
    geom_density_ridges() +
    theme(legend.position = "none") +
    theme(axis.title.y=element_blank() )

ggplot(sp500, aes(macd.diff, gics.sector, fill = gics.sector)) + 
    geom_density_ridges() +
    theme(legend.position = "none") +
    theme(axis.title.y=element_blank() ) +
    xlab("MACD - Signal") +
    xlim(c(-4,2))

ggplot(sp500, aes(ADX, gics.sector, fill = gics.sector)) + 
    geom_density_ridges() +
    theme(legend.position = "none") +
    theme(axis.title.y=element_blank() ) +
    xlab("ADX indicator") +
    xlim(c(0,40))

# plot tickers as scatter plot
splot <- function(sector) {
    sp500 %>%
        filter(gics.sector == sector) %>%
        ggplot(aes(macd.diff, RSI, label = symbol)) + 
        geom_point() +
        geom_text_repel() +
        theme(legend.position = "none") +
        xlab("MACD - Signal")  +
        labs(subtitle = sector)
}


sectors = unique(sp500$gics.sector)

splot(sectors[5])

# put all sectors in one plot
gridExtra::grid.arrange(splot(sectors[1]), 
                        splot(sectors[2]), 
                        splot(sectors[3]),
                        splot(sectors[4]),
                        splot(sectors[5]),
                        splot(sectors[6]),
                        splot(sectors[7]),
                        splot(sectors[8]),
                        splot(sectors[9]),
                        splot(sectors[10]),
                        splot(sectors[11]),
                        ncol = 2)



plot_ly(data   = sp500,
        type   = "scatter",
        mode   = "markers",
        x      = ~ RSI,
        y      = ~ macd.signal,
        color  = ~ gics.sector,
        colors = "Blues",
        size   = ~ ADX,
        text   = ~ str_c("<em>", security, "</em><br>",
                         "Ticker: ", symbol, "<br>",
                         "Sector: ", gics.sector, "<br>",
                         "Sub Sector: ", gics.sub.industry),
        marker = list(opacity = 0.8,
                      symbol = 'circle',
                      sizemode = 'diameter',
                      sizeref = 4.0,
                      line = list(width = 2, color = '#FFFFFF'))
)


write.csv(sp500[,-c(5,11)], "sp500.csv")

top <- sp500 %>%
    filter(macd.diff > 0, RSI > 30, ADX > 20) 
top$trend = "Up"

bottom <- sp500 %>%
    filter(macd.diff < 0, RSI < 35, ADX < 25) 
bottom$trend = "Down"

selected <- rbind(top,bottom)

selected %>%
    ggplot(aes(macd.diff, RSI,  label = symbol, color = trend)) + 
    geom_point() +
    geom_text_repel() +
    xlab("MACD - Signal")  +
    labs(subtitle = "Selected stocks")

selected$symbol

i = 12
stock <- getSymbols(selected$symbol[i], from=today()-90, to=today(), auto.assign = FALSE,)
chartSeries(stock,
            name = paste(selected$symbol[i], selected$security[i]),
            type="candlesticks",
            #subset='2007',
            theme=chartTheme('white'))
addSMA(n=3,on=1,col = "blue")
addSMA(n=10,on=1,col = "red")
addRSI(n=14)
addMACD(fast = 12, slow = 26, signal = 9)

















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
             