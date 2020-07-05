# server logic for WGG stock screener
# xijin Ge



library(shiny)
library(ggridges)   # Ridge plots
library(ggrepel)    # label data points
library(dplyr)
library(DT) # for renderDataTable

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$rsiPlot <- renderPlot({
        # plot RSI by sector
        ggplot(sp500, aes(RSI, sector, fill = sector)) + 
            geom_density_ridges() +
            theme(legend.position = "none") +
            geom_vline(xintercept = 30, linetype="dashed", color = "green", size = 1) +
            geom_vline(xintercept = 70, linetype="dashed", color = "red", size = 1) +
            theme(axis.title.y=element_blank() ) +
            xlab("RSI (>70 overbought; <30 oversold)") +
            xlim(c(0,100))
    })
    
    output$macdPlot <- renderPlot({
        # plot MACD - signal by sector
        ggplot(sp500, aes(macd.diff, sector, fill = sector)) + 
            geom_density_ridges() +
            theme(legend.position = "none") +
            theme(axis.title.y=element_blank() ) +
            geom_vline(xintercept = 0, linetype="dashed", color = "red", size = 1) + 
            xlab("MACD - Signal (>0 trending up)") +
            xlim(c(-4,2))
    })
    
    output$adxPlot <- renderPlot({
        # plot MACD - signal by sector
        ggplot(sp500, aes(ADX, sector, fill = sector)) + 
            geom_density_ridges() +
            theme(legend.position = "none") +
            theme(axis.title.y=element_blank() ) +
            xlab("ADX indicator (>25 strong trend)") +
            geom_vline(xintercept = 25, linetype="dashed", color = "red", size = 1) +  #not work
            xlim(c(0,40))
    })  
    
    
# Screening --------------------------------------------------------------------
    output$selectedStocks <- DT::renderDataTable({

        if( is.null( input$selectedSpecies)) return(NULL) 
        
        selected <- sp500 %>%
            filter(sector = input$sector) %>%
            arrange(desc(macd.diff))
        
        selected
        
    }, selection = 'single', options = list(pageLength = 5 ) # only 5 rows shown
    )
    
    output$sectorScatterPlot <- renderPlot({
        # plot MACD - signal vs. RSI for sectors
        sp500 %>%
            filter(sector == input$sector) %>%
            ggplot(aes(macd.diff, RSI, label = symbol)) + 
            geom_point() +
            geom_text_repel() +
            theme(legend.position = "none") +
            xlab("MACD - Signal")  + 
            geom_vline(xintercept = 0, linetype="dashed", color = "red", size = 1  )+
            xlim(c(-2,2)) +
            ylim(c(min(sp500$RSI), max(sp500$RSI)))
            
    })      
    
    
})
