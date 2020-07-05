# Shiny app for showing the number of confirmed coronavirus cases across China
# Xijin Ge 2/5/2020

library(shiny)
library(plotly)
library(DT) # for renderDataTable
#library(shinyBS,verbose=FALSE) # for popup figures

ui <- fluidPage(
    #titlePanel("GWW"),
    tabsetPanel(
        tabPanel("Market"
                 ,plotOutput("rsiPlot")
                 ,br(),br()
                 ,plotOutput("macdPlot")
                 ,br(),br()
                 ,plotOutput("adxPlot")
                 ,br(),br()
                 
        ) #tab2 --------------------------------------------------
        ,tabPanel("Screen"
                  ,selectInput("sector", NULL, unique(sp500$sector))
                 ,plotOutput("sectorScatterPlot")
                 ,DT::dataTableOutput('selectedStocks')
                 
        ) #tab2 --------------------------------------------------
        
        ,tabPanel("About Momenta"

                  ,h5("3/27/20 V. 0.8 Add us data from the New York Times.")
                  ,h5("3/28/20 V. 0.8 Add detailed county level data from the New York Times.")
        )
    )
    #,tags$head(includeScript("ga.js")) # tracking usage with Google analytics      
)