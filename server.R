#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    require(tidyverse)
    require(blotter)
    require(binancer)
    
    source("utils.R")
    
    observeEvent(input$syncButton, {
        binance_sync()
    })
    
    observe({
        if (!file.exists("binance_wallet.RDS")) binance_sync()
        message("Reading wallet and trades from disk...")
        bwallet <<- readRDS("binance_wallet.RDS")
        btrades <<- readRDS("binance_trades.RDS")
        
        
    })
    
    output$positionPlot <- renderPlot({
        ggplot(bwallet) + 
            geom_col(aes(x=reorder(asset, -total), y=total))
    })
    
    
})
