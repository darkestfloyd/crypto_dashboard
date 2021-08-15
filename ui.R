#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
shinyUI(
    dashboardPage(
        header = dashboardHeader(title = "Crypto Dashboard"),
        sidebar = dashboardSidebar(disable = F, collapsed = T,
                                   actionButton("syncButton", "Sync Binance")),
        body = dashboardBody(
            
            box(title = "Position Distribution",
                plotOutput(outputId = "positionPlot"))
            
        )))
