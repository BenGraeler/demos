#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


availableStats <- list('nights spent' = "Gaesteuebernachtungen",
                       'offered beds' = "Angebotene_Gaestebetten",
                       'nights spent per sqkm' = "nspskm")


library(shiny)
library(leaflet)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Fusion WPS"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       radioButtons("time", "Year:",
                    choices = 2013:2015),
       selectInput("stat", "Select a statistic:",
                   availableStats)
    ),
    
    mainPanel(leafletOutput("map", height=800))
  )
))
