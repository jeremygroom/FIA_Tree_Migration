## FIA tree migration Shiny app ##




## Libraries ##
library(tidyverse)
library(shiny)
library(shinyjs)  # allows toggleState function
library(shinydashboard)
library(viridis)
library(ggtext)
library(tidyverse)
library(extrafont)
library(cowplot)
library(grid)
library(gridExtra)
library(ggplotify) # enables as.grob() function
library(plotly)
library(maps)
library(hexbin) # For plotting hexagons to summarize many data points.  May want to remove this. 
library(ggridges)


FIAapp <- function(){
  source("ShinyCode/global.R")
  source("ShinyCode/Home_Module.R")
  source("ShinyCode/EstComp_Module.R")
  source("ShinyCode/CrossedComp_Module.R")
  source("ShinyCode/TableComp1_Module.R")
  source("ShinyCode/Map1_Module.R")
  source("ShinyCode/Background_Module.R")
  source("ShinyCode/functions.r")
  
#  addResourcePath(prefix = 'www', directoryPath = 'ShinyCode/')
  
  ui <- dashboardPage(
    
    dashboardHeader(title = "FIA Tree Migration Data Visualization",
                    tags$li(a(href = 'http://www.groomanalytics.com',   
                              img(src = 'GroomAnalyticsH.jpg',
                                  title = "Groom Analytics Home", height = "40px"),
                              style = "padding-top:5px; padding-bottom:5px; margin-right:10px"),
                            class = "dropdown"),
                    titleWidth = 300),    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Home", tabName = "home"),
        menuItem("Background", tabName = "bkgrnd"),
        menuItem(HTML("Comparison of species<br/>change estimates"), tabName = "EstComp"),
        menuItem(HTML("Crossed comparison of<br/>species change estimates"), tabName = "CrossedComp"),
        menuItem(HTML("Map of temperature, precipitation,<br/>and their changes over time"), tabName = "map1"),
        menuItem(HTML("Tabular comparison of<br/>difference for variance<br/>and timing approaches"), tabName = "Tab1Comp"),
        menuItem("Files and Code", icon = icon("github"), href = 'https://github.com/jeremygroom/FIA_Tree_Migration')
        

      )),
      dashboardBody(
        tags$head(         # CSS to make the header have a consistent color
          tags$style(HTML('                   
        .skin-blue .main-header .logo {
          background-color: #3c8dbc;
        }
        .skin-blue .main-header .logo:hover {
          background-color: #3c8dbc;
        }
        
      '))),
        tabItems(
          tabItem(tabName = "home",
                  fluidRow(
                    Home_UI("Home1")
                  )),
          tabItem(tabName = "bkgrnd",
                  fluidRow(
                    Bkgrnd_UI("Background")
                  )),
          tabItem(tabName = "EstComp",
                  fluidRow(
                    EstComp_UI("EstComp1")
                  )),
          tabItem(tabName = "CrossedComp",
                  fluidRow(
                    CrossedComp_UI("CrossedComp1")
                  )),
          tabItem(tabName = "map1",
                  fluidRow(
                    Map1_UI("MapFig1")
                  )),
          tabItem(tabName = "Tab1Comp",
                  fluidRow(
                    TabVarTiming_UI("TabVarTiming")
                  )))
      )
  )
  
  server <- function(input, output, session) {
    output$Home1 <- Home_Server("Home1") 
    output$Background <- Bkgrnd_Server("Background") 
    output$EstComp1 <- EstComp_Server("EstComp1") 
    output$CrossedComp1 <- CrossedComp_Server("CrossedComp1")
    output$MapFig1 <- Map1_Server("MapFig1")
    output$TabVarTiming <- TabVarTiming_Server("TabVarTiming")

    
  }
  
  shinyApp(ui = ui, server = server)
}

FIAapp()
















