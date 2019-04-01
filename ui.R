
library(shiny)
library(wqTools)
library(magrittr)
require(leaflet)
require(leaflet.extras)
require(RColorBrewer)
require(sf)
require(plyr)
require(DT)
require(shinyBS)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Header
  headerPanel(title=tags$a(href='https://deq.utah.gov/division-water-quality/',tags$img(src='deq_dwq_logo.png', height = 75, width = 75*2.85)),
              windowTitle="Toxics and Conventionals Dashboard"
  ),
  
  # Title
  titlePanel("", 
             tags$head(
               tags$link(rel = "icon", type = "image/png", href = "dwq_logo_small.png"),
               tags$style(".modal-dialog{ width:auto}"),
               tags$style(".modal-body{ min-height:auto}")
             )
  ),
  sidebarLayout(
    sidebarPanel(h4("Filter data using options below"),
                 selectInput("sitetype", "Site Type", c("Rivers and Streams","Lakes")),
                 selectInput("cattype", "Site Category", c("Not Supporting", "Insufficient Data - Exceedances","Insufficient Data - No Exceedances", "TMDL Approved", "No Evidence of Impairment","Supporting")),
                 actionButton("reset_filter", "Reset Filter")),
    mainPanel(
      bsCollapse(id = "collpanels", multiple = TRUE, open = "Map",
                bsCollapsePanel("Map",
                               fluidRow(column(6,actionButton("reset_zoom", label = "Reset map zoom",style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%'))),      
                               fluidRow(shinycssloaders::withSpinner(leaflet::leafletOutput("map", height="600px"),size=2, color="#0080b7"))),
                bsCollapsePanel("Data"),
                bsCollapsePanel("Visualizations"))
  )
  )))
