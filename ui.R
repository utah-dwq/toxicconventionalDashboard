
library(shiny)
library(wqTools)
library(magrittr)
require(leaflet)
require(leaflet.extras)
require(RColorBrewer)
require(sf)
require(plyr)
require(DT)

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
  )
  
 
))
