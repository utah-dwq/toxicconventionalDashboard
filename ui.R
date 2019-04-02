
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
  # sidebarLayout(
  #   sidebarPanel(h4("Filter data using options below"),
  #                selectInput("sitetype", "Site Type", c("Rivers and Streams","Lakes")),
  #                selectInput("cattype", "Site Category", c("Not Supporting", "Insufficient Data - Exceedances","Insufficient Data - No Exceedances", "TMDL Approved", "No Evidence of Impairment","Supporting")),
  #                actionButton("reset_filter", "Reset Filter")),
    fluidRow(
      useShinyjs(),
      br(),
      h4("Expand and collapse Select Data, Filter Data, and View Data panes by clicking the pane title."),
      bsCollapse(id = "collpanels", multiple = TRUE,
                bsCollapsePanel("Select Data",
                                bsCollapse(id = "seldatpanels", multiple=TRUE,
                                bsCollapsePanel("Map",
                                                strong("Click site markers to view summary data. Click 'Select Site' button to add site to list below to view data."),
                                         # fluidRow(column(6,actionButton("reset_zoom", label = "Reset map zoom",style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%'))),
                                         # br(),      
                                         fluidRow(shinycssloaders::withSpinner(leaflet::leafletOutput("map", height="600px"),size=2, color="#0080b7"))),
                                bsCollapsePanel("Data",
                                                fluidRow(strong("Select site(s) to analyze in tables and plots below."),
                                                         div(DT::DTOutput("site_list"), style = "font-size:70%"))
                                ),
                                bsCollapsePanel("Selected Site(s)",
                                                fluidRow(strong("Selected site(s) from map and table will appear here.")),
                                                br(),
                                                fluidRow(actionButton("clear_sitelist","Clear Site List",style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%')),
                                                br(),
                                                fluidRow(div(DT::DTOutput("selsite_list"), style = "font-size:70%"))
                                                )
                                )),
                bsCollapsePanel("Filter Data",
                                h4("Further drill down data specifics in this tab before exploring plots, below"),
                                br(),
                                br(),
                                fluidRow(column(4,uiOutput("sel_params")),
                                         column(4, uiOutput("sel_use")),
                                         column(4,uiOutput("sel_date"))),
                                br(),
                                fluidRow(div(DT::DTOutput("filtered_data"), style = "font-size:70%"))
                                ),
                bsCollapsePanel("View Data"))
  )
  ))
