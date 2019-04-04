
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
library(shinyjs)
library(plotly)

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
      h4("Expand and collapse panels by clicking the panel title."),
      bsCollapse(id = "collpanels", multiple = TRUE,
                bsCollapsePanel("Select Sites",
                                h4("Use the Map and Data panels to add sites to the Selected Site(s) panel."),
                                br(),
                                bsCollapse(id = "seldatpanels", multiple=TRUE,
                                bsCollapsePanel("Map",
                                                strong("Click site markers to view summary data. Click 'Select Site' button to add site to selected site list below. Point colors and sizes are based on site category and overall sample N count, respectively."),
                                         # fluidRow(column(6,actionButton("reset_zoom", label = "Reset map zoom",style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%'))),
                                         # br(),      
                                         fluidRow(shinycssloaders::withSpinner(leaflet::leafletOutput("map", height="600px"),size=2, color="#0080b7"))),
                                bsCollapsePanel("Data",
                                                fluidRow(strong("Click site records to add to selected sites list."),
                                                         div(DT::DTOutput("site_list"), style = "font-size:70%"))
                                ),
                                bsCollapsePanel("Selected Site(s)",
                                                fluidRow(strong("Selected site(s) from map and table will appear here. Click 'Clear Site List' to clear selected sites.")),
                                                br(),
                                                fluidRow(actionButton("clear_sitelist","Clear Site List",style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%')),
                                                br(),
                                                fluidRow(div(DT::DTOutput("selsite_list"), style = "font-size:70%"))
                                                )
                                )),
                bsCollapsePanel("View Data",
                                br(),
                                fluidRow(column(2,actionButton("build_dat", "Build Data Table", style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%')),
                                         column(2,actionButton("clear_dat", "Clear Data Table", style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%'))),
                                br(),
                                bsCollapse(id = "viewdat_panels",multiple = TRUE, open = NULL,
                                           bsCollapsePanel("Data",
                                                           strong("Review the prepped data below. Select any questionable records and click 'Make Comment' to flag data records for review."),
                                                           fluidRow(actionButton("dt_comment", "Make Comment", style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%')),
                                                           fluidRow(div(DT::DTOutput("selsite_data"), style = "font-size:70%"))
                                           ),
                                           bsCollapsePanel("Plots",
                                                           tabsetPanel(
                                                             tabPanel("Single Site Time Series",
                                                                        fluidRow(column(3,uiOutput("sel_param_site"))),
                                                                        fluidRow(column(3,uiOutput("sel_use1")),
                                                                                 column(3,uiOutput("sel_use2"))),
                                                                        fluidRow(column(3,uiOutput("sel_param1")),
                                                                                 column(3,uiOutput("sel_param2")),
                                                                                 column(3,uiOutput("sel_param3"))),
                                                                         fluidRow(plotlyOutput("compare_params"))),
                                                             tabPanel("Multi-Site Time Series",
                                                                      fluidRow(column(3,uiOutput("sel_comparam")),
                                                                               column(3,uiOutput("sel_comparuse1")),
                                                                               column(3,uiOutput("sel_comparuse2"))),
                                                                      fluidRow(plotlyOutput("compare_sites"))),
                                                             tabPanel("Scatter Plots"),
                                                             tabPanel("Concentration Map")
                                                           )))
                                ))
  )
  ))


# bsCollapsePanel("Filter Data",
#                 h4("Further drill down data specifics in this tab before exploring plots, below"),
#                 br(),
#                 br(),
#                 fluidRow(column(4,uiOutput("sel_params")),
#                          column(4, uiOutput("sel_use")),
#                          column(4,uiOutput("sel_date"))),
#                 br(),
#                 fluidRow(column(4, actionButton("filter_data", "Filter Data", style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%')))
#                 ),
