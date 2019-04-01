#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

prepped_dat <- read.csv("prepped_tox_conv.csv")
assessed_dat <- read.csv("assessed_tox_conv.csv")
unicats = unique(assessed_dat[,c("IR_MLID", "R317Descrp","BEN_CLASS","IR_Lat","IR_Long","IR_Cat")])
unicats1 = plyr::ddply(unicats, .(IR_MLID, R317Descrp, BEN_CLASS, IR_Lat, IR_Long),summarize, Categories = paste(IR_Cat, collapse = ", "))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   

  
})
