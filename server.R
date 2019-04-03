#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plyr)

### Load prepped data ###
prepped_dat <- read.csv("prepped_tox_conv.csv")
prepped_dat$ActivityStartDate = as.Date(as.character(prepped_dat$ActivityStartDate),"%m/%d/%Y")

# Separate numeric criteria from data
num_crit = unique(prepped_dat[,c("BeneficialUse","R3172ParameterName","CriterionUnits","CriterionLabel","IR_MLID",
                                 "IR_ActivityType","AssessmentType","CriterionType","DailyAggFun","AsmntAggPeriod",
                                 "AsmntAggPeriodUnit","AsmntAggFun","NumericCriterion","SSC_StartMon","SSC_EndMon",
                                 "SSC_MLID")])
site_params = unique(prepped_dat[,!names(prepped_dat)%in%c("BeneficialUse","CriterionUnits","CriterionLabel","IR_ActivityType",
                                                           "AssessmentType","CriterionType","DailyAggFun","AsmntAggPeriod",
                                                           "AsmntAggPeriodUnit","AsmntAggFun","NumericCriterion","SSC_StartMon",
                                                           "SSC_EndMon","SSC_MLID")])
# Separate flow data (do not have in test dataset)
site_flow = site_params[site_params$R3172ParameterName=="Flow",]

# Determine the number of samples and parameters for each MLID
uni_type_name = unique(prepped_dat[,c("IR_MLID","IR_MLNAME","AU_Type")])
data_amount = unique(prepped_dat[,c("IR_MLID","ActivityStartDate","R3172ParameterName")])
data_amount = plyr::ddply(data_amount, .(IR_MLID), summarize, NCount = length(R3172ParameterName), NParam = length(unique(R3172ParameterName)))

### Load assessed data ###
assessed_dat <- read.csv("assessed_tox_conv.csv")

# Rank the categories for colors
assessed_dat = within(assessed_dat,{
  Num_Cat = NA
  Num_Cat[IR_Cat == "idNE"] <- 1
  Num_Cat[IR_Cat == "FS"] <- 2
  Num_Cat[IR_Cat == "idE"] <- 3
  Num_Cat[IR_Cat == "TMDL"] <- 4
  Num_Cat[IR_Cat == "NS"] <- 5
})
unicats = unique(assessed_dat[,c("IR_MLID", "AU_NAME","R317Descrp","BEN_CLASS","IR_Lat","IR_Long","IR_Cat", "Num_Cat")])
unicats1 = plyr::ddply(unicats, .(IR_MLID, AU_NAME, R317Descrp, BEN_CLASS, IR_Lat, IR_Long),summarize, Categories = paste(IR_Cat, collapse = ", "), MaxCat = max(Num_Cat))

# Merge assessed data with Ncount and NParam
siteinfo = merge(unicats1, uni_type_name, all.x = TRUE)
siteinfo = merge(siteinfo, data_amount, all.x = TRUE)
siteinfo$radius = log(siteinfo$NCount)*3

# Determine params causing NS
siteinfo_ns = unique(assessed_dat[assessed_dat$Num_Cat==5,!names(assessed_dat)%in%c("BeneficialUse")])
siteinfo_ns = plyr::ddply(siteinfo_ns, .(IR_MLID, AU_NAME, R317Descrp, BEN_CLASS, IR_Lat, IR_Long),summarize, Impaired_Params = paste(R3172ParameterName, collapse = ", "))

site_coords = merge(siteinfo, siteinfo_ns, all.x=TRUE)

# Data table and plotting cast
#vizdat <- unique(prepped_dat[,c("BeneficialUse","BEN_CLASS","IR_MLID", "R3172ParameterName","ActivityStartDate","IR_Unit","AssessmentType","CriterionType","NumericCriterion","CriterionLabel","IR_Value")])
vizdat <- unique(prepped_dat[,c("BEN_CLASS","IR_MLID", "R3172ParameterName","ActivityStartDate","IR_Unit","IR_Value")])

################ SERVER ##################

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

# Map site colors
pal <- rev(RColorBrewer::brewer.pal(length(unique(unicats1$MaxCat)),"Spectral"))
pal1 = leaflet::colorFactor(pal, domain=unicats1$MaxCat)  
  
# Empty reactive values object
reactive_objects=reactiveValues()

### SELECT DATA ###

observe({
  req("map")
  reactive_objects$site_coords = site_coords
})

# Map
map = leaflet::createLeafletMap(session, 'map')

# Map proxy
map_proxy = leaflet::leafletProxy("map")
# map
session$onFlushed(once = T, function() {
  output$map <- leaflet::renderLeaflet({
    map = wqTools::buildMap(plot_polys=FALSE, search="")
    map = addPolygons(map, data=wqTools::au_poly,group="Assessment units",smoothFactor=3,opacity=0.9, fillOpacity = 0.1, weight = 2, color = "purple", options = pathOptions(pane = "underlay_polygons"),
                      popup=paste0(
                        "AU name: ", au_poly$AU_NAME,
                        "<br> AU ID: ", au_poly$ASSESS_ID,
                        "<br> AU type: ", au_poly$AU_Type,
                        "<br> Category: ", au_poly$au_colors))
    map = addPolygons(map, data=wqTools::bu_poly,group="Beneficial uses",smoothFactor=3,opacity=0.9,fillOpacity = 0.1,weight=2,color="green", options = pathOptions(pane = "underlay_polygons"),
                      popup=paste0(
                        "Description: ", bu_poly$R317Descrp,
                        "<br> Uses: ", bu_poly$bu_class))
    map = addPolygons(map, data=wqTools::ss_poly,group="Site-specific standards",smoothFactor=3,opacity=0.9,fillOpacity = 0.1,weight=2,color="blue", options = pathOptions(pane = "underlay_polygons"),
                      popup=paste0("SS std: ", ss_poly$SiteSpecif))
    map = leaflet::addLayersControl(map,
                                    position ="topleft",
                                    baseGroups = c("Topo","Satellite"),overlayGroups = c("Sites", "Assessment units","Beneficial uses", "Site-specific standards"),
                                    options = leaflet::layersControlOptions(collapsed = TRUE, autoZIndex=TRUE))
    map=addMapPane(map,"site_markers", zIndex = 450)
    map = addCircleMarkers(map, data = reactive_objects$site_coords, lat=reactive_objects$site_coords$IR_Lat, lng=reactive_objects$site_coords$IR_Long, layerId = reactive_objects$site_coords$IR_MLID,group="Sites",
                           weight = 2, fill = TRUE, opacity=0.95, fillOpacity = 0.5, fillColor =~pal1(reactive_objects$site_coords$MaxCat),radius = as.numeric(reactive_objects$site_coords$radius), color =~pal1(reactive_objects$site_coords$MaxCat), options = pathOptions(pane = "site_markers"),
                           popup = paste0(
                             "MLID: ", reactive_objects$site_coords$IR_MLID,
                             "<br> ML Name: ", reactive_objects$site_coords$IR_MLNAME,
                             "<br> Site Type: ", reactive_objects$site_coords$AU_Type,
                             "<br> Categories: ", reactive_objects$site_coords$Categories,
                             "<br> Impaired Params: ", reactive_objects$site_coords$Impaired_Params,
                             "<br> Parameter Count: ", reactive_objects$site_coords$NParam,
                             "<br> Sample Count: ", as.character(reactive_objects$site_coords$NCount),
                             "<br></br>",
                             actionButton("sel_site", "Select Site", onclick = 'Shiny.onInputChange(\"button_click\",  Math.random())')
                             ))
    map=hideGroup(map, "Assessment units")
    map=hideGroup(map, "Site-specific standards")
    map=hideGroup(map, "Beneficial uses") 
    map=removeMeasure(map)
    map=leaflet::addLegend(map, position = 'topright',
                           colors = unique(pal1(reactive_objects$site_coords$MaxCat)), 
                           labels = c("idE","FS","idNE","NS"))
    #map=selectFeatures(reactive_objects$site_coords, mode = "click")
  })
})

siteinfo_abridged = siteinfo[,!names(siteinfo)%in%c("IR_Lat","IR_Long","R317Descrp","MaxCat","radius")]

## SITE LIST TO SELECT FROM
output$site_list <- DT::renderDT({
  DT::datatable(
    siteinfo_abridged, rownames=FALSE, filter="top",
    options = list(scrollY = '300px', paging = FALSE, scrollX=TRUE, dom="ltipr")
  )
})
outputOptions(output, "site_list", suspendWhenHidden = FALSE)

## Site list proxy
sitelist_proxy <- DT::dataTableProxy('site_list', session=shiny::getDefaultReactiveDomain(), deferUntilFlush = TRUE)


# Register map clicks
observeEvent(input$button_click,{
  if(is.null(reactive_objects$sel_sites)){
    reactive_objects$sel_sites = input$map_marker_click$id
  }else{reactive_objects$sel_sites = unique(append(reactive_objects$sel_sites,input$map_marker_click$id))}
# map_proxy%>%removePopup(layerId = reactive_objects$site_coords$IR_MLID)
})

# Register row selections
observeEvent(input$site_list_rows_selected,{
  mlids = as.character(siteinfo_abridged[c(input$site_list_rows_selected),"IR_MLID"])
  if(is.null(reactive_objects$sel_sites)){
    reactive_objects$sel_sites = mlids
  }else{reactive_objects$sel_sites = unique(append(reactive_objects$sel_sites,mlids))}
})

# Populate selected site list
observe({
  if(!is.null(reactive_objects$sel_sites)){
    selsites = siteinfo_abridged[siteinfo_abridged$IR_MLID%in%reactive_objects$sel_sites,c("IR_MLID","IR_MLNAME")]
  }else{
    selsites = siteinfo_abridged[0,c("IR_MLID","IR_MLNAME")]
  }
  output$selsite_list <- DT::renderDT({
    DT::datatable(
      selsites, rownames=FALSE,
      options = list(scrollY = '200px', paging = FALSE, dom="t")
    )
  })
  outputOptions(output, "selsite_list", suspendWhenHidden = FALSE)
  })

# Clear table filter buttons
observeEvent(input$clear_sitelist, ignoreInit=T,{
  reactive_objects$sel_sites = NULL
  reactive_objects$crit = NULL
  reactive_objects$params = NULL
  reactive_objects$uses = NULL
  reactive_objects$data = NULL
  reactive_objects$mindate = min(site_params$ActivityStartDate)
  reactive_objects$maxdate = max(site_params$ActivityStartDate)
  sitelist_proxy%>%selectRows(selected = NULL)%>%clearSearch()
  output$filtered_data <- DT::renderDT({
    DT::datatable(
      empty_filters, rownames=FALSE,
      options = list(scrollY = '400px', scrollX = TRUE, paging = FALSE, dom="t")
    )
  })
})

##### FILTER DATA ####

# Filtering dependencies 
observe({
  if(!is.null(reactive_objects$sel_sites)){
    reactive_objects$crit = num_crit[num_crit$IR_MLID%in%reactive_objects$sel_sites,]
    reactive_objects$params = unique(reactive_objects$crit$R3172ParameterName)
  }else{
    reactive_objects$crit = NULL
    reactive_objects$params = NULL
    reactive_objects$uses = NULL
  }
  if(!is.null(input$sel_params)){
    updatecrit <- isolate(reactive_objects$crit)
    reactive_objects$crit = updatecrit[updatecrit$R3172ParameterName%in%reactive_objects$params,]
    reactive_objects$uses = unique(reactive_objects$crit$BeneficialUse)}
  if(!is.null(input$sel_use)){
    updatecrit <- isolate(reactive_objects$crit)
    reactive_objects$crit = updatecrit[updatecrit$BeneficialUse%in%reactive_objects$uses,]
    }
})

# Reactive user interface items
output$sel_params <- renderUI({
  selectizeInput("sel_params","Select R3172 Parameter(s)",choices = reactive_objects$params, multiple = TRUE, selected = NULL)
})

output$sel_use <- renderUI({
  selectizeInput("sel_use", "Select Uses", choices = reactive_objects$uses, multiple = TRUE, selected = NULL)
})

output$sel_date <- renderUI({
  req(reactive_objects$mindate)
  sliderInput("sel_date", "Select Date Range", value = c(reactive_objects$mindate,reactive_objects$maxdate), min = reactive_objects$mindate, max = reactive_objects$maxdate)
})

# Define date range for slider input - changes based on whether a partially filtered data object is present in the reactivity object
# observe({
#   if(!is.null(reactive_objects$data)){
#     if(dim(reactive_objects$data)[1]>0){
#       reactive_objects$mindate = min(reactive_objects$data$ActivityStartDate)
#       reactive_objects$maxdate = max(reactive_objects$data$ActivityStartDate) 
#     }else{
#       reactive_objects$mindate = min(site_params$ActivityStartDate)
#       reactive_objects$maxdate = max(site_params$ActivityStartDate)
#     }
#   }else{
#     reactive_objects$mindate = min(site_params$ActivityStartDate)
#     reactive_objects$maxdate = max(site_params$ActivityStartDate)
#   }
# })

# Filter data based on crit filters
observe({
  if(!is.null(input$sel_use)){
    dates = site_params$ActivityStartDate[site_params$IR_MLID%in%reactive_objects$sel_sites&site_params$R3172ParameterName%in%input$sel_params]
    reactive_objects$mindate = min(dates)
    reactive_objects$maxdate = max(dates)
  }else{
    reactive_objects$mindate = min(site_params$ActivityStartDate)
    reactive_objects$maxdate = max(site_params$ActivityStartDate)
    }
})

# Finally, filter data one last time based on date input.
# Define empty table for clear site list button
empty_filters = site_params[0,]

# On filter data click, create data table
observeEvent(input$filter_data,{
  filteredat = site_params[site_params$IR_MLID%in%reactive_objects$sel_sites&site_params$R3172ParameterName%in%input$sel_params,]
  filteredat = filteredat[filteredat$ActivityStartDate>=input$sel_date[1]&filteredat$ActivityStartDate<=input$sel_date[2],]
  reactive_objects$data = filteredat
  
  output$filtered_data <- DT::renderDT({
    DT::datatable(
      filteredat, rownames=FALSE,filter="top",
      options = list(scrollY = '400px', paging = FALSE, scrollX=TRUE, dom="ltipr")
    )
  })
  outputOptions(output, "filtered_data", suspendWhenHidden = FALSE)
  })

### PLOTTING ###

# Time series - plot all parameters over time with standards plotted
output$time_series <- renderPlotly({
  filtdat = isolate(reactive_objects$data)
  p = plot_ly(type='scatter', x = filtdat$ActivityStartDate, y = filtdat$IR_Value,mode = 'lines+markers',
              transforms = list(
                list(
                  type = 'groupby',
                  groups = c(filtdat$R3172ParameterName, filtdat$IR_MLID)
                )
              ))
})

# Conc v. flow scatterplot - plot chosen parameter versus flow (if available)

# Spatial concentration relationships - Map site points with radius proportional to concentration


})
