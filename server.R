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
library(shinyjs)
library(plotly)

### Load prepped data ###
prepped_dat <- read.csv("prepped_tox_conv.csv", stringsAsFactors = FALSE)
prepped_dat$ActivityStartDate = as.Date(as.character(prepped_dat$ActivityStartDate),"%m/%d/%Y")

# Separate numeric criteria from data
num_crit = unique(prepped_dat[,c("BeneficialUse","R3172ParameterName","CriterionUnits","CriterionLabel","IR_MLID",
                                 "IR_ActivityType","AssessmentType","CriterionType","DailyAggFun","AsmntAggPeriod",
                                 "AsmntAggPeriodUnit","AsmntAggFun","NumericCriterion","SSC_StartMon","SSC_EndMon",
                                 "SSC_MLID")])

# Identify parameters with different units for different uses
num_crit_uni = unique(num_crit[,c("R3172ParameterName", "CriterionUnits")])
num_crit_dups = as.character(num_crit_uni$R3172ParameterName[duplicated(num_crit_uni$R3172ParameterName)])
num_crit_dups1 = num_crit_uni[num_crit_uni$R3172ParameterName%in%num_crit_dups,]
num_crit_dups1 = num_crit_dups1[!duplicated(num_crit_dups1$R3172ParameterName),]
names(num_crit_dups1)[names(num_crit_dups1)=="CriterionUnits"] = "IR_Unit"
num_crit_dups1$label = "duplicated"

# Isolate site data (CONTAINS DUPLICATED DATA FOR THOSE WITH USES THAT HAVE DIFFERENT STANDARD UNITS)
site_params = unique(prepped_dat[,!names(prepped_dat)%in%c("BeneficialUse","CriterionUnits","CriterionLabel","IR_ActivityType",
                                                           "AssessmentType","CriterionType","DailyAggFun","AsmntAggPeriod",
                                                           "AsmntAggPeriodUnit","AsmntAggFun","NumericCriterion","SSC_StartMon",
                                                           "SSC_EndMon","SSC_MLID")])
# Separate flow data (do not have in test dataset)
site_flow = site_params[site_params$R3172ParameterName=="Flow",]

# Create dataset without duplicates (for site stats and site table)
site_params_nodups = merge(site_params, num_crit_dups1, all.x = TRUE)
site_params_nodups = site_params_nodups[is.na(site_params_nodups$label),!names(site_params_nodups)%in%c("label")]

# Determine the number of samples and parameters for each MLID
uni_type_name = unique(site_params_nodups[,c("IR_MLID","IR_MLNAME","AU_Type")])
data_amount = unique(site_params_nodups[,c("IR_MLID","ActivityStartDate","R3172ParameterName")])
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
  reactive_objects$selsite_data = NULL
  selsite_data = site_params_nodups[0,]
  output$selsite_data <- DT::renderDT({
    DT::datatable(
      selsite_data, rownames=FALSE,filter="top",
      options = list(scrollY = '400px', paging = FALSE, scrollX=TRUE, dom="ltipr")
    )
  })
  outputOptions(output, "selsite_data", suspendWhenHidden = FALSE)
  sitelist_proxy%>%selectRows(selected = NULL)%>%clearSearch()
})

#### FILTERING BY SITES ####

# Load data (no duplicates!)
observeEvent(input$build_dat,{
  # Data without duplicated samples due to units
  selsite_data_nodups <- site_params_nodups[site_params_nodups$IR_MLID%in%reactive_objects$sel_sites,]
  
  # Data with duplicated samples due to units
  selsite_data <- site_params[site_params$IR_MLID%in%reactive_objects$sel_sites,]
  
  reactive_objects$selsite_data = selsite_data
  reactive_objects$selsite_data_nodups = selsite_data_nodups
  
  # Table does not show duplicates to cur down on confusion
    output$selsite_data <- DT::renderDT({
      DT::datatable(
        selsite_data_nodups, rownames=FALSE,filter="top",
        options = list(scrollY = '400px', paging = FALSE, scrollX=TRUE, dom="ltipr")
      )
    })
    outputOptions(output, "selsite_data", suspendWhenHidden = FALSE)
})

# Clear data
observeEvent(input$clear_dat,{
  reactive_objects$selsite_data = NULL
  selsite_data_nodups = site_params_nodups[0,]
  output$selsite_data <- DT::renderDT({
    DT::datatable(
      selsite_data_nodups, rownames=FALSE,filter="top",
      options = list(scrollY = '400px', paging = FALSE, scrollX=TRUE, dom="ltipr")
    )
  })
  outputOptions(output, "selsite_data", suspendWhenHidden = FALSE)
})

#### COMPARE PARAMS ####

# Site selection
observe({
  if(!is.null(reactive_objects$selsite_data)){
    reactive_objects$sel.param.sites = unique(reactive_objects$selsite_data$IR_MLID)
    }
  output$sel_param_site <- renderUI({
    selectInput("sel_param_site","Select Site", choices = c("",reactive_objects$sel.param.sites), selected = "")
  })
})

# Isolate numeric criteria for selected sites; Use 1 selection based on site
observe({
  if(!is.null(input$sel_param_site)){
    site_num_crit = num_crit[num_crit$IR_MLID==input$sel_param_site,]
    reactive_objects$site_num_crit = site_num_crit
    reactive_objects$sel.use1 = unique(site_num_crit$BeneficialUse)
  }
  output$sel_use1 <- renderUI({
    selectInput("sel_use1", "Select Use 1", choices = c("",reactive_objects$sel.use1), selected = "")
  })
})

# Select use 2 based on use 1 input
observe({
  if(!is.null(input$sel_use1)){
    uses = reactive_objects$sel.use1
    uses1 = uses[!(uses==input$sel_use1)]
    if(length(uses1)>0){
      reactive_objects$sel.use2 = uses1
    }else{reactive_objects$sel.use2 = NULL}
  }
  output$sel_use2 <- renderUI({
    selectInput("sel_use2", "Select Use 2", choices = c("",reactive_objects$sel.use2), selected = "")
  })
})


# Parameter 1 selection based on site and uses (from crit table)
observe({
  if(!is.null(input$sel_use1)){
    crit = reactive_objects$site_num_crit
    params = unique(crit$R3172ParameterName[crit$BeneficialUse%in%c(input$sel_use1,input$sel_use2)])
    reactive_objects$sel.param1 = params
  }
  output$sel_param1 <- renderUI({
    selectInput("sel_param1", "Select Parameter 1", choices = c("",reactive_objects$sel.param1), selected = "")
  })
})

# Parameter 2 selection based on parameter 1
observe({
  if(!is.null(input$sel_param1)){
    params = reactive_objects$sel.param1
    params1 = params[!(params==input$sel_param1)]
    if(length(params1)>0){
      reactive_objects$sel.param2 = params1
    }else{
        reactive_objects$sel.param2=NULL}
    }
  output$sel_param2 <- renderUI({
    selectInput("sel_param2", "Select Parameter 2", choices = c("",reactive_objects$sel.param2), selected = "")
  })
  
})

# Parameter 3 selection based on parameter 2
# observe({
#   if(!is.null(input$sel_param2)){
#     params = reactive_objects$sel.param2
#     params1 = params[!(params==input$sel_param2)]
#     if(length(params1)>0){
#       reactive_objects$sel.param3 = params1
#     }else{
#       reactive_objects$sel.param3=NULL}
#   }
#   output$sel_param3 <- renderUI({
#     selectInput("sel_param3", "Select Parameter 3", choices = c("",reactive_objects$sel.param3), selected = "")
#   })
#   
# })

# Filter data to inputs for plotting
observe({
  if(!is.null(input$sel_use1)&!is.null(input$sel_param1)){
    data = reactive_objects$selsite_data
    crit = reactive_objects$site_num_crit
    reactive_objects$plotdata = data[data$IR_MLID==input$sel_param_site&data$R3172ParameterName%in%c(input$sel_param1, input$sel_param2, input$sel_param3),]
    reactive_objects$plotcrit = crit[crit$BeneficialUse%in%c(input$sel_use1, input$sel_use2)&crit$R3172ParameterName%in%c(input$sel_param1, input$sel_param2, input$sel_param3),]
    }
})

## PLOT ##

# Time series - plot all parameters over time
### NOTE ### STANDARDS NEED TO BE ADDED

  output$compare_params <- renderPlotly({
    req(input$sel_param1)
    
    plotdata = reactive_objects$plotdata
    plotdata = plotdata[order(plotdata$ActivityStartDate),]
    plotcrit = reactive_objects$plotcrit
    
    # Get rid of duplicated samples with multiple units (still possible to plot both...)
    uniq_units = unique(plotcrit[,c("R3172ParameterName","CriterionUnits")])
    uniq_units$label = "keep"
    names(uniq_units)[names(uniq_units)=="CriterionUnits"] = "IR_Unit"
    print(uniq_units)
    plotdata_units = merge(plotdata, uniq_units, all.x=TRUE)
    plotdata_units = plotdata_units[!is.na(plotdata_units$label),!names(plotdata_units)%in%c("label")]
    
    param1 = plotdata[plotdata$R3172ParameterName==input$sel_param1,]
    param2 = plotdata[plotdata$R3172ParameterName==input$sel_param2,]
    #param3 = plotdata[plotdata$R3172ParameterName==input$sel_param3,]
    
    p = plot_ly(type = 'scatter')%>% 
      layout(title = param1$IR_MLID[1],
             yaxis1 = list(title = param1$IR_Unit[1]),
             yaxis2 = list(side="right", overlaying = "y",title = param2$IR_Unit[1]))%>%
            # yaxis3 = list(side = "right", overlaying = "y", title = param3$IR_Unit[1]))%>%
      add_trace(x = param1$ActivityStartDate, y = param1$IR_Value, name = param1$R3172ParameterName[1], mode='lines+markers')%>%
      add_trace(x = param2$ActivityStartDate, y = param2$IR_Value, name = param2$R3172ParameterName[1], mode='lines+markers', yaxis = "y2")
     #%>% add_trace(x = param3$ActivityStartDate, y = param3$IR_Value, name = param3$R3172ParameterName[1], mode='lines+markers', yaxis = "y3")
  })

#### COMPARE SITES ####

# Create parameter selection drop down (from non duplicated dataset)
  observe({
    if(!is.null(reactive_objects$selsite_data_nodups)){
      reactive_objects$comparameters = unique(reactive_objects$selsite_data_nodups$R3172ParameterName)
    }
    output$sel_comparameter <- renderUI({
      selectInput("sel_comparameter","Select Parameter", choices = c("",reactive_objects$comparameters), selected = "")
    })
  })

# Narrow down sites based on parameter
  observe({
    if(!is.null(input$sel_comparameter)){
      data = reactive_objects$selsite_data_nodups
      sites = unique(data$IR_MLID[data$R3172ParameterName==input$sel_comparameter])
      reactive_objects$sel.comparam.site1 = sites
    }
    output$sel_comparam_site1 <- renderUI({
      selectInput("sel_comparam_site1","Select Site 1", choices = c("",reactive_objects$sel.comparam.site1), selected = "")
    })  
  })
  
# Narrow down site 2 based on site 1
observe({
  if(!is.null(input$sel_comparam_site1)){
    sites = reactive_objects$sel.comparam.site1 
    sites1 = sites[!(sites==input$sel_comparam_site1)]
    if(length(sites1)>0){
      reactive_objects$sel.comparam.site2 = sites1
    }else{
      reactive_objects$sel.comparam.site2=NULL}
  }
  output$sel_comparam_site2 <- renderUI({
    selectInput("sel_comparam_site2","Select Site 2", choices = c("",reactive_objects$sel.comparam.site2), selected = "")
  }) 
})  

# Narrow down site 3 based on site 2
observe({
  if(!is.null(input$sel_comparam_site2)){
    sites = reactive_objects$sel.comparam.site2 
    sites1 = sites[!(sites==input$sel_comparam_site2)]
    if(length(sites1)>0){
      reactive_objects$sel.comparam.site3 = sites1
    }else{
      reactive_objects$sel.comparam.site3=NULL}
  }
  output$sel_comparam_site3 <- renderUI({
    selectInput("sel_comparam_site3","Select Site 3", choices = c("",reactive_objects$sel.comparam.site3), selected = "")
  }) 
})   

## PLOT ##

output$compare_sites <- renderPlotly({
  req(input$sel_comparam_site2)
  
  plotdata = reactive_objects$selsite_data_nodups
  plotdata = plotdata[order(plotdata$ActivityStartDate),]
  plotdata = plotdata[plotdata$R3172ParameterName==input$sel_comparameter&plotdata$IR_MLID%in%c(input$sel_comparam_site1,input$sel_comparam_site2,input$sel_comparam_site3),]
  print(plotdata)
  
  site1 = plotdata[plotdata$IR_MLID==input$sel_comparam_site1,]
  site2 = plotdata[plotdata$IR_MLID==input$sel_comparam_site2,]
  site3 = plotdata[plotdata$IR_MLID==input$sel_comparam_site3,]
  
  p = plot_ly(type = 'scatter')%>% 
    layout(title = site1$R3172ParameterName[1],
           yaxis1 = list(title = site1$IR_Unit[1]))%>%
    add_trace(x = site1$ActivityStartDate, y = site1$IR_Value, name = site1$IR_MLID[1], mode='lines+markers')%>%
    add_trace(x = site2$ActivityStartDate, y = site2$IR_Value, name = site2$IR_MLID[1], mode='lines+markers')%>%
    add_trace(x = site3$ActivityStartDate, y = site3$IR_Value, name = site3$IR_MLID[1], mode='lines+markers')
})





# Conc v. flow scatterplot - plot chosen parameter versus flow (if available)

# Spatial concentration relationships - Map site points with radius proportional to concentration






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

# ##### FILTER DATA ####
# 
# # Filtering dependencies 
# observe({
#   if(!is.null(reactive_objects$sel_sites)){
#     reactive_objects$crit = num_crit[num_crit$IR_MLID%in%reactive_objects$sel_sites,]
#     reactive_objects$params = unique(reactive_objects$crit$R3172ParameterName)
#   }else{
#     reactive_objects$crit = NULL
#     reactive_objects$params = NULL
#     reactive_objects$uses = NULL
#   }
#   if(!is.null(input$sel_params)){
#     updatecrit <- isolate(reactive_objects$crit)
#     reactive_objects$crit = updatecrit[updatecrit$R3172ParameterName%in%reactive_objects$params,]
#     reactive_objects$uses = unique(reactive_objects$crit$BeneficialUse)}
#   if(!is.null(input$sel_use)){
#     updatecrit <- isolate(reactive_objects$crit)
#     reactive_objects$crit = updatecrit[updatecrit$BeneficialUse%in%reactive_objects$uses,]
#     }
# })
# 
# # Reactive user interface items
# output$sel_params <- renderUI({
#   selectizeInput("sel_params","Select R3172 Parameter(s)",choices = reactive_objects$params, multiple = TRUE, selected = NULL)
# })
# 
# output$sel_use <- renderUI({
#   selectizeInput("sel_use", "Select Uses", choices = reactive_objects$uses, multiple = TRUE, selected = NULL)
# })
# 
# output$sel_date <- renderUI({
#   req(reactive_objects$mindate)
#   sliderInput("sel_date", "Select Date Range", value = c(reactive_objects$mindate,reactive_objects$maxdate), min = reactive_objects$mindate, max = reactive_objects$maxdate)
# })
# 
# # Filter data based on crit filters
# observe({
#   if(!is.null(input$sel_use)){
#     dates = site_params$ActivityStartDate[site_params$IR_MLID%in%reactive_objects$sel_sites&site_params$R3172ParameterName%in%input$sel_params]
#     reactive_objects$mindate = min(dates)
#     reactive_objects$maxdate = max(dates)
#   }else{
#     reactive_objects$mindate = min(site_params$ActivityStartDate)
#     reactive_objects$maxdate = max(site_params$ActivityStartDate)
#     }
# })
# 
# # Finally, filter data one last time based on date input.
# # Define empty table for clear site list button
# empty_filters = site_params[0,]
# 
# # On filter data click, create data table
# observeEvent(input$filter_data,{
#   filteredat = site_params[site_params$IR_MLID%in%reactive_objects$sel_sites&site_params$R3172ParameterName%in%input$sel_params,]
#   filteredat = filteredat[filteredat$ActivityStartDate>=input$sel_date[1]&filteredat$ActivityStartDate<=input$sel_date[2],]
#   reactive_objects$data = filteredat
#   
#   output$filtered_data <- DT::renderDT({
#     DT::datatable(
#       filteredat, rownames=FALSE,filter="top",
#       options = list(scrollY = '400px', paging = FALSE, scrollX=TRUE, dom="ltipr")
#     )
#   })
#   outputOptions(output, "filtered_data", suspendWhenHidden = FALSE)
#   })

# param_inputs = c(input$sel_param1, input$sel_param2, input$sel_param3)
# reactive_objects$param.inputs = param_inputs
# print(reactive_objects$param.inputs)
# if(length(param_inputs)>0){
#   site_param_crit = num_crit[num_crit$IR_MLID%in%reactive_objects$sel_sites&num_crit$R3172ParameterName%in%param_inputs,]
#   reactive_objects$site_param_crit = site_param_crit
#   reactive_objects$sel.use1 = unique(site_param_crit$BeneficialUse)
# }

# yax1 <- list(
#   tickfont = list(color = "red"),
#   overlaying = "y",
#   side = "left",
#   title = param1$IR_Unit[1]
# )
# 
# yax2 <- list(
#   tickfont = list(color = "red"),
#   overlaying = "y",
#   side = "right",
#   title = param2$IR_Unit[1]
# )