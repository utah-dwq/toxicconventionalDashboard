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

prepped_dat <- read.csv("prepped_tox_conv.csv")
uni_type_name = unique(prepped_dat[,c("IR_MLID","IR_MLNAME","AU_Type")])
data_amount = unique(prepped_dat[,c("IR_MLID","ActivityStartDate","R3172ParameterName")])
data_amount = plyr::ddply(data_amount, .(IR_MLID), summarize, NCount = length(R3172ParameterName))

assessed_dat <- read.csv("assessed_tox_conv.csv")
assessed_dat = within(assessed_dat,{
  Num_Cat = NA
  Num_Cat[IR_Cat == "FS"] <- 1
  Num_Cat[IR_Cat == "idE"] <- 2
  Num_Cat[IR_Cat == "idNE"] <- 3
  Num_Cat[IR_Cat == "TMDL"] <- 4
  Num_Cat[IR_Cat == "NS"] <- 5
})
unicats = unique(assessed_dat[,c("IR_MLID", "AU_NAME","R317Descrp","BEN_CLASS","IR_Lat","IR_Long","IR_Cat", "Num_Cat")])
unicats1 = plyr::ddply(unicats, .(IR_MLID, AU_NAME, R317Descrp, BEN_CLASS, IR_Lat, IR_Long),summarize, Categories = paste(IR_Cat, collapse = ", "), MaxCat = max(Num_Cat))

siteinfo = merge(unicats1, uni_type_name, all.x = TRUE)
siteinfo = merge(siteinfo, data_amount, all.x = TRUE)
siteinfo$radius = log(siteinfo$NCount)*3
# Site coordinates
site_coords=sf::st_as_sf(siteinfo, coords=c("IR_Long","IR_Lat"), crs=4326, remove=F)

# Data table and plotting cast
vizdat <- unique(prepped_dat[,c("BeneficialUse","BEN_CLASS","IR_MLID", "R3172ParameterName","ActivityStartDate","IR_Unit","AssessmentType","CriterionType","NumericCriterion","CriterionLabel","IR_Value")])

vizdat1 <- tidyr::spread(vizdat, BeneficialUse, NumericCriterion, fill= NA)

################ SERVER ##################

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

# Map site colors
pal <- rev(RColorBrewer::brewer.pal(length(unique(unicats1$MaxCat)),"Spectral"))
pal1 = leaflet::colorFactor(pal, domain=unicats1$MaxCat)  
  
# Empty reactive values object
reactive_objects=reactiveValues()

observe({
  req("map")
  reactive_objects$site_coords = site_coords
})

# Map
map = leaflet::createLeafletMap(session, 'map')
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
    map = addPolygons(map, data=wqTools::bu_poly,group="Beneficial uses",smoothFactor=3,opacity=0.9, ,fillOpacity = 0.1,weight=2,color="green", options = pathOptions(pane = "underlay_polygons"),
                      popup=paste0(
                        "Description: ", bu_poly$R317Descrp,
                        "<br> Uses: ", bu_poly$bu_class))
    map = addPolygons(map, data=wqTools::ss_poly,group="Site-specific standards",smoothFactor=3,opacity=0.9, ,fillOpacity = 0.1,weight=2,color="blue", options = pathOptions(pane = "underlay_polygons"),
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
                             "<br> Sample Count: ", as.character(reactive_objects$site_coords$NCount)))
    map=hideGroup(map, "Assessment units")
    map=hideGroup(map, "Site-specific standards")
    map=hideGroup(map, "Beneficial uses") 
    
  }) 
})  

  
})
