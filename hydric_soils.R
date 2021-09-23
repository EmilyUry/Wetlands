

## Hydric soils

#' This is one step in the process for identifying *Historic Wetlands* 
#' We are using SSOLRIS soils data, very fine scale resolution soils maps
#' and filtering by hydric soil types 
#' Then dissolving those hydric soil types back into a single shapefile
#' The maps are too larged and detailed to run the dissolve function in ArcMap
#' so we are trying it this way
#' 
#' 
#' 

setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files/SSURGO_soil_maps")


library(rgdal)
library(rgeos)
#library(dplyr)
#library(sf)

OH_soils <- st_read("LEB_Ohio.shp")

#plot(OH_soils)  #check - takes a reaaaally long time 

NY_soils <- readOGR("LEB_NewYork.shp")
#plot(NY_soils)

