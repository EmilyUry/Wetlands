

## Hydric soils


#' Obtain an index of hydric soil types from the Soil Database Interface (USDA-NCSS)
#' using package `soilDB`.
#' 
#'   Dylan Beaudette, Jay Skovlin, Stephen Roecker and Andrew Brown (2021). soilDB: Soil Database
#'   Interface. R package version 2.6.5. https://CRAN.R-project.org/package=soilDB

setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files/SSURGO_soil_maps")

library(soilDB)

# List of the 35 county codes for Ohio counties in the Lake Erie Basin: 
LEB_OH <- c('OH003', 'OH005', 'OH007', 'OH011', 'OH033', 'OH035', 'OH039', 'OH043', 
            'OH051', 'OH055', 'OH063', 'OH065', 'OH069', 'OH077', 'OH085', 'OH093',
            'OH095', 'OH101', 'OH103', 'OH107', 'OH123', 'OH125', 'OH133', 'OH137',
            'OH143', 'OH149', 'OH151', 'OH153', 'OH155', 'OH161', 'OH171', 'OH173',
            'OH175', 'OH139', 'OH147')

# pull the list of soils in these 35 counties
LEB_OH_soils <- get_SDA_hydric(LEB_OH)

## filter HYDRIC_RATING for "Hydric" and "Predominantly Hydric"

LEB_OH_soils$HYDRIC_RATING <- as.factor(LEB_OH_soils$HYDRIC_RATING)
LEB_OH_hydric <- LEB_OH_soils[which(LEB_OH_soils$HYDRIC_RATING == "Hydric" |
                                      LEB_OH_soils$HYDRIC_RATING == "Predominantly Hydric"),]
one <- c(1:5)
mukey <- (LEB_OH_hydric$mukey)
list <- paste(shQuote(one, type="cmd"), collapse = ", ")



write.table(list, "file.text")





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

