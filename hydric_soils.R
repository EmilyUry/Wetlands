

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
list <- paste(shQuote(mukey, type="sh"), collapse = ", ")
write.table(list, "hydric_OH.text")


## repeat for NY

LEB_NY <- c('NY003', 'NY009', 'NY013', 'NY029', 'NY037', 'NY121', 'NY605')
soils <- get_SDA_hydric(LEB_NY)
soils$HYDRIC_RATING <- as.factor(soils$HYDRIC_RATING)
hydric <- soils[which(soils$HYDRIC_RATING == "Hydric" |
                                      soils$HYDRIC_RATING == "Predominantly Hydric"),]
mukey <- (hydric$mukey)
list <- paste(shQuote(mukey, type="sh"), collapse = ", ")
write.table(list, "hydric_NY.text")


LEB_MI <- c('MI023', 'MI059', 'MI065', 'MI075', 'MI087', 'MI091', 'MI093', 'MI099',
            'MI115', 'MI125', 'MI147', 'MI151', 'MI161', 'MI163')
soils <- get_SDA_hydric(LEB_MI)
soils$HYDRIC_RATING <- as.factor(soils$HYDRIC_RATING)
hydric <- soils[which(soils$HYDRIC_RATING == "Hydric" |
                        soils$HYDRIC_RATING == "Predominantly Hydric"),]
mukey <- (hydric$mukey)
list <- paste(shQuote(mukey, type="sh"), collapse = ", ")
write.table(list, "hydric_MI.text")



LEB_PA <- c('PA039', 'PA049')
soils <- get_SDA_hydric(LEB_PA)
soils$HYDRIC_RATING <- as.factor(soils$HYDRIC_RATING)
hydric <- soils[which(soils$HYDRIC_RATING == "Hydric" |
                        soils$HYDRIC_RATING == "Predominantly Hydric"),]
mukey <- (hydric$mukey)
list <- paste(shQuote(mukey, type="sh"), collapse = ", ")
write.table(list, "hydric_PA.text")



LEB_IN <- c('IN001', 'IN003', 'IN033', 'IN113', 'IN151', 'IN179')
soils <- get_SDA_hydric(LEB_IN)
soils$HYDRIC_RATING <- as.factor(soils$HYDRIC_RATING)
hydric <- soils[which(soils$HYDRIC_RATING == "Hydric" |
                        soils$HYDRIC_RATING == "Predominantly Hydric"),]
mukey <- (hydric$mukey)
list <- paste(shQuote(mukey, type="sh"), collapse = ", ")
write.table(list, "hydric_IN.text")


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

