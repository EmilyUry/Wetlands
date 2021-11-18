


#' # P surplus calculations

#' ### Last update: 2021-11-18

#' This script is to calculate the P surplus layer for LEB
#' Input files include
#' P fertilizer input
#' P manure input
#' P crop uptake
#' 
#' Simple math balance calculation:
#'     P surplus = P fertilizer + P manure - P crop 
#'     
#' We will also use spatial averaging to convert the P surplus layer from a 
#' county level aggragation, to a watershed aggregation.
#' 
setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files")
library(rgdal)
library(raster)
library(maptools)


DataWD <- "C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files/NWI_Wetlands_US/Lake_erie_basin"


watersheds <- readOGR("Lake_Erie_HUC8.shp")
HU04090001 <- readOGR("NWI_Wetlands_US/Lake_erie_basin/HU8_04090001_watershed/HU8_04090001_Watershed.shp")
HU04100008 <- readOGR("NWI_Wetlands_US/Lake_erie_basin/HU8_04100008_watershed/HU8_04100008_Watershed.shp")



lvst_P <- raster("Downscaled_ag_P_lake_erie/US/ag_lvst_P_kgha_2017.tif")
uptake <- raster("Downscaled_ag_P_lake_erie/US/ag_uptake_P_kgha_2017.tif")
fert_P <- raster("Downscaled_ag_P_lake_erie/US/ag_fertilizer_P_kgha_2017.tif")

# Mass balance calculation to determine p-surplus
P_sur <- lvst_P + fert_P - uptake  ## if this raster math takes too long, can try
                                    ## using the overlay() function.
                                    ## see https://www.neonscience.org/resources/learning-hub/tutorials/dc-raster-calculations-r

writeRaster(P_sur, "Downscaled_ag_P_lake_erie/US/P_sur.tif", overwrite = TRUE)


## the above step and the extraction (below) takes a long time in R
## definitely faster to run in arcmap -- working on it
## for now - work around by opening P_sur.tif in ArcMap and do the following:
#' Zonal Statistics tool on P_sur, with HUC8 watersheds as the zones
#' Zonal Statistics as Table to export to CSV

plot(P_sur)
plot(watersheds, add = TRUE)


mean_P_sur <- extract(P_sur, watersheds, fun = mean, na.rm = TRUE, df = TRUE)

US_P_sur <- df(mean_P_sur)
write.csv(US_P_sur, file = "Downscaled_ag_P_lake_erie/US/US_P_sur")












### junk code

# mean1 <- raster::extract(P_sur, HU04090001, fun = mean, na.rm = TRUE) ## takes about 3 minutes
# library(exactextractr)
# means <- exact_extract(P_sur, HU04100008, 'mean')





