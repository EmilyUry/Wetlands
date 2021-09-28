


#' # P surplus calculations

#' ### Last update: 2021-09-28

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


DataWD <- "C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files/NWI_Wetlands_US/Lake_erie_basin"


watersheds <- readOGR("Lake_Erie_HUC8.shp")

plot(watersheds, main = "Lake Erie Watersheds")


lvst_up <- raster("Downscaled_Agricultural_P/lvst_uptake_ag_2017.tif")
lvst_P <- raster("Downscaled_Agricultural_P/lvst_P_ag_2017.tif")
fert_P <- raster("Downscaled_Agricultural_P/fertilizer_P_ag_2017.tif")

plot(lvst_up)
plot(watersheds, add = TRUE)

P_sur <- lvst_P + fert_P - lvst_up

extract(lvst_up, watersheds)


