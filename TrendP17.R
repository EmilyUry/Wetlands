

### Trend P 2017

#' # P surplus calculations

#' ### Last update: 2022-08-08

#' This script is to calculate the P surplus layer for US
#' Input files include
#' P fertilizer input
#' P manure (dairy, equine, hog, cattle, poultry, sheep) input
#' P crop uptake
#' 
#' Simple math balance calculation:
#'     P surplus = P fertilizer + P manure - P crop 
#'     
#' We will also use spatial averaging to convert the P surplus layer from a 
#' 250 m level pixel, to a watershed aggregation.
#' 
setwd("C:/Users/uryem/OneDrive - University of Waterloo/Wetlands_local/Data_files")
library(rgdal)
library(raster)
library(maptools)


DataWD <- "C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files/NWI_Wetlands_US/Lake_erie_basin"


watersheds <- readOGR("Lake_Erie_HUC8.shp")
HU04090001 <- readOGR("NWI_Wetlands_US/Lake_erie_basin/HU8_04090001_watershed/HU8_04090001_Watershed.shp")
HU04100008 <- readOGR("NWI_Wetlands_US/Lake_erie_basin/HU8_04100008_watershed/HU8_04100008_Watershed.shp")

plot(watersheds)
plot(HU04090001, col = "blue", add = TRUE)
plot(HU04100008, col = "red", add = TRUE)



uptake <- raster("TrendP2017/CropUptake_Agriculture_Agriculture_LU_rasterized_2017.tif")
fert_P <- raster("TrendP2017/Fertilizer_Agriculture_Agriculture_LU_rasterized_2017.tif")
lvst_dairy_P <- raster("TrendP2017/Lvst_DairyCattle_Agriculture_LU_rasterized_2017.tif")
lvst_equine_P <- raster("TrendP2017/Lvst_Equine_Agriculture_LU_rasterized_2017.tif")
lvst_hog_P <- raster("TrendP2017/Lvst_Hogs_Agriculture_LU_rasterized_2017.tif")
lvst_beef_P <- raster("TrendP2017/Lvst_OtherCattle_Agriculture_LU_rasterized_2017.tif")
lvst_poultry_P <- raster("TrendP2017/Lvst_Poultry_Agriculture_LU_rasterized_2017.tif")
lvst_sheep_P <- raster("TrendP2017/Lvst_SheepGoat_Agriculture_LU_rasterized_2017.tif")

manure <- lvst_dairy_P + lvst_equine_P + lvst_hog_P + lvst_beef_P + lvst_poultry_P + lvst_sheep_P
 ## took about 10 mins

P_inputs <- fert_P + manure

# Mass balance calculation to determine p-surplus
P_sur <- manure + fert_P - uptake  ## if this raster math takes too long, can try
                                   ## using the overlay() function.
                                   ## see https://www.neonscience.org/resources/learning-hub/tutorials/dc-raster-calculations-r

plot(P_sur)

writeRaster(P_sur, "TrendP2017/Downscaled_P_sur.tif", overwrite = TRUE)

writeRaster(P_inputs, "TrendP2017/Downscaled_P_inputs.tif", overwrite = TRUE)


plot(watersheds, add = TRUE)
#extract mean surplus for each watershed
mean_P_sur <- extract(P_sur, watersheds, fun = mean, na.rm = TRUE, df = TRUE)
## time start 11:19 AM
## time end 

US_P_sur <- data.frame(mean_P_sur)





plot(watersheds)
plot(P_sur, breaks = c(-10,-7.5, -5, -2.5, 0, 2.5, 5, 7.5, 10),
     col = terrain.colors(8),legend=F, add = TRUE)
plot(watersheds, add = TRUE)
legend("bottomright", legend = c("-7.5", "-5", "-2.5", "0", "2.5", "5", "7.5", "10"), 
       fill = rev(terrain.colors(8)))
