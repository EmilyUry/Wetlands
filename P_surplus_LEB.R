


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
HU04090001 <- readOGR("NWI_Wetlands_US/Lake_erie_basin/HU8_04090001_watershed/HU8_04090001_Watershed.shp")
HU04100008 <- readOGR("NWI_Wetlands_US/Lake_erie_basin/HU8_04100008_watershed/HU8_04100008_Watershed.shp")



lvst_up <- raster("Downscaled_Agricultural_P/lvst_uptake_ag_2017.tif")
lvst_P <- raster("Downscaled_Agricultural_P/lvst_P_ag_2017.tif")
fert_P <- raster("Downscaled_Agricultural_P/fertilizer_P_ag_2017.tif")

# Mass balance calculation to determine p-surplus
P_sur <- lvst_P + fert_P - lvst_up  ## if this raster math takes too long, can try
                                    ## using the overlay() function.
                                    ## see https://www.neonscience.org/resources/learning-hub/tutorials/dc-raster-calculations-r

plot(P_sur)
plot(watersheds, add = TRUE)




start_time <- Sys.time()

mean1 <- raster::extract(P_sur, HU04090001, fun = mean, na.rm = TRUE) ## takes about 3 minutes

end_time <- Sys.time()
end_time - start_time


library(exactextractr)

start_time <- Sys.time()
mean1 <- exact_extract(P_sur, HU04100008, 'mean')
end_time <- Sys.time()
end_time - start_time






## ext <- raster::extract(lvst_up, watersheds) ## does not seem to work (or at least takes forever)

r.vals <- extract(P_sur, watersheds, fun = mean, na.rm = TRUE)
mean_p_sur <- lapply(r.vals, FUN=mean)


watersheds$mean_val <- raster::extract(P_sur, watersheds, mean, method = 'simple') 






wts1 <- watersheds[,1]


# NOT RUN {
rast <- raster::raster(matrix(1:100, ncol=10), xmn=0, ymn=0, xmx=10, ymx=10)
poly <- sf::st_as_sfc('POLYGON ((2 2, 7 6, 4 9, 2 2))')

# named summary operation on RasterLayer, returns vector
exact_extract(rast, poly, 'mean')



