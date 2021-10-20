

## raster merge

#' Downloaded 2-m raster elevation product for Ontario from:
#' https://geohub.lio.gov.on.ca/maps/mnrf::ontario-digital-elevation-model-imagery-derived/explore?location=42.535514%2C-81.616128%2C7.56
#' Each of the SWOOP packages (C-H) has ~5000 1km2 tiles.
#' All of the tiles must be merged into a single raster
#' We will do this separately for each package to keep the output sizes managable
#' 
#' 1. Convert .img file to raster
#' 2. Merge rasters into a single mosaic
#' 3. Write out mosaic as a .tif


library(raster)

setwd("C:/Users/Emily Ury/Downloads/SWOOP2015_DEM-B/PackageB")
filesB <- list.files(path = ".", pattern = "*.img")
rastersB <- lapply(filesB, raster)

setwd("C:/Users/Emily Ury/Downloads/SWOOP2015_DEM-C/PackageC")
filesC <- list.files(path = ".", pattern = "*.img")
rastersC <- lapply(filesC, raster)

setwd("C:/Users/Emily Ury/Downloads/SWOOP2015_DEM-D/PackageD")
filesD <- list.files(path = ".", pattern = "*.img")
rastersD <- lapply(filesD, raster)

setwd("C:/Users/Emily Ury/Downloads/SWOOP2015_DEM-E/PackageE")
filesE <- list.files(path = ".", pattern = "*.img")
rastersE <- lapply(filesE, raster)

setwd("C:/Users/Emily Ury/Downloads/SWOOP2015_DEM-F/PackageF")
filesF <- list.files(path = ".", pattern = "*.img")
rastersF <- lapply(filesF, raster)

setwd("C:/Users/Emily Ury/Downloads/SWOOP2015_DEM-G/PackageG")
filesG <- list.files(path = ".", pattern = "*.img")
rastersG <- lapply(filesG, raster)

setwd("C:/Users/Emily Ury/Downloads/SWOOP2015_DEM-H/PackageH")
filesH <- list.files(path = ".", pattern = "*.img")
rastersH <- lapply(filesH, raster)


setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files/Historic_wetlands_CAN/DEM")

start_time <- Sys.time()
mosaic_B <- do.call(merge, rastersB)
writeRaster(mosaic_B,'DEM_B.tif')
end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()
mosaic_C <- do.call(merge, rastersC)
writeRaster(mosaic_C,'DEM_C.tif')
end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()
mosaic_D <- do.call(merge, rastersD)
writeRaster(mosaic_D,'DEM_D.tif')
end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()
mosaic_E <- do.call(merge, rastersE)
writeRaster(mosaic_E,'DEM_E.tif')
end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()
mosaic_F <- do.call(merge, rastersF)
writeRaster(mosaic_F,'DEM_F.tif')
end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()
mosaic_G <- do.call(merge, rastersG)
writeRaster(mosaic_G,'DEM_G.tif')
end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()
mosaic_H <- do.call(merge, rastersH)
writeRaster(mosaic_H,'DEM_H.tif')
end_time <- Sys.time()
end_time - start_time





## this following works petter in R

map <- do.call(merge, rastersB[1:3])
plot(map)


map10 <- aggregate(map, 5, mean)
plot(map10)


plot(mosaic_C)
start_time <- Sys.time()
DEM_C_10 <- aggregate(mosaic_C, 5, mean)  ## took 10 mins to run the aggregation
end_time <- Sys.time()
end_time - start_time

plot(DEM_C_10)
