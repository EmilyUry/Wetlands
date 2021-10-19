

## raster merge


### merge a million .tif files to make a single elevation raster for southern ontario


library(raster)


r1 <- raster('1km17505047950SWOOP2015.img')
r2 <- raster('1km17505047960SWOOP2015.img')

rm <- merge(r1, r2)
plot(rm)


x <- list(r1, r2)




setwd("C:/Users/Emily Ury/Downloads/SWOOP2015_DEM-C/PackageC")

files <- list.files(path = ".", pattern = "*.img")

start_time <- Sys.time()
rasters <- lapply(files, raster)
end_time <- Sys.time()
end_time - start_time




setwd("C:/Users/Emily Ury/Downloads/SWOOP2015_DEM-F/PackageF")

filesF <- list.files(path = ".", pattern = "*.img")

start_time <- Sys.time()
rastersF <- lapply(filesF, raster)
end_time <- Sys.time()
end_time - start_time


setwd("C:/Users/Emily Ury/Downloads/SWOOP2015_DEM-G/PackageG")

filesG <- list.files(path = ".", pattern = "*.img")

start_time <- Sys.time()
rastersF <- lapply(filesG, raster)
end_time <- Sys.time()
end_time - start_time







mosaic <- do.call(merge, rasters)
plot(mosaic)


m <- do.call(merge, x)   # x is a list of files
plot(m)


