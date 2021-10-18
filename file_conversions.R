

## convert .geojson to .shp

##### note: this can take a while to run for large files. 


library(sf)

setwd("C:/Users/Emily Ury/Downloads")
d <- read_sf("Soil_Survey_Complex.geojson") 


st_write(d, "CAN_soil.shp")
