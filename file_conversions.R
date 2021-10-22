

## convert .geojson to .shp

##### note: this can take a while to run for large files. 


library(sf)

setwd("C:/Users/Emily Ury/Downloads")
d <- read_sf("Soil_Survey_Complex.geojson") 


st_write(d, "CAN_soil.shp")



### convert a list to a string

setwd("C:/Users/Emily Ury/Desktop/New folder")

x <- read.csv("soilCode.csv")
list <- x[,1]


string <- paste(shQuote(list, type = 'sh'), collapse =', ')
string



x <- read.csv("symbol1.csv")
list <- x[,1]
string <- paste(shQuote(list, type = 'sh'), collapse =', ')
string
