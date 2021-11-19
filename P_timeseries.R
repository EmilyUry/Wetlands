



setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files/TREND-phosphorus P Surplus Dataset Publication Files (2017)/TREND-phosphorus P Surplus Dataset Publication Files")

# 
# 
# 
library(sf)
counties <- st_read("LEB_counties_US.shp")
plot(counties)


##counties <- readOGR("LEB_counties_US.shp")



info <- read.csv("LEB_counties.csv", head = TRUE)

LC <- info$GEOID
LCs <- paste("X", LC, sep = "")


year <- read.csv("CropUptake_Cropland.csv") %>%
  select(NaN.)
names(year) <- "year"
year$Xyear <- paste("X", year$year, sep = "")


CUc <- read.csv("CropUptake_Cropland.csv") %>%
  select(LCs)

CUp <- read.csv("CropUptake_Pasture.csv") %>%
  select(LCs)

FA <- read.csv("Fertilizer_Agriculture.csv") %>%
  select(LCs)

FD <- read.csv("Fertilizer_Domestic.csv") %>%
  select(LCs)

Ldc <- read.csv("Lvst_DairyCattle.csv") %>%
  select(LCs)

Le <- read.csv("Lvst_Equine.csv") %>%
  select(LCs) 

Lh <- read.csv("Lvst_Hogs.csv") %>%
  select(LCs) 

Loc <- read.csv("Lvst_OtherCattle.csv") %>%
  select(LCs)

Lp <- read.csv("Lvst_Poultry.csv") %>%
  select(LCs) 

Lsg <- read.csv("Lvst_SheepGoat.csv") %>%
  select(LCs) 



Surplus_calc <- function (a) {round(FA[a,] + FD[a,] + Ldc[a,] + Le[a,] + Lh[a,] + Loc[a,] + Lp[a,] + Lsg[a,] - CUc[a,] - CUp[a,], 4)}
Psur <- sapply(1:88, Surplus_calc)

Psur <- as.data.frame((Psur))
names(Psur) <- year$Xyear
Psur <- t(Psur)
Psur <- as.data.frame((Psur))

plot(year$year, Psur$X36029, type = 'l', col = "blue", ylim = c(-10,10) )
points(year$year, Psur$X36003, type = 'l', col = "blue" )
points(year$year, Psur$X36009, type = 'l', col = "blue" )
points(year$year, Psur$X39153, type = 'l', col = "blue" )
points(year$year, Psur$X26125, type = 'l', col = "black" )
points(year$year, Psur$X26093, type = 'l', col = "black" )
points(year$year, Psur$X26087, type = 'l', col = "black" )
points(year$year, Psur$X18113, type = 'l', col = "red" )
points(year$year, Psur$X18003, type = 'l', col = "red" )
points(year$year, Psur$X18151, type = 'l', col = "red" )
points(year$year, Psur$X18179, type = 'l', col = "red" )
points(year$year, Psur$X42049, type = 'l', col = "green" )
points(year$year, Psur$X42039, type = 'l', col = "green" )
points(year$year, Psur$X36037, type = 'l', col = "purple" )






Inputs_sum <- function (a) {round(FA[a,] + FD[a,] + Ldc[a,] + Le[a,] + Lh[a,] + Loc[a,] + Lp[a,] + Lsg[a,], 4)}
P.in <- sapply(1:88, Inputs_sum)

P.in <- as.data.frame((P.in))
names(P.in) <- year$Xyear
P.in <- t(P.in)
P.in <- as.data.frame((P.in))

plot(year$year, P.in$X36029, type = 'l', col = "blue", ylim = c(0,25) )
points(year$year, P.in$X36003, type = 'l', col = "blue" )
points(year$year, P.in$X36009, type = 'l', col = "blue" )
points(year$year, P.in$X39153, type = 'l', col = "blue" )
points(year$year, P.in$X26125, type = 'l', col = "black" )
points(year$year, P.in$X26093, type = 'l', col = "black" )
points(year$year, P.in$X26087, type = 'l', col = "black" )
points(year$year, P.in$X18113, type = 'l', col = "red" )
points(year$year, P.in$X18003, type = 'l', col = "red" )
points(year$year, P.in$X18151, type = 'l', col = "red" )
points(year$year, P.in$X18179, type = 'l', col = "red" )
points(year$year, P.in$X42049, type = 'l', col = "green" )
points(year$year, P.in$X42039, type = 'l', col = "green" )
points(year$year, P.in$X36037, type = 'l', col = "purple" )




Uptake_sum <- function (a) {round(CUc[a,] + CUp[a,], 4)}
P.up <- sapply(1:88, Uptake_sum)

P.up <- as.data.frame((P.up))
names(P.up) <- year$Xyear
P.up <- t(P.up)
P.up <- as.data.frame((P.up))

plot(year$year, P.up$X36029, type = 'l', col = "blue", ylim = c(0,30) )
points(year$year, P.up$X36003, type = 'l', col = "blue" )
points(year$year, P.up$X36009, type = 'l', col = "blue" )
points(year$year, P.up$X39153, type = 'l', col = "blue" )
points(year$year, P.up$X26125, type = 'l', col = "black" )
points(year$year, P.up$X26093, type = 'l', col = "black" )
points(year$year, P.up$X26087, type = 'l', col = "black" )
points(year$year, P.up$X18113, type = 'l', col = "red" )
points(year$year, P.up$X18003, type = 'l', col = "red" )
points(year$year, P.up$X18151, type = 'l', col = "red" )
points(year$year, P.up$X18179, type = 'l', col = "red" )
points(year$year, P.up$X42049, type = 'l', col = "green" )
points(year$year, P.up$X42039, type = 'l', col = "green" )
points(year$year, P.up$X36037, type = 'l', col = "purple" )




tiff(filename = "P timeseries.tiff", height=4800, width=4800, units= "px", res=300, compression= "lzw")
dev.off()














hist(info$P_sur, breaks = 20, col = "dodgerblue", xlab = "Cumulative P surplus (kg/ha)", 
     main = "Cumulative P surplus")

counties$P_sur <- data

plot(counties)

st_write(counties, "P_Surplus_LEB_counties_US.shp")
