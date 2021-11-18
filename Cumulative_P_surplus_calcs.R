



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


CUc <- read.csv("CropUptake_Cropland.csv") %>%
  select(LCs) %>%
  colSums()

CUp <- read.csv("CropUptake_Pasture.csv") %>%
  select(LCs) %>%
  colSums()

FA <- read.csv("Fertilizer_Agriculture.csv") %>%
  select(LCs) %>%
  colSums()

FD <- read.csv("Fertilizer_Domestic.csv") %>%
  select(LCs) %>%
  colSums()

Ldc <- read.csv("Lvst_DairyCattle.csv") %>%
  select(LCs) %>%
  colSums()

Le <- read.csv("Lvst_Equine.csv") %>%
  select(LCs) %>%
  colSums()

Lh <- read.csv("Lvst_Hogs.csv") %>%
  select(LCs) %>%
  colSums()

Loc <- read.csv("Lvst_OtherCattle.csv") %>%
  select(LCs) %>%
  colSums()

Lp <- read.csv("Lvst_Poultry.csv") %>%
  select(LCs) %>%
  colSums()

Lsg <- read.csv("Lvst_SheepGoat.csv") %>%
  select(LCs) %>%
  colSums()

data <- FA + FD + Ldc + Le + Lh + Loc + Lp + Lsg - CUc - CUp

info$P_sur <- data


hist(info$P_sur, breaks = 20, col = "dodgerblue", xlab = "Cumulative P surplus (kg/ha)", 
     main = "Cumulative P surplus")

counties$P_sur <- data

plot(counties)

st_write(counties, "P_Surplus_LEB_counties_US.shp")
