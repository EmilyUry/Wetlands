
#' # Wetland Nitrogen Removal Calculations

#' ### Last update: 2021-08-20
#'
#' This script is based on the analysis by [1] Cheng et al. (2020) *Nature* 
#' The purpose is to estimate N removal by wetlands for a watershed (size
#' HUC8). The inputs include Nitrogen surplus value at the HUC8 scale
#' and wetland area for all wetlands within a given HUC8 watershed. 
#' Wetland size data is from NWI https://www.fws.gov/wetlands/index.html
#' and includes the following designations:
#'   - Freshwater pond
#'   - Freshwater emergent
#'   - Freshwater forested
#'   - Other
#' The following designations have been excluded:
#'   - Lake
#'   - Riverine
#'   - Estuarine Deepwater/wetland
#'   
#'   
#' ## Contents
#' 
#'  1. Pull in NWI wetland area information from shapefile
#'  2. Pull in N surplus for HUC8
#'  3. Calculate K, TAU, CA, and N.IN from wetland area and N surplus data
#'  4. Calculate N removal on a per HUC8 basis
#'  
#' ## Notes
#'  
#'  Steps 3 and 4 use a Monte Carlo approach with a Latin Hypercube sampling
#'  technique to include uncertainty around the variables K, TAU, CA and N.IN 
#'  This approach uses the uncertainty to generate a distribution of final
#'  N removal values from which we deliver the median and the 95% confidence
#'  interval. 
#'  
#' ## Index
#'  
#'  **K**  The constant ...
#'  
#'  **TAU**  Wetland residence time modeled from area [2]
#'  
#'  **CA**  Contributing Area (or catchment area)
#'  
#'  **N.IN**  Nitrogen input (from N surplus data) [3]
#'  
#'  **N.REM** Nitrogen removal value, kg_ha_yr
#'  
#' ## References
#'  
#'  [1] Cheng et al. (2020) https://doi.org/10.1038/s41586-020-03042-5
#'  
#'  [2] Cheng and Basu 2019
#'  
#'  [3] Byrnes et al. TREND data
#'  

setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files")

library(sf)
library(lhc)

 
#' ## Part 1: Wetland area
#' Pull in a shapefile of wetland area for one HUC8 watershed, 
#' Filter for wetland types of interest. 
#' Put out a string of wetland areas (in acres) of length n, where
#' n is the number of wetlands in the HUC

wetlands <- st_read("Wetland_shapefiles/HU8_04110001_Watershed/HU8_04110001_Wetlands.shp")
wetlands_select <- wetlands[which(wetlands$WETLAND_TY == "Freshwater Emergent Wetland" | 
                                    wetlands$WETLAND_TY == "Freshwater Forested/Shrub Wetland" |
                                    wetlands$WETLAND_TY == "Freshwater Pond" |
                                    wetlands$WETLAND_TY == "Other"),]
wetlands <- wetlands_select$ACRES  # area in acres
wetlands_skm <- wetlands/247.105   # wetland areas in square km
rm(wetlands, wetlands_select)

#' Also retrieve HUC8 area
watershed <- st_read("Wetland_shapefiles/HU8_04110001_Watershed/HU8_04110001_Watershed.shp")
watershed_skm <- watershed$AREASQKM  #HUC8 watershed area in square km
rm(watershed)


#' ## Part 2: N Surplus
#' Pull in N surplus data

data <- read.csv("N_surplus_toy.csv")
data$HUC_name <- ifelse(data$HUC8>9999999, paste("HU8_", data$HUC8, sep = ""), paste("HU8_0", data$HUC8, sep = ""))
N.IN <- data$Nsurplus_Kg_ha_yr[1]


           