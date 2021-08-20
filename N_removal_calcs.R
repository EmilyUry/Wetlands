
#' # Wetland Nitrogen Removal Calculations

#' ### Last update: 2021-08-20

#Info:


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
#'  **K**  The removal rate constant
#'  
#'  **TAU**  Wetland residence time modeled from area [2]
#'  
#'  **CA**  Contributing Area (or catchment area)
#'  
#'  **N.IN**  Nitrogen input (from N surplus data) [3]
#'  
#'  **R** Nitrogen removal 
#'  
#'  **Rp** Removal percent
#'  
#'  **alpha**  catchment-area-to-wetland-area ratio of α [4]
#'  
#'  **gamma** proportion of N in a CA that reaches the wetland from lit [?]
#'  
#'  ## Conversions
#'  1 square kilometers (skm) = 100 hectare (Ha)
#'  
#' ## References
#'  
#'  [1] Cheng et al. (2020) https://doi.org/10.1038/s41586-020-03042-5
#'  
#'  [2] Cheng and Basu 2019
#'  
#'  [3] Byrnes et al. TREND data
#'  
#'  [4] Wu and Lane (2017)
#'  


#SetUp


setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files")

library(sf)
#library(lhc)

start_time <- Sys.time()   # check to see how long the code takes to run at the end
 
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
wetlands_m2 <- wetlands_skm*1000000
wetlands_ha <- wetlands_m2/10000
rm(wetlands, wetlands_select)

#' Also retrieve HUC8 area
watershed <- st_read("Wetland_shapefiles/HU8_04110001_Watershed/HU8_04110001_Watershed.shp")
watershed_skm <- watershed$AREASQKM #HUC8 watershed area in square km
watershed_ha <- watershed_skm*100
rm(watershed)



#' ## Part 2: N Surplus
#' Pull in N surplus data

data <- read.csv("N_surplus_toy.csv")
data$HUC_name <- ifelse(data$HUC8>9999999, paste("HU8_", data$HUC8, sep = ""), paste("HU8_0", data$HUC8, sep = ""))
N_SUR_kg_ha_yr <- data$Nsurplus_Kg_ha_yr[3]

# Calculate N surplus within the watershed of givin area
N.IN_watershed <- N_SUR_kg_ha_yr*watershed_ha*100  ## kg nitrogen entering the watershed each year




#' ## Part 3: Calculate coefficients

#' Pull in literature values
#' These will be replaced with ranges in the hypercube method

a <- 1.51
b <- 0.23
c <- 0.38
d <- -0.91
TAU <- a * wetlands_m2 ^ b  # coefficients from [2] eq. 13 (Table 4)
K   <- c * TAU ^ d  # coefficients from [2] figure 3A
alpha <- 0.1431  # from [4] the contributing area relative to wetland size
gamma <- 0.4  # catchment to area ratio

## calculate the removal potential of a wetland of a given size
Rp_wetland <- (1-exp(-K*TAU))  ## this is proportion of N that each wetland can remove based on size alone

## estimate the contributing area of a wetland of givent size
CA_ha  <- wetlands_ha / alpha    ## Contributing area for each wetland in Ha




#' ## Part 4: Calculate the N removal of the HUC
N.IN_wetland <- N_SUR_kg_ha_yr*CA_ha*gamma   ## kg per year
R_wetland_kg_yr <- N.IN_wetland * Rp_wetland  # this is the N removal of each wetlands in Kg
R_HUC_kg_yr <- sum(R_wetland_kg_yr) # total N removal within the givin huc
R_HUC_kg_ha_yr <- R_HUC_kg_yr/watershed_ha # total N removal within the givin huc per hectare

# and prportion N removal removed in the watershed
P_watershed <- sum(CA_ha * Rp_wetland)/watershed_ha




##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




#' ## Monte Carlo


#####  redo Part 3 + 4 in a Monte Carlo framework --
## using a range of estimates for each variable
## these are all found in Extended Data Table 2 [1]

a1 <- 1.48
a2 <- 1.62
b1 <- 0.21
b2 <- 0.25
c1 <- 0.31
c2 <- 0.45
d1 <- -0.86
d2 <- -0.7
alpha1 <- .03
alpha2 <- 0.2 
gamma1 <- 0.3  # catchment to area ratio
gamma2 <- 0.5

TAU <- a * wetlands_m2 ^ b  # coefficients from [2] eq. 13 (Table 4)
K   <- c * TAU ^ d  # coefficients from [2] figure 3A

num_sim <- 250
TAU_MC <- replicate(num_sim, 1*runif(1,a1, a2) * wetlands_m2 ^ runif(1, b1, b2))
K_MC <- 1*runif(1, c1, c2) * TAU_MC ^ runif(1,d1, d2)

R_MC <- 1-exp(-K_MC * TAU_MC)

CA_MC  <- replicate(num_sim,  wetlands_ha / runif(1, alpha1, alpha2))

N.IN_wetland_MC <- N_SUR_kg_ha_yr * CA_MC * runif(1, gamma1, gamma2)
R_wetland_kg_yr_MC <- N.IN_wetland_MC * R_MC # this is the N removal of each wetlands in Kg
R_HUC_kg_yr_MC <- colSums(R_wetland_kg_yr_MC) # total N removal within the givin huc
R_HUC_kg_ha_yr_MC <- R_HUC_kg_yr_MC/watershed_ha # total N removal within the givin huc per hectare


##summary stats
median(R_HUC_kg_ha_yr_MC)
mean(R_HUC_kg_ha_yr_MC)
quantile(R_HUC_kg_ha_yr_MC, 0.05)
quantile(R_HUC_kg_ha_yr_MC, 0.25)
quantile(R_HUC_kg_ha_yr_MC, 0.50)
quantile(R_HUC_kg_ha_yr_MC, 0.75)
quantile(R_HUC_kg_ha_yr_MC, 0.95)
mean(R_HUC_kg_ha_yr_MC) 
sd(R_HUC_kg_ha_yr_MC)

# and prportion N removal removed in the watershed
P_watershed <- colSums(CA_MC * R_MC)/watershed_ha
mean(P_watershed)

end_time <- Sys.time()
end_time - start_time

           