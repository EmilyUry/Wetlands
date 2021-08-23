
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
#'  1. Pull in N surplus for each HUC8
#'  2. Pull in NWI wetland area information from shapefile
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


#' ## STEP 1: Read in Nitrogen input data
#' This step is also used to generate an index of all HUCs in the data set

data <- read.csv("N_surplus_toy.csv")
data$HUC_name <- ifelse(data$HUC8>9999999, paste("HU8_", data$HUC8, sep = ""), paste("HU8_0", data$HUC8, sep = ""))
## HUC_name is the INDEX

### For 1 run purpose <- eventually replace this line with an apply function or loop
#
###
#
INDEX <- data$HUC_name
#
###
#

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#####  Coefficient ranges for use in Monte Carlo framework --
## using a range of estimates for each variable
## these are all found in Extended Data Table 2 [1]

a1 <- 1.48    # coefficients from [2] eq. 13 (Table 4)
a2 <- 1.62
b1 <- 0.21
b2 <- 0.25
c1 <- 0.31     # coefficients from [2] figure 3A
c2 <- 0.45
d1 <- -0.86
d2 <- -0.7
alpha1 <- .03
alpha2 <- 0.2 
gamma1 <- 0.3  # catchment to area ratio
gamma2 <- 0.5
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



RemovalsPerHUC8 <- function(INDEX, data){
  
  N_SUR_kg_ha_yr <- data$Nsurplus_Kg_ha_yr[which(data$HUC_name == INDEX)]
  
  
  #' ## STEP 2: Read in Wetland and watershed shapefiles using index created in STEP 1
  #' This step also generates a list of wetland areas and finds the area of the entire watershed
  
  DataWD <- "C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_shapefiles/"
  
  wetlands <- st_read(paste(DataWD, INDEX, '_Watershed/', INDEX, '_wetlands.shp', sep = ""))
  
  ## Filter by wetland type
  wetlands_select <- wetlands[which(wetlands$WETLAND_TY == "Freshwater Emergent Wetland" | 
                                      wetlands$WETLAND_TY == "Freshwater Forested/Shrub Wetland" |
                                      wetlands$WETLAND_TY == "Freshwater Pond" |
                                      wetlands$WETLAND_TY == "Other"),]
  wetlands <- wetlands_select$ACRES  # area in acres
  wetlands_m2 <- wetlands/247.105*1000000   # wetland areas in m2
  wetlands_ha <- wetlands_m2/10000 # wetland areas in m2
  rm(wetlands, wetlands_select)
  
  #' Also retrieve HUC8 area
  watershed <- st_read(paste(DataWD, INDEX, '_Watershed/', INDEX, '_watershed.shp', sep = ""))
  
  watershed_ha <- watershed$AREASQKM*100 #HUC8 watershed area in square km _> hectare
  
  # Calculate N surplus within the watershed of givin area
  N.IN_watershed <- N_SUR_kg_ha_yr*watershed_ha  ## kg nitrogen entering the watershed each year
  
  
  
  #' STEP 3 + 4: Determine K and T and N removal using n = 250 Monte Carlo simulations
  
  num_sim =250
  wetlands_HUC <- as.data.frame(wetlands_m2)
  
  TAU_MC <- replicate(num_sim, apply(wetlands_HUC, 1, function(x) runif(1, a1, a2) 
                                     * x[1] ^ runif(1, b1 , b2)))
  
  K_MC_ex <- replicate(num_sim, runif(length(wetlands_m2), d1, d2))
  K_MC_const <-  replicate(num_sim, runif(length(wetlands_m2), c1, c2))
  K_MC <- K_MC_const*(TAU_MC ^ K_MC_ex)
  
  R_MC <- 1-exp(-1*(TAU_MC * K_MC))
  
  remove(TAU_MC, K_MC, K_MC_ex, K_MC_const)
  
  CA_MC  <- replicate(num_sim,  wetlands_ha / runif(1, alpha1, alpha2))
  
  ## comment this
  CA_RATIO <- colSums(CA_MC) / watershed_ha
  CA_RATIO[CA_RATIO<1] <- 1  
  CA_MC <- CA_MC / CA_RATIO
  
  gamma_matrix <- replicate(num_sim, runif(length(wetlands_m2), gamma1, gamma2))
  N.IN_wetland_MC <- (N_SUR_kg_ha_yr * CA_MC * gamma_matrix)
  
  R_wetland_kg_yr_MC <- N.IN_wetland_MC * R_MC # this is the N removal of each wetlands in Kg
  R_HUC_kg_yr_MC <- colSums(R_wetland_kg_yr_MC) # total N removal within the givin huc (kg/yr)
  R_HUC_kg_ha_yr_MC <- R_HUC_kg_yr_MC/watershed_ha # total N removal within the givin huc per hectare
  
  WeightedRp <- colSums(CA_MC*R_MC) / watershed_ha  ## weighting each wetland by contributing area
  
  #NSUR_to_wet_kg_MC <-  CA_MC * runif(1, 0.4,0.6) * watershed_ha/10000
  
  #Ra_HUC <- colSums(R_MC * N.IN_wetland_MC)
  
  OUTPUT <- data.frame(INDEX, NumWet = nrow(wetlands_HUC), 
                       sum_wetSA = sum(wetlands_HUC[1]), sum_CA = median(colSums(CA_MC)),
                       Rp5  = quantile(WeightedRp, 0.05),
                       Rp25 = quantile(WeightedRp, 0.25),
                       Rp50 = quantile(WeightedRp, 0.50),
                       Rp75 = quantile(WeightedRp, 0.75),
                       Rp95 = quantile(WeightedRp, 0.95),
                       MeanRp = mean(WeightedRp), 
                       sdRp = sd(WeightedRp),
                       Ra5  = quantile(R_HUC_kg_yr_MC, 0.05),
                       Ra25 = quantile(R_HUC_kg_yr_MC, 0.25),
                       Ra50 = quantile(R_HUC_kg_yr_MC, 0.50),
                       Ra75 = quantile(R_HUC_kg_yr_MC, 0.75),
                       Ra95 = quantile(R_HUC_kg_yr_MC, 0.95),
                       MeanRa = mean(R_HUC_kg_yr_MC), 
                       sdRa = sd(R_HUC_kg_yr_MC))
  
  }

start_time <- Sys.time()
HUC8_Removals <- do.call('rbind', lapply(INDEX, FUN =  RemovalsPerHUC8, data = data))
end_time <- Sys.time()
end_time - start_time

           