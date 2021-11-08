

#' ## P_removal_calcs


#' # Wetland Nitrogen Removal Calculations

#' ### Last update: 2021-11-04

#' This script is based on the analysis by [1] Cheng et al. (2020) *Nature* 
#' The purpose is to estimate P removal by wetlands for a watershed (size
#' HUC8). The inputs include P surplus value at the HUC8 scale [3]
#' and wetland area for all wetlands within a given HUC8 watershed. 
#' Wetland area data is from NWI https://www.fws.gov/wetlands/index.html
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
#' ## Contents
#' 
#'  1. Pull in P surplus for each HUC8
#'  2. Pull in NWI wetland area information from shapefile
#'  3. Calculate C and P.IN from wetland area and P surplus data
#'  4. Calculate K an TAU
#'  4. Calculate P removal on a per HUC8 basis
#'  
#' ## Notes
#'  
#'  Steps 3 and 4 use a Monte Carlo approach
#'  to include uncertainty around the variables K, TAU, CA and N.IN 
#'  This approach uses the uncertainty to generate a distribution of final
#'  P removal values from which we deliver the median and confidence interval.

#' ## Index
#'  
#'  **K**  Removal rate constant, modeled from TAU and coefficients c and d [2]
#'  
#'  **TAU**  Residence time modeled from area and coefficients a and b [2]
#'  
#'  **CA**  Contributing Area (or catchment area) [1]
#'  
#'  **N.IN**  Nitrogen input (from N surplus data) [3]
#'  
#'  **R** Nitrogen removal  - calculated here from K and TAU
#'  
#'  **Rp** Removal percent based on the N.IN
#'  
#'  **alpha**  approximation ofcatchment-area-to-wetland-area ratio [4]
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


# SetUp ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files")
library(sf)

#####  Define coefficient ranges for use in Monte Carlo framework --
## using a range of estimates for each variable
## these are all found in Extended Data Table 2 [1]

alpha1 <- .03  # this is the wetland to contributing area ratio [4]
alpha2 <- 0.2 
gamma1 <- 0.3  # reduction factor: fraction of P surplus that enters wetland
gamma2 <- 0.5

a1 <- 1.48    # coefficients from [2] eq. 13 (Table 4)
a2 <- 1.62
b1 <- 0.21
b2 <- 0.25
c1 <- 0.31     # coefficients from [2] figure 3A
c2 <- 0.45
d1 <- -0.86
d2 <- -0.7

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#' ## STEP 1: Read in Nitrogen input data
#' This step is also used to generate an index of all HUCs in the data set

data <- read.csv("N_surplus_LEB-US.csv")
data$HUC_name <- ifelse(data$HUC8>9999999, paste("HU8_", data$HUC8, sep = ""),
                        paste("HU8_0", data$HUC8, sep = ""))
## we will use HUC_name as a unique index for each watershed

INDEX <- data$HUC_name   # list of all the watersheds in the dataset

#INDEX <- data$HUC_name[1]  # to run the code for just the first HUC in the list


#' The function `RemovalsPerHUC8` is used to calculate the N removal of each
#' wetland within a HUC8 watershed, based on its size, which can then be 
#' aggregated over the entire watershed:
RemovalsPerHUC8 <- function(INDEX, data){
  
  # Call in the N surplus value for the HUC
  N_SUR_kg_ha_yr <- data$Nsurplus_Kg_ha_yr[which(data$HUC_name == INDEX)]
  
  
  
  #' ## STEP 2: Read in Wetland and watershed shapefiles
  #' These shapefiles are located in a file directory organized by HUC8
  #' Each HUC8 has its own folder containing shapefiles for wetlands and 
  #' watershed boundary. The containing folder has the HUC8 number identifier 
  #' in the name which matches the INDEX created in step 1.
  #' This step also generates a list of wetland areas and 
  #' finds the area of the entire watershed
  
  DataWD <- "C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_shapefiles/"
  
  wetlands <- st_read(paste(DataWD, INDEX, '_Watershed/', 
                            INDEX, '_wetlands.shp', sep = ""))
  
  ## Filter by wetland type
  wetlands_select <- wetlands[which(
    wetlands$WETLAND_TY == "Freshwater Emergent Wetland" | 
      wetlands$WETLAND_TY == "Freshwater Forested/Shrub Wetland" |
      wetlands$WETLAND_TY == "Freshwater Pond" |
      wetlands$WETLAND_TY == "Other"),]
  wetlands_m2 <- wetlands_select$ACRES/247.105*1000000  # wetland areas in m2
  wetlands_ha <- wetlands_m2/10000 # wetland areas in m2
  rm(wetlands, wetlands_select)
  
  #' Also retrieve HUC8 area
  watershed <- st_read(paste(DataWD, INDEX, '_Watershed/', INDEX, '_watershed.shp', sep = ""))
  watershed_ha <- watershed$AREASQKM*100 #HUC8 watershed area in square km _> hectare
  
  # Calculate N surplus within the entire watershed
  N.IN_watershed <- N_SUR_kg_ha_yr*watershed_ha  ## kg N entering watershed each yr
  
  
  
  #' STEP 3: Calculate CA and P.IN
  
  num_sim <- 250  # number of Monte Carlo simulations to run
  
  # Determine the approximate contributing area of each wetland (in hectares)
  CA_MC  <- replicate(num_sim,  wetlands_ha / runif(1, alpha1, alpha2)) 
  
  ## This code is a normalization of the Contributing Area that will rescale it
  ## if, and only if, total CA exceeds the area of the total watershed
  CA_RATIO <- colSums(CA_MC) / watershed_ha
  CA_RATIO[CA_RATIO<1] <- 1  
  CA_MC <- CA_MC / CA_RATIO
  
  ## Gamma, "reduction factor", proportion of N within the CA that enters the wetland
  ## Gamma is an approximation drawn from the literature and accounts for things like 
  ## N that may have been denitrified upstream of the wetland or retained in soils
  gamma_matrix <- replicate(num_sim, runif(length(wetlands_m2), gamma1, gamma2))
  
  # Nitrogen in kg/yr that enters each wetland
  N.IN_wetland_MC <- (N_SUR_kg_ha_yr * CA_MC * gamma_matrix)
  

  
  #' STEP 4: Determine K and TAU using monte carlo

  wetlands_HUC <- as.data.frame(wetlands_m2)
  
  # Generate 250 Tau values for each wetland in the watershed based on random 
  # generation of coefficients within excepted ranges
  TAU_MC <- replicate(num_sim, apply(wetlands_HUC, 1, function(x) runif(1, a1, a2) 
                                     * x[1] ^ runif(1, b1 , b2)))
  # Similarly generate 250 random coefficient sets for K, use to generate K from Tau
  K_MC_ex <- replicate(num_sim, runif(length(wetlands_m2), d1, d2))
  K_MC_const <-  replicate(num_sim, runif(length(wetlands_m2), c1, c2))
  K_MC <- K_MC_const*(TAU_MC ^ K_MC_ex)
  
  
  
  #' STEP 5: Calculate P removal 

  R_MC <- 1-exp(-1*(TAU_MC * K_MC))  # Proportion of N removal - Eq.2 [1]
  
  remove(TAU_MC, K_MC, K_MC_ex, K_MC_const)
  
  R_wetland_kg_yr_MC <- N.IN_wetland_MC * R_MC # this is the N removal of each wetlands in Kg
  R_HUC_kg_yr_MC <- colSums(R_wetland_kg_yr_MC) # total N removal within the givin huc (kg/yr)
  R_HUC_kg_ha_yr_MC <- R_HUC_kg_yr_MC/watershed_ha # total N removal within the huc per hectare
  
  WeightedRp <- colSums(CA_MC*R_MC) / watershed_ha  ## weighting each wetland by CA
  
  ## summary statistics generated from the 250 simulations
  ## for both `WeightedRp`, the proportion of N removed by wetlands
  ## and `R_HUC_Kg_yr_MC`, the total mass N removed per hectare
  OUTPUT <- data.frame(INDEX, NumWet = nrow(wetlands_HUC), 
                       sum_wetSA = sum(wetlands_HUC[1]), sum_CA = median(colSums(CA_MC)),
                       Rp5  = quantile(WeightedRp, 0.05),
                       Rp25 = quantile(WeightedRp, 0.25),
                       Rp50 = quantile(WeightedRp, 0.50),
                       Rp75 = quantile(WeightedRp, 0.75),
                       Rp95 = quantile(WeightedRp, 0.95),
                       MeanRp = mean(WeightedRp), 
                       sdRp = sd(WeightedRp),
                       Ra5  = quantile(R_HUC_kg_ha_yr_MC, 0.05),
                       Ra25 = quantile(R_HUC_kg_ha_yr_MC, 0.25),
                       Ra50 = quantile(R_HUC_kg_ha_yr_MC, 0.50),
                       Ra75 = quantile(R_HUC_kg_ha_yr_MC, 0.75),
                       Ra95 = quantile(R_HUC_kg_ha_yr_MC, 0.95),
                       MeanRa = mean(R_HUC_kg_ha_yr_MC), 
                       sdRa = sd(R_HUC_kg_ha_yr_MC))
}

#' Run function and generate summary statistics for the 
#' 24 watersheds on the US side of the LE basin
#' Track time elapsed for this function to run -- should be ~ 8 mins.
start_time <- Sys.time()
HUC8_Removals <- do.call('rbind', lapply(INDEX, FUN =  RemovalsPerHUC8, data = data))
end_time <- Sys.time()
end_time - start_time








