

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
#'  3. Calculate K, TAU, CA, and N.IN from wetland area and P surplus data
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