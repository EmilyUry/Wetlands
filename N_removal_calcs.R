


##########################################################################
#
#        Wetland Nitrogen Removal Calculations
#
##########################################################################

#' This script is based on the analysis by F.Cheng et al. (2020) *Nature* 
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