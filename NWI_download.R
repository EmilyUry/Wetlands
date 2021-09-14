

#' Wetland download script
#' 
#' This script is for downloading the National Wetland Inventory data for wetland
#' polygons. 
#' 
#' This script requires an input file of a list of the names of all the target 
#' watersheds (HUC8). 
#' 
#' We will use region 09 (MN and ND) as an example. There are 46 HUC8 watersheds
#' in region nine beginning with 09010002 and ending with 09040002.
#' 
##########################################################################################


setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files/NWI_Wetlands_US")


ROI <- "09"

DataWD <- "C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files/NWI_Wetlands_US/"
input <- read.csv(paste(DataWD, 'Region_', ROI, '_key.csv', sep = ""), header = F)

