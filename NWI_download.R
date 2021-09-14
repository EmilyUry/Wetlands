

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

#' Define a region of interest (there are 18 in the conterminus US)
ROI <- "05"  


#' Calls in an input file, the INDEX, which is a list of the HUC numbers in
#' Region of interest
DataWD <- "C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files/NWI_Wetlands_US/"
INDEX <- read.csv(paste(DataWD, 'Region_', ROI, '_key.csv', sep = ""), header = F)
INDEX <- INDEX[,1] # now index is a list instead of a dataframe


#' Write a function to download the file of interest
#' NOTE: for regions 10-18, you need to delete the leading zero in lines 33 and 35
Download_Wetlands <- function(INDEX) {
  download.file(paste('https://www.fws.gov/wetlands/downloads/Watershed/HU8_0',
                      INDEX, '_watershed.zip', sep = ""), 
                paste(DataWD, 'Region_', ROI,  '/HU8_0', INDEX, '_watershed.zip', sep = "")) 
}


#' run the function, over the entire INDEX list and 
#' note how long it takes to download 'n' watersheds
start_time <- Sys.time()

lapply(INDEX, Download_Wetlands)

end_time <- Sys.time()
end_time - start_time
### takes about 0.5 minutes * INDEX length






