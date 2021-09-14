

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


ROI <- "09"  # define a region of interest, there are 18 (19-21 are AK, HI and PR)

DataWD <- "C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files/NWI_Wetlands_US/"
INDEX <- read.csv(paste(DataWD, 'Region_', ROI, '_key.csv', sep = ""), header = F)
INDEX <- INDEX[,1] # now index is a list instead of a dataframe


## this calls in an input file, the INDEX, which is a list of the HUC numbers in
## the Region of Interest

## this works for the first watershed in the INDEX

HUC <- INDEX[1,1]

download.file(paste('https://www.fws.gov/wetlands/downloads/Watershed/HU8_0',
                    HUC, '_watershed.zip', sep = ""), 
              paste(DataWD, 'Region_09/HU8_0', HUC, '_watershed.zip', sep = ""))



Download_Wetlands <- function(INDEX) {
  download.file(paste('https://www.fws.gov/wetlands/downloads/Watershed/HU8_0',
                      INDEX, '_watershed.zip', sep = ""), 
                paste(DataWD, 'Region_09/HU8_0', INDEX, '_watershed.zip', sep = ""))
}


## run the function and note how long it takes to download 'n' watersheds
start_time <- Sys.time()

#Download_Wetlands(INDEX)  # this works for first in list INDEX
lapply(INDEX, Download_Wetlands)

end_time <- Sys.time()
end_time - start_time




