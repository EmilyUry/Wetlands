


#' # P surplus calculations

#' ### Last update: 2021-09-28

#' This script is to calculate the P surplus layer for LEB
#' Input files include
#' P fertilizer input
#' P manure input
#' P crop uptake
#' 
#' Simple math balance calculation:
#'     P surplus = P fertilizer + P manure - P crop 
#'     
#' We will also use spatial averaging to convert the P surplus layer from a 
#' county level aggragation, to a watershed aggregation.
#' 
#' 