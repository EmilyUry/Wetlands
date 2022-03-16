

#' ---
#' title: "DU Data Exploration"
#' author: "Emily Ury"
#' last update: "March 16, 2022"
#' output: github_document
#' ---
#' 
#' Data exploration of 2 years of solute flux data
#' from 8 restored wetlands in Southern Ontario.

setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files/DUCs_SO_data")

library(ggplot2)
library(tidyverse)
library(viridis)

x <- read.csv("P_flux.csv", head = TRUE)

#######################

## data set-up

## merge multiple inflows for OH, KE, DY; and outflows for FE

table(x$Wetland_ID, x$Station)

x$Flow_volume_m3 <- as.numeric(x$Flow_volume_m3)

Tile <- x %>%
  group_by(Wetland_ID, Water_year) %>%
  filter(Station == "Inflow_tile") %>%
  summarise(Tile_Inflow = sum(Flow_volume_m3))



## Annual summary



