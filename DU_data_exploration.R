

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



