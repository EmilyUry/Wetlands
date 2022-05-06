


#' ---
#' title: "DU Data Exploration -- summarized weekly"
#' author: "Emily Ury"
#' last update: "March 30, 2022"
#' output: github_document
#' ---
#' 
#' Data exploration of 2 years of solute flux data
#' from 8 restored wetlands in Southern Ontario.

setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files/DUCs_SO_data")

library(ggplot2)
library(tidyverse)
library(viridis)
library(gridExtra)

x <- read.csv("DU_summary_select.csv", head = TRUE)

## data set-up


## merge multiple inflows and outflows for volume and solutes
##
###### inflows = Surface + Tile + Precip
###### outflows = Outflow + Outflow Leak (FE only)




### calcualte mass removal, percent removal and concentration (mg/L)
rem.calcs <- x %>% 
  rowwise() %>%
  mutate(flow.atten = sum(Qin, -Qout, na.rm = TRUE)) %>%
  mutate(flow.atten.percent = flow.atten/Qin*100) %>%
  mutate(TP.rem = sum(TPin, -TPout, na.rm = TRUE)) %>%
  mutate(TP.rem.percent = TP.rem/TPin*100) %>%
  mutate(SRP.rem = sum(SRPin, -SRPout, na.rm = TRUE)) %>%
  mutate(SRP.rem.percent = SRP.rem/SRPin*100) %>%
  mutate(cTPin = TPin/Qin*1000) %>%
  mutate(cTPout = TPout/Qout*1000) %>%
  mutate(cSRPin= SRPin/Qin*1000) %>%
  mutate(cSRPout = SRPout/Qout*1000)



#### add site info
aux.d <- read.csv("Wetland_Info.csv", head = TRUE)
names(aux.d) <- c("Wetland_ID", "Name", "Established", "Area", "Volume", "CA",
                  "CA_WA", "Crop18", "Crop19", "Crop20", "Crop21", "SOM", "SBP", "UBP")


### Weekly summaries
a <- 1:52
seq <- rep(a, each= 7)
Week <- rep(c(seq, 52), 16)

rem.calcs$Week <- Week

weekly.summary <- rem.calcs %>%
  group_by(Wetland_ID, Water_year, Week) %>%
  mutate(max.flow = max(Qin)) %>%
  group_by(Wetland_ID, Water_year, Week) %>%
  summarise(across(where(is.numeric), median, na.rm = TRUE))

weekly.summary <- weekly.summary  %>%
  left_join(aux.d, by = "Wetland_ID")

subset <- weekly.summary %>%
  filter(Wetland_ID == "OH" | Wetland_ID == "MA" |
           Wetland_ID == "KE" | Wetland_ID == "FE")


subset <- weekly.summary %>%
  filter(Water_year == 2021)


############ data visualization

ws <- subset
# ws["TP.rem.percent"][ws["TP.rem.percent"] == "-Inf"] <- NaN
# ws["SRP.rem.percent"][ws["SRP.rem.percent"] == "-Inf"] <- NaN

ws$behav <- ifelse(ws$TP.rem.percent < 0, "source", "sink")
ggplot(ws, aes(x = Week, y = TP.rem.percent, group = 1, color = behav)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values=c("#0000FF99", "#FF000099")) +
  facet_wrap(.~Wetland_ID, nrow = 4, scales = "free") +
  scale_x_continuous(" ", breaks = c(0, 10, 20, 30, 40, 50), 
                       label = c("Oct", "Dec", "Feb", "Apr", "Jun", "Aug"))+
  ylab("TP removal (%)")



ws$behav <- ifelse(ws$SRP.rem.percent < 0, "source", "sink")
ggplot(ws, aes(x = Week, y = SRP.rem.percent, group = 1, color = behav)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values=c("#0000FF99", "#FF000099")) +
  facet_wrap(.~Wetland_ID, nrow = 4, scales = "free") +
  scale_x_continuous(" ", breaks = c(0, 10, 20, 30, 40, 50), 
                     label = c("Oct", "Dec", "Feb", "Apr", "Jun", "Aug"))+
  ylab("SRP removal (%)")



#### mass removal
ws$behav <- ifelse(ws$TP.rem < 0, "source", "sink")
ggplot(ws, aes(x = Week, y = TP.rem, group = 1, color = behav)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values=c("#0000FF99", "#FF000099")) +
  facet_wrap(.~Wetland_ID, nrow = 4, scales = "free") +
  scale_x_continuous(" ", breaks = c(0, 10, 20, 30, 40, 50), 
                     label = c("Oct", "Dec", "Feb", "Apr", "Jun", "Aug"))+
  ylab("TP removal (kg/day)")



ws$behav <- ifelse(ws$SRP.rem < 0, "source", "sink")
ggplot(ws, aes(x = Week, y = SRP.rem, group = 1, color = behav)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values=c("#0000FF99", "#FF000099")) +
  facet_wrap(.~Wetland_ID, nrow = 4, scales = "free") +
  scale_x_continuous(" ", breaks = c(0, 10, 20, 30, 40, 50), 
                     label = c("Oct", "Dec", "Feb", "Apr", "Jun", "Aug"))+
  ylab("SRP removal (kg/day)")




### flow
ggplot(ws, aes(x = Week, y = Qin, group = 1, color = behav)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values=c("#55555599", "#55555599")) +
  facet_wrap(.~Wetland_ID, nrow = 4, scales = "free") +
  scale_x_continuous(" ", breaks = c(0, 10, 20, 30, 40, 50), 
                     label = c("Oct", "Dec", "Feb", "Apr", "Jun", "Aug"))+
  ylab("Inflow volume (m3/day)")







MA <- weekly.summary %>%
  filter(Wetland_ID == "MA")
MA$behav <- ifelse(MA$SRP.rem < 0, "source", "sink")
ggplot(MA, aes(x = (Qin), y = SRP.rem, group = 1, color = behav)) +
  geom_point() +
  scale_color_manual(values=c("#0000FF99", "#FF000099")) +
  ylab("SRP removal (kg/day)") +
  xlab("Q inflow")


ggplot(MA, aes(x = (cSRPin), y = SRP.rem, group = 1, color = behav)) +
  geom_point() +
  scale_color_manual(values=c("#0000FF99", "#FF000099")) +
  ylab("SRP removal (kg/day)") +
  xlab("Inflow SRP concentration (mg/L)")






ggplot(ws, aes(x = Week, y = SRP.rem.percent, group = 1)) +
  geom_line() +
  geom_point() +
  facet_wrap(Wetland_ID~Water_year, scales = "free") +
  scale_x_continuous(" ", breaks = c(0, 10, 20, 30, 40, 50), 
                     label = c("Oct", "Dec", "Feb", "Apr", "Jun", "Aug"))



























