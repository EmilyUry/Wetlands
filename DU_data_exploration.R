

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

## merge multiple inflows and outflows for volume and solutes
##
###### inflows = Surface + Tile + Precip
###### outflows = Outflow + Outflow Leak (FE only)

flows.combine <- x %>%
  pivot_wider(names_from = Station, 
              values_from = c(Flow_volume_m3, TP_kg_day, TDP_kg_day, 
                              SRP_kg_day, PP_kg_day)) %>%
  rowwise() %>%
  mutate(VOL.IN = sum(Flow_volume_m3_Inflow_tile, Flow_volume_m3_Inflow_surface, 
                          Flow_volume_m3_Precipitation, na.rm = TRUE)) %>%
  mutate(VOL.OUT = sum(Flow_volume_m3_Outflow, Flow_volume_m3_Outflow_Leak, na.rm = TRUE)) %>%
  mutate(TP.IN = sum(TP_kg_day_Inflow_tile, TP_kg_day_Inflow_tile,
                         TP_kg_day_Precipitation, na.rm = TRUE)) %>%
  mutate(TP.OUT = sum(TP_kg_day_Outflow, TP_kg_day_Outflow_Leak, na.rm = TRUE)) %>%
  mutate(TDP.IN = sum(TDP_kg_day_Inflow_tile, TDP_kg_day_Inflow_tile,
                           TDP_kg_day_Precipitation, na.rm = TRUE)) %>%
  mutate(TDP.OUT = sum(TDP_kg_day_Outflow, TDP_kg_day_Outflow_Leak, na.rm = TRUE)) %>%  
  mutate(SRP.IN = sum(SRP_kg_day_Inflow_tile, SRP_kg_day_Inflow_tile,
                           SRP_kg_day_Precipitation, na.rm = TRUE)) %>%
  mutate(SRP.OUT = sum(SRP_kg_day_Outflow, SRP_kg_day_Outflow_Leak, na.rm = TRUE)) %>%
  mutate(PP.IN = sum(PP_kg_day_Inflow_tile, PP_kg_day_Inflow_tile,
                           PP_kg_day_Precipitation, na.rm = TRUE)) %>%
  mutate(PP.OUT = sum(PP_kg_day_Outflow, PP_kg_day_Outflow_Leak, na.rm = TRUE)) %>%
  select(Wetland_ID, Date, Water_year, Month, Day, VOL.IN, VOL.OUT, TP.IN, TP.OUT,
         TDP.IN, TDP.OUT, SRP.IN, SRP.OUT, PP.IN, PP.OUT) 


### calcualte mass removal, percent removal and concentration (mg/L)
rem.calcs <- flows.combine %>% 
  rowwise() %>%
  mutate(flow.atten = sum(VOL.IN, -VOL.OUT, na.rm = TRUE)) %>%
  mutate(flow.atten.percent = flow.atten/VOL.IN*100) %>%
  mutate(TP.rem = sum(TP.IN, -TP.OUT, na.rm = TRUE)) %>%
  mutate(TP.rem.percent = TP.rem/TP.IN*100) %>%
  mutate(TDP.rem = sum(TDP.IN, -TDP.OUT, na.rm = TRUE)) %>%
  mutate(TDP.rem.percent = TDP.rem/TDP.IN*100) %>%
  mutate(SRP.rem = sum(SRP.IN, -SRP.OUT, na.rm = TRUE)) %>%
  mutate(SRP.rem.percent = SRP.rem/SRP.IN*100) %>%
  mutate(PP.rem = sum(PP.IN, -PP.OUT, na.rm = TRUE)) %>%
  mutate(PP.rem.percent = PP.rem/PP.IN*100) %>%
  mutate(TP.conc.IN = TP.IN/VOL.IN*1000) %>%
  mutate(TP.conc.OUT = TP.OUT/VOL.OUT*1000) %>%
  mutate(TDP.conc.IN = TDP.IN/VOL.IN*1000) %>%
  mutate(TDP.conc.OUT = TDP.OUT/VOL.OUT*1000) %>%
  mutate(SRP.conc.IN = SRP.IN/VOL.IN*1000) %>%
  mutate(SRP.conc.OUT = SRP.OUT/VOL.OUT*1000) %>%
  mutate(PP.conc.IN = PP.IN/VOL.IN*1000) %>%
  mutate(PP.conc.OUT = PP.OUT/VOL.OUT*1000) 



### Annual summaries

rem.calcs[rem.calcs == -Inf] <- NaN
rem.calcs[rem.calcs == Inf] <- NaN

annual.summary <- rem.calcs %>%
  group_by(Wetland_ID, Water_year) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

### Seasonal summaries


### Fall = Oct, Nov, Dec; Winter = Jan, Feb, Mar; Spring = Apr, May, Jun; Summer = Jul, Aug, Sep
Month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
           "Sep", "Oct", "Nov", "Dec")
Season <- c("Winter", "Winter", "Winter", "Spring", "Spring", "Spring", 
            "Summer", "Summer", "Summer", "Fall", "Fall", "Fall")
key1 <- data.frame(Month, Season); 
seasonal.summary1 <- rem.calcs %>%
  left_join(key1, by = "Month") %>%
  group_by(Wetland_ID, Water_year, Season) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

  
### Seasons 2 -- the way DUCS does it in the report
### Fall = Oct, Nov; Winter = Dec, Jan; Freshet = Feb, Mar, Apr, May; Summer = Jun, Jul, Aug, Sep
Season <- c("Winter", "Freshet", "Freshet", "Freshet", "Freshet", "Summer", 
              "Summer", "Summer", "Summer", "Fall", "Fall", "Winter")
key2 <- data.frame(Month, Season); rm(Month,Season)
seasonal.summary2 <- rem.calcs %>%
  left_join(key2, by = "Month") %>%
  group_by(Wetland_ID, Water_year, Season) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))
  
rm(key1, key2)

### Monthly summaries

test <- rem.calcs %>%
  group_by(Wetland_ID, Water_year, Month) %>%
  mutate(max.flow = max(VOL.IN))

monthly.summary <- test %>%
  group_by(Wetland_ID, Water_year, Month) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
    mutate(Month = fct_relevel(Month, 
                            "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr",
                            "May", "Jun", "Jul", "Aug", "Sep", )) 



#### data visualization
library(gridExtra)

OH <- monthly.summary[which(monthly.summary$Wetland_ID == "OH"),]
plot(OH$Month, OH$TP.IN)


flow <- ggplot(OH, aes(x = Month, y = VOL.IN)) +
  geom_bar(stat = "identity") + 
  facet_wrap(~Water_year)

TP.in <- ggplot(OH, aes(x = Month, y = TP.IN)) +
  geom_bar(stat = "identity") + 
  facet_wrap(~Water_year)

TP.out <- ggplot(OH, aes(x = Month, y = TP.OUT)) +
  geom_bar(stat = "identity") + 
  facet_wrap(~Water_year)

TP.ret <- ggplot(OH, aes(x = Month, y = TP.rem.percent)) +
  geom_bar(stat = "identity") + 
  facet_wrap(~Water_year) +
  coord_cartesian(ylim = c(-250,100), clip = "off")


grid.arrange(flow, TP.in, TP.out, TP.ret, nrow = 4) 




plot(OH$max.flow, OH$TP.rem.percent)


ggplot(monthly.summary, aes(x = VOL.IN, y = SRP.rem.percent)) +
  geom_point() +
  geom_smooth(method = lm) +
  facet_wrap(~Wetland_ID, scales = "free")


ggplot(monthly.summary, aes(x = max.flow, y = SRP.rem.percent)) +
  geom_point() +
  geom_smooth(method = lm) +
  facet_wrap(~Wetland_ID, scales = "free")









#######
## Junk Code


rem.calcs["TP.rem.percent"][rem.calcs["TP.rem.percent"] == "-Inf"] <- -99
rem.calcs["flow.atten.percent"][rem.calcs["flow.atten.percent"] == "-Inf"] <- -99

test <- rem.calcs[which(rem.calcs$flow.atten.percent == "NaN"),]

hist <- ggplot(rem.calcs, aes(TP.rem.percent)) +
  geom_histogram(bins = 50) +
  xlim(-100,100) +
  facet_wrap(~Wetland_ID)

hist <- ggplot(rem.calcs, aes(flow.atten.percent)) +
  geom_histogram(bins = 50) +
  xlim(-100,100) +
  facet_wrap(~Wetland_ID)

hist


hist <- ggplot(rem.calcs, aes(PP.conc.IN)) +
  geom_histogram(bins = 50) +
  xlim(0,0.2) +
  facet_wrap(~Wetland_ID)
hist

ggplot(rem.calcs, aes(x = Wetland_ID, y = PP.conc.IN)) +
  geom_boxplot()

ggplot(rem.calcs, aes(x = Wetland_ID, y = PP.IN)) +
  geom_boxplot() +
  ylim(0,0.05)

