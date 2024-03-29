

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
library(gridExtra)

x <- read.csv("P_flux.csv", head = TRUE)

#######################

## data set-up
x$Water_year <- as.factor(x$Water_year)
x$Date <- as.Date(x$Date)
x$Wetland_ID <- ordered(x$Wetland_ID, levels = c("MO", "BL", "MA", "KE", "DY", "LL", "FE", "OH"))
## merge multiple inflows and outflows for volume and solutes
##
###### inflows = Surface + Tile + Precip
###### outflows = Outflow + Outflow Leak (FE only)

flows.combine <- x %>%
  pivot_wider(names_from = Station, 
              values_from = c(Flow_volume_m3, TP_kg_day, TDP_kg_day, 
                              SRP_kg_day, PP_kg_day)) %>%
  rowwise() %>%
  mutate(Precip = Flow_volume_m3_Precipitation) %>%
  mutate(VOL.IN = sum(Flow_volume_m3_Inflow_tile, Flow_volume_m3_Inflow_surface, 
                          Flow_volume_m3_Precipitation, na.rm = TRUE)) %>%
  mutate(VOL.OUT = sum(Flow_volume_m3_Outflow, Flow_volume_m3_Outflow_Leak, na.rm = TRUE)) %>%
  mutate(TP.IN = sum(TP_kg_day_Inflow_tile, TP_kg_day_Inflow_surface,
                         TP_kg_day_Precipitation, na.rm = TRUE)) %>%
  mutate(TP.OUT = sum(TP_kg_day_Outflow, TP_kg_day_Outflow_Leak, na.rm = TRUE)) %>%
  mutate(TDP.IN = sum(TDP_kg_day_Inflow_tile, TDP_kg_day_Inflow_surface,
                           TDP_kg_day_Precipitation, na.rm = TRUE)) %>%
  mutate(TDP.OUT = sum(TDP_kg_day_Outflow, TDP_kg_day_Outflow_Leak, na.rm = TRUE)) %>%  
  mutate(SRP.IN = sum(SRP_kg_day_Inflow_tile, SRP_kg_day_Inflow_surface,
                           SRP_kg_day_Precipitation, na.rm = TRUE)) %>%
  mutate(SRP.OUT = sum(SRP_kg_day_Outflow, SRP_kg_day_Outflow_Leak, na.rm = TRUE)) %>%
  mutate(PP.IN = sum(PP_kg_day_Inflow_tile, PP_kg_day_Inflow_surface,
                           PP_kg_day_Precipitation, na.rm = TRUE)) %>%
  mutate(PP.OUT = sum(PP_kg_day_Outflow, PP_kg_day_Outflow_Leak, na.rm = TRUE)) 


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



#### add site info
aux.d <- read.csv("Wetland_Info.csv", head = TRUE)
names(aux.d) <- c("Wetland_ID", "Name", "Established", "Area", "Volume", "CA",
                  "CA_WA", "Crop18", "Crop19", "Crop20", "Crop21", "SOM", "SBP", "UBP")
aux.d$Wetland_ID <- ordered(aux.d$Wetland_ID, levels = c("MO", "BL", "MA", "KE", "DY", "LL", "FE", "OH"))

data <- rem.calcs  %>%
  left_join(aux.d, by = "Wetland_ID")

subset <- data %>%
  filter(Wetland_ID == "OH" | Wetland_ID == "MA" |
           Wetland_ID == "KE" | Wetland_ID == "FE")

Y1 <- data[which(data$Water_year == "2019"),]
Y2 <- data[which(data$Water_year == "2021"),]


## data visualization


ggplot(Y1, aes(Date, TP.IN)) +
  geom_point(aes(color = "TP in"), size = 1) +
  geom_point(aes(Date, TP.OUT, color = "TP out"), size = 1)+
  scale_color_manual(values = c("#0000FF77", "#FF000066")) +
  facet_wrap(.~Wetland_ID, scales = "free" ) +
  scale_x_date(name = "2019", date_breaks = "1 month",
               date_labels = c("S", "O", "N","D", "J", "F", "M","A", "M","J", "J", "A"))


OH <- Y1[which(Y1$Wetland_ID == "OH"),]
ggplot(OH, aes(Date, TP.IN)) +
  geom_point(aes(color = "TP in"), size = 1) +
  geom_point(aes(Date, TP.OUT, color = "TP out"), size = 1)+
  scale_color_manual(values = c("#0000FF77", "#FF000066")) +
  ylab("TP (kg/day)") 

we <- Y1[which(Y1$Wetland_ID == "OH"),]
source.days <- nrow(we[which(we$SRP.ret < 0),])
source.days
source.days/365*100




ggplot(subset, aes(log(VOL.IN), TP.rem)) +
  geom_point() +
  #ylim(-300, 100) +
  facet_wrap(.~Wetland_ID)
  

rbPal <- colorRampPalette(c('red','blue'))
subset$col <- rbPal(2)[as.numeric(cut(subset$TP.rem, breaks = c(-Inf, 0, Inf)))]

subset$behav <- ifelse(subset$TP.rem < 0, "source", "sink")

ggplot(subset, aes(log(VOL.IN), TP.rem, color = behav)) + 
  scale_color_manual(values=c("#0000FF55", "#FF000055"))+
  geom_point() +
  #ylim(-300, 100) +
  facet_wrap(.~Wetland_ID)




ggplot(subset, aes(log(VOL.IN), log(TP.conc.IN), color = behav)) + 
  scale_color_manual(values=c("#0000FF55", "#FF000055"))+
  geom_point() +
  #ylim(-300, 100) +
  facet_wrap(.~Wetland_ID) +
  xlab("log(Q)") +
  ylab("log(C)TP")


subset$behav <- ifelse(subset$SRP.rem < 0, "source", "sink")

ggplot(subset, aes(log(VOL.IN), log(SRP.conc.IN), color = behav)) + 
  scale_color_manual(values=c("#0000FF55", "#FF000055"))+
  geom_point() +
  #ylim(-300, 100) +
  facet_wrap(.~Wetland_ID) +
  xlab("log(Q)") +
  ylab("log(C)SRP")





ggplot(subset, aes(SRP.rem.percent, TP.rem.percent, color = behav)) + 
  scale_color_manual(values=c("#0000FF55", "#FF000055"))+
  geom_point() +
  ylim(-300, 100) +
  xlim(-250,100) +
  facet_wrap(.~Wetland_ID) +
  geom_abline(slope = 1, intercept = 1)




ggplot(subset, aes(log(VOL.IN), log(TP.conc.OUT), color = behav)) + 
  scale_color_manual(values=c("#0000FF55", "#FF000055"))+
  geom_point() +
  #ylim(-300, 100) +
  facet_wrap(.~Wetland_ID) +
  xlab("log(Q)") +
  ylab("log(C)TP")




## Retention ~ rainfall
data$Precip[is.na(data$Precip)] <- 0
plot(data$Precip, data$TP.rem)

## n days of preceding rain
n <- 60
cs <- cumsum(data$Precip)
data$rain.sum <- c(rep_len(NA, n-1), tail(cs, -(n-1)) - c(0, head(cs, -n)))
plot(data$Precip, data$rain.sum)

rbPal <- colorRampPalette(c('red','blue'))
data$col <- rbPal(3)[as.numeric(cut(data$TP.rem.percent, breaks = c(-Inf, 0, 20, 500)))]
plot(data$Precip, data$rain.sum, pch = 16, cex = 0.5, col = data$col)
we <- data[which(data$Wetland_ID == "OH"),]
plot(we$Precip, we$rain.sum, pch = 16, cex = 0.5, col = we$col)



summary(data$TP.rem.percent)


plot(data$rain.sum, data$TP.OUT)
fit <- lm(flows.combine$TP.ret ~ flows.combine$rain.sum)
abline(fit)
summary(fit)
plot(data$VOL.OUT, data$SRP.OUT)



ggplot(Y1, aes(Date, TP.IN)) +
  geom_point(aes(color = "TP in"), size = 1) +
  geom_point(aes(Date, TP.OUT, color = "TP out"), size = 1)+
  scale_color_manual(values = c("#0000FF77", "#FF000066")) +
  facet_wrap(.~Wetland_ID, scales = "free" ) +
  scale_x_date(name = "2019", date_breaks = "1 month",
               date_labels = c("S", "O", "N","D", "J", "F", "M","A", "M","J", "J", "A"))














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





