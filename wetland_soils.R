

## wetland soils exploration

library(soilDB)
library(ggplot2)
library(cowplot)
library(dplyr)
library(maps)

setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files/Soil_analysis_R")
#setwd("C:/Users/Emily Ury/Desktop/New folder")

d <- read.csv("Mi_wetland_soils_clip.csv") ## small region in Michigan to use to test analyses



LEB_MI <- c( 'MI093', 'MI163')
soils <- get_SDA_hydric(LEB_MI)
soils$HYDRIC_RATING <- as.factor(soils$HYDRIC_RATING)
hydric <- soils[which(soils$HYDRIC_RATING == "Hydric" |
                        soils$HYDRIC_RATING == "Predominantly Hydric"),]
mukey <- (hydric$mukey)
list <- paste(shQuote(mukey, type="sh"), collapse = ", ")


(unique(soils$musym))
s <- unique(d$MUSYM)
h <- unique(hydric$musym)

### assigning hydric rating to the wetland parcels' soils 
soils$MUSYM <- soils$musym
dd <- left_join(d, soils, "MUSYM")
dd$HYDRIC_RATING <- factor(dd$HYDRIC_RATING, levels = c("Nonhydric", 
                                                                    "Predominantly Nonhydric", "Partially Hydric", 
                                                                    "Predominantly Hydric", "Hydric"))
ggplot(dd, aes(x= HYDRIC_RATING)) +
  geom_bar() + xlab("") +
  theme(axis.text.x = element_text(size=8)) +
  ggtitle("Wetland soils by 'Hydric Rating'")
  scale_x_discrete(labels=c("Non-hydric", "Predominantly \n Non-hydric ", 
                            "Partially \nHydric", "Predominantly \n Hydric ", "Hydric")) 


### below code makes a series of barplots and inset map
### for the soils of the LEB

##what do the soils of the LEB look like

# List of the 35 county codes for Ohio counties in the Lake Erie Basin: 
LEB_OH <- c('OH003', 'OH005', 'OH007', 'OH011', 'OH033', 'OH035', 'OH039', 'OH043', 
            'OH051', 'OH055', 'OH063', 'OH065', 'OH069', 'OH077', 'OH085', 'OH093',
            'OH095', 'OH101', 'OH103', 'OH107', 'OH123', 'OH125', 'OH133', 'OH137',
            'OH143', 'OH149', 'OH151', 'OH153', 'OH155', 'OH161', 'OH171', 'OH173',
            'OH175', 'OH139', 'OH147')

# pull the list of soils in these 35 counties
OH_soils <- get_SDA_hydric(LEB_OH)
OH_soils$HYDRIC_RATING <- factor(OH_soils$HYDRIC_RATING, levels = c("Nonhydric", 
                                                           "Predominantly Nonhydric", "Partially Hydric", 
                                                           "Predominantly Hydric", "Hydric"))

LEB_NY <- c('NY003', 'NY009', 'NY013', 'NY029', 'NY037', 'NY121', 'NY605')
NY_soils <- get_SDA_hydric(LEB_NY)
NY_soils$HYDRIC_RATING <- factor(NY_soils$HYDRIC_RATING, levels = c("Nonhydric", 
                                                                    "Predominantly Nonhydric", "Partially Hydric", 
                                                                    "Predominantly Hydric", "Hydric"))

LEB_MI <- c('MI023', 'MI059', 'MI065', 'MI075', 'MI087', 'MI091', 'MI093', 'MI099',
            'MI115', 'MI125', 'MI147', 'MI151', 'MI161', 'MI163')
MI_soils <- get_SDA_hydric(LEB_MI)
MI_soils$HYDRIC_RATING <- factor(MI_soils$HYDRIC_RATING, levels = c("Nonhydric", 
                                                                    "Predominantly Nonhydric", "Partially Hydric", 
                                                                    "Predominantly Hydric", "Hydric"))
LEB_PA <- c('PA039', 'PA049')
PA_soils <- get_SDA_hydric(LEB_PA)
PA_soils$HYDRIC_RATING <- factor(PA_soils$HYDRIC_RATING, levels = c("Nonhydric", 
                                                                    "Predominantly Nonhydric", "Partially Hydric", 
                                                                    "Predominantly Hydric", "Hydric"))

LEB_IN <- c('IN001', 'IN003', 'IN033', 'IN113', 'IN151', 'IN179')
IN_soils <- get_SDA_hydric(LEB_IN)
IN_soils$HYDRIC_RATING <- factor(IN_soils$HYDRIC_RATING, levels = c("Nonhydric", 
                                                                    "Predominantly Nonhydric", "Partially Hydric", 
                                                                    "Predominantly Hydric", "Hydric"))



p1 <-  ggplot(OH_soils, aes(x= HYDRIC_RATING)) +
  geom_bar() + xlab("") +
  theme(axis.text.x = element_text(size=8)) +
  scale_x_discrete(labels=c("Non-hydric", "Predominantly \n Non-hydric ", 
                            "Partially \nHydric", "Predominantly \n Hydric ", "Hydric")) 
 
p2 <- ggplot(NY_soils, aes(x= HYDRIC_RATING)) +
  geom_bar() + xlab("") +
  theme(axis.text.x = element_text(size=8)) +
  scale_x_discrete(labels=c("Non-hydric", "Predominantly \n Non-hydric ", 
                            "Partially \nHydric", "Predominantly \n Hydric ", "Hydric"))
  
p3 <- ggplot(MI_soils, aes(x= HYDRIC_RATING)) +
  geom_bar() + xlab("") +
  theme(axis.text.x = element_text(size=8)) +
  scale_x_discrete(labels=c("Non-hydric", "Predominantly \n Non-hydric ", 
                            "Partially \nHydric", "Predominantly \n Hydric ", "Hydric")) 
p4 <- ggplot(PA_soils, aes(x= HYDRIC_RATING)) +
  geom_bar() + xlab("") + 
  theme(axis.text.x = element_text(size=8)) +
  scale_x_discrete(labels=c("Non-hydric", "Predominantly \n Non-hydric ", 
                            "Partially \nHydric", "Predominantly \n Hydric ", "Hydric"))
p5 <- ggplot(IN_soils, aes(x= HYDRIC_RATING)) +
  geom_bar() + xlab("") +
  theme(axis.text.x = element_text(size=8)) +
  scale_x_discrete(labels=c("Non-hydric", "Predominantly \n Non-hydric ", 
                            "Partially \nHydric", "Predominantly \n Hydric ", "Hydric"))

plots <- plot_grid(p3, map, p2, p5, p1, p4, ncol = 3, labels = c("MI", " ", "NY", "IN", "OH", "PA"))


title <- ggdraw() + 
  draw_label("Soils of the Lake Erie Basin (US) by State",
    fontface = 'bold', x = 0, hjust = 0) +
  theme(plot.margin = margin(0, 0, 0, 7))

plot_grid(title, plots, ncol = 1, rel_heights = c(0.1, 1))




#### inset map
library(rgdal)
library(ggspatial)
library(mapproj)

US <- map_data("state")
NAm <- map_data("world", region = c("Canada")) # Create basemap form GGplot
ny <- filter(US, region == "new york")
mi <- filter(US, region == "michigan")
pa <- filter(US, region == "pennsylvania")
oh <- filter(US, region == "ohio")
ind <- filter(US, region == "indiana")

wshed <- readOGR("LEB_watershed_84.shp")
wshed <- fortify(wshed)
lakes <- readOGR("GreatLakes.shp")
lakes <- fortify(lakes)

map <- ggplot(NAm, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "gray90", col = "black") +
  geom_polygon(data = US, fill = "gray95", col = "black") +
  geom_polygon(data = ny, fill="gray35", col = "black") +
  geom_polygon(data = oh, fill="gray35", col = "black") +
  geom_polygon(data = ind, fill="gray35", col = "black") +
  geom_polygon(data = mi, fill="gray35", col = "black") +
  geom_polygon(data = pa, fill="gray35", col = "black") +
  geom_polygon(data = lakes, fill = "darkblue") +
  geom_polygon(data = wshed, fill = "mediumspringgreen", alpha = 0.6) +
  theme_void() + 
  theme(panel.background = element_rect(fill = "darkblue")) +
  coord_cartesian(xlim = c(-87, -76), ylim = c(39, 46)) +
  #coord_cartesian(xlim = c(-90, -71), ylim = c(36, 49)) +
  theme(plot.margin = margin(5, 35, 45, 35),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

plots <- plot_grid(p3, map, p2, p5, p1, p4, ncol = 3, labels = c("MI", " ", "NY", "IN", "OH", "PA"))
plot_grid(title, plots, ncol = 1, rel_heights = c(0.1, 1))

tiff(filename = "SoilsMap.tiff", height=5200, width=9600, units= "px", res=800, compression= "lzw")
plot_grid(title, plots, ncol = 1, rel_heights = c(0.1, 1))
dev.off()

