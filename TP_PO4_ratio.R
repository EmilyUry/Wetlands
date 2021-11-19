



### Wetland P Removal
## TP:PO4


#' This script is used to analyze wetland retention of Phosphorus,
#' specifically the different forms of P in the inflow and outflow.
#' We are curious about the transformations of P occuring in wetlands,
#' also the bioavailability of the effluent P is an important factor 
#' for influencing algal blooms downstream
#'
#' Here we are using data from a meta-analysis put together by Fred
#' Eventually we will update the data to use info from a lit compilation
#' I put together myself. 
#' 


setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files")


library(viridis)
library(dplyr)
library(tidyr)


## Fred's meta-analysis of literature values
x <- read.csv("P_speciation2.csv", header = T)


## bin data by wetland size into 5 roughly evenly sized bins
breaks  <- c(0,30, 3000, 15000, 100000, 10000000000)
#breaks <- c(-1, 100, 1000, 10000, 100000, 100000000000)
tags <- c("0-30","30-3k", "3k-15k", "15k-100k", "100k+")
#tags <- c("0-100","100-1k", "1k-10k", "10k-100k", "100k+")
group_tags <- cut(x$area,
                  breaks = breaks,
                  include.lowest = TRUE, right = FALSE, 
                  labels = tags)
summary(group_tags)
area_bins <- factor(group_tags, levels = tags, ordered = TRUE)
x$area_bins <- area_bins


## boxplots of P retention
par(mfrow = c(2,1))
boxplot(TP_Retention ~ area_bins, data = x, ylab = "fraction retained",
        xlab = "Wetland area (m2)", main = "TP")
boxplot(PO4_Retention ~ area_bins, data = x, ylab = "fraction retained",
        xlab = "Wetland area (m2)", main = "PO4")

## filter data to remove P retention outlier

x <- x[which(x$TP_Retention > -1),]
x <- x[which(x$PO4_Retention > -1),]

boxplot(TP_Retention ~ area_bins, data = x, ylab = "fraction retained",
        xlab = "Wetland area (m2)", main = "TP")
boxplot(PO4_Retention ~ area_bins, data = x, ylab = "fraction retained",
        xlab = "Wetland area (m2)", main = "PO4")


### magnification factor = PO4/TP

x$mag.f <- x$PO4_Retention/x$TP_Retention
x <- x[which(x$mag.f < 15),]
boxplot(mag.f ~ area_bins, data = x, ylab = "PO4 retention / TP retention",
        xlab = "Wetland area (m2)", main = "Magnification factor")

x$mag.f.norm <- x$mag.f/x$TP_in
boxplot(mag.f.norm ~ area_bins, data = x, ylab = "PO4 retention / TP retention / TP(in)",
        xlab = "Wetland area (m2)", main = "Magnification factor, normalized by TP(inflow)",
        ylim = c(-50,150))

## summary figure

# tiff(filename = "P Retention.tiff", height=9600, width=4800, units= "px", res=800, compression= "lzw")
# 
# {
# layout(matrix(1:3, ncol = 1))
# par(oma = c(3,3,1,4), mar =c(4,4,1,1))
# boxplot(TP_Retention ~ area_bins, data = x, ylab = "fraction retained",
#         xlab = "", main = "TP")
# boxplot(PO4_Retention ~ area_bins, data = x, ylab = "fraction retained",
#         xlab = "", main = "PO4")
# boxplot(mag.f ~ area_bins, data = x, ylab = "PO4 retention / TP retention",
#         xlab = "Wetland area (m2)", main = "Magnification factor")
# }
# dev.off()


## just surface water wetlands

SFW <- x[which(x$Type == "CWSF"),]
boxplot(TP_Retention ~ area_bins, data = SFW, ylab = "fraction retained",
        xlab = "", main = "TP")
boxplot(PO4_Retention ~ area_bins, data = SFW, ylab = "fraction retained",
        xlab = "", main = "PO4")
boxplot(mag.f ~ area_bins, data = SFW, ylab = "PO4 retention / TP retention",
        xlab = "Wetland area (m2)", main = "Magnification factor")
summary(SFW$area_bins)




## convert the data to long, so we can box plot TP and PO4 together

newd <- select(SFW, c(study, Depth, area, TP_Retention, PO4_Retention, area_bins))
  
mewd <- pivot_longer(newd, c(TP_Retention, PO4_Retention), names_to = "Species")  
mewd$Species <- factor(mewd$Species, levels = c("TP_Retention", "PO4_Retention"), ordered = TRUE)


tiff(filename = "P Retention.tiff", height=9600, width=5600, units= "px", res=800, compression= "lzw")

{
  layout(matrix(1:3, ncol = 1))
  par(oma = c(3,3,2,4), mar =c(4,4,2,1))
  boxplot(TP_Retention ~ area_bins, data = SFW, ylab = "fraction retained",
          xlab = "", main = "TP")
  boxplot(PO4_Retention ~ area_bins, data = SFW, ylab = "fraction retained",
          xlab = "", main = "PO4")
  boxplot(value ~ Species*area_bins, data = mewd, col = c("gray50", "white"), ylab = "fraction retained",
          xlab = "", main = "Fraction P Retention", xaxt = "n", 
          at = c(1,2,5,6,9,10,13,14,17,18))
  axis(1, at = c(1.5, 5.5, 9.5, 13.5, 17.5), 
       labels = tags)
  legend("bottomleft", c("TP", "PO4"), pch = 22, pt.cex = 2, pt.bg = c("gray50", "white"))
}
dev.off()


tiff(filename = "P Retention2.tiff", height=7200, width=5600, units= "px", res=800, compression= "lzw") 
{
  layout(matrix(1:2, ncol = 1))
  par(oma = c(2,3,1,4), mar =c(4,4,4,1))
  boxplot(mag.f ~ area_bins, data = SFW, ylab = "PO4 retention / TP retention",
          xlab = "Wetland area (m2)", main = "Magnification factor")
  
  x$mag.f.norm <- x$mag.f/x$TP_in
  boxplot(mag.f.norm ~ area_bins, data = SFW, ylab = "PO4 retention / TP retention / TP(in)",
          xlab = "Wetland area (m2)", main = "Magnification factor, normalized by TP(inflow)",
          ylim = c(-50,150)) 
}
dev.off()  









### a quick look at the same for depth

summary(SFW$Depth)

## bin data by wetland size into 5 roughly evenly sized bins
breaks  <- c(0,0.2,0.3,0.4, 0.7, 3)
tags <- c("0-0.2","0.2-0.3", "0.3-0.4", "0.4-0.7", "0.7+")
group_tags <- cut(SFW$Depth,
                  breaks = breaks,
                  include.lowest = TRUE, right = FALSE, 
                  labels = tags)
summary(group_tags)
depth_bins <- factor(group_tags, levels = tags, ordered = TRUE)
SFW$depth_bins <- depth_bins

newd <- select(SFW, c(study, Depth, area, TP_Retention, PO4_Retention, area_bins, depth_bins))

mewd <- pivot_longer(newd, c(TP_Retention, PO4_Retention), names_to = "Species")  
mewd$Species <- factor(mewd$Species, levels = c("TP_Retention", "PO4_Retention"), ordered = TRUE)



#### P retention by DEPTH
layout(matrix(1:3, ncol = 1))
par(oma = c(3,3,2,4), mar =c(4,4,2,1))
boxplot(TP_Retention ~ depth_bins, data = SFW, ylab = "fraction retained",
        xlab = "", main = "TP")
boxplot(PO4_Retention ~ depth_bins, data = SFW, ylab = "fraction retained",
        xlab = "", main = "PO4")
boxplot(value ~ Species*depth_bins, data = mewd, col = c("gray50", "white"), ylab = "fraction retained",
        xlab = "", main = "Fraction P Retention", xaxt = "n", 
        at = c(1,2,5,6,9,10,13,14,17,18))
axis(1, at = c(1.5, 5.5, 9.5, 13.5, 17.5), 
     labels = tags)
legend("bottomleft", c("TP", "PO4"), pch = 22, pt.cex = 2, pt.bg = c("gray50", "white"))

### Magnification factors by DEPTH
layout(matrix(1:2, ncol = 1))
par(oma = c(2,3,1,4), mar =c(4,4,4,1))
boxplot(mag.f ~ depth_bins, data = SFW, ylab = "PO4 retention / TP retention",
        xlab = "Wetland area (m2)", main = "Magnification factor")

x$mag.f.norm <- x$mag.f/x$TP_in
boxplot(mag.f.norm ~ depth_bins, data = SFW, ylab = "PO4 retention / TP retention / TP(in)",
        xlab = "Wetland area (m2)", main = "Magnification factor, normalized by TP(inflow)",
        ylim = c(-50,150)) 

####

# dot plot

SFW$in.ratio <- SFW$PO4_in/SFW$TP_in
SFW$out.ratio <- SFW$PO4_out/SFW$TP_out

#filter <- SFW[which(SFW$in.ratio < 2),]
filter <- SFW

pal <- viridis(5)
#breaks <- c(-1, 100, 1000, 10000, 100000, 100000000000)
breaks  <- c(0,30, 3000, 15000, 100000, 10000000000)
rank <- as.factor(as.numeric(cut(filter$area, breaks)))
size <- c(0.5, 0.9, 1.3, 1.7, 2.3, 3)
par(mfrow = c(1,1), mar = c(5, 5, 4, 2), xpd= FALSE)
plot(filter$in.ratio, filter$out.ratio, cex = size[rank], pch = 16, col = pal[rank],
     xlab = "Inlet PO4:TP", ylab = "Outlet PO4:TP", 
     main = "Wetland P speciation: Inlet vs Outlet", 
     xlim = c(0,1.25), ylim = c(0,1.1))
mtext("111 observations from 20 sources", side = 3, line = 0.5)
abline(a = 0, b = 1)
legend("bottomright", c("small ", " ", " ", " ", "large "), pch = 16, col = pal, pt.cex = size, 
       bty = "0", title = "Wetland area")


##### look at points above, vs below the one to one line

filter$ratio <- filter$out.ratio/filter$in.ratio
summary(filter$ratio)

breaks  <- c(0,1,7)
tags <- c("LT1","GT1")
group_tags <- cut(filter$ratio,
                  breaks = breaks,
                  include.lowest = TRUE, right = FALSE, 
                  labels = tags)
summary(group_tags)
ratio_bins <- factor(group_tags, levels = tags, ordered = TRUE)
filter$ratio_bins <- ratio_bins

df <- as.data.frame(table(filter$area_bins, filter$ratio_bins))

ndf <- pivot_wider(df, names_from = Var2, values_from = Freq)
tot <- ndf$LT1+ndf$GT1
lt1 <- ndf$LT1
labs <- c("0-30","30-3k", "3k-15k", "15k-100k", "100k+")
barplot(tot, name = labs, col = "gray40", ylab = "Number of Wetlands",
        main = "Ratio of PO4:TP")
barplot(lt1, col = "gray80", add = TRUE)
legend("topright", c("> 1", "< 1"), pch = 22, pt.cex = 2, pt.bg = c("gray40", "gray80"), title ="(outflow/inflow)") 




### stack bar plots

tot
num <- tot/tot
num
lt1
frac <- lt1/tot
frac
par(xpd= TRUE)
barplot(num, name = labs, col = "gray40", ylab = "Fraction of Wetlands",
        main = "Ratio of PO4:TP")
barplot(frac, col = "gray80", add = TRUE)
legend("bottomright", c("> 1", "< 1"), pch = 22, pt.cex = 2, pt.bg = c("gray40", "gray80"), title ="(outflow/inflow)") 
text(0.8,1.02, "n=30")
text(1.9,1.02, "n=23")
text(3.1,1.02, "n=41")
text(4.2,1.02, "n=26")
text(5.4,1.02, "n=16")




### stacked bar plots of TP and OF source/sink by size

source <- SFW[which(SFW$TP_Retention < 0),]
sink <- SFW[which(SFW$TP_Retention > 0),]
table(SFW$area_bins)
table(source$area_bins)
table(sink$area_bins)

num <- c(30, 35, 41, 26, 16)
TP_sink <- c(30, 24, 34, 26, 15)
frac.TP_sink <- TP_sink/num
ones <- c(1,1,1,1,1)

par(mfrow = c(1,1))
barplot(ones, name = labs, col = "gray40", ylab = "Fraction of Wetlands",
        main = "TP Retention")
barplot(frac.TP_sink, col = "gray80", add = TRUE)
legend("bottomright", c("Source", "Sink"), pch = 22, pt.cex = 2, pt.bg = c("gray40", "gray80"), title ="TP") 
text(0.8,1.02, "n=30")
text(1.9,1.02, "n=23")
text(3.1,1.02, "n=41")
text(4.2,1.02, "n=26")
text(5.4,1.02, "n=16")





source <- SFW[which(SFW$PO4_Retention < 0),]
sink <- SFW[which(SFW$PO4_Retention > 0),]
table(SFW$area_bins)
table(source$area_bins)
table(sink$area_bins)

num <- c(30, 35, 41, 26, 16)
PO4_sink <- c(30, 16, 27, 25, 13)
frac.PO4_sink <- PO4_sink/num

par(mfrow = c(1,1))
barplot(ones, name = labs, col = "gray40", ylab = "Fraction of Wetlands",
        main = "PO4 Retention")
barplot(frac.PO4_sink, col = "gray80", add = TRUE)
legend("bottomright", c("Source", "Sink"), pch = 22, pt.cex = 2, pt.bg = c("gray40", "gray80"), title ="TP") 
text(0.8,1.02, "n=30")
text(1.9,1.02, "n=23")
text(3.1,1.02, "n=41")
text(4.2,1.02, "n=26")
text(5.4,1.02, "n=16")


















### extra stuff/scratch



## density plot the distribution of TP and PO4 retention
d <- density(x$TP_Retention, na.rm = TRUE)
d2 <- density(x$PO4_Retention, na.rm = TRUE)
plot(d, main = "Phosphorus Retention", xlim = c(-1.2,1.2), xlab = "Fractional retention")
polygon(d, col = "#FF000099", border="red", lwd = 3)
polygon(d2, col = "#0000FF99", border="blue", lwd = 3)
legend("topleft", c("TP", "PO4"), pch = 22, col = c("red", "blue"),
       pt.bg = c("#FF000099", "#0000FF99"), pt.cex = 2,  lwd = 3, lty = NA)


## TP:PO4 ratio


x$ratio <- x$TP_Retention/x$PO4_Retention


plot(x$tau, x$ratio, log = "x")
plot(x$area, x$ratio, log = "x")

plot(x$TP_in, x$ratio, log = "x")


x$in.ratio <- x$PO4_in/x$TP_in
x$out.ratio <- x$PO4_out/x$TP_out

filter <- x[which(x$out.ratio < 9 & x$in.ratio < 2),]


# plot(filter$in.ratio, filter$out.ratio)
# abline(a = 0, b = 1)


# hist(x$TP_Retention)
# pal <- viridis(5)
# breaks <- c(-1, 0.5, 0, 0.3, 0.6, 1)
# rank <- as.factor(as.numeric(cut(x$TP_Retention, breaks)))
# plot(filter$in.ratio, filter$out.ratio, pch = 16, col = pal[rank])
# abline(a = 0, b = 1)

hist(x$area)
pal <- viridis(5)
breaks <- c(-1, 100, 1000, 10000, 100000, 100000000000)
#breaks  <- c(0,30, 3000, 15000, 100000, 10000000000)
rank <- as.factor(as.numeric(cut(x$area, breaks)))
size <- c(0.5, 0.9, 1.3, 1.7, 2.3, 3)
par(mfrow = c(1,1), mar = c(5, 5, 4, 2))
plot(filter$in.ratio, filter$out.ratio, cex = size[rank], pch = 16, col = pal[rank],
     xlab = "Inlet PO4:TP", ylab = "Outlet PO4:TP", 
     main = "Wetland P speciation: Inlet vs Outlet")
mtext("160 observations from 16 sources", side = 3, line = 0.5)
abline(a = 0, b = 1)
legend("bottomright", c("small ", " ", " ", " ", "large "), pch = 16, col = pal, pt.cex = size, 
       bty = "0", title = "Wetland area")












filter$Rratio <- filter$out.ratio/filter$in.ratio


plot(filter$area, filter$Rratio, log = "x")



