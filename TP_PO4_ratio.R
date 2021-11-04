



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


## Fred's meta-analysis of literature values
x <- read.csv("P_speciation2.csv", header = T)


## bin data by wetland size into 5 roughly evenly sized bins
breaks  <- c(0,30, 2000, 15000, 500000, 10000000000)
tags <- c("0-30","30-2k", "2k-15k", "15k-500k", "500k+")
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





## summary figure


tiff(filename = "P Retention.tiff", height=9600, width=4800, units= "px", res=800, compression= "lzw")

{
layout(matrix(1:3, ncol = 1))
par(oma = c(3,3,1,4), mar =c(4,4,1,1))
boxplot(TP_Retention ~ area_bins, data = x, ylab = "fraction retained",
        xlab = "", main = "TP")
boxplot(PO4_Retention ~ area_bins, data = x, ylab = "fraction retained",
        xlab = "", main = "PO4")
boxplot(mag.f ~ area_bins, data = x, ylab = "PO4 retention / TP retention",
        xlab = "Wetland area (m2)", main = "Magnification factor")
}
dev.off()



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


plot(filter$in.ratio, filter$out.ratio)
abline(a = 0, b = 1)



hist(x$TP_Retention)
pal <- viridis(5)
breaks <- c(-1, 0.5, 0, 0.3, 0.6, 1)
rank <- as.factor(as.numeric(cut(x$TP_Retention, breaks)))
plot(filter$in.ratio, filter$out.ratio, pch = 16, col = pal[rank])
abline(a = 0, b = 1)





hist(x$area)
pal <- viridis(5)
breaks <- c(-1, 100, 1000, 10000, 100000, 100000000000)
rank <- as.factor(as.numeric(cut(x$area, breaks)))
size <- c(0.5, 0.9, 1.3, 1.7, 2.3, 3)
par(mar = c(5, 5, 4, 2))
plot(filter$in.ratio, filter$out.ratio, cex = size[rank], pch = 16, col = pal[rank],
     xlab = "Inlet PO4:TP", ylab = "Outlet PO4:TP", 
     main = "Wetland P speciation: Inlet vs Outlet")
mtext("160 observations from 16 sources", side = 3, line =1)
abline(a = 0, b = 1)
legend("bottomright", c("small ", " ", " ", " ", "large "), pch = 16, col = pal, pt.cex = size, 
       bty = "0", title = "Wetland area")



filter$Rratio <- filter$out.ratio/filter$in.ratio


plot(filter$area, filter$Rratio, log = "x")



