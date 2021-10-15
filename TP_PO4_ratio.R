



### Wetland P Removal
## TP:PO4


setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files")

x <- read.csv("P_speciation2.csv", header = T)


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
plot(filter$in.ratio, filter$out.ratio, pch = 16, col = pal[rank])
abline(a = 0, b = 1)







