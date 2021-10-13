



### Wetland P Removal
## TP:PO4


setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files")

x <- read.csv("P_speciation.csv", header = T)

hist(x$TP_Retention, col = "#9400D399", breaks = 15)
hist(x$TP_Retention, col = "purple")

d <- density(x$TP_Retention, na.rm = TRUE)
d2 <- density(x$PO4_Retention, na.rm = TRUE)
plot(d, main = "Phosphorus Retention", xlim = c(-1.2,1.2), xlab = "Fractional retention")
polygon(d, col = "#FF000099", border="red", lwd = 3)
polygon(d2, col = "#0000FF99", border="blue", lwd = 3)
legend("topleft", c("TP", "PO4"), pch = 22, col = c("red", "blue"),
       pt.bg = c("#FF000099", "#0000FF99"), pt.cex = 2,  lwd = 3, lty = NA)

