

## historic wetland data exploration

#' exploring how methodological choices made in generation of historic wetlands
#' polygons will affect the resulting wetland size distribution
#' 
#' Exported area files from arc
#' generating histograms and density plots



## Size distribution of hydric sinks

data <- read.csv("LEB_hydric_sinks.csv", header = TRUE)



col = c("#08519c", "#3182bd", "#6baed6", "#9ecae1", rep("#d9d9d9", 30))
col = c("#9ecae1", "#6baed6", "#3182bd", "#08519c", rep("#d9d9d9", 30))

hist(data$area, breaks = 30000, xlim = c(0,2500), col = col)


w100 <- nrow(data[which(data$area <100),])
w200 <- nrow(data[which(data$area >101 & data$area < 200),])
w400 <- nrow(data[which(data$area > 201 & data$area < 400),])
w1000 <- nrow(data[which(data$area > 401 & data$area < 1000),])
w5000 <- nrow(data[which(data$area > 1001 & data$area < 5000),])
w10000 <- nrow(data[which(data$area > 5001 & data$area < 10000),])
wxl <- nrow(data[which(data$area > 10001),])

bin <- c(1,2,3,4,5,6, 7)
size <- c("0-100", "100-200", "200-400", "400-1k", "1k-2k","5k-10k", "10k+")
count <- c(w100, w200, w400, w1000, w5000, w10000, wxl)

x <- data.frame(bin, size, count)

barplot(x$count, names.arg = x$size, xlab = "Area (m2)")


max(data$area)


hist <- hist(data$area, breaks = 1000)
plot(hist$mids, hist$density, log = "y", type = 'b')

barplot(data$area, log = "y")



d <- density(data$area)
plot(d, log = "x")

#' For lidar size analysis, we are looking at only a small section of the LEB





d <- density(x$TP_Retention, na.rm = TRUE)
d2 <- density(x$PO4_Retention, na.rm = TRUE)
plot(d, main = "Phosphorus Retention", xlim = c(-1.2,1.2), xlab = "Fractional retention")
polygon(d, col = "#FF000099", border="red", lwd = 3)
polygon(d2, col = "#0000FF99", border="blue", lwd = 3)
legend("topleft", c("TP", "PO4"), pch = 22, col = c("red", "blue"),
       pt.bg = c("#FF000099", "#0000FF99"), pt.cex = 2,  lwd = 3, lty = NA)
