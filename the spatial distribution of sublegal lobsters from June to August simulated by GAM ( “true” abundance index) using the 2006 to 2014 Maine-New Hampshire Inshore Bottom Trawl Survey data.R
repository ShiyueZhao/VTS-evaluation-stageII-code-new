











setwd("D:/evaluating")
#### Install and library packages ####
list_of_packages <- c("maptools", "rgdal", "mgcv", "classInt","RColorBrewer","maps", "mapdata",  "mgcv", "raster", "SDMTools", "dplyr", "akima", "viridis", "sp", "rgeos" )
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(maptools)
library(rgdal)
library(classInt) 
library(RColorBrewer)
library(maps)
library(mapdata)
library(mgcv)
library(raster)
library(SDMTools)
library(dplyr)
library(akima)
library(viridis)
library(sp)
library(rgeos)
#### Load bottom trawl survey data and map shapefile#### 
nao_temperature <- read.csv("./data/NAO_temperature.csv")
lobster_catch_data = read.csv("./data/bottom_trawl_survey/Lobster_MENHTrawl_Catch.csv")
lobster_catch_data_env = read.csv("./data/bottom_trawl_survey/Lobster_MENHTrawl_Catch_old.csv")
lobster_length_data = read.csv("./data/bottom_trawl_survey/Lobster_MENHTrawl_Length.csv")

lobster_catch_data_env <- lobster_catch_data_env[which(as.character(lobster_catch_data_env$catch_sum)!="#N/A"),]
lobster_catch_data <- lobster_catch_data[which(as.character(lobster_catch_data$catch_sum)!="#N/A"),]
lobster_catch_data <- lobster_catch_data[which(as.character(lobster_catch_data$catch_sum) != ""),]

length(unique(lobster_catch_data_env$UniqueTowID))
length(unique(lobster_catch_data$UniqueTowID))

for(i in 1:nrow(lobster_catch_data)){
  id <- which(lobster_catch_data_env$UniqueTowID==lobster_catch_data$UniqueTowID[i])
  print(i);print(length(id))
  if (length(id)==0) {
    lobster_catch_data$Water_Temp_DegC[i] <- NA
    lobster_catch_data$Salinity_psu[i] <- NA
  }
  else{
    lobster_catch_data$Water_Temp_DegC[i] <- as.numeric(as.character(lobster_catch_data_env$Water_Temp_DegC[id]))
    lobster_catch_data$Salinity_psu[i] <- as.numeric(as.character(lobster_catch_data_env$Salinity_psu[id]))
  }
}

statistical_areas <-rgdal::readOGR("./data/gis/Statistical_Areas_2010.shp")
sa511_513 <- statistical_areas[which(statistical_areas@data$Id %in% c(511:513)),]
vts_gom_strata <- rgdal::readOGR("./data/gis/GOM_VTS_Strata.shp")

head(lobster_catch_data)
class_convert_var <- c("UniqueTowID", "TowNum", "Region", "Stratum", "Grid_ID", "NumberCaught", "catch_sum", "Weight", "weight_sum", "end_lat", "end_lon", "end_depth", "Tow_Time", "Tow_LengthNM", "EXP_Catch", "Year", "Water_Temp_DegC","Salinity_psu")
for(i in 1:length(class_convert_var)){
  lobster_catch_data[,class_convert_var[i]] <- as.numeric(as.character(lobster_catch_data[,class_convert_var[i]]))
}

head(lobster_length_data)
class_convert_var <- c("TowNum", "Region", "Stratum", "Year", "Length", "end_lat", "end_lon")
for(i in 1:length(class_convert_var)){
  lobster_length_data[,class_convert_var[i]] <- as.numeric(as.character(lobster_length_data[,class_convert_var[i]]))
}
#### Check outliers ####
summary(lobster_catch_data)
summary(lobster_length_data)
identical(lobster_catch_data$NumberCaught, lobster_catch_data$catch_sum)
mismatch_lobster_catch <- lobster_catch_data[which(!(lobster_catch_data$NumberCaught %in% lobster_catch_data$catch_sum)),]
write.csv(mismatch_lobster_catch, file = "./output/mismatch_lobster_catch.csv")

####save time change TRUE to FALSE
if(FALSE){
  jpeg(filename = "./plot/outliers_lobster_catch.jpeg", width = 170, height = 80, res = 600, units="mm")
  par(mfrow=c(2,3), mar=c(4,4,1,1))
  dotchart(lobster_catch_data$NumberCaught, groups=as.factor(lobster_catch_data$Region), ylab="Region", xlab="Catch (#)/Tow")
  dotchart(lobster_catch_data$EXP_Catch, groups=as.factor(lobster_catch_data$Region), ylab="Region", xlab="Expect Catch (#)/Tow")
  dotchart(lobster_catch_data$end_depth, groups=as.factor(lobster_catch_data$Region), ylab="Region", xlab="Depth")
  dotchart(lobster_catch_data$Water_Temp_DegC, groups=as.factor(lobster_catch_data$Region), ylab="Region", xlab="Temperature")
  dotchart(lobster_catch_data$Salinity_psu, groups=as.factor(lobster_catch_data$Region), ylab="Region", xlab="Salinity")
  dev.off()
  
  lobster_catch_data[which(lobster_catch_data$NumberCaught>10000),]
  
  summary(lobster_length_data$Unit_Len)
  lobster_length_data[which(lobster_length_data$Unit_Len=="CM"),]
  
  jpeg(filename = "./plot/outliers_lobster_length.jpeg", width = 200, height = 40, res = 600, units="mm")
  par(mfrow=c(1,3), mar=c(4,4,1,1))
  dotchart(lobster_length_data$Length, groups=as.factor(lobster_length_data$Region), ylab="Region", xlab="Length")
  dotchart(lobster_length_data$end_lat, groups=as.factor(lobster_length_data$Region), ylab="Region", xlab="Latitude")
  dotchart(lobster_length_data$end_lon, groups=as.factor(lobster_length_data$Region), ylab="Region", xlab="Longitude")
  dev.off()
}

#outlier_catch=lobster_catch_data[which(lobster_catch_data$NumberCaught>10000),]
#write.csv(outlier_catch, file = "./output/outlier_catch.csv")
#lobster_catch_data<-lobster_catch_data[which(lobster_catch_data$NumberCaught<10000),]
#### Clean and aggregate lobster length dataset ####
outlier_length=lobster_length_data[which(lobster_length_data$Length>300),]
write.csv(outlier_length, file = "./output/outlier_length.csv")
lobster_length_data <- lobster_length_data[which(lobster_length_data$Length<300),]
lobster_length_data[which(is.na(lobster_length_data$Length)),]

juvenile_length <- 83
juvenile_length_data <- lobster_length_data[which(lobster_length_data$Length<juvenile_length),]
juvenile_per_tow <- aggregate(juvenile_length_data$Length, by=list(juvenile_length_data$Survey, juvenile_length_data$TowNum), length)
colnames(juvenile_per_tow) <- c("Survey", "TowNum", "JuvNum")
sum_per_tow <- aggregate(lobster_length_data$Length, by=list(lobster_length_data$Survey, lobster_length_data$TowNum), length)
colnames(sum_per_tow) <- c("Survey", "TowNum", "SumNum")
juvenile_ratio <- sum_per_tow
for (i in 1:nrow(juvenile_ratio)){
  juv_num <- juvenile_per_tow$JuvNum[which(juvenile_per_tow$Survey==juvenile_ratio$Survey[i] & juvenile_per_tow$TowNum==juvenile_ratio$TowNum[i])]
  if (length(juv_num)==0) juv_num=0
  juvenile_ratio$SumNum[i] <- juv_num/juvenile_ratio$SumNum[i]
}
colnames(juvenile_ratio) <- c("Survey", "TowNum", "JuvRatio")
#### Match lobster catch and length datasets ####
month_day <- strsplit(as.character(lobster_catch_data$End_Date), split = '/')
for (i in 1:length(month_day)){
  lobster_catch_data$Month[i] <- as.numeric(month_day[[i]][1])
  lobster_catch_data$Day[i] <- as.numeric(month_day[[i]][2])
}

lobster_catch_data <- lobster_catch_data[which(lobster_catch_data$Survey!="FL18"),]
lobster_catch_data$Survey <- factor(lobster_catch_data$Survey)
summary(lobster_catch_data)

for (i in 1:nrow(lobster_catch_data)){
  juv_ratio <- juvenile_ratio$JuvRatio[which(juvenile_ratio$Survey==lobster_catch_data$Survey[i] & juvenile_ratio$TowNum==lobster_catch_data$TowNum[i])]
  if (length(juv_ratio)==0) juv_ratio=0
  lobster_catch_data$JuvRatio[i] <- juv_ratio
}
lobster_catch_data$juv_num <- as.numeric(lobster_catch_data$EXP_Catch)*lobster_catch_data$JuvRatio
lobster_catch_data$adu_num <-as.numeric(lobster_catch_data$EXP_Catch)*(1-lobster_catch_data$JuvRatio)

#dotchart(lobster_catch_data$juv_num, groups=as.factor(lobster_catch_data$Region), ylab="Region", xlab="Juveniles (#)/Tow")

#### Plot distribution of raw data ####
lobster_catch_data <- lobster_catch_data[-which(is.na(lobster_catch_data[,c( "Salinity_psu", "end_lat", "end_lon", "Year", "Month")])),]

map_data <- lobster_catch_data
map_data$log_data <- ifelse(map_data$juv_num==0, NA, log(map_data$juv_num))

var="log_data"
plotvar <- map_data[,var]
nclr=6
plotclr <- rev(brewer.pal(nclr,"RdBu"))
class <- classIntervals(plotvar, nclr, style="equal")
class$brks = round(class$brks, digits=2)
colcode <- findColours(class, plotclr)

start_x <- range(lobster_catch_data$end_lon)[1]
end_x <- range(lobster_catch_data$end_lon)[2]
start_y <- range(lobster_catch_data$end_lat)[1]
end_y <- range(lobster_catch_data$end_lat)[2]

jpeg(filename = paste("./plot/juv_catch_map.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
layout(matrix(c(1,1,1,3,2,2,2,3), 2, 4, byrow = TRUE))
par(mar=c(4,4,1,1))
plot(map_data$end_lon, map_data$end_lat, pch=16, col=colcode, cex=0.3, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, type="n", xlab="Longitude", ylab="Latitude")
map(database = "worldHires", ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE)
#map(coastline, add =T, fill = TRUE, col = "gray95", border="black", lwd=0.2)
points(map_data$end_lon[which(map_data$Month<7&is.na(map_data$log_data))], map_data$end_lat[which(map_data$Month<7&is.na(map_data$log_data))], pch=4, cex=0.3, col="gray")
points(map_data$end_lon[which(map_data$Month<7)], map_data$end_lat[which(map_data$Month<7)], pch=16, col=colcode[which(map_data$Month<7)], cex=0.3)
box()
degAxis(1)
degAxis(2)
legend("topleft", "Spring", bty="n")

plot(map_data$end_lon, map_data$end_lat, pch=16, col=colcode, cex=0.3, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, type="n", xlab="Longitude", ylab="Latitude")
map(database = "worldHires", ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE)
#map(coastline, add =T, fill = TRUE, col = "gray95", border="black", lwd=0.2)
points(map_data$end_lon[which(map_data$Month>8&is.na(map_data$log_data))], map_data$end_lat[which(map_data$Month>8&is.na(map_data$log_data))], pch=4, cex=0.3, col="gray")
points(map_data$end_lon[which(map_data$Month>8)], map_data$end_lat[which(map_data$Month>8)], pch=16, col=colcode[which(map_data$Month>8)], cex=0.3)
box()
degAxis(1)
degAxis(2)
legend("topleft", "Fall", bty="n")

par(mar=c(0.5,0.5,0.5,0.5))
plot.new()
legend("left", legend=c(names(attr(colcode, "table")), "No lobster"), fill=c(attr(colcode, "palette"), "gray"), cex=0.9, bty="n", title="Log(#)/Tow")
dev.off()

#### Temperature map ####
var="Water_Temp_DegC"
plotvar <- map_data[,var]
nclr=6
plotclr <- rev(brewer.pal(nclr,"RdBu"))
class <- classIntervals(plotvar, nclr, style="equal")
class$brks = round(class$brks, digits=2)
colcode <- findColours(class, plotclr)

start_x <- range(lobster_catch_data$end_lon)[1]
end_x <- range(lobster_catch_data$end_lon)[2]
start_y <- range(lobster_catch_data$end_lat)[1]
end_y <- range(lobster_catch_data$end_lat)[2]

jpeg(filename = paste("./plot/temperature_map.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
layout(matrix(c(1,1,1,3,2,2,2,3), 2, 4, byrow = TRUE))
par(mar=c(4,4,1,1))
plot(map_data$end_lon, map_data$end_lat, pch=16, col=colcode, cex=0.3, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, type="n", xlab="Longitude", ylab="Latitude")
map(database = "worldHires", ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE)
#map(coastline, add =T, fill = TRUE, col = "gray95", border="black", lwd=0.2)
points(map_data$end_lon[which(map_data$Month<7)], map_data$end_lat[which(map_data$Month<7)], pch=16, col=colcode[which(map_data$Month<7)], cex=0.3)
box()
degAxis(1)
degAxis(2)
legend("topleft", "Spring", bty="n")

plot(map_data$end_lon, map_data$end_lat, pch=16, col=colcode, cex=0.3, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, type="n", xlab="Longitude", ylab="Latitude")
map(database = "worldHires", ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE)
#map(coastline, add =T, fill = TRUE, col = "gray95", border="black", lwd=0.2)
points(map_data$end_lon[which(map_data$Month>8)], map_data$end_lat[which(map_data$Month>8)], pch=16, col=colcode[which(map_data$Month>8)], cex=0.3)
box()
degAxis(1)
degAxis(2)
legend("topleft", "Fall", bty="n")

par(mar=c(0.5,0.5,0.5,0.5))
plot.new()
legend("left", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=0.9, bty="n", title="Temperature")
dev.off()
#### Salinity map ####
var="Salinity_psu"
plotvar <- map_data[,var]
nclr=6
plotclr <- rev(brewer.pal(nclr,"RdBu"))
class <- classIntervals(plotvar, nclr, style="equal")
class$brks = round(class$brks, digits=2)
colcode <- findColours(class, plotclr)

start_x <- range(lobster_catch_data$end_lon)[1]
end_x <- range(lobster_catch_data$end_lon)[2]
start_y <- range(lobster_catch_data$end_lat)[1]
end_y <- range(lobster_catch_data$end_lat)[2]

jpeg(filename = paste("./plot/salinity_map.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
layout(matrix(c(1,1,1,3,2,2,2,3), 2, 4, byrow = TRUE))
par(mar=c(4,4,1,1))
plot(map_data$end_lon, map_data$end_lat, pch=16, col=colcode, cex=0.3, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, type="n", xlab="Longitude", ylab="Latitude")
map(database = "worldHires", ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE)
#map(coastline, add =T, fill = TRUE, col = "gray95", border="black", lwd=0.2)
points(map_data$end_lon[which(map_data$Month<7)], map_data$end_lat[which(map_data$Month<7)], pch=16, col=colcode[which(map_data$Month<7)], cex=0.3)
box()
degAxis(1)
degAxis(2)
legend("topleft", "Spring", bty="n")

plot(map_data$end_lon, map_data$end_lat, pch=16, col=colcode, cex=0.3, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, type="n", xlab="Longitude", ylab="Latitude")
map(database = "worldHires", ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE)
#map(coastline, add =T, fill = TRUE, col = "gray95", border="black", lwd=0.2)
points(map_data$end_lon[which(map_data$Month>8)], map_data$end_lat[which(map_data$Month>8)], pch=16, col=colcode[which(map_data$Month>8)], cex=0.3)
box()
degAxis(1)
degAxis(2)
legend("topleft", "Fall", bty="n")

par(mar=c(0.5,0.5,0.5,0.5))
plot.new()
legend("left", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=0.9, bty="n", title="Salinity")
dev.off()

#### Depth map####
var="end_depth"
plotvar <- map_data[,var]
nclr=6
plotclr <- rev(brewer.pal(nclr,"RdBu"))
class <- classIntervals(plotvar, nclr, style="equal")
class$brks = round(class$brks, digits=2)
colcode <- findColours(class, plotclr)

start_x <- range(lobster_catch_data$end_lon)[1]
end_x <- range(lobster_catch_data$end_lon)[2]
start_y <- range(lobster_catch_data$end_lat)[1]
end_y <- range(lobster_catch_data$end_lat)[2]

jpeg(filename = paste("./plot/depth_map.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
layout(matrix(c(1,1,1,3,2,2,2,3), 2, 4, byrow = TRUE))
par(mar=c(4,4,1,1))
plot(map_data$end_lon, map_data$end_lat, pch=16, col=colcode, cex=0.3, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, type="n", xlab="Longitude", ylab="Latitude")
map(database = "worldHires", ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE)
#map(coastline, add =T, fill = TRUE, col = "gray95", border="black", lwd=0.2)
points(map_data$end_lon[which(map_data$Month<7)], map_data$end_lat[which(map_data$Month<7)], pch=16, col=colcode[which(map_data$Month<7)], cex=0.3)
box()
degAxis(1)
degAxis(2)
legend("topleft", "Spring", bty="n")

plot(map_data$end_lon, map_data$end_lat, pch=16, col=colcode, cex=0.3, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, type="n", xlab="Longitude", ylab="Latitude")
map(database = "worldHires", ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE)
#map(coastline, add =T, fill = TRUE, col = "gray95", border="black", lwd=0.2)
points(map_data$end_lon[which(map_data$Month>8)], map_data$end_lat[which(map_data$Month>8)], pch=16, col=colcode[which(map_data$Month>8)], cex=0.3)
box()
degAxis(1)
degAxis(2)
legend("topleft", "Fall", bty="n")

par(mar=c(0.5,0.5,0.5,0.5))
plot.new()
legend("left", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=0.9, bty="n", title="Depth (Fathom)")
dev.off()

#### Seasonal GAM ####
summary(lobster_catch_data$juv_num)
spring_model <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(Salinity_psu, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data = lobster_catch_data[which(lobster_catch_data$Month<7),])
summary(spring_model)

jpeg(filename = paste("./plot/spring_gam_relationship.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
par(mfrow=c(3,2), mar=c(4,4,1,1))
plot.gam(spring_model, xlab=c("Depth"), ylab="Lobster density", select = 1)
plot.gam(spring_model, xlab=c("Temperature"), ylab="Lobster density", select = 2)
plot.gam(spring_model, xlab=c("Salinity"), ylab="Lobster density", select = 3)
plot.gam(spring_model, xlab=c("Latitude"), ylab="Lobster density", select = 4)
plot.gam(spring_model, xlab=c("Longitude"), ylab="Lobster density", select = 5)
dev.off()

fall_model <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(Salinity_psu, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data = lobster_catch_data[which(lobster_catch_data$Year<2014 & (lobster_catch_data$Month==6 | lobster_catch_data$Month>8)),])
summary(fall_model)

fall_model <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data = lobster_catch_data[which(lobster_catch_data$Month==6 | lobster_catch_data$Month>8),])
summary(fall_model)

jpeg(filename = paste("./plot/fall_gam_relationship.jpeg", sep=""), width=100, height=80, units = "mm", res = 600)
par(mfrow=c(2,2), mar=c(4,4,1,1))
plot.gam(fall_model, xlab=c("Depth"), ylab="Lobster density", select = 1)
plot.gam(fall_model, xlab=c("Temperature"), ylab="Lobster density", select = 2)
plot.gam(fall_model, xlab=c("Latitude"), ylab="Lobster density", select = 3)
plot.gam(fall_model, xlab=c("Longitude"), ylab="Lobster density", select = 4)
dev.off()

if(FALSE){
  gam_model <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(Salinity_psu, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data <- lobster_catch_data)
  summary(gam_model)
  
  jpeg(filename = paste("./plot/gam_relationship.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
  par(mfrow=c(3,2), mar=c(4,4,1,1))
  plot.gam(gam_model, xlab=c("Depth"), ylab="Lobster density", select = 1)
  plot.gam(gam_model, xlab=c("Temperature"), ylab="Lobster density", select = 2)
  plot.gam(gam_model, xlab=c("Salinity"), ylab="Lobster density", select = 3)
  plot.gam(gam_model, xlab=c("Latitude"), ylab="Lobster density", select = 4)
  plot.gam(gam_model, xlab=c("Longitude"), ylab="Lobster density", select = 5)
  dev.off()
  
  jpeg(filename = paste("./plot/flowchart_gam_relationship.jpeg", sep=""), width=80, height=100, units = "mm", res = 600)
  par(mfrow=c(2,1), mar=c(4,4,1,1))
  
  plot.gam(gam_model, xlab=c("Temperature"), ylab="Lobster density", select = 2)
  plot.gam(gam_model, xlab=c("Salinity"), ylab="Lobster density", select = 3)
  
  dev.off()
} ## use all data

if(FALSE){
  gam_model <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(Salinity_psu, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data <- lobster_catch_data[which(lobster_catch_data$Year<2014 & (lobster_catch_data$Month==6 | lobster_catch_data$Month>8)),])
  #gam_model <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data <- lobster_catch_data[which(lobster_catch_data$Year<2014 & (lobster_catch_data$Month==6 | lobster_catch_data$Month>8)),])
  summary(gam_model)
  
  jpeg(filename = paste("./plot/gam_relationship.jpeg", sep=""), width=100, height=100, units = "mm", res = 800)
  par(mfrow=c(3,2), mar=c(4,4,1,1))
  plot.gam(gam_model, xlab=c("Depth"), ylab="Lobster density", select = 1)
  plot.gam(gam_model, xlab=c("Temperature"), ylab="Lobster density", select = 2)
  plot.gam(gam_model, xlab=c("Temperature"), ylab="Lobster density", select = 3)
  plot.gam(gam_model, xlab=c("Latitude"), ylab="Lobster density", select = 4)
  plot.gam(gam_model, xlab=c("Longitude"), ylab="Lobster density", select = 5)
  dev.off()
} ## use June and fall data

if(FALSE){
  gam_model <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(Salinity_psu, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data <- lobster_catch_data[which(lobster_catch_data$Month==6 |lobster_catch_data$Month==9),])
  summary(gam_model)
  
  gam_model <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data <- lobster_catch_data[which(lobster_catch_data$Month==6 |lobster_catch_data$Month==9),])
  summary(gam_model)
  
  jpeg(filename = paste("./plot/gam_relationship.jpeg", sep=""), width=100, height=100, units = "mm", res = 800)
  par(mfrow=c(2,2), mar=c(4,4,1,1))
  plot.gam(gam_model, xlab=c("Depth"), ylab="Lobster density", select = 1)
  plot.gam(gam_model, xlab=c("Temperature"), ylab="Lobster density", select = 2)
  plot.gam(gam_model, xlab=c("Latitude"), ylab="Lobster density", select = 3)
  plot.gam(gam_model, xlab=c("Longitude"), ylab="Lobster density", select = 4)
  dev.off()
} ## use June and September data

if(FALSE){
  trawl_points <- SpatialPoints(cbind(lobster_catch_data$end_lon, lobster_catch_data$end_lat), proj4string=CRS("+proj=longlat +datum=NAD83 +no_defs")) 
  polygon_points <- over(trawl_points , sa511_513)
  
  gam_model_511 = gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(Salinity_psu, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data = lobster_catch_data[which(polygon_points$Id=="511"),])
  summary(gam_model_511)
  gam_model_511 = gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data = lobster_catch_data[which(polygon_points$Id=="511"),])
  summary(gam_model_511)
  
  jpeg(filename = paste("./plot/gam511_relationship.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
  par(mfrow=c(2,2), mar=c(4,4,1,1))
  plot.gam(gam_model_511, xlab=c("Depth"), ylab="Lobster density", select = 1)
  plot.gam(gam_model_511, xlab=c("Temperature"), ylab="Lobster density", select = 2)
  plot.gam(gam_model_511, xlab=c("Latitude"), ylab="Lobster density", select = 3)
  plot.gam(gam_model_511, xlab=c("Longitude"), ylab="Lobster density", select = 4)
  dev.off()
  
  gam_model_512 <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(Salinity_psu, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data <- lobster_catch_data[which(polygon_points$Id=="512"),])
  summary(gam_model_512)
  
  jpeg(filename = paste("./plot/gam512_relationship.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
  par(mfrow=c(3,2), mar=c(4,4,1,1))
  plot.gam(gam_model_512, xlab=c("Depth"), ylab="Lobster density", select = 1)
  plot.gam(gam_model_512, xlab=c("Temperature"), ylab="Lobster density", select = 2)
  plot.gam(gam_model_512, xlab=c("Salinity"), ylab="Lobster density", select = 3)
  plot.gam(gam_model_512, xlab=c("Latitude"), ylab="Lobster density", select = 4)
  plot.gam(gam_model_512, xlab=c("Longitude"), ylab="Lobster density", select = 5)
  dev.off()
  
  gam_model_513 <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(Salinity_psu, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data <- lobster_catch_data[which(polygon_points$Id=="513"),])
  summary(gam_model_513)
  
  jpeg(filename = paste("./plot/gam513_relationship.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
  par(mfrow=c(3,2), mar=c(4,4,1,1))
  plot.gam(gam_model_513, xlab=c("Depth"), ylab="Lobster density", select = 1)
  plot.gam(gam_model_513, xlab=c("Temperature"), ylab="Lobster density", select = 2)
  plot.gam(gam_model_513, xlab=c("Salinity"), ylab="Lobster density", select = 3)
  plot.gam(gam_model_513, xlab=c("Latitude"), ylab="Lobster density", select = 4)
  plot.gam(gam_model_513, xlab=c("Longitude"), ylab="Lobster density", select = 5)
  dev.off()
} ## use all data but separate data by statistical area

if(FALSE){
  trawl_points <- SpatialPoints(cbind(lobster_catch_data$end_lon, lobster_catch_data$end_lat), proj4string=CRS("+proj=longlat +datum=NAD83 +no_defs")) 
  polygon_points <- over(trawl_points , sa511_513)
  
  gam_model_511 = gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(Salinity_psu, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data = lobster_catch_data[which(polygon_points$Id=="511" & lobster_catch_data$Month>8),])
  summary(gam_model_511)
  
  #gam_model_511 <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data <- lobster_catch_data[which(polygon_points$Id=="511"),])
  #summary(gam_model_511)
  
  jpeg(filename = paste("./plot/gam511_relationship.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
  par(mfrow=c(3,2), mar=c(4,4,1,1))
  plot.gam(gam_model_511, xlab=c("Depth"), ylab="Lobster density", select = 1)
  plot.gam(gam_model_511, xlab=c("Temperature"), ylab="Lobster density", select = 2)
  plot.gam(gam_model_511, xlab=c("Salinity"), ylab="Lobster density", select = 3)
  plot.gam(gam_model_511, xlab=c("Latitude"), ylab="Lobster density", select = 4)
  plot.gam(gam_model_511, xlab=c("Longitude"), ylab="Lobster density", select = 5)
  dev.off()
  
  gam_model_512 <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(Salinity_psu, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data <- lobster_catch_data[which(polygon_points$Id=="512"& lobster_catch_data$Month>8),])
  summary(gam_model_512)
  
  jpeg(filename = paste("./plot/gam512_relationship.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
  par(mfrow=c(3,2), mar=c(4,4,1,1))
  plot.gam(gam_model_512, xlab=c("Depth"), ylab="Lobster density", select = 1)
  plot.gam(gam_model_512, xlab=c("Temperature"), ylab="Lobster density", select = 2)
  plot.gam(gam_model_512, xlab=c("Salinity"), ylab="Lobster density", select = 3)
  plot.gam(gam_model_512, xlab=c("Latitude"), ylab="Lobster density", select = 4)
  plot.gam(gam_model_512, xlab=c("Longitude"), ylab="Lobster density", select = 5)
  dev.off()
  
  gam_model_513 <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(Salinity_psu, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data <- lobster_catch_data[which(polygon_points$Id=="513"& lobster_catch_data$Month>8),])
  summary(gam_model_513)
  
  jpeg(filename = paste("./plot/gam513_relationship.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
  par(mfrow=c(3,2), mar=c(4,4,1,1))
  plot.gam(gam_model_513, xlab=c("Depth"), ylab="Lobster density", select = 1)
  plot.gam(gam_model_513, xlab=c("Temperature"), ylab="Lobster density", select = 2)
  plot.gam(gam_model_513, xlab=c("Salinity"), ylab="Lobster density", select = 3)
  plot.gam(gam_model_513, xlab=c("Latitude"), ylab="Lobster density", select = 4)
  plot.gam(gam_model_513, xlab=c("Longitude"), ylab="Lobster density", select = 5)
  dev.off()
} ## use fall data and separate data by statistical area

if(TRUE){
  trawl_points <- SpatialPoints(cbind(lobster_catch_data$end_lon, lobster_catch_data$end_lat), proj4string=CRS("+proj=longlat +datum=NAD83 +no_defs")) 
  polygon_points <- over(trawl_points , sa511_513)
  
  gam_model_511 <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data = lobster_catch_data[which(polygon_points$Id=="511" & lobster_catch_data$Year<2015 & (lobster_catch_data$Month==6 | lobster_catch_data$Month>8)),])
  summary(gam_model_511)
  
  jpeg(filename = paste("./plot/gam511_relationship.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
  par(mfrow=c(2,2), mar=c(4,4,1,1))
  plot.gam(gam_model_511, xlab=c("Depth"), ylab="Lobster density", select = 1)
  plot.gam(gam_model_511, xlab=c("Temperature"), ylab="Lobster density", select = 2)
  #plot.gam(gam_model_511, xlab=c("Salinity"), ylab="Lobster density", select = 3)
  plot.gam(gam_model_511, xlab=c("Latitude"), ylab="Lobster density", select = 3)
  plot.gam(gam_model_511, xlab=c("Longitude"), ylab="Lobster density", select = 4)
  dev.off()
  
  gam_model_512 <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data = lobster_catch_data[which(polygon_points$Id=="512"& lobster_catch_data$Year<2015 & (lobster_catch_data$Month==6 |lobster_catch_data$Month>8)),])
  summary(gam_model_512)
  
  jpeg(filename = paste("./plot/gam512_relationship.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
  par(mfrow=c(2,2), mar=c(4,4,1,1))
  plot.gam(gam_model_512, xlab=c("Depth"), ylab="Lobster density", select = 1)
  plot.gam(gam_model_512, xlab=c("Temperature"), ylab="Lobster density", select = 2)
  #plot.gam(gam_model_512, xlab=c("Salinity"), ylab="Lobster density", select = 3)
  plot.gam(gam_model_512, xlab=c("Latitude"), ylab="Lobster density", select = 3)
  plot.gam(gam_model_512, xlab=c("Longitude"), ylab="Lobster density", select = 4)
  dev.off()
  
  gam_model_513 <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data = lobster_catch_data[which(polygon_points$Id=="513"& lobster_catch_data$Year<2015 & (lobster_catch_data$Month==6 |lobster_catch_data$Month>8)),])
  summary(gam_model_513)
  
  jpeg(filename = paste("./plot/gam513_relationship.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
  par(mfrow=c(2,2), mar=c(4,4,1,1))
  plot.gam(gam_model_513, xlab=c("Depth"), ylab="Lobster density", select = 1)
  plot.gam(gam_model_513, xlab=c("Temperature"), ylab="Lobster density", select = 2)
  #plot.gam(gam_model_513, xlab=c("Salinity"), ylab="Lobster density", select = 3)
  plot.gam(gam_model_513, xlab=c("Latitude"), ylab="Lobster density", select = 3)
  plot.gam(gam_model_513, xlab=c("Longitude"), ylab="Lobster density", select = 4)
  dev.off()
  
  save(gam_model_511, gam_model_512, gam_model_513, file="./output/gam_fit_2006_2014.RData")
} ## use June and Fall data but separate data by statisitcal area

if(FALSE){
  trawl_points <- SpatialPoints(cbind(lobster_catch_data$end_lon, lobster_catch_data$end_lat), proj4string=CRS("+proj=longlat +datum=NAD83 +no_defs")) 
  polygon_points <- over(trawl_points , sa511_513)
  
  gam_model_511 = gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(Salinity_psu, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data = lobster_catch_data[which(polygon_points$Id=="511" & lobster_catch_data$Year<2014 & (lobster_catch_data$Month==6 | lobster_catch_data$Month==10)),])
  summary(gam_model_511)
  
  #gam_model_511 <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data = lobster_catch_data[which(polygon_points$Id=="511" & (lobster_catch_data$Month==6 | lobster_catch_data$Month==10)),])
  #summary(gam_model_511)
  
  jpeg(filename = paste("./plot/gam511_relationship.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
  par(mfrow=c(3,2), mar=c(4,4,1,1))
  plot.gam(gam_model_511, xlab=c("Depth"), ylab="Lobster density", select = 1)
  plot.gam(gam_model_511, xlab=c("Temperature"), ylab="Lobster density", select = 2)
  plot.gam(gam_model_511, xlab=c("Salinity"), ylab="Lobster density", select = 3)
  plot.gam(gam_model_511, xlab=c("Latitude"), ylab="Lobster density", select = 4)
  plot.gam(gam_model_511, xlab=c("Longitude"), ylab="Lobster density", select = 5)
  dev.off()
  
  gam_model_512 = gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(Salinity_psu, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data = lobster_catch_data[which(polygon_points$Id=="512"& lobster_catch_data$Year<2014 &(lobster_catch_data$Month==6 |lobster_catch_data$Month==10)),])
  summary(gam_model_512)
  
  jpeg(filename = paste("./plot/gam512_relationship.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
  par(mfrow=c(3,2), mar=c(4,4,1,1))
  plot.gam(gam_model_512, xlab=c("Depth"), ylab="Lobster density", select = 1)
  plot.gam(gam_model_512, xlab=c("Temperature"), ylab="Lobster density", select = 2)
  plot.gam(gam_model_512, xlab=c("Salinity"), ylab="Lobster density", select = 3)
  plot.gam(gam_model_512, xlab=c("Latitude"), ylab="Lobster density", select = 4)
  plot.gam(gam_model_512, xlab=c("Longitude"), ylab="Lobster density", select = 5)
  dev.off()
  
  gam_model_513 = gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(Salinity_psu, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data = lobster_catch_data[which(polygon_points$Id=="513"& lobster_catch_data$Year<2014 &(lobster_catch_data$Month==6 |lobster_catch_data$Month==9)),])
  summary(gam_model_513)
  
  jpeg(filename = paste("./plot/gam513_relationship.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
  par(mfrow=c(3,2), mar=c(4,4,1,1))
  plot.gam(gam_model_513, xlab=c("Depth"), ylab="Lobster density", select = 1)
  plot.gam(gam_model_513, xlab=c("Temperature"), ylab="Lobster density", select = 2)
  plot.gam(gam_model_513, xlab=c("Salinity"), ylab="Lobster density", select = 3)
  plot.gam(gam_model_513, xlab=c("Latitude"), ylab="Lobster density", select = 4)
  plot.gam(gam_model_513, xlab=c("Longitude"), ylab="Lobster density", select = 5)
  dev.off()
} ## use June and September data but separate data by statistical area

if(FALSE){
  trawl_points <- SpatialPoints(cbind(lobster_catch_data$end_lon, lobster_catch_data$end_lat), proj4string=CRS("+proj=longlat +datum=NAD83 +no_defs")) 
  polygon_points <- over(trawl_points , sa511_513)
  
  gam_model_511512 <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(Salinity_psu, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data = lobster_catch_data[which((polygon_points$Id=="511" | polygon_points$Id=="512") & lobster_catch_data$Year<2014 & (lobster_catch_data$Month==6 | lobster_catch_data$Month==10)),])
  summary(gam_model_511512)
  
  #gam_model_511 <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data = lobster_catch_data[which(polygon_points$Id=="511" & (lobster_catch_data$Month==6 | lobster_catch_data$Month==10)),])
  #summary(gam_model_511)
  
  jpeg(filename = paste("./plot/gam511512_relationship.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
  par(mfrow=c(3,2), mar=c(4,4,1,1))
  plot.gam(gam_model_511512, xlab=c("Depth"), ylab="Lobster density", select = 1)
  plot.gam(gam_model_511512, xlab=c("Temperature"), ylab="Lobster density", select = 2)
  plot.gam(gam_model_511512, xlab=c("Salinity"), ylab="Lobster density", select = 3)
  plot.gam(gam_model_511512, xlab=c("Latitude"), ylab="Lobster density", select = 4)
  plot.gam(gam_model_511512, xlab=c("Longitude"), ylab="Lobster density", select = 5)
  dev.off()
  
  gam_model_513 <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(Salinity_psu, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data = lobster_catch_data[which(polygon_points$Id=="513"& lobster_catch_data$Year<2014 &(lobster_catch_data$Month==6 |lobster_catch_data$Month==9)),])
  summary(gam_model_513)
  
  jpeg(filename = paste("./plot/gam513_relationship.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
  par(mfrow=c(3,2), mar=c(4,4,1,1))
  plot.gam(gam_model_513, xlab=c("Depth"), ylab="Lobster density", select = 1)
  plot.gam(gam_model_513, xlab=c("Temperature"), ylab="Lobster density", select = 2)
  plot.gam(gam_model_513, xlab=c("Salinity"), ylab="Lobster density", select = 3)
  plot.gam(gam_model_513, xlab=c("Latitude"), ylab="Lobster density", select = 4)
  plot.gam(gam_model_513, xlab=c("Longitude"), ylab="Lobster density", select = 5)
  dev.off()
} ## use June and September data but separate data by 2 statistical areas

if(FALSE){
  trawl_points <- SpatialPoints(cbind(lobster_catch_data$end_lon, lobster_catch_data$end_lat), proj4string=CRS("+proj=longlat +datum=NAD83 +no_defs")) 
  polygon_points <- over(trawl_points , sa511_513)
  
  gam_model_511512 <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(Salinity_psu, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data = lobster_catch_data[which((polygon_points$Id=="511" | polygon_points$Id=="512") & lobster_catch_data$Year<2014 & (lobster_catch_data$Month==6 | lobster_catch_data$Month>8)),])
  summary(gam_model_511512)
  
  #gam_model_511 <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data = lobster_catch_data[which(polygon_points$Id=="511" & (lobster_catch_data$Month==6 | lobster_catch_data$Month==10)),])
  #summary(gam_model_511)
  
  jpeg(filename = paste("./plot/gam511512_relationship.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
  par(mfrow=c(3,2), mar=c(4,4,1,1))
  plot.gam(gam_model_511512, xlab=c("Depth"), ylab="Lobster density", select = 1)
  plot.gam(gam_model_511512, xlab=c("Temperature"), ylab="Lobster density", select = 2)
  plot.gam(gam_model_511512, xlab=c("Salinity"), ylab="Lobster density", select = 3)
  plot.gam(gam_model_511512, xlab=c("Latitude"), ylab="Lobster density", select = 4)
  plot.gam(gam_model_511512, xlab=c("Longitude"), ylab="Lobster density", select = 5)
  dev.off()
  
  gam_model_513 <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(Salinity_psu, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data = lobster_catch_data[which(polygon_points$Id=="513"& lobster_catch_data$Year<2014 &(lobster_catch_data$Month==6 |lobster_catch_data$Month>8)),])
  summary(gam_model_513)
  
  jpeg(filename = paste("./plot/gam513_relationship.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
  par(mfrow=c(3,2), mar=c(4,4,1,1))
  plot.gam(gam_model_513, xlab=c("Depth"), ylab="Lobster density", select = 1)
  plot.gam(gam_model_513, xlab=c("Temperature"), ylab="Lobster density", select = 2)
  plot.gam(gam_model_513, xlab=c("Salinity"), ylab="Lobster density", select = 3)
  plot.gam(gam_model_513, xlab=c("Latitude"), ylab="Lobster density", select = 4)
  plot.gam(gam_model_513, xlab=c("Longitude"), ylab="Lobster density", select = 5)
  dev.off()
} ## use June and fall data but separate data by 2 statistical areas
#### Seasonal GAM prediction ability check####
reference_fall_data <- lobster_catch_data[which(lobster_catch_data$Month>8),c("end_depth", "Water_Temp_DegC", "Salinity_psu", "end_lat", "end_lon", "juv_num", "Year", "Month")]
reference_spring_data <- lobster_catch_data[which(lobster_catch_data$Month<7),c("end_depth", "Water_Temp_DegC", "Salinity_psu", "end_lat", "end_lon", "juv_num", "Year", "Month")]

summary(lobster_catch_data$juv_num)
spring_model <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(Salinity_psu, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data = lobster_catch_data[which(lobster_catch_data$Month<7),])
summary(spring_model)
jpeg(filename = paste("./plot/spring_gam_check.jpeg", sep=""), width=130, height=130, units = "mm", res = 800)
par(mfrow=c(2,2))
gam.check(spring_model, pch=19, cex=0.3)
dev.off()

spring_res <- spring_model$fitted.values-reference_spring_data$juv_num
jpeg(filename = paste("./plot/spring_gam_boxplot.jpeg", sep=""), width=120, height=100, units = "mm", res = 800)
par(mfrow=c(2,1), mar=c(4,4,1,1))
boxplot(log(spring_res)~reference_spring_data$Year, ylab="Residuals (log #)", xlab="Year", pch=19, cex=0.3)
boxplot(log(spring_res)~reference_spring_data$Month, ylab="Residuals (log #)", xlab="Month", pch=19, cex=0.3)
dev.off()

jpeg(filename = paste("./plot/spring_gam_relationship.jpeg", sep=""), width=100, height=100, units = "mm", res = 800)
par(mfrow=c(3,2), mar=c(4,4,1,1))
plot.gam(spring_model, xlab=c("Depth"), ylab="Lobster density", select = 1)
plot.gam(spring_model, xlab=c("Temperature"), ylab="Lobster density", select = 2)
plot.gam(spring_model, xlab=c("Salinity"), ylab="Lobster density", select = 3)
plot.gam(spring_model, xlab=c("Latitude"), ylab="Lobster density", select = 4)
plot.gam(spring_model, xlab=c("Longitude"), ylab="Lobster density", select = 5)
dev.off()

spring_pred_fall_model <- predict(spring_model, newdata = reference_fall_data, se.fit=T, type="response")
spring_pred_spring_model <- predict(spring_model, newdata = reference_spring_data, se.fit=T, type="response")

fall_model <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(Salinity_psu, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data = lobster_catch_data[which(lobster_catch_data$Month>8),])
summary(fall_model)

fall_model <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data = lobster_catch_data[which(lobster_catch_data$Month>8),])
summary(fall_model)
jpeg(filename = paste("./plot/fall_gam_check.jpeg", sep=""), width=130, height=130, units = "mm", res = 800)
par(mfrow=c(2,2))
gam.check(fall_model, pch=19, cex=0.3)
dev.off()

fall_res <- fall_model$fitted.values-reference_fall_data$juv_num
jpeg(filename = paste("./plot/fall_gam_boxplot.jpeg", sep=""), width=120, height=100, units = "mm", res = 800)
par(mfrow=c(2,1), mar=c(4,4,1,1))
boxplot(log(fall_res)~reference_fall_data$Year, ylab="Residuals (log #)", xlab="Year", pch=19, cex=0.3)
boxplot(log(fall_res)~reference_fall_data$Month, ylab="Residuals (log #)", xlab="Month", pch=19, cex=0.3)
dev.off()

jpeg(filename = paste("./plot/fall_gam_relationship.jpeg", sep=""), width=100, height=80, units = "mm", res = 800)
par(mfrow=c(2,2), mar=c(4,4,1,1))
plot.gam(fall_model, xlab=c("Depth"), ylab="Lobster density", select = 1)
plot.gam(fall_model, xlab=c("Temperature"), ylab="Lobster density", select = 2)
plot.gam(fall_model, xlab=c("Latitude"), ylab="Lobster density", select = 3)
plot.gam(fall_model, xlab=c("Longitude"), ylab="Lobster density", select = 4)
dev.off()

fall_pred_fall_model <- predict(fall_model, newdata = reference_fall_data, se.fit=T, type="response")
fall_pred_spring_model <- predict(fall_model, newdata = reference_spring_data, se.fit=T, type="response")

#### Reference and Prediction correlation ####
cor_data <- cbind(reference_spring_data$juv_num, spring_pred_spring_model$fit, fall_pred_spring_model$fit)
colnames(cor_data) <- c("Spring Reference", "Spring Model Prediction", "Fall Model Predction")
bai_cor <- function (R, histogram = TRUE, method = c("pearson", "kendall", "spearman"), ...) {
  x = checkData(R, method = "matrix")
  if (missing(method)) 
    method = method[1]
  panel.cor <- function(x, y, digits = 2, prefix = "", use = "pairwise.complete.obs", 
                        method = "pearson", cex.cor, ...) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- cor(x, y, use = use, method = method)
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste(prefix, txt, sep = "")
    if (missing(cex.cor)) 
      cex <- 0.8/strwidth(txt)
    test <- cor.test(as.numeric(x), as.numeric(y), method = method)
    Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                     cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", 
                                                                              "**", "*", ".", " "))
    text(0.5, 0.5, txt, cex = cex * (abs(r) + 0.3)/1.3)
    text(0.8, 0.8, Signif, cex = cex, col = 2)
  }
  f <- function(t) {
    dnorm(t, mean = mean(x), sd = sd.xts(x))
  }
  dotargs <- list(...)
  dotargs$method <- NULL
  rm(method)
  hist.panel = function(x, ... = NULL) {
    par(new = TRUE)
    hist(x, col = "light gray", probability = TRUE, axes = FALSE, 
         main = "", breaks = 5)
    lines(density(x, na.rm = TRUE), col = "red", lwd = 1)
    rug(x)
  }
  if (histogram) 
    pairs(x, gap = 0, lower.panel = panel.smooth, upper.panel = panel.cor, 
          diag.panel = hist.panel)
  else pairs(x, gap = 0, lower.panel = panel.smooth, upper.panel = panel.cor)
}

# jpeg(filename = paste("./plot/spring_prediction_reference_correlation.jpeg", sep=""), width=100, height=100, units = "mm", res = 800)
# bai_cor(cor_data, histogram=TRUE, pch=19)
# dev.off()

cor_data <- cbind(reference_fall_data$juv_num, spring_pred_fall_model$fit, fall_pred_fall_model$fit)
colnames(cor_data) <- c("Fall Reference", "Spring Model Prediction", "Fall Model Predction")
bai_cor <- function (R, histogram = TRUE, method = c("pearson", "kendall", "spearman"), ...) {
  x = checkData(R, method = "matrix")
  if (missing(method)) 
    method = method[1]
  panel.cor <- function(x, y, digits = 2, prefix = "", use = "pairwise.complete.obs", 
                        method = "pearson", cex.cor, ...) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- cor(x, y, use = use, method = method)
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste(prefix, txt, sep = "")
    if (missing(cex.cor)) 
      cex <- 0.8/strwidth(txt)
    test <- cor.test(as.numeric(x), as.numeric(y), method = method)
    Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                     cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", 
                                                                              "**", "*", ".", " "))
    text(0.5, 0.5, txt, cex = cex * (abs(r) + 0.3)/1.3)
    text(0.8, 0.8, Signif, cex = cex, col = 2)
  }
  f <- function(t) {
    dnorm(t, mean = mean(x), sd = sd.xts(x))
  }
  dotargs <- list(...)
  dotargs$method <- NULL
  rm(method)
  hist.panel = function(x, ... = NULL) {
    par(new = TRUE)
    hist(x, col = "light gray", probability = TRUE, axes = FALSE, 
         main = "", breaks = 5)
    lines(density(x, na.rm = TRUE), col = "red", lwd = 1)
    rug(x)
  }
  if (histogram) 
    pairs(x, gap = 0, lower.panel = panel.smooth, upper.panel = panel.cor, 
          diag.panel = hist.panel)
  else pairs(x, gap = 0, lower.panel = panel.smooth, upper.panel = panel.cor)
}

# jpeg(filename = paste("./plot/fall_prediction_reference_correlation.jpeg", sep=""), width=100, height=100, units = "mm", res = 800)
# bai_cor(cor_data, histogram=TRUE, pch=19)
# dev.off()

#### Reference and Prediction time series ####
spring_reference_mean <- aggregate(reference_spring_data$juv_num, by=list(reference_spring_data$Year),  mean, na.rm=T)
spring_reference_median <- aggregate(reference_spring_data$juv_num, by=list(reference_spring_data$Year),  median, na.rm=T)
spring_pred_spring_mean <- aggregate(spring_pred_spring_model$fit, by=list(reference_spring_data$Year), mean, na.rm=T)
spring_pred_spring_median <- aggregate(spring_pred_spring_model$fit, by=list(reference_spring_data$Year), median, na.rm=T)
fall_pred_spring_mean <- aggregate(fall_pred_spring_model$fit, by=list(reference_spring_data$Year), mean, na.rm=T)
fall_pred_spring_median <- aggregate(fall_pred_spring_model$fit, by=list(reference_spring_data$Year), median, na.rm=T)

fall_reference_mean <- aggregate(reference_fall_data$juv_num, by=list(reference_fall_data$Year),  mean, na.rm=T)
fall_reference_median <- aggregate(reference_fall_data$juv_num, by=list(reference_fall_data$Year),  median, na.rm=T)
spring_pred_fall_mean <- aggregate(spring_pred_fall_model$fit, by=list(reference_fall_data$Year), mean, na.rm=T)
spring_pred_fall_median <- aggregate(spring_pred_fall_model$fit, by=list(reference_fall_data$Year), median, na.rm=T)
fall_pred_fall_mean <- aggregate(fall_pred_fall_model$fit, by=list(reference_fall_data$Year), mean, na.rm=T)
fall_pred_fall_median <- aggregate(fall_pred_fall_model$fit, by=list(reference_fall_data$Year), median, na.rm=T)

jpeg(filename = paste("./plot/spring_mean_median.jpeg", sep=""), width=100, height=100, units = "mm", res = 800)
par(mfrow=c(2,1), mar=c(2,4,1,1))
plot(spring_reference_median, type="o", lty=1, col="black", pch=1, ylim=range(spring_reference_median$x, fall_pred_spring_median$x), xlab="Year", ylab="Median Lobster Density")
lines(spring_pred_spring_median, type="o", lty=2, col="blue", pch=2)
lines(fall_pred_spring_median, type="o", lty=3, col="red", pch=3)
legend("topleft", c("Spring Reference", "Spring Model Prediction", "Fall Model Prediction"), pch=c(1,2,3), lty=c(1,2,3), col=c("black", "blue", "red"), bty="n", cex=0.5)

plot(spring_reference_mean, type="o", lty=1, col="black", pch=1, ylim=range(spring_reference_mean$x,fall_pred_spring_mean$x ), xlab="Year", ylab="Mean Lobster Density")
lines(spring_pred_spring_mean, type="o", lty=2, col="blue", pch=2)
lines(fall_pred_spring_mean, type="o", lty=3, col="red", pch=3)
legend("topleft", c("Spring Reference", "Spring Model Prediction", "Fall Model Prediction"), pch=c(1,2,3), lty=c(1,2,3), col=c("black", "blue", "red"), bty="n", cex=0.5)
dev.off()

jpeg(filename = paste("./plot/fall_mean_median.jpeg", sep=""), width=100, height=100, units = "mm", res = 800)
par(mfrow=c(2,1), mar=c(2,4,1,1))
plot(fall_reference_median, type="o", lty=1, col="black", pch=1, ylim=range(fall_reference_median$x,spring_pred_fall_median$x ), xlab="Year", ylab="Median Lobster Density")
lines(spring_pred_fall_median, type="o", lty=2, col="blue", pch=2)
lines(fall_pred_fall_median, type="o", lty=3, col="red", pch=3)
legend("topleft", c("Fall Reference", "Spring Model Prediction", "Fall Model Prediction"), pch=c(1,2,3), lty=c(1,2,3), col=c("black", "blue", "red"), bty="n", cex=0.5)

plot(fall_reference_mean, type="o", lty=1, col="black", pch=1, ylim=range(fall_reference_mean$x, spring_pred_fall_mean$x), xlab="Year", ylab="Mean Lobster Density")
lines(spring_pred_fall_mean, type="o", lty=2, col="blue", pch=2)
lines(fall_pred_fall_mean, type="o", lty=3, col="red", pch=3)
legend("topleft", c("Fall Reference", "Spring Model Prediction", "Fall Model Prediction"), pch=c(1,2,3), lty=c(1,2,3), col=c("black", "blue", "red"), bty="n", cex=0.5)
dev.off()

#### Overall mean and median residuals ####
summary(spring_pred_spring_model$fit-reference_spring_data$juv_num)
summary(fall_pred_spring_model$fit-reference_spring_data$juv_num)
summary(spring_pred_spring_model$fit-fall_pred_spring_model$fit)
hist((spring_pred_spring_model$fit-fall_pred_spring_model$fit))

summary(spring_pred_fall_model$fit-reference_fall_data$juv_num)
summary(fall_pred_fall_model$fit-reference_fall_data$juv_num)
summary(spring_pred_fall_model$fit-fall_pred_fall_model$fit)
hist(spring_pred_fall_model$fit-fall_pred_fall_model$fit)

#### Grid time ID ####
yrs<-2006:2016
dates<-as.data.frame(matrix(NA,length(yrs)*3,3))
colnames(dates)<-c('y','m','d')
dates$y <- rep(yrs, each=3)
dates$d<-rep(16,length(yrs))
dates$m<-rep(c(6,7,8),length(yrs))
dates$date <- do.call(paste, list(dates$m, dates$d, dates$y))
dates$date <- as.Date(dates$date, format=c("%m %d %Y"))
dates$julian_date <- as.numeric(dates$date) + 2440588
dates$modified_julian_date <- floor(dates$julian_date-2400000.5)
dates$modified_julian_date <- dates$modified_julian_date+0.5 
#### Download FVCOM time ID ####
time_id <- c()
for (i in 1:nrow(dates)){
  fvcom_data <-as.data.frame(read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom3_", dates$y[i], "0", dates$m[i], ".nc.ascii?time[0:1:720]", sep="")))
   temp <- as.data.frame(fvcom_data$Dataset..[5:nrow(fvcom_data)])
  names(temp) <-"modified_julian_date"
  temp$modified_julian_date <- as.numeric(as.character(temp$modified_julian_date))
  temp$id <- 0:(nrow(temp)-1)
  if(length(temp)==0) {
    time_id[i] <- NA
  } else {
    time_id[i] <- temp$id[temp$modified_julian_date==dates$modified_julian_date[i]]
  }
}
#### Download FVCOM location data ####
lat <- read.csv("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom3_200606.nc.ascii?lat[0:1:48450]")
lon <- read.csv("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom3_200606.nc.ascii?lon[0:1:48450]")
names(lat) <- "lat"
names(lon) <- "lon"

latitude <- lat$lat[5:nrow(lat)]
longitude <- lon$lon[5:nrow(lon)]
latitude <- as.numeric(as.character(latitude))
longitude <- as.numeric(as.character(longitude))
#### Grid location####
start_x <- floor(range(lobster_catch_data$end_lon)[1])
end_x <- ceiling(range(lobster_catch_data$end_lon)[2])
start_y <- floor(range(lobster_catch_data$end_lat)[1])
end_y <- ceiling(range(lobster_catch_data$end_lat)[2])
my_mesh=expand.grid(seq(start_x, end_x, by=0.01), seq(start_y, end_y, by=0.01))
coordinates(my_mesh) <- ~Var1 + Var2
grid <- aggregate(vts_gom_strata)
grid <- disaggregate(grid)

boundary_polygon = list()
for(i in 1:length(grid@polygons)){
  boundary <- grid@polygons[[i]]@Polygons[[1]]@coords
  colnames(boundary) <- c("lon", "lat")
  boundary_polygon[[i]] <- Polygon(boundary)
  boundary_list <- Polygons(boundary_polygon, paste("boundary",i))
  boundary_line <- SpatialPolygons(list(boundary_list))
}
#plot(boundary_line, axes=T)
gom_grid=crop(my_mesh, boundary_line)
#gom_grid=crop(my_mesh, sa511_513)
#grid <- aggregate(vts_gom_strata)
#gom_grid=crop(my_mesh, vts_gom_strata)
#gom_grid <- intersect(my_mesh,extent(grid))
grid_data <-as.data.frame(gom_grid@coords)
colnames(grid_data) <-c("lon", "lat")
grid_data <- grid_data[which(grid_data$lat>42.9),]

#### Download grid depth/temperature/salinity data####
##save time if(TRUE){
  #### Download depth data ####
  depth_raster <- raster("./data/gis/ne_atl_crm_v1.asc")
  sub_depth_raster <- crop(depth_raster, gom_grid)
  grid_data$depth <- extract.data(grid_data, sub_depth_raster)
  grid_data$fathom <- grid_data$depth/1.8288
  write.csv(grid_data, file="./output/grid_depth_data.csv")
  
  #### Download temperature data ####
  temperature_fvcom_data<-list()
  for (i in 1:length(time_id)){
    print(i)
    temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom3_", dates$y[i], "0", dates$m[i], ".nc.ascii?temp[", time_id[i], ":1:", time_id[i], "][44:1:44][0:1:48450]", sep=""))
    ###http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom3_200606.nc.ascii?temp[720:1:720][44:1:44][0:1:48450]
    temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
    temperature_fvcom_data[[i]] <- cbind(longitude, latitude, temp_data)
    colnames(temperature_fvcom_data[[i]]) <- c("lon", "lat", "temperature")
  }
  save(temperature_fvcom_data, file="./data/temperature_data.RData")
  
  load("./data/temperature_data.RData")
  temperature_raster_data <- list()
  grid_data <-as.data.frame(gom_grid@coords)
  colnames(grid_data) <-c("lon", "lat")
  grid_data <- grid_data[which(grid_data$lat>42.9),]
  for(i in 1:length(time_id)){
    print(i)
    temp_data <- as.data.frame(temperature_fvcom_data[[i]])
    rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
    rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
    akima.smooth <- with(temp_data, interp(lon, lat, temperature, nx=rast_col, ny=rast_row))
    rast <- raster(akima.smooth)
    temperature_raster_data[[i]] <- extract.data(grid_data, rast)
  }
  save(temperature_raster_data, file="./output/temperature_raster_data.RData")
  #### Download salinity data ####
  salinity_fvcom_data<-list()
  for (i in 1:length(time_id)){
    print(i)
    temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom3_", dates$y[i], "0", dates$m[i], ".nc.ascii?salinity[", time_id[i], ":1:", time_id[i], "][44:1:44][0:1:48450]", sep=""))
    temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
    salinity_fvcom_data[[i]] <- cbind(longitude, latitude, temp_data)
    colnames(salinity_fvcom_data[[i]]) <- c("lon", "lat", "salinity")
  }
  save(salinity_fvcom_data, file="./data/salinity_data.RData")
  
  load("./data/salinity_data.RData")
  salinity_raster_data <- list()
  grid_data <-as.data.frame(gom_grid@coords)
  colnames(grid_data) <-c("lon", "lat")
  grid_data <- grid_data[which(grid_data$lat>42.9),]
  for(i in 1:length(time_id)){
    print(i)
    temp_data <- as.data.frame(salinity_fvcom_data[[i]])
    rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
    rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
    akima.smooth <- with(temp_data, interp(lon, lat, salinity, nx=rast_col, ny=rast_row))
    rast <- raster(akima.smooth)
    salinity_raster_data[[i]] <- extract.data(grid_data, rast)
  }
  save(salinity_raster_data, file="./output/salinity_raster_data.RData")

#### Plot depth grid map ####
grid_data <- read.csv(file="./output/grid_depth_data.csv")
depth_grid_plot <- read.csv("./output/grid_depth_data.csv")
plot_data <- as.data.frame(cbind(depth_grid_plot$lon, depth_grid_plot$lat, -depth_grid_plot$fathom))
colnames(plot_data) <- c("Longitude", "Latitude", "Y")
depth_odd_id <- which(plot_data$Y<=0)
summary(plot_data)
plot_data <- na.omit(plot_data)
plot_data <- plot_data[which(plot_data$Y>0),]
summary(plot_data)

plotvar <- plot_data$Y
nclr=8
plotclr <- brewer.pal(nclr+1,"Blues")[2:length(brewer.pal(nclr+1,"Blues"))]
class <- classIntervals(plotvar, nclr, style="equal")
class$brks = round(class$brks, digits=2)
colcode <- findColours(class, plotclr)

start_x <- range(lobster_catch_data$end_lon)[1]
end_x <- range(lobster_catch_data$end_lon)[2]
start_y <- range(lobster_catch_data$end_lat)[1]
end_y <- range(lobster_catch_data$end_lat)[2]
jpeg(filename = paste("./plot/grid_depth_map.jpeg", sep=""), width=100, height=50, units = "mm", res = 600)
layout(matrix(c(1,1,1,2), 1, 4, byrow = TRUE))
par(mar=c(4,4,1,1))
plot(plot_data$Longitude, plot_data$Latitude, pch=16, col=colcode, cex=0.1, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, xlab="Longitude", ylab="Latitude")
map(database = "worldHires", ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE, lwd=0.1)
box()
degAxis(1)
degAxis(2)
par(mar=c(0.5,0.5,0.5,0.5))
plot.new()
legend("left", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=0.75, bty="n", title="Depth (Fathom)")
dev.off()

#### Plot temperature grid map ####
load("./output/temperature_raster_data.RData")

jpeg(filename = paste("./plot/grid_temperature_map.jpeg", sep=""), width=120, height=140, units = "mm", res = 600)
m <- cbind(rep(34, 13), matrix(c(37, 37, 37, 1:33, 36, 36, 36), nrow=13, ncol=3, byrow=T), rep(35, 13))
layout(m)
par(mar=c(0,0,0,0))
plotvar=unlist(temperature_raster_data)
#na_id <- which(is.na(plotvar))
#plotvar <- plotvar[-na_id]
nclr=10
plotclr <- rev(brewer.pal(nclr,"RdYlBu"))
class <- classIntervals(plotvar, nclr, style="equal")
fix_break<-round(class$brks, digits = 2)
for(i in 1:length(temperature_raster_data)){
  print(i)
  #na_id <- which(is.na(temperature_raster_data[[i]]))
  #plotvar <- temperature_raster_data[[i]][-na_id]
  plotvar <- temperature_raster_data[[i]]
  nclr=10
  plotclr <- rev(brewer.pal(nclr,"RdYlBu"))
  class <- classIntervals(plotvar, nclr, style="fixed", fixedBreaks=fix_break)
  colcode <- findColours(class, plotclr)
  
  start_x <- range(lobster_catch_data$end_lon)[1]
  end_x <- range(lobster_catch_data$end_lon)[2]
  start_y <- range(lobster_catch_data$end_lat)[1]
  end_y <- range(lobster_catch_data$end_lat)[2]
  plot(depth_grid_plot$lon, depth_grid_plot$lat, pch=16, col=colcode, cex=0.1, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F)
  map(database = "worldHires", ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE, lwd=0.1)
  #plot(sa511_513, add=T)
  box()
  legend("topleft", paste(dates$y[i], "-", dates$m[i], sep=""), bty="n", cex=0.7)
  
  if (i == (length(temperature_fvcom_data)-3+1)) {
    axis(1, cex=0.5)
  }
  if (i == length(temperature_fvcom_data)) {
    axis(1, cex=0.5)
  }
  if (i == 2) axis(3, cex=0.5)
  else {
    if ((i+5)%%6 == 0) axis(2, cex=0.5)
    if (i%%6 == 0) axis(4, cex=0.5)
  }
  
  if (i == 16) mtext("Latitude", side=2, line=2.2)
  if (i == 32) mtext("Longitude", side=1, line=2.2)
}

par(mar=c(0.5,0.5,0.5,0.5))
plot.new()

par(mar=c(0.1,2,0.1,0.1))
plot.new()
legend("left", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=0.7, bty="n", title="Temperature (°C)")
dev.off()

#### Plot salinity grid map ####
load("./output/salinity_raster_data.RData")

jpeg(filename = paste("./plot/grid_salinity_map.jpeg", sep=""), width=120, height=140, units = "mm", res = 600)
m <- cbind(rep(34, 13), matrix(c(37, 37, 37, 1:33, 36, 36, 36), nrow=13, ncol=3, byrow=T), rep(35, 13))
layout(m)
par(mar=c(0,0,0,0))

plotvar=unlist(salinity_raster_data)
nclr=10
plotclr <- rev(brewer.pal(nclr,"RdYlBu"))
class <- classIntervals(plotvar, nclr, style="quantile")
fix_break<-round(class$brks, digits = 2)
for(i in 1:length(salinity_raster_data)){
  print(i)
  plotvar <- salinity_raster_data[[i]]
  
  nclr=10
  plotclr <- rev(brewer.pal(nclr,"RdYlBu"))
  class <- classIntervals(plotvar, nclr, style="fixed", fixedBreaks=fix_break)
  colcode <- findColours(class, plotclr)
  
  start_x <- range(lobster_catch_data$end_lon)[1]
  end_x <- range(lobster_catch_data$end_lon)[2]
  start_y <- range(lobster_catch_data$end_lat)[1]
  end_y <- range(lobster_catch_data$end_lat)[2]
  plot(depth_grid_plot$lon, depth_grid_plot$lat, pch=16, col=colcode, cex=0.1, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F)
  map(database = "worldHires", ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE, lwd=0.1)
  #plot(sa511_513, add=T)
  box()
  legend("topleft", paste(dates$y[i], "-", dates$m[i], sep=""), bty="n", cex=0.7)
  
  if (i == (length(temperature_fvcom_data)-3+1)) {
    axis(1, cex=0.5)
  }
  if (i == length(temperature_fvcom_data)) {
    axis(1, cex=0.5)
  }
  if (i == 2) axis(3, cex=0.5)
  else {
    if ((i+5)%%6 == 0) axis(2, cex=0.5)
    if (i%%6 == 0) axis(4, cex=0.5)
  }
  
  if (i == 16) mtext("Latitude", side=2, line=2.2)
  if (i == 32) mtext("Longitude", side=1, line=2.2)
}

par(mar=c(0.5,0.5,0.5,0.5))
plot.new()

par(mar=c(0.1,2,0.1,0.1))
plot.new()
legend("left", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=0.7, bty="n", title="Salinity (psu)")
dev.off()


#### Prediction ####
grid_data <- read.csv(file="./output/grid_depth_data.csv")
depth_grid_plot <- read.csv("./output/grid_depth_data.csv")
load("./output/temperature_raster_data.RData")
load("./output/salinity_raster_data.RData")

if (FALSE){
  prediction_mean <- list()
  prediction_se <- list()
  for (i in 1:nrow(dates)){
    print(i)
    temp_data <- as.data.frame(cbind(depth_grid_plot$fathom[which(depth_grid_plot$fathom<0)], temperature_raster_data[[i]][which(depth_grid_plot$fathom<0)], salinity_raster_data[[i]][which(depth_grid_plot$fathom<0)], depth_grid_plot$lat[which(depth_grid_plot$fathom<0)], depth_grid_plot$lon[which(depth_grid_plot$fathom<0)]))
    colnames(temp_data) <- c("end_depth", "Water_Temp_DegC", "Salinity_psu", "end_lat", "end_lon")
    temp_data$end_depth <- -temp_data$end_depth
    prediction_model <- predict(gam_model, newdata = temp_data, se.fit=T, type="response")
    rm(list="temp_data")
    gc()
    prediction_mean[[i]] <- prediction_model$fit
    prediction_se[[i]] <- prediction_model$se.fit
    rm(list="prediction_model")
    gc()
  }
} ## predict data without separating the statistical area

if(TRUE){
  prediction_mean <- list()
  prediction_se <- list()
  for (i in 1:nrow(dates)){
    print(i)
    temp_data <- as.data.frame(cbind(depth_grid_plot$fathom[which(depth_grid_plot$fathom<0)], temperature_raster_data[[i]][which(depth_grid_plot$fathom<0)], salinity_raster_data[[i]][which(depth_grid_plot$fathom<0)], depth_grid_plot$lat[which(depth_grid_plot$fathom<0)], depth_grid_plot$lon[which(depth_grid_plot$fathom<0)]))
    colnames(temp_data) <- c("end_depth", "Water_Temp_DegC", "Salinity_psu", "end_lat", "end_lon")
    
    temp_points <- SpatialPoints(cbind(temp_data$end_lon, temp_data$end_lat), proj4string=CRS("+proj=longlat +datum=NAD83 +no_defs"))
    #sa511_513_points <- SpatialPoints(cbind(sa511_513@bbox, sa511_513$), proj4string=CRS("+proj=longlat +datum=WGS84")) 
    polygon_points <- over(temp_points, sa511_513)
    
    temp_data$end_depth <- -temp_data$end_depth
    
    mean_val <- c()
    se_val <- c()
    for(j in 1:nrow(temp_data)){
      if (is.na(polygon_points$Id[j])) {
        mean_val[j] <-NA
        se_val[j] <- NA
      }
      else {
        if (polygon_points$Id[j]=="511") {
          mean_val[j] <- predict(gam_model_511, newdata = temp_data[j,], se.fit=T, type="response")$fit
          se_val[j] <- predict(gam_model_511, newdata = temp_data[j,], se.fit=T, type="response")$se.fit
        }
        if (polygon_points$Id[j]=="512") {
          mean_val[j] <- predict(gam_model_512, newdata = temp_data[j,], se.fit=T, type="response")$fit
          se_val[j] <- predict(gam_model_512, newdata = temp_data[j,], se.fit=T, type="response")$se.fit
        }
        if (polygon_points$Id[j]=="513") {
          mean_val[j] <- predict(gam_model_513, newdata = temp_data[j,], se.fit=T, type="response")$fit
          se_val[j] <- predict(gam_model_513, newdata = temp_data[j,], se.fit=T, type="response")$se.fit
        }
      }
    }
    
    rm(list="temp_data")
    gc()
    prediction_mean[[i]] <- mean_val
    prediction_se[[i]] <- se_val
    rm(list="prediction_model")
    gc()
  }
} ## predict data with three models separting the statistical area;final choice

if(FALSE){
  prediction_mean <- list()
  prediction_se <- list()
  for (i in 1:nrow(dates)){
    print(i)
    temp_data <- as.data.frame(cbind(depth_grid_plot$fathom[which(depth_grid_plot$fathom<0)], temperature_raster_data[[i]][which(depth_grid_plot$fathom<0)], salinity_raster_data[[i]][which(depth_grid_plot$fathom<0)], depth_grid_plot$lat[which(depth_grid_plot$fathom<0)], depth_grid_plot$lon[which(depth_grid_plot$fathom<0)]))
    colnames(temp_data) <- c("end_depth", "Water_Temp_DegC", "Salinity_psu", "end_lat", "end_lon")
    
    temp_points <- SpatialPoints(cbind(temp_data$end_lon, temp_data$end_lat), proj4string=CRS("+proj=longlat +datum=NAD83 +no_defs")) 
    polygon_points <- over(x=temp_points, y=sa511_513)
    
    temp_data$end_depth <- -temp_data$end_depth
    
    mean_val <- c()
    se_val <- c()
    for(j in 1:nrow(temp_data)){
      if (is.na(polygon_points$Id[j])) {
        mean_val[j] <-NA
        se_val[j] <- NA
      }
      else {
        if (polygon_points$Id[j]==511 | polygon_points$Id[j]==512) {
          mean_val[j] <- predict(gam_model_511512, newdata <- temp_data[j,], se.fit=T, type="response")$fit
          se_val[j] <- predict(gam_model_511512, newdata <- temp_data[j,], se.fit=T, type="response")$se.fit
        }
        
        if (polygon_points$Id[j]=="513") {
          mean_val[j] <- predict(gam_model_513, newdata <- temp_data[j,], se.fit=T, type="response")$fit
          se_val[j] <- predict(gam_model_513, newdata <- temp_data[j,], se.fit=T, type="response")$se.fit
        }
      }
    }
    
    rm(list="temp_data")
    gc()
    prediction_mean[[i]] <- mean_val
    prediction_se[[i]] <- se_val
    rm(list="prediction_model")
    gc()
  }
} ## predict data with two models separting the statistical area

save(prediction_mean, prediction_se, file="./output/prediction6_fall.RData")
#### 1000 predicted population ####
sim_num=1000
load("./output/prediction6_fall.RData")
if (TRUE){
  
  population_1000 <- list()
  
  for(i in 1:length(prediction_mean)){
    print(i)
    temp <- matrix(NA,nrow=length(prediction_mean[[i]]), ncol=sim_num)
    for (j in 1:nrow(temp)){
      temp_data <- rnorm(1500, mean=prediction_mean[[i]][j], sd=prediction_se[[i]][j])
      temp[j,] <- temp_data[which(temp_data>0)][1:sim_num]
    }
    population_1000[[i]] <- temp
  }
  save(population_1000, file="./output/population_1000.RData")
}

#### Load VTS individual lobster information ####
vts_lobster_data <- read.csv("./data/vts/VTS_biodata2006_2017.csv")
vts_trap_data <- read.csv("./data/vts/VTS_traps.csv")
#save(vts_lobster_data, file="./output/vts_trap_data.RData")
#load("./output/vts_trap_data.RData")
#### VTS abundance index ####
#summary(vts_lobster_data)
start_date <- as.Date(vts_lobster_data$TRIP_START_DATE, "%m/%d/%Y")
vts_lobster_data$month <- as.numeric(format(start_date, format="%m"))
vts_lobster_data$year <- as.numeric(format(start_date, format="%Y"))

trip_date <- as.Date(vts_trap_data$TRIP_START_DATE, "%m/%d/%Y")
vts_trap_data$month <- as.numeric(format(trip_date, format="%m"))
vts_trap_data$year <- as.numeric(format(trip_date, format="%Y"))
## lobster number per trap ##
sublegal_lobsters <- vts_lobster_data[which(vts_lobster_data$SAMPLE_LENGTH < 83 & vts_lobster_data$year < 2015 & (vts_lobster_data$TRAP_TYPE=="V" | vts_lobster_data$TRAP_TYPE=="v")),]

#lobster_per_trap <- aggregate(sublegal_lobsters$SAMPLE_SEQ_NO, by=list(sublegal_lobsters$TRAP_ID), length)

lobster_per_trap <- aggregate(sublegal_lobsters$SAMPLE_LENGTH, by=list(sublegal_lobsters$TRAP_ID, sublegal_lobsters$SET_OVER_DAYS), length)
lobster_per_trap$x <- round(lobster_per_trap$x/lobster_per_trap$Group.2)*3
lobster_per_trap <- as.data.frame(cbind(lobster_per_trap$Group.1, lobster_per_trap$x))
colnames(lobster_per_trap) <- c("Group.1", "x")

for (i in 1:nrow(vts_trap_data)){
  print(i)
  if (vts_trap_data$TRAP_ID[i] %in% lobster_per_trap$Group.1) vts_trap_data$quantity[i] <- lobster_per_trap$x[which(lobster_per_trap$Group.1==vts_trap_data$TRAP_ID[i])]
  else vts_trap_data$quantity[i] <- NA
}

vts_vtrap_data <- vts_trap_data[which(vts_trap_data$year<2015 & (vts_trap_data$TRAP_TYPE=="V" |vts_trap_data$TRAP_TYPE=="v")),]
vts_vtrap_data <- na.omit(vts_vtrap_data)
write.csv(vts_vtrap_data, "./data/vts/vts_vtrap_data_2006_2014.csv")

plot_data <- as.data.frame(cbind(vts_vtrap_data$LONGITUDE_DECIMAL, vts_vtrap_data$LATITUDE_DECIMAL, vts_vtrap_data$quantity))
colnames(plot_data) <- c("Longitude", "Latitude", "Y")


plotvar <- plot_data$Y
nclr=8
plotclr <- rev(brewer.pal(nclr,"RdYlBu"))
class <- classIntervals(plotvar, nclr, style="quantile")
class$brks = round(class$brks, digits=2)
colcode <- findColours(class, plotclr)

start_x <- range(lobster_catch_data$end_lon)[1]
end_x <- range(lobster_catch_data$end_lon)[2]
start_y <- range(lobster_catch_data$end_lat)[1]
end_y <- range(lobster_catch_data$end_lat)[2]
jpeg(filename = paste("./plot/vts_lobster_quantity_map.jpeg", sep=""), width=100, height=50, units = "mm", res = 600)
layout(matrix(c(1,1,1,2), 1, 4, byrow = TRUE))
par(mar=c(4,4,1,1))
plot(plot_data$Longitude, plot_data$Latitude, pch=16, col=colcode, cex=0.3, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, xlab="Longitude", ylab="Latitude")
map(database = "worldHires", ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE, lwd=0.1)
box()
degAxis(1)
degAxis(2)
par(mar=c(0.5,0.5,0.5,0.5))
plot.new()
legend("left", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=0.7, bty="n", title="Lobster Density (#/trap)")
dev.off()

lobster_per_year_sa_depth <- aggregate(vts_vtrap_data$quantity, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata), sum)
colnames(lobster_per_year_sa_depth) <- c("Year", "SA", "Depth", "Lob_Quantity") 

lobster_per_year_sa_depth$Trap_Quantity <- aggregate(vts_vtrap_data$TRAP_ID, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata), length)$x

lobster_per_year_sa_depth$Site_Quantity <- aggregate(vts_vtrap_data$SITE_ID, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata), function(x) length(unique(x)))$x
summary(lobster_per_year_sa_depth)

lobster_per_year_sa_depth$Ave_Lob_Trap <- lobster_per_year_sa_depth$Lob_Quantity/lobster_per_year_sa_depth$Trap_Quantity

area_per_sa_depth <- as.data.frame(matrix(c(122, 82, 92, 296, 566, 395, 420, 1381, 315, 338, 198, 851), nrow=3, byrow=T))
colnames(area_per_sa_depth) <- c("depth1", "depth2", "depth3", "total")
rownames(area_per_sa_depth) <- c("511", "512", "513")
## substrat_mean
substrat_mean <- matrix(NA, nrow=length(unique(lobster_per_year_sa_depth$Year)), ncol=3)
year_id <- unique(lobster_per_year_sa_depth$Year)
colnames(substrat_mean) <- c("511", "512", "513")
rownames(substrat_mean) <- year_id
for (i in 1:length(year_id)){
  substrat_mean[i,1] <- area_per_sa_depth["511", "depth1"]/area_per_sa_depth["511", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["511", "depth2"]/area_per_sa_depth["511", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="21 to 40"& lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["511", "depth3"]/area_per_sa_depth["511", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="41 to 60"& lobster_per_year_sa_depth$Year==year_id[i])] 
  
  substrat_mean[i,2] <- area_per_sa_depth["512", "depth1"]/area_per_sa_depth["512", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["512", "depth2"]/area_per_sa_depth["512", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="21 to 40"& lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["512", "depth3"]/area_per_sa_depth["512", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="41 to 60"& lobster_per_year_sa_depth$Year==year_id[i])] 
  
  substrat_mean[i,3] <- area_per_sa_depth["513", "depth1"]/area_per_sa_depth["513", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["513", "depth2"]/area_per_sa_depth["513", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="21 to 40"& lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["513", "depth3"]/area_per_sa_depth["513", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="41 to 60"& lobster_per_year_sa_depth$Year==year_id[i])] 
  
}

## substrat_variance
lobster_per_year_sa_depth_site <- aggregate(vts_vtrap_data$quantity, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata, vts_vtrap_data$SITE_ID), sum)
colnames(lobster_per_year_sa_depth_site) <- c("Year", "SA", "Depth", "Site", "Lob_Quantity") 

lobster_per_year_sa_depth_site$Trap_Quantity <- aggregate(vts_vtrap_data$TRAP_ID, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata, vts_vtrap_data$SITE_ID), length)$x

lobster_per_year_sa_depth_site$Quantity_per_Trap <- lobster_per_year_sa_depth_site$Lob_Quantity/lobster_per_year_sa_depth_site$Trap_Quantity

lobster_per_year_sa_depth_site=merge(lobster_per_year_sa_depth_site, lobster_per_year_sa_depth[, c("Year", "SA", "Depth", "Ave_Lob_Trap")], by=c("Year", "SA", "Depth"))

lobster_per_year_sa_depth_site$Sub <- (lobster_per_year_sa_depth_site$Ave_Lob_Trap - lobster_per_year_sa_depth_site$Quantity_per_Trap)^2

individual_value <- aggregate(lobster_per_year_sa_depth_site$Sub, by=list(lobster_per_year_sa_depth_site$Year, lobster_per_year_sa_depth_site$SA, lobster_per_year_sa_depth_site$Depth), sum)
colnames(individual_value) <- c("Year", "SA", "Depth", "x")
individual_value$individual_value <- individual_value$x/aggregate(lobster_per_year_sa_depth_site$Site, by=list(lobster_per_year_sa_depth_site$Year, lobster_per_year_sa_depth_site$SA, lobster_per_year_sa_depth_site$Depth), length)$x

substrat_var <- matrix(NA, nrow=length(unique(lobster_per_year_sa_depth$Year)), ncol=3)
colnames(substrat_var) <- c("511", "512", "513")
rownames(substrat_var) <- year_id

for (i in 1:length(year_id)){
  substrat_var[i,1] <- 1/area_per_sa_depth["511", "total"]^2*((area_per_sa_depth["511", "depth1"]*(area_per_sa_depth["511", "depth1"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["511", "depth2"]*(area_per_sa_depth["511", "depth2"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["511", "depth3"]*(area_per_sa_depth["511", "depth3"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])]))
  
  substrat_var[i,2] <- 1/area_per_sa_depth["512", "total"]^2*((area_per_sa_depth["512", "depth1"]*(area_per_sa_depth["512", "depth1"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["512", "depth2"]*(area_per_sa_depth["512", "depth2"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["512", "depth3"]*(area_per_sa_depth["512", "depth3"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])]))
  
  substrat_var[i,3] <- 1/area_per_sa_depth["513", "total"]^2*((area_per_sa_depth["513", "depth1"]*(area_per_sa_depth["513", "depth1"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["513", "depth2"]*(area_per_sa_depth["513", "depth2"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["513", "depth3"]*(area_per_sa_depth["513", "depth3"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])]))
}

save(substrat_mean, substrat_var, file="./output/vts_lobster_substrat_mean_real_2006_2014.RData")
jpeg(filename = "./plot/vts_lobster_substrat_mean_real.jpeg", width=150, height=50, units = "mm", res = 600)
par(mfrow=c(1,3))
ylim_min <- min(substrat_mean-substrat_var)
ylim_max <- max(substrat_mean+substrat_var)

plot(year_id, substrat_mean[,"513"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=0.5, cex=0.4, ylim=c(ylim_min, ylim_max))
polygon(x=c(year_id, rev(year_id)), y=c(substrat_mean[,"513"]+substrat_var[,"513"], rev(substrat_mean[,"513"]-substrat_var[,"513"])), col=adjustcolor("deepskyblue3", alpha.f = 0.20), border = NA)
legend("topleft", "513", bty="n")

plot(year_id, substrat_mean[,"512"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=0.5, cex=0.4, ylim=c(ylim_min, ylim_max))
polygon(x=c(year_id, rev(year_id)), y=c(substrat_mean[,"512"]+substrat_var[,"512"], rev(substrat_mean[,"512"]-substrat_var[,"512"])), col=adjustcolor("deepskyblue3", alpha.f = 0.20), border = NA)
legend("topleft", "512", bty="n")

plot(year_id, substrat_mean[,"511"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=0.5, cex=0.4, ylim=c(ylim_min, ylim_max))
polygon(x=c(year_id, rev(year_id)), y=c(substrat_mean[,"511"]+substrat_var[,"511"], rev(substrat_mean[,"511"]-substrat_var[,"511"])), col=adjustcolor("deepskyblue3", alpha.f = 0.20), border = NA)
legend("topleft", "511", bty="n")

dev.off()

#### simulated samples ####
depth_grid_plot <- read.csv("./output/grid_depth_data.csv")
prediction_points <- SpatialPoints(cbind(depth_grid_plot$lon[which(depth_grid_plot$fathom<0)], depth_grid_plot$lat[which(depth_grid_plot$fathom<0)]), proj4string=CRS("+proj=longlat +datum=NAD83 +no_defs")) 
vts_points <- SpatialPoints(cbind(vts_vtrap_data$LONGITUDE_DECIMAL, vts_vtrap_data$LATITUDE_DECIMAL), proj4string=CRS("+proj=longlat +datum=NAD83 +no_defs"))
##save time TRUE to FALSE
if(TRUE){
  nearest_pred_points <- c()
  # for (i in 1:length(vts_points)){
  #   print(i)
  #   nearest_pred_points[i] <- which.min(gDistance(vts_points[i], prediction_points, byid=T))
  #   gc()
  # }
  nearest_pred_points <- sapply(1:length(vts_points), function(x) which.min(gDistance(vts_points[x], prediction_points, byid=T)))
  save(nearest_pred_points, file="./data/vts/vts_nearest_prediction_point.RData")
}

if(TRUE){
  load("./output/population_1000.RData")
  load("./data/vts/vts_nearest_prediction_point.RData")
  #load("./data/vts/vts_nearest_prediction_point.RData")
  #load("./output/s5_prediction_fall_sa.RData")
  #load("./output/s5_prediction_spring6_fall9_sa.RData")
  dates$id <- 1:nrow(dates)
  simulated_vts_samples <- matrix(NA, nrow = length(vts_points), ncol=sim_num)
  for (i in 1:length(vts_points)){
    print(i)
    if(vts_vtrap_data[i,"year"]==2017) simulated_vts_samples[i,] <- rep(NA, sim_num)
    #if(vts_vtrap_data[i,"year"]==2017 | vts_vtrap_data[i,"year"]==2014) simulated_vts_samples[i,] <- rep(NA, sim_num)
    else {
      if(vts_vtrap_data[i,"month"]==9) time_id <- dates$id[which(dates$y==vts_vtrap_data[i,"year"] & dates$m == 8)]
      else {
        time_id <- dates$id[which(dates$y==vts_vtrap_data[i,"year"] & dates$m == vts_vtrap_data[i,"month"])]
      }
      temp_value <- population_1000[[time_id]][nearest_pred_points[i],]
      if (any(is.na(temp_value))) simulated_vts_samples[i,]<- rep(NA, sim_num)
      else simulated_vts_samples[i,]<- temp_value
    }
  }
  summary(simulated_vts_samples)
  save(simulated_vts_samples, file="./output/simulated_vts_samples_1000.RData")
  #vts_vtrap_data$sim_quantity <- simulated_vts_samples
  #summary(vts_vtrap_data$sim_quantity)
  #save(vts_vtrap_data, file="./output/vts_simulated_samples.RData")
}

#### Calculate simulated samples abundance index and index with consideration of q ####
if(FALSE){
  #### catchability over space ####
  #vts_vtrap_data$catchability<- vts_vtrap_data$quantity/vts_vtrap_data$sim_quantity
  vts_vtrap_data$catchability<- vts_vtrap_data$quantity/simulated_vts_samples[,sim]
  vts_vtrap_data[which(vts_vtrap_data$catchability>1),]
  
  temp_data <- na.omit(vts_vtrap_data)
  temp_data$month[(temp_data$month==9)] <- 8
  
  catchability_mean <- aggregate(temp_data$catchability, by=list(temp_data$year, temp_data$month, temp_data$STATAREA, temp_data$assigneddepthstrata), mean)
  catchability_sd <- aggregate(temp_data$catchability, by=list(temp_data$year, temp_data$month, temp_data$STATAREA, temp_data$assigneddepthstrata), sd)
  colnames(catchability_mean) <- c("year", "month", "STATAREA", "assigneddepthstrata", "mean")
  colnames(catchability_sd) <- c("year", "month", "STATAREA", "assigneddepthstrata", "sd")
  
  #jpeg(filename = "./plot/catchability_year_month_strata.jpeg", width=150, height=150, units = "mm", res = 600)
  #par(mfrow=c(2,2))
  #boxplot(catchability_mean$mean~catchability_mean$year, xlab="Year", ylab="Catchability", ylim=c(0,0.5), pch=16, cex=0.5)
  #boxplot(catchability_mean$mean~catchability_mean$month, xlab="Month", ylab="Catchability", ylim=c(0,0.5), pch=16, cex=0.5)
  #boxplot(catchability_mean$mean~catchability_mean$STATAREA, xlab="Statistical Area", ylab="Catchability", ylim=c(0,0.5), pch=16, cex=0.5)
  #boxplot(catchability_mean$mean~catchability_mean$assigneddepthstrata, xlab="Depth Strata", ylab="Catchability", ylim=c(0,0.5), xlim=c(1.5,4.5), pch=16, cex=0.5)
  #dev.off()
  #catchability_data <- na.omit(vts_vtrap_data)
  #catchability_data <- catchability_data[which(catchability_data$catchability>=0 & catchability_data$catchability<=1),]
  #catchability_data$month[(catchability_data$month==9)] <- 8
  
  catchability_data <- vts_vtrap_data
  
  
  if(TRUE){
    load("./output/temperature_raster_data.RData")
    load("./data/vts/vts_nearest_prediction_point.RData")
    temperature_catchability_data <- matrix(NA, nrow = nrow(catchability_data), ncol=sim_num)
    for(i in 1:nrow(catchability_data)){
      print(i)
      if(catchability_data[i,"year"]==2017 | catchability_data[i,"year"]==2014) temperature_catchability_data[i] <- NA
      else{
        if(catchability_data[i,"month"]==9) catchability_data_time <- dates$id[which(dates$y==catchability_data[i,"year"] & dates$m == 8)]
        else {
          catchability_data_time <- dates$id[which(dates$y==catchability_data[i,"year"] & dates$m == catchability_data[i,"month"])]
        }
        temp_data <- temperature_raster_data[[catchability_data_time]][which(depth_grid_plot$fathom<0)]
        temperature_catchability_data[i,sim] <- temp_data[nearest_pred_points[i]]
      }
    }
    
  }
  save(temperature_catchability_data, file="./output/temperature_catchability_data.RData")
  #summary(temperature_catchability_data)
  
  #load("./output/temperature_catchability_data.RData")
  
  #catchability_data$temperature <- temperature_catchability_data
  catchability_data$month[(catchability_data$month==9)] <- 8
  
  merge_data <- merge(catchability_data, catchability_mean, by=c("year","month", "STATAREA", "assigneddepthstrata"))
  catchability_data$q_mean <- merge_data$mean
  merge_data <- merge(catchability_data, catchability_sd, by=c("year","month", "STATAREA", "assigneddepthstrata"))
  catchability_data$q_sd <- merge_data$sd
  #catchability_data <- catchability_data[which(catchability_data$catchability>=0 & catchability_data$catchability<=1),]
  
  simulated_vts_samples_q <- c()
  for(i in 1:length(simulated_vts_samples)){
    if(is.na(simulated_vts_samples[i])) simulated_vts_samples_q[i] <- NA
    else{
      simulated_vts_samples_q[i] <- simulated_vts_samples[i]*rnorm(1, mean=catchability_data$q_mean[i], sd=catchability_data$q_sd[i])
      if(simulated_vts_samples_q[i]<0) simulated_vts_samples_q[i]<-simulated_vts_samples[i]*catchability_data$q_mean[i]
    }
  }
  
  vts_vtrap_data$sim_quantity_q <- simulated_vts_samples_q
  
  
  #catchability_model <- gam(catchability~s(temperature, k=5)+as.factor(month), data <- catchability_data[which(catchability_data$catchability>=0 & catchability_data$catchability<=1),], family=betar)
  
  #catchability_model <- gam(catchability~as.factor(STATAREA)+as.factor(year), data <- catchability_data, family=tw())
  
  #catchability_model <- gam(catchability~s(temperature, k=5)+s(DEPTH, k=5), data <- catchability_data, family=tw())
  #catchability_model <- gam(catchability~s(temperature, k=5)+s(DEPTH, k=5)+as.factor(STATAREA), data <- catchability_data, family=tw())
  #catchability_model <- gam(catchability~s(temperature, k=5)+s(LATITUDE_DECIMAL, k=5)+s(LONGITUDE_DECIMAL, k=5), data <- catchability_data, family=tw()) ## FOR S4
  #catchability_model <- gam(catchability~s(temperature, k=5)+s(DEPTH, k=5)+s(LATITUDE_DECIMAL, k=5)+s(LONGITUDE_DECIMAL, k=5), data <- catchability_data, family=tw())
  #catchability_model <- gam(catchability~s(temperature, k=5)+s(LATITUDE_DECIMAL, k=5)+s(LONGITUDE_DECIMAL, k=5), data <- catchability_data[which(catchability_data$catchability<=1),], family=betar()) ## FOR S4
  
  #catchability_model <- gam(catchability~s(temperature, k=5)+as.factor(year), data <- catchability_data[which(catchability_data$catchability<=1),], family=betar()) 
  
  #catchability_model <- gam(catchability~s(DEPTH, k=5)+as.factor(month), data <- catchability_data[which(catchability_data$catchability>=0 & catchability_data$catchability<=1),], family=betar)
  #summary(catchability_model)
  
  jpeg(filename = paste("./plot/catchability_gam.jpeg", sep=""), width=110, height=100, units = "mm", res = 600)
  par(mfrow=c(2,2), mar=c(4,4,1,1))
  plot(catchability_model, shade=T, all.terms=T, rug=TRUE)
  #termplot(catchability_model, se=T, ask=F, col.term = 1, col.se = 1, rug = TRUE)
  dev.off()
  
  
  summary(catchability_model$fitted.values)
  summary(catchability_data$catchability)
  
  if (FALSE){
    load("./output/temperature_raster_data.RData")
    load("./output/salinity_raster_data.RData")
    catchability_prediction_mean <- list()
    catchability_prediction_se <- list()
    for (i in 1:nrow(dates)){
      print(i)
      temp_data <- as.data.frame(cbind(depth_grid_plot$fathom[which(depth_grid_plot$fathom<0)], temperature_raster_data[[i]][which(depth_grid_plot$fathom<0)], salinity_raster_data[[i]][which(depth_grid_plot$fathom<0)], depth_grid_plot$lat[which(depth_grid_plot$fathom<0)], depth_grid_plot$lon[which(depth_grid_plot$fathom<0)]))
      colnames(temp_data) <- c("DEPTH", "temperature", "Salinity_psu", "LATITUDE_DECIMAL", "LONGITUDE_DECIMAL")
      temp_data$DEPTH <- -temp_data$DEPTH
      prediction_model <- predict(catchability_model, newdata = temp_data, se.fit=T, type="response")
      rm(list="temp_data")
      gc()
      catchability_prediction_mean[[i]] <- prediction_model$fit
      catchability_prediction_se[[i]] <- prediction_model$se.fit
      rm(list="prediction_model")
      gc()
    }
  } ## Did not consider month
  save(catchability_prediction_mean, catchability_prediction_se, file="./output/catchability_prediction.RData")
  if (FALSE){
    load("./output/temperature_raster_data.RData")
    load("./output/salinity_raster_data.RData")
    catchability_prediction_mean <- list()
    catchability_prediction_se <- list()
    for (i in 1:nrow(dates)){
      print(i)
      temp_data <- as.data.frame(cbind(depth_grid_plot$fathom[which(depth_grid_plot$fathom<0)], temperature_raster_data[[i]][which(depth_grid_plot$fathom<0)], salinity_raster_data[[i]][which(depth_grid_plot$fathom<0)], depth_grid_plot$lat[which(depth_grid_plot$fathom<0)], depth_grid_plot$lon[which(depth_grid_plot$fathom<0)], rep(dates$m[i], length(depth_grid_plot$fathom[which(depth_grid_plot$fathom<0)]))))
      colnames(temp_data) <- c("DEPTH", "temperature", "Salinity_psu", "end_lat", "end_lon", "month")
      temp_data$DEPTH <- -temp_data$DEPTH
      prediction_model <- predict(catchability_model, newdata = temp_data, se.fit=T, type="response")
      rm(list="temp_data")
      gc()
      catchability_prediction_mean[[i]] <- prediction_model$fit
      catchability_prediction_se[[i]] <- prediction_model$se.fit
      rm(list="prediction_model")
      gc()
    }
  } ## consider month
  save(catchability_prediction_mean, catchability_prediction_se, file="./output/catchability_prediction.RData")
  
  #### simulated samples with consideration of q ####
  #load("./output/catchability_prediction.RData")
  
  dates$id <- 1:nrow(dates)
  simulated_vts_samples_q <- c()
  for (i in 1:length(vts_points)){
    print(i)
    if(vts_vtrap_data[i,"year"]==2017 | vts_vtrap_data[i,"year"]==2014) simulated_vts_samples_q[i] <- NA
    else {
      if(vts_vtrap_data[i,"month"]==9) time_id <- dates$id[which(dates$y==vts_vtrap_data[i,"year"] & dates$m == 8)]
      else {
        time_id <- dates$id[which(dates$y==vts_vtrap_data[i,"year"] & dates$m == vts_vtrap_data[i,"month"])]
      }
      temp_value <- simulated_vts_samples[i]*rnorm(1, mean=catchability_prediction_mean[[time_id]][nearest_pred_points[i]], sd=catchability_prediction_se[[time_id]][nearest_pred_points[i]])
      if (is.na(temp_value)) simulated_vts_samples_q[i] <- temp_value
      else{
        if (temp_value <0) simulated_vts_samples_q[i] <- simulated_vts_samples[i]*catchability_prediction_mean[[time_id]][nearest_pred_points[i]]
        else simulated_vts_samples_q[i] <- simulated_vts_samples[i]*catchability_prediction_mean[[time_id]][nearest_pred_points[i]]
        #else simulated_vts_samples_q[i] <- simulated_vts_samples[i]*rnorm(1, mean=catchability_prediction_mean[[time_id]][nearest_pred_points[i]], sd=catchability_prediction_se[[time_id]][nearest_pred_points[i]])
      }
      
    }
  }
  summary(simulated_vts_samples_q)
  
  vts_vtrap_data$sim_quantity_q <- simulated_vts_samples_q
  
  #### vts simulated abundance index ####
  #vts_vtrap_data <- vts_vtrap_data[-which(vts_vtrap_data$year != 2014 & vts_vtrap_data$year != 2017 & is.na(vts_vtrap_data$sim_quantity)),]
  #vts_vtrap_data <- na.omit(vts_vtrap_data)
  summary(vts_vtrap_data)
  vts_vtrap_data <- na.omit(vts_vtrap_data)
  
  sim_lobster_per_year_sa_depth <- aggregate(vts_vtrap_data$sim_quantity, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata), sum)
  colnames(sim_lobster_per_year_sa_depth) <- c("Year", "SA", "Depth", "Lob_Quantity") 
  sim_lobster_per_year_sa_depth$Trap_Quantity <- aggregate(vts_vtrap_data$TRAP_ID, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata), length)$x
  sim_lobster_per_year_sa_depth$Site_Quantity <- aggregate(vts_vtrap_data$TRIP_ID, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata), function(x) length(unique(x)))$x
  summary(sim_lobster_per_year_sa_depth)
  sim_lobster_per_year_sa_depth$Ave_Lob_Trap <- sim_lobster_per_year_sa_depth$Lob_Quantity/sim_lobster_per_year_sa_depth$Trap_Quantity
  
  area_per_sa_depth <- as.data.frame(matrix(c(122, 82, 92, 296, 566, 395, 420, 1381, 315, 338, 198, 851), nrow=3, byrow=T))
  colnames(area_per_sa_depth) <- c("depth1", "depth2", "depth3", "total")
  rownames(area_per_sa_depth) <- c("511", "512", "513")
  
  ## sim_substrat_mean
  sim_substrat_mean <- matrix(NA, nrow=length(unique(sim_lobster_per_year_sa_depth$Year)), ncol=3)
  year_id <- unique(sim_lobster_per_year_sa_depth$Year)
  colnames(sim_substrat_mean) <- c("511", "512", "513")
  rownames(sim_substrat_mean) <- year_id
  for (i in 1:length(year_id)){
    sim_substrat_mean[i,1] <- area_per_sa_depth["511", "depth1"]/area_per_sa_depth["511", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["511", "depth2"]/area_per_sa_depth["511", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="21 to 40"& sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["511", "depth3"]/area_per_sa_depth["511", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="41 to 60"& sim_lobster_per_year_sa_depth$Year==year_id[i])] 
    
    sim_substrat_mean[i,2] <- area_per_sa_depth["512", "depth1"]/area_per_sa_depth["512", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["512", "depth2"]/area_per_sa_depth["512", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="21 to 40"& sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["512", "depth3"]/area_per_sa_depth["512", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="41 to 60"& sim_lobster_per_year_sa_depth$Year==year_id[i])] 
    
    sim_substrat_mean[i,3] <- area_per_sa_depth["513", "depth1"]/area_per_sa_depth["513", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["513", "depth2"]/area_per_sa_depth["513", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="21 to 40"& sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["513", "depth3"]/area_per_sa_depth["513", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="41 to 60"& sim_lobster_per_year_sa_depth$Year==year_id[i])] 
  }
  
  ## sim_substrat_var
  sim_lobster_per_year_sa_depth_site <- aggregate(vts_vtrap_data$sim_quantity, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata, vts_vtrap_data$SITE_ID), sum)
  colnames(sim_lobster_per_year_sa_depth_site) <- c("Year", "SA", "Depth", "Site", "Lob_Quantity") 
  
  sim_lobster_per_year_sa_depth_site$Trap_Quantity <- aggregate(vts_vtrap_data$TRAP_ID, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata, vts_vtrap_data$SITE_ID), length)$x
  
  sim_lobster_per_year_sa_depth_site$Quantity_per_Trap <- sim_lobster_per_year_sa_depth_site$Lob_Quantity/sim_lobster_per_year_sa_depth_site$Trap_Quantity
  
  sim_lobster_per_year_sa_depth$Ave_Lob_Trap <- sim_lobster_per_year_sa_depth$Lob_Quantity/sim_lobster_per_year_sa_depth$Trap_Quantity
  
  sim_lobster_per_year_sa_depth_site=merge(sim_lobster_per_year_sa_depth_site, sim_lobster_per_year_sa_depth[, c("Year", "SA", "Depth", "Ave_Lob_Trap")], by=c("Year", "SA", "Depth"))
  
  sim_lobster_per_year_sa_depth_site$Sub <- (sim_lobster_per_year_sa_depth_site$Ave_Lob_Trap - sim_lobster_per_year_sa_depth_site$Quantity_per_Trap)^2
  
  individual_value <- aggregate(sim_lobster_per_year_sa_depth_site$Sub, by=list(sim_lobster_per_year_sa_depth_site$Year, sim_lobster_per_year_sa_depth_site$SA, sim_lobster_per_year_sa_depth_site$Depth), sum)
  colnames(individual_value) <- c("Year", "SA", "Depth", "x")
  individual_value$individual_value <- individual_value$x/aggregate(sim_lobster_per_year_sa_depth_site$Site, by=list(sim_lobster_per_year_sa_depth_site$Year, sim_lobster_per_year_sa_depth_site$SA, sim_lobster_per_year_sa_depth_site$Depth), length)$x
  
  sim_substrat_var <- matrix(NA, nrow=length(unique(sim_lobster_per_year_sa_depth$Year)), ncol=3)
  colnames(sim_substrat_var) <- c("511", "512", "513")
  rownames(sim_substrat_var) <- year_id
  
  for (i in 1:length(year_id)){
    sim_substrat_var[i,1] <- 1/area_per_sa_depth["511", "total"]^2*((area_per_sa_depth["511", "depth1"]*(area_per_sa_depth["511", "depth1"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["511", "depth2"]*(area_per_sa_depth["511", "depth2"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["511", "depth3"]*(area_per_sa_depth["511", "depth3"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]))
    
    sim_substrat_var[i,2] <- 1/area_per_sa_depth["512", "total"]^2*((area_per_sa_depth["512", "depth1"]*(area_per_sa_depth["512", "depth1"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["512", "depth2"]*(area_per_sa_depth["512", "depth2"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["512", "depth3"]*(area_per_sa_depth["512", "depth3"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]))
    
    sim_substrat_var[i,3] <- 1/area_per_sa_depth["513", "total"]^2*((area_per_sa_depth["513", "depth1"]*(area_per_sa_depth["513", "depth1"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["513", "depth2"]*(area_per_sa_depth["513", "depth2"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["513", "depth3"]*(area_per_sa_depth["513", "depth3"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]))
  }
  
  jpeg(filename = "./plot/vts_lobster_substrat_mean_sim.jpeg", width=150, height=50, units = "mm", res = 600)
  par(mfrow=c(1,3))
  ylim_min <- min(na.omit(sim_substrat_mean))
  ylim_max <- max(na.omit(sim_substrat_mean))
  
  plot(year_id, sim_substrat_mean[,"513"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=1, cex=0.5, ylim=c(ylim_min, ylim_max))
  legend("topright", "513", bty="n")
  
  plot(year_id, sim_substrat_mean[,"512"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=1, cex=0.5, ylim=c(ylim_min, ylim_max))
  legend("topright", "512", bty="n")
  
  plot(year_id, sim_substrat_mean[,"511"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=1, cex=0.5, ylim=c(ylim_min, ylim_max))
  legend("topright", "511", bty="n")
  
  dev.off()
  
  if (FALSE) {
    sp6_fall9_cor <- c()
    sp6_fall9_cor[1] <- cor(substrat_mean[,"511"], sim_substrat_mean[,"511"], use = "pairwise.complete.obs")
    sp6_fall9_cor[2] <- cor(substrat_mean[,"512"], sim_substrat_mean[,"512"], use = "pairwise.complete.obs")
    sp6_fall9_cor[3] <- cor(substrat_mean[,"513"], sim_substrat_mean[,"513"], use = "pairwise.complete.obs")
    sp6_fall9_cor
  }
  
  
  fall_cor <- c()
  fall_cor[1] <- cor(substrat_mean[,"511"], sim_substrat_mean[,"511"], use = "pairwise.complete.obs")
  fall_cor[2] <- cor(substrat_mean[,"512"], sim_substrat_mean[,"512"], use = "pairwise.complete.obs")
  fall_cor[3] <- cor(substrat_mean[,"513"], sim_substrat_mean[,"513"], use = "pairwise.complete.obs")
  fall_cor
  
  
  #### Plot sim vts trends with consideration of q ####
  sim_lobster_per_year_sa_depth <- aggregate(vts_vtrap_data$sim_quantity_q, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata), sum)
  colnames(sim_lobster_per_year_sa_depth) <- c("Year", "SA", "Depth", "Lob_Quantity") 
  sim_lobster_per_year_sa_depth$Trap_Quantity <- aggregate(vts_vtrap_data$TRAP_ID, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata), length)$x
  sim_lobster_per_year_sa_depth$Site_Quantity <- aggregate(vts_vtrap_data$TRIP_ID, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata), function(x) length(unique(x)))$x
  summary(sim_lobster_per_year_sa_depth)
  sim_lobster_per_year_sa_depth$Ave_Lob_Trap <- sim_lobster_per_year_sa_depth$Lob_Quantity/sim_lobster_per_year_sa_depth$Trap_Quantity
  
  area_per_sa_depth <- as.data.frame(matrix(c(122, 82, 92, 296, 566, 395, 420, 1381, 315, 338, 198, 851), nrow=3, byrow=T))
  colnames(area_per_sa_depth) <- c("depth1", "depth2", "depth3", "total")
  rownames(area_per_sa_depth) <- c("511", "512", "513")
  
  ## sim_substrat_mean
  sim_substrat_mean <- matrix(NA, nrow=length(unique(sim_lobster_per_year_sa_depth$Year)), ncol=3)
  year_id <- unique(sim_lobster_per_year_sa_depth$Year)
  colnames(sim_substrat_mean) <- c("511", "512", "513")
  rownames(sim_substrat_mean) <- year_id
  for (i in 1:length(year_id)){
    sim_substrat_mean[i,1] <- area_per_sa_depth["511", "depth1"]/area_per_sa_depth["511", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["511", "depth2"]/area_per_sa_depth["511", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="21 to 40"& sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["511", "depth3"]/area_per_sa_depth["511", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="41 to 60"& sim_lobster_per_year_sa_depth$Year==year_id[i])] 
    
    sim_substrat_mean[i,2] <- area_per_sa_depth["512", "depth1"]/area_per_sa_depth["512", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["512", "depth2"]/area_per_sa_depth["512", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="21 to 40"& sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["512", "depth3"]/area_per_sa_depth["512", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="41 to 60"& sim_lobster_per_year_sa_depth$Year==year_id[i])] 
    
    sim_substrat_mean[i,3] <- area_per_sa_depth["513", "depth1"]/area_per_sa_depth["513", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["513", "depth2"]/area_per_sa_depth["513", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="21 to 40"& sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["513", "depth3"]/area_per_sa_depth["513", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="41 to 60"& sim_lobster_per_year_sa_depth$Year==year_id[i])] 
  }
  
  ## sim_substrat_var
  sim_lobster_per_year_sa_depth_site <- aggregate(vts_vtrap_data$sim_quantity, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata, vts_vtrap_data$SITE_ID), sum)
  colnames(sim_lobster_per_year_sa_depth_site) <- c("Year", "SA", "Depth", "Site", "Lob_Quantity") 
  
  sim_lobster_per_year_sa_depth_site$Trap_Quantity <- aggregate(vts_vtrap_data$TRAP_ID, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata, vts_vtrap_data$SITE_ID), length)$x
  
  sim_lobster_per_year_sa_depth_site$Quantity_per_Trap <- sim_lobster_per_year_sa_depth_site$Lob_Quantity/sim_lobster_per_year_sa_depth_site$Trap_Quantity
  
  sim_lobster_per_year_sa_depth$Ave_Lob_Trap <- sim_lobster_per_year_sa_depth$Lob_Quantity/sim_lobster_per_year_sa_depth$Trap_Quantity
  
  sim_lobster_per_year_sa_depth_site=merge(sim_lobster_per_year_sa_depth_site, sim_lobster_per_year_sa_depth[, c("Year", "SA", "Depth", "Ave_Lob_Trap")], by=c("Year", "SA", "Depth"))
  
  sim_lobster_per_year_sa_depth_site$Sub <- (sim_lobster_per_year_sa_depth_site$Ave_Lob_Trap - sim_lobster_per_year_sa_depth_site$Quantity_per_Trap)^2
  
  individual_value <- aggregate(sim_lobster_per_year_sa_depth_site$Sub, by=list(sim_lobster_per_year_sa_depth_site$Year, sim_lobster_per_year_sa_depth_site$SA, sim_lobster_per_year_sa_depth_site$Depth), sum)
  colnames(individual_value) <- c("Year", "SA", "Depth", "x")
  individual_value$individual_value <- individual_value$x/aggregate(sim_lobster_per_year_sa_depth_site$Site, by=list(sim_lobster_per_year_sa_depth_site$Year, sim_lobster_per_year_sa_depth_site$SA, sim_lobster_per_year_sa_depth_site$Depth), length)$x
  
  sim_substrat_var <- matrix(NA, nrow=length(unique(sim_lobster_per_year_sa_depth$Year)), ncol=3)
  colnames(sim_substrat_var) <- c("511", "512", "513")
  rownames(sim_substrat_var) <- year_id
  
  for (i in 1:length(year_id)){
    sim_substrat_var[i,1] <- 1/area_per_sa_depth["511", "total"]^2*((area_per_sa_depth["511", "depth1"]*(area_per_sa_depth["511", "depth1"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["511", "depth2"]*(area_per_sa_depth["511", "depth2"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["511", "depth3"]*(area_per_sa_depth["511", "depth3"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]))
    
    sim_substrat_var[i,2] <- 1/area_per_sa_depth["512", "total"]^2*((area_per_sa_depth["512", "depth1"]*(area_per_sa_depth["512", "depth1"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["512", "depth2"]*(area_per_sa_depth["512", "depth2"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["512", "depth3"]*(area_per_sa_depth["512", "depth3"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]))
    
    sim_substrat_var[i,3] <- 1/area_per_sa_depth["513", "total"]^2*((area_per_sa_depth["513", "depth1"]*(area_per_sa_depth["513", "depth1"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["513", "depth2"]*(area_per_sa_depth["513", "depth2"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["513", "depth3"]*(area_per_sa_depth["513", "depth3"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]))
  }
  
  jpeg(filename = "./plot/vts_lobster_substrat_mean_sim_q.jpeg", width=150, height=50, units = "mm", res = 600)
  par(mfrow=c(1,3))
  ylim_min <- min(na.omit(sim_substrat_mean))
  ylim_max <- max(na.omit(sim_substrat_mean))
  
  plot(year_id, sim_substrat_mean[,"513"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=1, cex=0.5, ylim=c(ylim_min, ylim_max))
  legend("topright", "513", bty="n")
  
  plot(year_id, sim_substrat_mean[,"512"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=1, cex=0.5, ylim=c(ylim_min, ylim_max))
  legend("topright", "512", bty="n")
  
  plot(year_id, sim_substrat_mean[,"511"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=1, cex=0.5, ylim=c(ylim_min, ylim_max))
  legend("topright", "511", bty="n")
  
  dev.off()
  
  fall_cor <- c()
  fall_cor[1] <- cor(substrat_mean[,"511"], sim_substrat_mean[,"511"], use = "pairwise.complete.obs")
  fall_cor[2] <- cor(substrat_mean[,"512"], sim_substrat_mean[,"512"], use = "pairwise.complete.obs")
  fall_cor[3] <- cor(substrat_mean[,"513"], sim_substrat_mean[,"513"], use = "pairwise.complete.obs")
  fall_cor
  #### population index ####
  polygon_points <- over(prediction_points, sa511_513)
  summary(polygon_points$Id)
  
  population_511_month_index <- c()
  population_512_month_index <- c()
  population_513_month_index <- c()
  
  for (i in 1:length(prediction_mean)){
    population_511_month_index[i] <- sum(prediction_mean[[i]][which(polygon_points$Id==511)], na.rm = T)
    population_512_month_index[i] <- sum(prediction_mean[[i]][which(polygon_points$Id==512)], na.rm = T)
    population_513_month_index[i] <- sum(prediction_mean[[i]][which(polygon_points$Id==513)], na.rm = T)
  }
  population_511_year_index <- rowMeans(matrix(population_511_month_index, ncol=3, byrow = T))
  population_511_year_index <- c(population_511_year_index[1:8], NA, population_511_year_index[9:10], NA)
  population_512_year_index <- rowMeans(matrix(population_512_month_index, ncol=3, byrow = T))
  population_512_year_index <- c(population_512_year_index[1:8], NA, population_512_year_index[9:10], NA)
  population_513_year_index <- rowMeans(matrix(population_513_month_index, ncol=3, byrow = T))
  population_513_year_index <- c(population_513_year_index[1:8], NA, population_513_year_index[9:10], NA)
  population_index <- cbind(population_511_year_index, population_512_year_index, population_513_year_index)
  colnames(population_index) <- c("511", "512", "513")
  rownames(population_index) <- 2006:2017
  
  jpeg(filename = "./plot/pop_vts_lobster_substrat_mean.jpeg", width=175, height=60, units = "mm", res = 600)
  
  par(mfrow=c(1,3), mar=c(3.5,3.5,2,3.5))
  
  plot(x = year_id-0.13, y = population_513_year_index[1:length(year_id)], col = "deepskyblue3", type = "h", xlab = "", ylab="", main = "", lwd=3, xlim=c(2006, 2017))
  mtext("Simulated Lobster Population", side=2, line=2, col="deepskyblue3", cex=0.6)
  mtext("Year", side=1, line=2, cex=0.6)
  par(new = T)
  plot(x = year_id+0.13, y = sim_substrat_mean[1:length(year_id),"513"], col = "coral3", type = "h", xaxt = "n", yaxt = "n", xlab = "", ylab = "", lwd=3, xlim=c(2006, 2017))
  axis(4)
  mtext("VTS Abundance Index", side = 4, line = 2, col="coral3", cex=0.6)
  #legend("topleft", paste("513:\nCor = ",  round(cor(na.omit(sim_substrat_mean[,"513"]), na.omit(population_513_year_index)),2), sep=""), bty="n")
  
  plot(x = year_id-0.13, y = population_512_year_index[1:length(year_id)], col = "deepskyblue3", type = "h", xlab = "", ylab="", main = "", lwd=3, xlim=c(2006, 2017))
  mtext("Simulated Lobster Population", side=2, line=2, col="deepskyblue3", cex=0.6)
  mtext("Year", side=1, line=2, cex=0.6)
  par(new = T)
  plot(x = year_id+0.13, y = sim_substrat_mean[1:length(year_id),"512"], col = "coral3", type = "h", xaxt = "n", yaxt = "n", xlab = "", ylab = "", lwd=3, xlim=c(2006, 2017))
  axis(4)
  mtext("VTS Abundance Index", side = 4, line = 2, col="coral3", cex=0.6)
  #legend("topleft", paste("512:\nCor = ",  round(cor(na.omit(sim_substrat_mean[,"512"]), na.omit(population_512_year_index)),2), sep=""), bty="n")
  
  plot(x = year_id-0.13, y = population_511_year_index[1:length(year_id)], col = "deepskyblue3", type = "h", xlab = "", ylab="", main = "", lwd=3, xlim=c(2006, 2017))
  mtext("Simulated Lobster Population", side=2, line=2, col="deepskyblue3", cex=0.6)
  mtext("Year", side=1, line=2, cex=0.6)
  par(new = T)
  plot(x = year_id+0.13, y = sim_substrat_mean[1:length(year_id),"511"], col = "coral3", type = "h", xaxt = "n", yaxt = "n", xlab = "", ylab = "", lwd=3, xlim=c(2006, 2017))
  axis(4)
  mtext("VTS Abundance Index", side = 4, line = 2, col="coral3", cex=0.6)
  #legend("topleft", paste("511:\nCor = ",  round(cor(na.omit(sim_substrat_mean[,"511"]), na.omit(population_511_year_index)),2), sep=""), bty="n")
  dev.off()
  
  jpeg(filename = "./plot/pop_vts_lobster_substrat_mean2.jpeg", width=175, height=60, units = "mm", res = 300)
  
  par(mfrow=c(1,3), mar=c(3.5,3.5,2,3.5))
  
  plot(x = year_id, y = population_513_year_index[1:length(year_id)], col = "deepskyblue3", type = "h", xlab = "", ylab="", main = "", lwd=5, xlim=c(2006, 2017))
  mtext("Simulated Lobster Population", side=2, line=2, col="deepskyblue3", cex=0.6)
  mtext("Year", side=1, line=2, cex=0.6)
  par(new = T)
  plot(x = year_id, y = sim_substrat_mean[1:length(year_id),"513"], col = "coral3", type = "o", xaxt = "n", yaxt = "n", xlab = "", ylab = "", lwd=2, xlim=c(2006, 2017), pch=16)
  axis(4)
  mtext("VTS Abundance Index", side = 4, line = 2, col="coral3", cex=0.6)
  legend("topright", paste("513:\nCor = ",  round(cor(na.omit(sim_substrat_mean[,"513"]), na.omit(population_513_year_index)),2), sep=""), bty="n")
  
  plot(x = year_id, y = population_512_year_index[1:length(year_id)], col = "deepskyblue3", type = "h", xlab = "", ylab="", main = "", lwd=5, xlim=c(2006, 2017))
  mtext("Simulated Lobster Population", side=2, line=2, col="deepskyblue3", cex=0.6)
  mtext("Year", side=1, line=2, cex=0.6)
  par(new = T)
  plot(x = year_id, y = sim_substrat_mean[1:length(year_id),"512"], col = "coral3", type = "o", xaxt = "n", yaxt = "n", xlab = "", ylab = "", lwd=2, xlim=c(2006, 2017), pch=16)
  axis(4)
  mtext("VTS Abundance Index", side = 4, line = 2, col="coral3", cex=0.6)
  legend("topright", paste("512:\nCor = ",  round(cor(na.omit(sim_substrat_mean[,"512"]), na.omit(population_512_year_index)),2), sep=""), bty="n")
  
  plot(x = year_id, y = population_511_year_index[1:length(year_id)], col = "deepskyblue3", type = "h", xlab = "", ylab="", main = "", lwd=5, xlim=c(2006, 2017))
  mtext("Simulated Lobster Population", side=2, line=2, col="deepskyblue3", cex=0.6)
  mtext("Year", side=1, line=2, cex=0.6)
  par(new = T)
  plot(x = year_id, y = sim_substrat_mean[1:length(year_id),"511"], col = "coral3", type = "o", xaxt = "n", yaxt = "n", xlab = "", ylab = "", lwd=2, xlim=c(2006, 2017), pch=16)
  axis(4)
  mtext("VTS Abundance Index", side = 4, line = 2, col="coral3", cex=0.6)
  legend("topright", paste("511:\nCor = ",  round(cor(na.omit(sim_substrat_mean[,"511"]), na.omit(population_511_year_index)),2), sep=""), bty="n")
  dev.off()
  
  
} ## Original thoughts: catchability with gam
if (FALSE){
  vts_vtrap_data$catchability<- vts_vtrap_data$quantity/vts_vtrap_data$sim_quantity
  vts_vtrap_data[which(vts_vtrap_data$catchability>1),]
  
  temp_data <- na.omit(vts_vtrap_data)
  temp_data <- temp_data[which(temp_data$catchability<=1),]
  temp_data$month[(temp_data$month==9)] <- 8
  catchability_mean <- aggregate(temp_data$catchability, by=list(temp_data$year, temp_data$month, temp_data$STATAREA, temp_data$assigneddepthstrata), mean)
  catchability_sd <- aggregate(temp_data$catchability, by=list(temp_data$year, temp_data$month, temp_data$STATAREA, temp_data$assigneddepthstrata), sd)
  colnames(catchability_mean) <- c("year", "month", "STATAREA", "assigneddepthstrata", "mean")
  colnames(catchability_sd) <- c("year", "month", "STATAREA", "assigneddepthstrata", "sd")
  
  jpeg(filename = "./plot/catchability_year_month_strata.jpeg", width=160, height=150, units = "mm", res = 600)
  par(mfrow=c(2,2))
  boxplot(catchability_mean$mean~catchability_mean$year, xlab="Year", ylab="Catchability", ylim=c(0,0.3), pch=16, cex=0.5)
  boxplot(catchability_mean$mean~catchability_mean$month, xlab="Month", ylab="Catchability", ylim=c(0,0.3), pch=16, cex=0.5)
  boxplot(catchability_mean$mean~catchability_mean$STATAREA, xlab="Statistical Area", ylab="Catchability", ylim=c(0,0.3), pch=16, cex=0.5)
  boxplot(catchability_mean$mean~catchability_mean$assigneddepthstrata, xlab="Depth Strata", ylab="Catchability", ylim=c(0,0.3), xlim=c(1.5,4.5), pch=16, cex=0.5)
  dev.off()
  
  catchability_data <- vts_vtrap_data
  
  catchability_data$month[(catchability_data$month==9)] <- 8
  
  merge_data <- merge(catchability_data, catchability_mean, by=c("year","month", "STATAREA", "assigneddepthstrata"))
  catchability_data$q_mean <- merge_data$mean
  merge_data <- merge(catchability_data, catchability_sd, by=c("year","month", "STATAREA", "assigneddepthstrata"))
  catchability_data$q_sd <- merge_data$sd
  
  
  simulated_vts_samples_q <- c()
  for(i in 1:length(simulated_vts_samples)){
    print(i)
    if(is.na(simulated_vts_samples[i])) simulated_vts_samples_q[i] <- NA
    else{
      simulated_vts_samples_q[i] <- simulated_vts_samples[i]*rnorm(1, mean=catchability_data$q_mean[i], sd=catchability_data$q_sd[i])
      if(simulated_vts_samples_q[i]<0) simulated_vts_samples_q[i]<-simulated_vts_samples[i]*catchability_data$q_mean[i]
    }
  }
  
  vts_vtrap_data$sim_quantity_q <- simulated_vts_samples_q
  
  catchability_data$month[(catchability_data$month==9)] <- 8
  
  #### vts simulated abundance index ####
  #vts_vtrap_data <- vts_vtrap_data[-which(vts_vtrap_data$year != 2014 & vts_vtrap_data$year != 2017 & is.na(vts_vtrap_data$sim_quantity)),]
  #vts_vtrap_data <- na.omit(vts_vtrap_data)
  summary(vts_vtrap_data)
  vts_vtrap_data <- na.omit(vts_vtrap_data)
  
  sim_lobster_per_year_sa_depth <- aggregate(vts_vtrap_data$sim_quantity, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata), sum)
  colnames(sim_lobster_per_year_sa_depth) <- c("Year", "SA", "Depth", "Lob_Quantity") 
  sim_lobster_per_year_sa_depth$Trap_Quantity <- aggregate(vts_vtrap_data$TRAP_ID, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata), length)$x
  sim_lobster_per_year_sa_depth$Site_Quantity <- aggregate(vts_vtrap_data$TRIP_ID, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata), function(x) length(unique(x)))$x
  summary(sim_lobster_per_year_sa_depth)
  sim_lobster_per_year_sa_depth$Ave_Lob_Trap <- sim_lobster_per_year_sa_depth$Lob_Quantity/sim_lobster_per_year_sa_depth$Trap_Quantity
  
  area_per_sa_depth <- as.data.frame(matrix(c(122, 82, 92, 296, 566, 395, 420, 1381, 315, 338, 198, 851), nrow=3, byrow=T))
  colnames(area_per_sa_depth) <- c("depth1", "depth2", "depth3", "total")
  rownames(area_per_sa_depth) <- c("511", "512", "513")
  
  ## sim_substrat_mean
  sim_substrat_mean <- matrix(NA, nrow=length(unique(sim_lobster_per_year_sa_depth$Year)), ncol=3)
  year_id <- unique(sim_lobster_per_year_sa_depth$Year)
  colnames(sim_substrat_mean) <- c("511", "512", "513")
  rownames(sim_substrat_mean) <- year_id
  for (i in 1:length(year_id)){
    sim_substrat_mean[i,1] <- area_per_sa_depth["511", "depth1"]/area_per_sa_depth["511", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["511", "depth2"]/area_per_sa_depth["511", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="21 to 40"& sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["511", "depth3"]/area_per_sa_depth["511", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="41 to 60"& sim_lobster_per_year_sa_depth$Year==year_id[i])] 
    
    sim_substrat_mean[i,2] <- area_per_sa_depth["512", "depth1"]/area_per_sa_depth["512", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["512", "depth2"]/area_per_sa_depth["512", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="21 to 40"& sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["512", "depth3"]/area_per_sa_depth["512", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="41 to 60"& sim_lobster_per_year_sa_depth$Year==year_id[i])] 
    
    sim_substrat_mean[i,3] <- area_per_sa_depth["513", "depth1"]/area_per_sa_depth["513", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["513", "depth2"]/area_per_sa_depth["513", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="21 to 40"& sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["513", "depth3"]/area_per_sa_depth["513", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="41 to 60"& sim_lobster_per_year_sa_depth$Year==year_id[i])] 
  }
  
  ## sim_substrat_var
  sim_lobster_per_year_sa_depth_site <- aggregate(vts_vtrap_data$sim_quantity, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata, vts_vtrap_data$SITE_ID), sum)
  colnames(sim_lobster_per_year_sa_depth_site) <- c("Year", "SA", "Depth", "Site", "Lob_Quantity") 
  
  sim_lobster_per_year_sa_depth_site$Trap_Quantity <- aggregate(vts_vtrap_data$TRAP_ID, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata, vts_vtrap_data$SITE_ID), length)$x
  
  sim_lobster_per_year_sa_depth_site$Quantity_per_Trap <- sim_lobster_per_year_sa_depth_site$Lob_Quantity/sim_lobster_per_year_sa_depth_site$Trap_Quantity
  
  sim_lobster_per_year_sa_depth$Ave_Lob_Trap <- sim_lobster_per_year_sa_depth$Lob_Quantity/sim_lobster_per_year_sa_depth$Trap_Quantity
  
  sim_lobster_per_year_sa_depth_site=merge(sim_lobster_per_year_sa_depth_site, sim_lobster_per_year_sa_depth[, c("Year", "SA", "Depth", "Ave_Lob_Trap")], by=c("Year", "SA", "Depth"))
  
  sim_lobster_per_year_sa_depth_site$Sub <- (sim_lobster_per_year_sa_depth_site$Ave_Lob_Trap - sim_lobster_per_year_sa_depth_site$Quantity_per_Trap)^2
  
  individual_value <- aggregate(sim_lobster_per_year_sa_depth_site$Sub, by=list(sim_lobster_per_year_sa_depth_site$Year, sim_lobster_per_year_sa_depth_site$SA, sim_lobster_per_year_sa_depth_site$Depth), sum)
  colnames(individual_value) <- c("Year", "SA", "Depth", "x")
  individual_value$individual_value <- individual_value$x/aggregate(sim_lobster_per_year_sa_depth_site$Site, by=list(sim_lobster_per_year_sa_depth_site$Year, sim_lobster_per_year_sa_depth_site$SA, sim_lobster_per_year_sa_depth_site$Depth), length)$x
  
  sim_substrat_var <- matrix(NA, nrow=length(unique(sim_lobster_per_year_sa_depth$Year)), ncol=3)
  colnames(sim_substrat_var) <- c("511", "512", "513")
  rownames(sim_substrat_var) <- year_id
  
  for (i in 1:length(year_id)){
    sim_substrat_var[i,1] <- 1/area_per_sa_depth["511", "total"]^2*((area_per_sa_depth["511", "depth1"]*(area_per_sa_depth["511", "depth1"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["511", "depth2"]*(area_per_sa_depth["511", "depth2"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["511", "depth3"]*(area_per_sa_depth["511", "depth3"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]))
    
    sim_substrat_var[i,2] <- 1/area_per_sa_depth["512", "total"]^2*((area_per_sa_depth["512", "depth1"]*(area_per_sa_depth["512", "depth1"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["512", "depth2"]*(area_per_sa_depth["512", "depth2"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["512", "depth3"]*(area_per_sa_depth["512", "depth3"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]))
    
    sim_substrat_var[i,3] <- 1/area_per_sa_depth["513", "total"]^2*((area_per_sa_depth["513", "depth1"]*(area_per_sa_depth["513", "depth1"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["513", "depth2"]*(area_per_sa_depth["513", "depth2"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["513", "depth3"]*(area_per_sa_depth["513", "depth3"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]))
  }
  
  jpeg(filename = "./plot/vts_lobster_substrat_mean_sim.jpeg", width=150, height=50, units = "mm", res = 600)
  par(mfrow=c(1,3))
  ylim_min <- min(na.omit(sim_substrat_mean))
  ylim_max <- max(na.omit(sim_substrat_mean))
  
  plot(year_id, sim_substrat_mean[,"513"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=1, cex=0.5, ylim=c(ylim_min, ylim_max))
  legend("topright", "513", bty="n")
  
  plot(year_id, sim_substrat_mean[,"512"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=1, cex=0.5, ylim=c(ylim_min, ylim_max))
  legend("topright", "512", bty="n")
  
  plot(year_id, sim_substrat_mean[,"511"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=1, cex=0.5, ylim=c(ylim_min, ylim_max))
  legend("topright", "511", bty="n")
  
  dev.off()
  
  if (FALSE) {
    sp6_fall9_cor <- c()
    sp6_fall9_cor[1] <- cor(substrat_mean[,"511"], sim_substrat_mean[,"511"], use = "pairwise.complete.obs")
    sp6_fall9_cor[2] <- cor(substrat_mean[,"512"], sim_substrat_mean[,"512"], use = "pairwise.complete.obs")
    sp6_fall9_cor[3] <- cor(substrat_mean[,"513"], sim_substrat_mean[,"513"], use = "pairwise.complete.obs")
    sp6_fall9_cor
  }
  
  
  fall_cor1 <- c()
  fall_cor1[1] <- cor(substrat_mean[,"511"], sim_substrat_mean[,"511"], use = "pairwise.complete.obs")
  fall_cor1[2] <- cor(substrat_mean[,"512"], sim_substrat_mean[,"512"], use = "pairwise.complete.obs")
  fall_cor1[3] <- cor(substrat_mean[,"513"], sim_substrat_mean[,"513"], use = "pairwise.complete.obs")
  fall_cor1
  
  
  #### Plot sim vts trends with consideration of q ####
  sim_lobster_per_year_sa_depth <- aggregate(vts_vtrap_data$sim_quantity_q, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata), sum)
  colnames(sim_lobster_per_year_sa_depth) <- c("Year", "SA", "Depth", "Lob_Quantity") 
  sim_lobster_per_year_sa_depth$Trap_Quantity <- aggregate(vts_vtrap_data$TRAP_ID, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata), length)$x
  sim_lobster_per_year_sa_depth$Site_Quantity <- aggregate(vts_vtrap_data$TRIP_ID, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata), function(x) length(unique(x)))$x
  summary(sim_lobster_per_year_sa_depth)
  sim_lobster_per_year_sa_depth$Ave_Lob_Trap <- sim_lobster_per_year_sa_depth$Lob_Quantity/sim_lobster_per_year_sa_depth$Trap_Quantity
  
  area_per_sa_depth <- as.data.frame(matrix(c(122, 82, 92, 296, 566, 395, 420, 1381, 315, 338, 198, 851), nrow=3, byrow=T))
  colnames(area_per_sa_depth) <- c("depth1", "depth2", "depth3", "total")
  rownames(area_per_sa_depth) <- c("511", "512", "513")
  
  ## sim_substrat_mean
  sim_substrat_mean <- matrix(NA, nrow=length(unique(sim_lobster_per_year_sa_depth$Year)), ncol=3)
  year_id <- unique(sim_lobster_per_year_sa_depth$Year)
  colnames(sim_substrat_mean) <- c("511", "512", "513")
  rownames(sim_substrat_mean) <- year_id
  for (i in 1:length(year_id)){
    sim_substrat_mean[i,1] <- area_per_sa_depth["511", "depth1"]/area_per_sa_depth["511", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["511", "depth2"]/area_per_sa_depth["511", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="21 to 40"& sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["511", "depth3"]/area_per_sa_depth["511", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="41 to 60"& sim_lobster_per_year_sa_depth$Year==year_id[i])] 
    
    sim_substrat_mean[i,2] <- area_per_sa_depth["512", "depth1"]/area_per_sa_depth["512", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["512", "depth2"]/area_per_sa_depth["512", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="21 to 40"& sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["512", "depth3"]/area_per_sa_depth["512", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="41 to 60"& sim_lobster_per_year_sa_depth$Year==year_id[i])] 
    
    sim_substrat_mean[i,3] <- area_per_sa_depth["513", "depth1"]/area_per_sa_depth["513", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["513", "depth2"]/area_per_sa_depth["513", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="21 to 40"& sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["513", "depth3"]/area_per_sa_depth["513", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="41 to 60"& sim_lobster_per_year_sa_depth$Year==year_id[i])] 
  }
  
  ## sim_substrat_var
  sim_lobster_per_year_sa_depth_site <- aggregate(vts_vtrap_data$sim_quantity, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata, vts_vtrap_data$SITE_ID), sum)
  colnames(sim_lobster_per_year_sa_depth_site) <- c("Year", "SA", "Depth", "Site", "Lob_Quantity") 
  
  sim_lobster_per_year_sa_depth_site$Trap_Quantity <- aggregate(vts_vtrap_data$TRAP_ID, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata, vts_vtrap_data$SITE_ID), length)$x
  
  sim_lobster_per_year_sa_depth_site$Quantity_per_Trap <- sim_lobster_per_year_sa_depth_site$Lob_Quantity/sim_lobster_per_year_sa_depth_site$Trap_Quantity
  
  sim_lobster_per_year_sa_depth$Ave_Lob_Trap <- sim_lobster_per_year_sa_depth$Lob_Quantity/sim_lobster_per_year_sa_depth$Trap_Quantity
  
  sim_lobster_per_year_sa_depth_site=merge(sim_lobster_per_year_sa_depth_site, sim_lobster_per_year_sa_depth[, c("Year", "SA", "Depth", "Ave_Lob_Trap")], by=c("Year", "SA", "Depth"))
  
  sim_lobster_per_year_sa_depth_site$Sub <- (sim_lobster_per_year_sa_depth_site$Ave_Lob_Trap - sim_lobster_per_year_sa_depth_site$Quantity_per_Trap)^2
  
  individual_value <- aggregate(sim_lobster_per_year_sa_depth_site$Sub, by=list(sim_lobster_per_year_sa_depth_site$Year, sim_lobster_per_year_sa_depth_site$SA, sim_lobster_per_year_sa_depth_site$Depth), sum)
  colnames(individual_value) <- c("Year", "SA", "Depth", "x")
  individual_value$individual_value <- individual_value$x/aggregate(sim_lobster_per_year_sa_depth_site$Site, by=list(sim_lobster_per_year_sa_depth_site$Year, sim_lobster_per_year_sa_depth_site$SA, sim_lobster_per_year_sa_depth_site$Depth), length)$x
  
  sim_substrat_var <- matrix(NA, nrow=length(unique(sim_lobster_per_year_sa_depth$Year)), ncol=3)
  colnames(sim_substrat_var) <- c("511", "512", "513")
  rownames(sim_substrat_var) <- year_id
  
  for (i in 1:length(year_id)){
    sim_substrat_var[i,1] <- 1/area_per_sa_depth["511", "total"]^2*((area_per_sa_depth["511", "depth1"]*(area_per_sa_depth["511", "depth1"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["511", "depth2"]*(area_per_sa_depth["511", "depth2"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["511", "depth3"]*(area_per_sa_depth["511", "depth3"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]))
    
    sim_substrat_var[i,2] <- 1/area_per_sa_depth["512", "total"]^2*((area_per_sa_depth["512", "depth1"]*(area_per_sa_depth["512", "depth1"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["512", "depth2"]*(area_per_sa_depth["512", "depth2"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["512", "depth3"]*(area_per_sa_depth["512", "depth3"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]))
    
    sim_substrat_var[i,3] <- 1/area_per_sa_depth["513", "total"]^2*((area_per_sa_depth["513", "depth1"]*(area_per_sa_depth["513", "depth1"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["513", "depth2"]*(area_per_sa_depth["513", "depth2"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["513", "depth3"]*(area_per_sa_depth["513", "depth3"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]))
  }
  
  jpeg(filename = "./plot/vts_lobster_substrat_mean_sim_q.jpeg", width=150, height=50, units = "mm", res = 600)
  par(mfrow=c(1,3))
  ylim_min <- min(na.omit(sim_substrat_mean))
  ylim_max <- max(na.omit(sim_substrat_mean))
  
  plot(year_id, sim_substrat_mean[,"513"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=1, cex=0.5, ylim=c(ylim_min, ylim_max))
  legend("topright", "513", bty="n")
  
  plot(year_id, sim_substrat_mean[,"512"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=1, cex=0.5, ylim=c(ylim_min, ylim_max))
  legend("topright", "512", bty="n")
  
  plot(year_id, sim_substrat_mean[,"511"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=1, cex=0.5, ylim=c(ylim_min, ylim_max))
  legend("topright", "511", bty="n")
  
  dev.off()
  
  fall_cor2 <- c()
  fall_cor2[1] <- cor(substrat_mean[,"511"], sim_substrat_mean[,"511"], use = "pairwise.complete.obs")
  fall_cor2[2] <- cor(substrat_mean[,"512"], sim_substrat_mean[,"512"], use = "pairwise.complete.obs")
  fall_cor2[3] <- cor(substrat_mean[,"513"], sim_substrat_mean[,"513"], use = "pairwise.complete.obs")
  fall_cor2
  #### population index ####
  polygon_points <- over(prediction_points, sa511_513)
  summary(polygon_points$Id)
  
  population_511_month_index <- c()
  population_512_month_index <- c()
  population_513_month_index <- c()
  
  for (i in 1:length(prediction_mean)){
    population_511_month_index[i] <- sum(prediction_mean[[i]][which(polygon_points$Id==511)], na.rm = T)
    population_512_month_index[i] <- sum(prediction_mean[[i]][which(polygon_points$Id==512)], na.rm = T)
    population_513_month_index[i] <- sum(prediction_mean[[i]][which(polygon_points$Id==513)], na.rm = T)
  }
  population_511_year_index <- rowMeans(matrix(population_511_month_index, ncol=3, byrow = T))
  population_511_year_index <- c(population_511_year_index[1:8], NA, population_511_year_index[9:10], NA)
  population_512_year_index <- rowMeans(matrix(population_512_month_index, ncol=3, byrow = T))
  population_512_year_index <- c(population_512_year_index[1:8], NA, population_512_year_index[9:10], NA)
  population_513_year_index <- rowMeans(matrix(population_513_month_index, ncol=3, byrow = T))
  population_513_year_index <- c(population_513_year_index[1:8], NA, population_513_year_index[9:10], NA)
  
  jpeg(filename = "./plot/pop_vts_lobster_substrat_mean.jpeg", width=175, height=60, units = "mm", res = 600)
  
  par(mfrow=c(1,3), mar=c(3.5,3.5,2,3.5))
  
  plot(x = year_id-0.13, y = population_513_year_index[1:length(year_id)], col = "deepskyblue3", type = "h", xlab = "", ylab="", main = "", lwd=3, xlim=c(2006, 2017))
  mtext("Simulated Lobster Population", side=2, line=2, col="deepskyblue3", cex=0.6)
  mtext("Year", side=1, line=2, cex=0.6)
  par(new = T)
  plot(x = year_id+0.13, y = sim_substrat_mean[1:length(year_id),"513"], col = "coral3", type = "h", xaxt = "n", yaxt = "n", xlab = "", ylab = "", lwd=3, xlim=c(2006, 2017))
  axis(4)
  mtext("VTS Abundance Index", side = 4, line = 2, col="coral3", cex=0.6)
  #legend("topleft", paste("513:\nCor = ",  round(cor(na.omit(sim_substrat_mean[,"513"]), na.omit(population_513_year_index)),2), sep=""), bty="n")
  
  plot(x = year_id-0.13, y = population_512_year_index[1:length(year_id)], col = "deepskyblue3", type = "h", xlab = "", ylab="", main = "", lwd=3, xlim=c(2006, 2017))
  mtext("Simulated Lobster Population", side=2, line=2, col="deepskyblue3", cex=0.6)
  mtext("Year", side=1, line=2, cex=0.6)
  par(new = T)
  plot(x = year_id+0.13, y = sim_substrat_mean[1:length(year_id),"512"], col = "coral3", type = "h", xaxt = "n", yaxt = "n", xlab = "", ylab = "", lwd=3, xlim=c(2006, 2017))
  axis(4)
  mtext("VTS Abundance Index", side = 4, line = 2, col="coral3", cex=0.6)
  #legend("topleft", paste("512:\nCor = ",  round(cor(na.omit(sim_substrat_mean[,"512"]), na.omit(population_512_year_index)),2), sep=""), bty="n")
  
  plot(x = year_id-0.13, y = population_511_year_index[1:length(year_id)], col = "deepskyblue3", type = "h", xlab = "", ylab="", main = "", lwd=3, xlim=c(2006, 2017))
  mtext("Simulated Lobster Population", side=2, line=2, col="deepskyblue3", cex=0.6)
  mtext("Year", side=1, line=2, cex=0.6)
  par(new = T)
  plot(x = year_id+0.13, y = sim_substrat_mean[1:length(year_id),"511"], col = "coral3", type = "h", xaxt = "n", yaxt = "n", xlab = "", ylab = "", lwd=3, xlim=c(2006, 2017))
  axis(4)
  mtext("VTS Abundance Index", side = 4, line = 2, col="coral3", cex=0.6)
  #legend("topleft", paste("511:\nCor = ",  round(cor(na.omit(sim_substrat_mean[,"511"]), na.omit(population_511_year_index)),2), sep=""), bty="n")
  dev.off()
  
  jpeg(filename = "./plot/pop_vts_lobster_substrat_mean2.jpeg", width=175, height=60, units = "mm", res = 300)
  
  par(mfrow=c(1,3), mar=c(3.5,3.5,2,3.5))
  
  plot(x = year_id, y = population_513_year_index[1:length(year_id)], col = "deepskyblue3", type = "h", xlab = "", ylab="", main = "", lwd=5, xlim=c(2006, 2017))
  mtext("Simulated Lobster Population", side=2, line=2, col="deepskyblue3", cex=0.6)
  mtext("Year", side=1, line=2, cex=0.6)
  par(new = T)
  plot(x = year_id, y = sim_substrat_mean[1:length(year_id),"513"], col = "coral3", type = "o", xaxt = "n", yaxt = "n", xlab = "", ylab = "", lwd=2, xlim=c(2006, 2017), pch=16)
  axis(4)
  mtext("VTS Abundance Index", side = 4, line = 2, col="coral3", cex=0.6)
  legend("topright", paste("513:\nCor = ",  round(cor(na.omit(sim_substrat_mean[,"513"]), na.omit(population_513_year_index)),2), sep=""), bty="n")
  
  plot(x = year_id, y = population_512_year_index[1:length(year_id)], col = "deepskyblue3", type = "h", xlab = "", ylab="", main = "", lwd=5, xlim=c(2006, 2017))
  mtext("Simulated Lobster Population", side=2, line=2, col="deepskyblue3", cex=0.6)
  mtext("Year", side=1, line=2, cex=0.6)
  par(new = T)
  plot(x = year_id, y = sim_substrat_mean[1:length(year_id),"512"], col = "coral3", type = "o", xaxt = "n", yaxt = "n", xlab = "", ylab = "", lwd=2, xlim=c(2006, 2017), pch=16)
  axis(4)
  mtext("VTS Abundance Index", side = 4, line = 2, col="coral3", cex=0.6)
  legend("topright", paste("512:\nCor = ",  round(cor(na.omit(sim_substrat_mean[,"512"]), na.omit(population_512_year_index)),2), sep=""), bty="n")
  
  plot(x = year_id, y = population_511_year_index[1:length(year_id)], col = "deepskyblue3", type = "h", xlab = "", ylab="", main = "", lwd=5, xlim=c(2006, 2017))
  mtext("Simulated Lobster Population", side=2, line=2, col="deepskyblue3", cex=0.6)
  mtext("Year", side=1, line=2, cex=0.6)
  par(new = T)
  plot(x = year_id, y = sim_substrat_mean[1:length(year_id),"511"], col = "coral3", type = "o", xaxt = "n", yaxt = "n", xlab = "", ylab = "", lwd=2, xlim=c(2006, 2017), pch=16)
  axis(4)
  mtext("VTS Abundance Index", side = 4, line = 2, col="coral3", cex=0.6)
  legend("topright", paste("511:\nCor = ",  round(cor(na.omit(sim_substrat_mean[,"511"]), na.omit(population_511_year_index)),2), sep=""), bty="n")
  dev.off()
} #### catchability with year, month, statistical area, and depth

if (FALSE){
  vts_vtrap_data$catchability<- vts_vtrap_data$quantity/vts_vtrap_data$sim_quantity
  vts_vtrap_data[which(vts_vtrap_data$catchability>1),]
  
  temp_data <- na.omit(vts_vtrap_data)
  temp_data <- temp_data[which(temp_data$catchability<=1),]
  temp_data$month[(temp_data$month==9)] <- 8
  catchability_mean <- aggregate(temp_data$catchability, by=list(temp_data$month, temp_data$STATAREA, temp_data$assigneddepthstrata), mean)
  catchability_sd <- aggregate(temp_data$catchability, by=list(temp_data$month, temp_data$STATAREA, temp_data$assigneddepthstrata), sd)
  colnames(catchability_mean) <- c("month", "STATAREA", "assigneddepthstrata", "mean")
  colnames(catchability_sd) <- c("month", "STATAREA", "assigneddepthstrata", "sd")
  
  jpeg(filename = "./plot/catchability_year_month_strata.jpeg", width=160, height=150, units = "mm", res = 600)
  par(mfrow=c(2,2))
  #boxplot(catchability_mean$mean~catchability_mean$year, xlab="Year", ylab="Catchability", ylim=c(0,0.3), pch=16, cex=0.5)
  boxplot(catchability_mean$mean~catchability_mean$month, xlab="Month", ylab="Catchability", ylim=c(0,0.3), pch=16, cex=0.5)
  boxplot(catchability_mean$mean~catchability_mean$STATAREA, xlab="Statistical Area", ylab="Catchability", ylim=c(0,0.3), pch=16, cex=0.5)
  boxplot(catchability_mean$mean~catchability_mean$assigneddepthstrata, xlab="Depth Strata", ylab="Catchability", ylim=c(0,0.3), xlim=c(1.5,4.5), pch=16, cex=0.5)
  dev.off()
  
  catchability_data <- vts_vtrap_data
  
  catchability_data$month[(catchability_data$month==9)] <- 8
  
  merge_data <- merge(catchability_data, catchability_mean, by=c("month", "STATAREA", "assigneddepthstrata"))
  catchability_data$q_mean <- merge_data$mean
  merge_data <- merge(catchability_data, catchability_sd, by=c("month", "STATAREA", "assigneddepthstrata"))
  catchability_data$q_sd <- merge_data$sd
  
  
  simulated_vts_samples_q <- c()
  for(i in 1:length(simulated_vts_samples)){
    print(i)
    if(is.na(simulated_vts_samples[i])) simulated_vts_samples_q[i] <- NA
    else{
      simulated_vts_samples_q[i] <- simulated_vts_samples[i]*rnorm(1, mean=catchability_data$q_mean[i], sd=catchability_data$q_sd[i])
      if(simulated_vts_samples_q[i]<0) simulated_vts_samples_q[i]<-simulated_vts_samples[i]*catchability_data$q_mean[i]
    }
  }
  
  vts_vtrap_data$sim_quantity_q <- simulated_vts_samples_q
  
  catchability_data$month[(catchability_data$month==9)] <- 8
  
  #### vts simulated abundance index ####
  #vts_vtrap_data <- vts_vtrap_data[-which(vts_vtrap_data$year != 2014 & vts_vtrap_data$year != 2017 & is.na(vts_vtrap_data$sim_quantity)),]
  #vts_vtrap_data <- na.omit(vts_vtrap_data)
  summary(vts_vtrap_data)
  vts_vtrap_data <- na.omit(vts_vtrap_data)
  
  sim_lobster_per_year_sa_depth <- aggregate(vts_vtrap_data$sim_quantity, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata), sum)
  colnames(sim_lobster_per_year_sa_depth) <- c("Year", "SA", "Depth", "Lob_Quantity") 
  sim_lobster_per_year_sa_depth$Trap_Quantity <- aggregate(vts_vtrap_data$TRAP_ID, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata), length)$x
  sim_lobster_per_year_sa_depth$Site_Quantity <- aggregate(vts_vtrap_data$TRIP_ID, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata), function(x) length(unique(x)))$x
  summary(sim_lobster_per_year_sa_depth)
  sim_lobster_per_year_sa_depth$Ave_Lob_Trap <- sim_lobster_per_year_sa_depth$Lob_Quantity/sim_lobster_per_year_sa_depth$Trap_Quantity
  
  area_per_sa_depth <- as.data.frame(matrix(c(122, 82, 92, 296, 566, 395, 420, 1381, 315, 338, 198, 851), nrow=3, byrow=T))
  colnames(area_per_sa_depth) <- c("depth1", "depth2", "depth3", "total")
  rownames(area_per_sa_depth) <- c("511", "512", "513")
  
  ## sim_substrat_mean
  sim_substrat_mean <- matrix(NA, nrow=length(unique(sim_lobster_per_year_sa_depth$Year)), ncol=3)
  year_id <- unique(sim_lobster_per_year_sa_depth$Year)
  colnames(sim_substrat_mean) <- c("511", "512", "513")
  rownames(sim_substrat_mean) <- year_id
  for (i in 1:length(year_id)){
    sim_substrat_mean[i,1] <- area_per_sa_depth["511", "depth1"]/area_per_sa_depth["511", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["511", "depth2"]/area_per_sa_depth["511", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="21 to 40"& sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["511", "depth3"]/area_per_sa_depth["511", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="41 to 60"& sim_lobster_per_year_sa_depth$Year==year_id[i])] 
    
    sim_substrat_mean[i,2] <- area_per_sa_depth["512", "depth1"]/area_per_sa_depth["512", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["512", "depth2"]/area_per_sa_depth["512", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="21 to 40"& sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["512", "depth3"]/area_per_sa_depth["512", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="41 to 60"& sim_lobster_per_year_sa_depth$Year==year_id[i])] 
    
    sim_substrat_mean[i,3] <- area_per_sa_depth["513", "depth1"]/area_per_sa_depth["513", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["513", "depth2"]/area_per_sa_depth["513", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="21 to 40"& sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["513", "depth3"]/area_per_sa_depth["513", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="41 to 60"& sim_lobster_per_year_sa_depth$Year==year_id[i])] 
  }
  
  ## sim_substrat_var
  sim_lobster_per_year_sa_depth_site <- aggregate(vts_vtrap_data$sim_quantity, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata, vts_vtrap_data$SITE_ID), sum)
  colnames(sim_lobster_per_year_sa_depth_site) <- c("Year", "SA", "Depth", "Site", "Lob_Quantity") 
  
  sim_lobster_per_year_sa_depth_site$Trap_Quantity <- aggregate(vts_vtrap_data$TRAP_ID, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata, vts_vtrap_data$SITE_ID), length)$x
  
  sim_lobster_per_year_sa_depth_site$Quantity_per_Trap <- sim_lobster_per_year_sa_depth_site$Lob_Quantity/sim_lobster_per_year_sa_depth_site$Trap_Quantity
  
  sim_lobster_per_year_sa_depth$Ave_Lob_Trap <- sim_lobster_per_year_sa_depth$Lob_Quantity/sim_lobster_per_year_sa_depth$Trap_Quantity
  
  sim_lobster_per_year_sa_depth_site=merge(sim_lobster_per_year_sa_depth_site, sim_lobster_per_year_sa_depth[, c("Year", "SA", "Depth", "Ave_Lob_Trap")], by=c("Year", "SA", "Depth"))
  
  sim_lobster_per_year_sa_depth_site$Sub <- (sim_lobster_per_year_sa_depth_site$Ave_Lob_Trap - sim_lobster_per_year_sa_depth_site$Quantity_per_Trap)^2
  
  individual_value <- aggregate(sim_lobster_per_year_sa_depth_site$Sub, by=list(sim_lobster_per_year_sa_depth_site$Year, sim_lobster_per_year_sa_depth_site$SA, sim_lobster_per_year_sa_depth_site$Depth), sum)
  colnames(individual_value) <- c("Year", "SA", "Depth", "x")
  individual_value$individual_value <- individual_value$x/aggregate(sim_lobster_per_year_sa_depth_site$Site, by=list(sim_lobster_per_year_sa_depth_site$Year, sim_lobster_per_year_sa_depth_site$SA, sim_lobster_per_year_sa_depth_site$Depth), length)$x
  
  sim_substrat_var <- matrix(NA, nrow=length(unique(sim_lobster_per_year_sa_depth$Year)), ncol=3)
  colnames(sim_substrat_var) <- c("511", "512", "513")
  rownames(sim_substrat_var) <- year_id
  
  for (i in 1:length(year_id)){
    sim_substrat_var[i,1] <- 1/area_per_sa_depth["511", "total"]^2*((area_per_sa_depth["511", "depth1"]*(area_per_sa_depth["511", "depth1"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["511", "depth2"]*(area_per_sa_depth["511", "depth2"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["511", "depth3"]*(area_per_sa_depth["511", "depth3"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]))
    
    sim_substrat_var[i,2] <- 1/area_per_sa_depth["512", "total"]^2*((area_per_sa_depth["512", "depth1"]*(area_per_sa_depth["512", "depth1"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["512", "depth2"]*(area_per_sa_depth["512", "depth2"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["512", "depth3"]*(area_per_sa_depth["512", "depth3"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]))
    
    sim_substrat_var[i,3] <- 1/area_per_sa_depth["513", "total"]^2*((area_per_sa_depth["513", "depth1"]*(area_per_sa_depth["513", "depth1"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["513", "depth2"]*(area_per_sa_depth["513", "depth2"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["513", "depth3"]*(area_per_sa_depth["513", "depth3"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]))
  }
  
  jpeg(filename = "./plot/vts_lobster_substrat_mean_sim.jpeg", width=150, height=50, units = "mm", res = 600)
  par(mfrow=c(1,3))
  ylim_min <- min(na.omit(sim_substrat_mean))
  ylim_max <- max(na.omit(sim_substrat_mean))
  
  plot(year_id, sim_substrat_mean[,"513"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=1, cex=0.5, ylim=c(ylim_min, ylim_max))
  legend("topright", "513", bty="n")
  
  plot(year_id, sim_substrat_mean[,"512"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=1, cex=0.5, ylim=c(ylim_min, ylim_max))
  legend("topright", "512", bty="n")
  
  plot(year_id, sim_substrat_mean[,"511"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=1, cex=0.5, ylim=c(ylim_min, ylim_max))
  legend("topright", "511", bty="n")
  
  dev.off()
  
  if (FALSE) {
    sp6_fall9_cor <- c()
    sp6_fall9_cor[1] <- cor(substrat_mean[,"511"], sim_substrat_mean[,"511"], use = "pairwise.complete.obs")
    sp6_fall9_cor[2] <- cor(substrat_mean[,"512"], sim_substrat_mean[,"512"], use = "pairwise.complete.obs")
    sp6_fall9_cor[3] <- cor(substrat_mean[,"513"], sim_substrat_mean[,"513"], use = "pairwise.complete.obs")
    sp6_fall9_cor
  }
  
  
  fall_cor1 <- c()
  fall_cor1[1] <- cor(substrat_mean[,"511"], sim_substrat_mean[,"511"], use = "pairwise.complete.obs")
  fall_cor1[2] <- cor(substrat_mean[,"512"], sim_substrat_mean[,"512"], use = "pairwise.complete.obs")
  fall_cor1[3] <- cor(substrat_mean[,"513"], sim_substrat_mean[,"513"], use = "pairwise.complete.obs")
  fall_cor1
  
  
  #### Plot sim vts trends with consideration of q ####
  sim_lobster_per_year_sa_depth <- aggregate(vts_vtrap_data$sim_quantity_q, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata), sum)
  colnames(sim_lobster_per_year_sa_depth) <- c("Year", "SA", "Depth", "Lob_Quantity") 
  sim_lobster_per_year_sa_depth$Trap_Quantity <- aggregate(vts_vtrap_data$TRAP_ID, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata), length)$x
  sim_lobster_per_year_sa_depth$Site_Quantity <- aggregate(vts_vtrap_data$TRIP_ID, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata), function(x) length(unique(x)))$x
  summary(sim_lobster_per_year_sa_depth)
  sim_lobster_per_year_sa_depth$Ave_Lob_Trap <- sim_lobster_per_year_sa_depth$Lob_Quantity/sim_lobster_per_year_sa_depth$Trap_Quantity
  
  area_per_sa_depth <- as.data.frame(matrix(c(122, 82, 92, 296, 566, 395, 420, 1381, 315, 338, 198, 851), nrow=3, byrow=T))
  colnames(area_per_sa_depth) <- c("depth1", "depth2", "depth3", "total")
  rownames(area_per_sa_depth) <- c("511", "512", "513")
  
  ## sim_substrat_mean
  sim_substrat_mean <- matrix(NA, nrow=length(unique(sim_lobster_per_year_sa_depth$Year)), ncol=3)
  year_id <- unique(sim_lobster_per_year_sa_depth$Year)
  colnames(sim_substrat_mean) <- c("511", "512", "513")
  rownames(sim_substrat_mean) <- year_id
  for (i in 1:length(year_id)){
    sim_substrat_mean[i,1] <- area_per_sa_depth["511", "depth1"]/area_per_sa_depth["511", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["511", "depth2"]/area_per_sa_depth["511", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="21 to 40"& sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["511", "depth3"]/area_per_sa_depth["511", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="41 to 60"& sim_lobster_per_year_sa_depth$Year==year_id[i])] 
    
    sim_substrat_mean[i,2] <- area_per_sa_depth["512", "depth1"]/area_per_sa_depth["512", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["512", "depth2"]/area_per_sa_depth["512", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="21 to 40"& sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["512", "depth3"]/area_per_sa_depth["512", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="41 to 60"& sim_lobster_per_year_sa_depth$Year==year_id[i])] 
    
    sim_substrat_mean[i,3] <- area_per_sa_depth["513", "depth1"]/area_per_sa_depth["513", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["513", "depth2"]/area_per_sa_depth["513", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="21 to 40"& sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["513", "depth3"]/area_per_sa_depth["513", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="41 to 60"& sim_lobster_per_year_sa_depth$Year==year_id[i])] 
  }
  
  ## sim_substrat_var
  sim_lobster_per_year_sa_depth_site <- aggregate(vts_vtrap_data$sim_quantity, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata, vts_vtrap_data$SITE_ID), sum)
  colnames(sim_lobster_per_year_sa_depth_site) <- c("Year", "SA", "Depth", "Site", "Lob_Quantity") 
  
  sim_lobster_per_year_sa_depth_site$Trap_Quantity <- aggregate(vts_vtrap_data$TRAP_ID, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata, vts_vtrap_data$SITE_ID), length)$x
  
  sim_lobster_per_year_sa_depth_site$Quantity_per_Trap <- sim_lobster_per_year_sa_depth_site$Lob_Quantity/sim_lobster_per_year_sa_depth_site$Trap_Quantity
  
  sim_lobster_per_year_sa_depth$Ave_Lob_Trap <- sim_lobster_per_year_sa_depth$Lob_Quantity/sim_lobster_per_year_sa_depth$Trap_Quantity
  
  sim_lobster_per_year_sa_depth_site=merge(sim_lobster_per_year_sa_depth_site, sim_lobster_per_year_sa_depth[, c("Year", "SA", "Depth", "Ave_Lob_Trap")], by=c("Year", "SA", "Depth"))
  
  sim_lobster_per_year_sa_depth_site$Sub <- (sim_lobster_per_year_sa_depth_site$Ave_Lob_Trap - sim_lobster_per_year_sa_depth_site$Quantity_per_Trap)^2
  
  individual_value <- aggregate(sim_lobster_per_year_sa_depth_site$Sub, by=list(sim_lobster_per_year_sa_depth_site$Year, sim_lobster_per_year_sa_depth_site$SA, sim_lobster_per_year_sa_depth_site$Depth), sum)
  colnames(individual_value) <- c("Year", "SA", "Depth", "x")
  individual_value$individual_value <- individual_value$x/aggregate(sim_lobster_per_year_sa_depth_site$Site, by=list(sim_lobster_per_year_sa_depth_site$Year, sim_lobster_per_year_sa_depth_site$SA, sim_lobster_per_year_sa_depth_site$Depth), length)$x
  
  sim_substrat_var <- matrix(NA, nrow=length(unique(sim_lobster_per_year_sa_depth$Year)), ncol=3)
  colnames(sim_substrat_var) <- c("511", "512", "513")
  rownames(sim_substrat_var) <- year_id
  
  for (i in 1:length(year_id)){
    sim_substrat_var[i,1] <- 1/area_per_sa_depth["511", "total"]^2*((area_per_sa_depth["511", "depth1"]*(area_per_sa_depth["511", "depth1"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["511", "depth2"]*(area_per_sa_depth["511", "depth2"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["511", "depth3"]*(area_per_sa_depth["511", "depth3"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]))
    
    sim_substrat_var[i,2] <- 1/area_per_sa_depth["512", "total"]^2*((area_per_sa_depth["512", "depth1"]*(area_per_sa_depth["512", "depth1"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["512", "depth2"]*(area_per_sa_depth["512", "depth2"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["512", "depth3"]*(area_per_sa_depth["512", "depth3"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]))
    
    sim_substrat_var[i,3] <- 1/area_per_sa_depth["513", "total"]^2*((area_per_sa_depth["513", "depth1"]*(area_per_sa_depth["513", "depth1"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["513", "depth2"]*(area_per_sa_depth["513", "depth2"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["513", "depth3"]*(area_per_sa_depth["513", "depth3"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]))
  }
  
  jpeg(filename = "./plot/vts_lobster_substrat_mean_sim_q.jpeg", width=150, height=50, units = "mm", res = 600)
  par(mfrow=c(1,3))
  ylim_min <- min(na.omit(sim_substrat_mean))
  ylim_max <- max(na.omit(sim_substrat_mean))
  
  plot(year_id, sim_substrat_mean[,"513"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=1, cex=0.5, ylim=c(ylim_min, ylim_max))
  legend("topright", "513", bty="n")
  
  plot(year_id, sim_substrat_mean[,"512"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=1, cex=0.5, ylim=c(ylim_min, ylim_max))
  legend("topright", "512", bty="n")
  
  plot(year_id, sim_substrat_mean[,"511"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=1, cex=0.5, ylim=c(ylim_min, ylim_max))
  legend("topright", "511", bty="n")
  
  dev.off()
  
  fall_cor2 <- c()
  fall_cor2[1] <- cor(substrat_mean[,"511"], sim_substrat_mean[,"511"], use = "pairwise.complete.obs")
  fall_cor2[2] <- cor(substrat_mean[,"512"], sim_substrat_mean[,"512"], use = "pairwise.complete.obs")
  fall_cor2[3] <- cor(substrat_mean[,"513"], sim_substrat_mean[,"513"], use = "pairwise.complete.obs")
  fall_cor2
  #### population index ####
  polygon_points <- over(prediction_points, sa511_513)
  summary(polygon_points$Id)
  
  population_511_month_index <- c()
  population_512_month_index <- c()
  population_513_month_index <- c()
  
  for (i in 1:length(prediction_mean)){
    population_511_month_index[i] <- sum(prediction_mean[[i]][which(polygon_points$Id==511)], na.rm = T)
    population_512_month_index[i] <- sum(prediction_mean[[i]][which(polygon_points$Id==512)], na.rm = T)
    population_513_month_index[i] <- sum(prediction_mean[[i]][which(polygon_points$Id==513)], na.rm = T)
  }
  population_511_year_index <- rowMeans(matrix(population_511_month_index, ncol=3, byrow = T))
  population_511_year_index <- c(population_511_year_index[1:8], NA, population_511_year_index[9:10], NA)
  population_512_year_index <- rowMeans(matrix(population_512_month_index, ncol=3, byrow = T))
  population_512_year_index <- c(population_512_year_index[1:8], NA, population_512_year_index[9:10], NA)
  population_513_year_index <- rowMeans(matrix(population_513_month_index, ncol=3, byrow = T))
  population_513_year_index <- c(population_513_year_index[1:8], NA, population_513_year_index[9:10], NA)
  
  jpeg(filename = "./plot/pop_vts_lobster_substrat_mean.jpeg", width=175, height=60, units = "mm", res = 600)
  
  par(mfrow=c(1,3), mar=c(3.5,3.5,2,3.5))
  
  plot(x = year_id-0.13, y = population_513_year_index[1:length(year_id)], col = "deepskyblue3", type = "h", xlab = "", ylab="", main = "", lwd=3, xlim=c(2006, 2017))
  mtext("Simulated Lobster Population", side=2, line=2, col="deepskyblue3", cex=0.6)
  mtext("Year", side=1, line=2, cex=0.6)
  par(new = T)
  plot(x = year_id+0.13, y = sim_substrat_mean[1:length(year_id),"513"], col = "coral3", type = "h", xaxt = "n", yaxt = "n", xlab = "", ylab = "", lwd=3, xlim=c(2006, 2017))
  axis(4)
  mtext("VTS Abundance Index", side = 4, line = 2, col="coral3", cex=0.6)
  #legend("topleft", paste("513:\nCor = ",  round(cor(na.omit(sim_substrat_mean[,"513"]), na.omit(population_513_year_index)),2), sep=""), bty="n")
  
  plot(x = year_id-0.13, y = population_512_year_index[1:length(year_id)], col = "deepskyblue3", type = "h", xlab = "", ylab="", main = "", lwd=3, xlim=c(2006, 2017))
  mtext("Simulated Lobster Population", side=2, line=2, col="deepskyblue3", cex=0.6)
  mtext("Year", side=1, line=2, cex=0.6)
  par(new = T)
  plot(x = year_id+0.13, y = sim_substrat_mean[1:length(year_id),"512"], col = "coral3", type = "h", xaxt = "n", yaxt = "n", xlab = "", ylab = "", lwd=3, xlim=c(2006, 2017))
  axis(4)
  mtext("VTS Abundance Index", side = 4, line = 2, col="coral3", cex=0.6)
  #legend("topleft", paste("512:\nCor = ",  round(cor(na.omit(sim_substrat_mean[,"512"]), na.omit(population_512_year_index)),2), sep=""), bty="n")
  
  plot(x = year_id-0.13, y = population_511_year_index[1:length(year_id)], col = "deepskyblue3", type = "h", xlab = "", ylab="", main = "", lwd=3, xlim=c(2006, 2017))
  mtext("Simulated Lobster Population", side=2, line=2, col="deepskyblue3", cex=0.6)
  mtext("Year", side=1, line=2, cex=0.6)
  par(new = T)
  plot(x = year_id+0.13, y = sim_substrat_mean[1:length(year_id),"511"], col = "coral3", type = "h", xaxt = "n", yaxt = "n", xlab = "", ylab = "", lwd=3, xlim=c(2006, 2017))
  axis(4)
  mtext("VTS Abundance Index", side = 4, line = 2, col="coral3", cex=0.6)
  #legend("topleft", paste("511:\nCor = ",  round(cor(na.omit(sim_substrat_mean[,"511"]), na.omit(population_511_year_index)),2), sep=""), bty="n")
  dev.off()
  
  jpeg(filename = "./plot/pop_vts_lobster_substrat_mean2.jpeg", width=175, height=60, units = "mm", res = 300)
  
  par(mfrow=c(1,3), mar=c(3.5,3.5,2,3.5))
  
  plot(x = year_id, y = population_513_year_index[1:length(year_id)], col = "deepskyblue3", type = "h", xlab = "", ylab="", main = "", lwd=5, xlim=c(2006, 2017))
  mtext("Simulated Lobster Population", side=2, line=2, col="deepskyblue3", cex=0.6)
  mtext("Year", side=1, line=2, cex=0.6)
  par(new = T)
  plot(x = year_id, y = sim_substrat_mean[1:length(year_id),"513"], col = "coral3", type = "o", xaxt = "n", yaxt = "n", xlab = "", ylab = "", lwd=2, xlim=c(2006, 2017), pch=16)
  axis(4)
  mtext("VTS Abundance Index", side = 4, line = 2, col="coral3", cex=0.6)
  legend("topright", paste("513:\nCor = ",  round(cor(na.omit(sim_substrat_mean[,"513"]), na.omit(population_513_year_index)),2), sep=""), bty="n")
  
  plot(x = year_id, y = population_512_year_index[1:length(year_id)], col = "deepskyblue3", type = "h", xlab = "", ylab="", main = "", lwd=5, xlim=c(2006, 2017))
  mtext("Simulated Lobster Population", side=2, line=2, col="deepskyblue3", cex=0.6)
  mtext("Year", side=1, line=2, cex=0.6)
  par(new = T)
  plot(x = year_id, y = sim_substrat_mean[1:length(year_id),"512"], col = "coral3", type = "o", xaxt = "n", yaxt = "n", xlab = "", ylab = "", lwd=2, xlim=c(2006, 2017), pch=16)
  axis(4)
  mtext("VTS Abundance Index", side = 4, line = 2, col="coral3", cex=0.6)
  legend("topright", paste("512:\nCor = ",  round(cor(na.omit(sim_substrat_mean[,"512"]), na.omit(population_512_year_index)),2), sep=""), bty="n")
  
  plot(x = year_id, y = population_511_year_index[1:length(year_id)], col = "deepskyblue3", type = "h", xlab = "", ylab="", main = "", lwd=5, xlim=c(2006, 2017))
  mtext("Simulated Lobster Population", side=2, line=2, col="deepskyblue3", cex=0.6)
  mtext("Year", side=1, line=2, cex=0.6)
  par(new = T)
  plot(x = year_id, y = sim_substrat_mean[1:length(year_id),"511"], col = "coral3", type = "o", xaxt = "n", yaxt = "n", xlab = "", ylab = "", lwd=2, xlim=c(2006, 2017), pch=16)
  axis(4)
  mtext("VTS Abundance Index", side = 4, line = 2, col="coral3", cex=0.6)
  legend("topright", paste("511:\nCor = ",  round(cor(na.omit(sim_substrat_mean[,"511"]), na.omit(population_511_year_index)),2), sep=""), bty="n")
  dev.off()
} #### catchability with month, statistical area, and depth

if (FALSE){
  #nao_temperature <- nao_temperature[which(nao_temperature$ï..Year>2005 & nao_temperature$ï..Year<2017),c("ï..Year","Jun", "Jul", "Aug")]
  nao_temperature_point <- c()
  for(i in 1:nrow(vts_vtrap_data)){
    if(vts_vtrap_data$month[i]==6) nao_temperature_point[i] <- nao_temperature[which(nao_temperature$ï..Year==vts_vtrap_data$year[i]),"Jun"]
    if(vts_vtrap_data$month[i]==7) nao_temperature_point[i] <- nao_temperature[which(nao_temperature$ï..Year==vts_vtrap_data$year[i]),"Jul"]
    if(vts_vtrap_data$month[i]==8) nao_temperature_point[i] <- nao_temperature[which(nao_temperature$ï..Year==vts_vtrap_data$year[i]),"Aug"]
    if(vts_vtrap_data$month[i]==9) nao_temperature_point[i] <- nao_temperature[which(nao_temperature$ï..Year==vts_vtrap_data$year[i]),"Aug"]
  }
  vts_vtrap_data$temperature <- nao_temperature_point
  vts_vtrap_data$catchability<- vts_vtrap_data$quantity/vts_vtrap_data$sim_quantity
  vts_vtrap_data[which(vts_vtrap_data$catchability>1),]
  
  temp_data <- na.omit(vts_vtrap_data)
  temp_data <- temp_data[which(temp_data$catchability<=1),]
  temp_data$month[(temp_data$month==9)] <- 8
  catchability_mean <- aggregate(temp_data$catchability, by=list(temp_data$temperature, temp_data$month, temp_data$STATAREA, temp_data$assigneddepthstrata), mean)
  catchability_sd <- aggregate(temp_data$catchability, by=list(temp_data$temperature, temp_data$month, temp_data$STATAREA, temp_data$assigneddepthstrata), sd)
  colnames(catchability_mean) <- c("temperature", "month", "STATAREA", "assigneddepthstrata", "mean")
  colnames(catchability_sd) <- c("temperature", "month", "STATAREA", "assigneddepthstrata", "sd")
  
  jpeg(filename = "./plot/catchability_year_month_strata.jpeg", width=160, height=150, units = "mm", res = 600)
  par(mfrow=c(2,2))
  boxplot(catchability_mean$mean~catchability_mean$temperature, xlab="Temperature", ylab="Catchability", ylim=c(0,0.3), pch=16, cex=0.5)
  boxplot(catchability_mean$mean~catchability_mean$month, xlab="Month", ylab="Catchability", ylim=c(0,0.3), pch=16, cex=0.5)
  boxplot(catchability_mean$mean~catchability_mean$STATAREA, xlab="Statistical Area", ylab="Catchability", ylim=c(0,0.3), pch=16, cex=0.5)
  boxplot(catchability_mean$mean~catchability_mean$assigneddepthstrata, xlab="Depth Strata", ylab="Catchability", ylim=c(0,0.3), xlim=c(1.5,4.5), pch=16, cex=0.5)
  dev.off()
  
  catchability_data <- vts_vtrap_data
  #catchability_data <- na.omit(catchability_data)
  
  catchability_data$month[(catchability_data$month==9)] <- 8
  
  merge_data <- merge(catchability_data, catchability_mean, by=c("temperature","month", "STATAREA", "assigneddepthstrata"))
  catchability_data$q_mean <- merge_data$mean
  merge_data <- merge(catchability_data, catchability_sd, by=c("temperature","month", "STATAREA", "assigneddepthstrata"))
  catchability_data$q_sd <- merge_data$sd
  
  
  simulated_vts_samples_q <- c()
  for(i in 1:length(simulated_vts_samples)){
    print(i)
    if(is.na(simulated_vts_samples[i])) simulated_vts_samples_q[i] <- NA
    else{
      simulated_vts_samples_q[i] <- simulated_vts_samples[i]*rnorm(1, mean=catchability_data$q_mean[i], sd=catchability_data$q_sd[i])
      if(simulated_vts_samples_q[i]<0) simulated_vts_samples_q[i]<-simulated_vts_samples[i]*catchability_data$q_mean[i]
    }
  }
  
  vts_vtrap_data$sim_quantity_q <- simulated_vts_samples_q
  
  catchability_data$month[(catchability_data$month==9)] <- 8
  
  #### vts simulated abundance index ####
  #vts_vtrap_data <- vts_vtrap_data[-which(vts_vtrap_data$year != 2014 & vts_vtrap_data$year != 2017 & is.na(vts_vtrap_data$sim_quantity)),]
  #vts_vtrap_data <- na.omit(vts_vtrap_data)
  summary(vts_vtrap_data)
  vts_vtrap_data <- na.omit(vts_vtrap_data)
  
  sim_lobster_per_year_sa_depth <- aggregate(vts_vtrap_data$sim_quantity, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata), sum)
  colnames(sim_lobster_per_year_sa_depth) <- c("Year", "SA", "Depth", "Lob_Quantity") 
  sim_lobster_per_year_sa_depth$Trap_Quantity <- aggregate(vts_vtrap_data$TRAP_ID, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata), length)$x
  sim_lobster_per_year_sa_depth$Site_Quantity <- aggregate(vts_vtrap_data$TRIP_ID, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata), function(x) length(unique(x)))$x
  summary(sim_lobster_per_year_sa_depth)
  sim_lobster_per_year_sa_depth$Ave_Lob_Trap <- sim_lobster_per_year_sa_depth$Lob_Quantity/sim_lobster_per_year_sa_depth$Trap_Quantity
  
  area_per_sa_depth <- as.data.frame(matrix(c(122, 82, 92, 296, 566, 395, 420, 1381, 315, 338, 198, 851), nrow=3, byrow=T))
  colnames(area_per_sa_depth) <- c("depth1", "depth2", "depth3", "total")
  rownames(area_per_sa_depth) <- c("511", "512", "513")
  
  ## sim_substrat_mean
  sim_substrat_mean <- matrix(NA, nrow=length(unique(sim_lobster_per_year_sa_depth$Year)), ncol=3)
  year_id <- unique(sim_lobster_per_year_sa_depth$Year)
  colnames(sim_substrat_mean) <- c("511", "512", "513")
  rownames(sim_substrat_mean) <- year_id
  for (i in 1:length(year_id)){
    sim_substrat_mean[i,1] <- area_per_sa_depth["511", "depth1"]/area_per_sa_depth["511", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["511", "depth2"]/area_per_sa_depth["511", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="21 to 40"& sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["511", "depth3"]/area_per_sa_depth["511", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="41 to 60"& sim_lobster_per_year_sa_depth$Year==year_id[i])] 
    
    sim_substrat_mean[i,2] <- area_per_sa_depth["512", "depth1"]/area_per_sa_depth["512", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["512", "depth2"]/area_per_sa_depth["512", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="21 to 40"& sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["512", "depth3"]/area_per_sa_depth["512", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="41 to 60"& sim_lobster_per_year_sa_depth$Year==year_id[i])] 
    
    sim_substrat_mean[i,3] <- area_per_sa_depth["513", "depth1"]/area_per_sa_depth["513", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["513", "depth2"]/area_per_sa_depth["513", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="21 to 40"& sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["513", "depth3"]/area_per_sa_depth["513", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="41 to 60"& sim_lobster_per_year_sa_depth$Year==year_id[i])] 
  }
  
  ## sim_substrat_var
  sim_lobster_per_year_sa_depth_site <- aggregate(vts_vtrap_data$sim_quantity, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata, vts_vtrap_data$SITE_ID), sum)
  colnames(sim_lobster_per_year_sa_depth_site) <- c("Year", "SA", "Depth", "Site", "Lob_Quantity") 
  
  sim_lobster_per_year_sa_depth_site$Trap_Quantity <- aggregate(vts_vtrap_data$TRAP_ID, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata, vts_vtrap_data$SITE_ID), length)$x
  
  sim_lobster_per_year_sa_depth_site$Quantity_per_Trap <- sim_lobster_per_year_sa_depth_site$Lob_Quantity/sim_lobster_per_year_sa_depth_site$Trap_Quantity
  
  sim_lobster_per_year_sa_depth$Ave_Lob_Trap <- sim_lobster_per_year_sa_depth$Lob_Quantity/sim_lobster_per_year_sa_depth$Trap_Quantity
  
  sim_lobster_per_year_sa_depth_site=merge(sim_lobster_per_year_sa_depth_site, sim_lobster_per_year_sa_depth[, c("Year", "SA", "Depth", "Ave_Lob_Trap")], by=c("Year", "SA", "Depth"))
  
  sim_lobster_per_year_sa_depth_site$Sub <- (sim_lobster_per_year_sa_depth_site$Ave_Lob_Trap - sim_lobster_per_year_sa_depth_site$Quantity_per_Trap)^2
  
  individual_value <- aggregate(sim_lobster_per_year_sa_depth_site$Sub, by=list(sim_lobster_per_year_sa_depth_site$Year, sim_lobster_per_year_sa_depth_site$SA, sim_lobster_per_year_sa_depth_site$Depth), sum)
  colnames(individual_value) <- c("Year", "SA", "Depth", "x")
  individual_value$individual_value <- individual_value$x/aggregate(sim_lobster_per_year_sa_depth_site$Site, by=list(sim_lobster_per_year_sa_depth_site$Year, sim_lobster_per_year_sa_depth_site$SA, sim_lobster_per_year_sa_depth_site$Depth), length)$x
  
  sim_substrat_var <- matrix(NA, nrow=length(unique(sim_lobster_per_year_sa_depth$Year)), ncol=3)
  colnames(sim_substrat_var) <- c("511", "512", "513")
  rownames(sim_substrat_var) <- year_id
  
  for (i in 1:length(year_id)){
    sim_substrat_var[i,1] <- 1/area_per_sa_depth["511", "total"]^2*((area_per_sa_depth["511", "depth1"]*(area_per_sa_depth["511", "depth1"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["511", "depth2"]*(area_per_sa_depth["511", "depth2"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["511", "depth3"]*(area_per_sa_depth["511", "depth3"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]))
    
    sim_substrat_var[i,2] <- 1/area_per_sa_depth["512", "total"]^2*((area_per_sa_depth["512", "depth1"]*(area_per_sa_depth["512", "depth1"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["512", "depth2"]*(area_per_sa_depth["512", "depth2"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["512", "depth3"]*(area_per_sa_depth["512", "depth3"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]))
    
    sim_substrat_var[i,3] <- 1/area_per_sa_depth["513", "total"]^2*((area_per_sa_depth["513", "depth1"]*(area_per_sa_depth["513", "depth1"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["513", "depth2"]*(area_per_sa_depth["513", "depth2"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["513", "depth3"]*(area_per_sa_depth["513", "depth3"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]))
  }
  
  jpeg(filename = "./plot/vts_lobster_substrat_mean_sim.jpeg", width=150, height=50, units = "mm", res = 600)
  par(mfrow=c(1,3))
  ylim_min <- min(na.omit(sim_substrat_mean))
  ylim_max <- max(na.omit(sim_substrat_mean))
  
  plot(year_id, sim_substrat_mean[,"513"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=1, cex=0.5, ylim=c(ylim_min, ylim_max))
  legend("topright", "513", bty="n")
  
  plot(year_id, sim_substrat_mean[,"512"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=1, cex=0.5, ylim=c(ylim_min, ylim_max))
  legend("topright", "512", bty="n")
  
  plot(year_id, sim_substrat_mean[,"511"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=1, cex=0.5, ylim=c(ylim_min, ylim_max))
  legend("topright", "511", bty="n")
  
  dev.off()
  
  if (FALSE) {
    sp6_fall9_cor <- c()
    sp6_fall9_cor[1] <- cor(substrat_mean[,"511"], sim_substrat_mean[,"511"], use = "pairwise.complete.obs")
    sp6_fall9_cor[2] <- cor(substrat_mean[,"512"], sim_substrat_mean[,"512"], use = "pairwise.complete.obs")
    sp6_fall9_cor[3] <- cor(substrat_mean[,"513"], sim_substrat_mean[,"513"], use = "pairwise.complete.obs")
    sp6_fall9_cor
  }
  
  
  fall_cor1 <- c()
  fall_cor1[1] <- cor(substrat_mean[,"511"], sim_substrat_mean[,"511"], use = "pairwise.complete.obs")
  fall_cor1[2] <- cor(substrat_mean[,"512"], sim_substrat_mean[,"512"], use = "pairwise.complete.obs")
  fall_cor1[3] <- cor(substrat_mean[,"513"], sim_substrat_mean[,"513"], use = "pairwise.complete.obs")
  fall_cor1
  
  
  #### Plot sim vts trends with consideration of q ####
  sim_lobster_per_year_sa_depth <- aggregate(vts_vtrap_data$sim_quantity_q, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata), sum)
  colnames(sim_lobster_per_year_sa_depth) <- c("Year", "SA", "Depth", "Lob_Quantity") 
  sim_lobster_per_year_sa_depth$Trap_Quantity <- aggregate(vts_vtrap_data$TRAP_ID, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata), length)$x
  sim_lobster_per_year_sa_depth$Site_Quantity <- aggregate(vts_vtrap_data$TRIP_ID, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata), function(x) length(unique(x)))$x
  summary(sim_lobster_per_year_sa_depth)
  sim_lobster_per_year_sa_depth$Ave_Lob_Trap <- sim_lobster_per_year_sa_depth$Lob_Quantity/sim_lobster_per_year_sa_depth$Trap_Quantity
  
  area_per_sa_depth <- as.data.frame(matrix(c(122, 82, 92, 296, 566, 395, 420, 1381, 315, 338, 198, 851), nrow=3, byrow=T))
  colnames(area_per_sa_depth) <- c("depth1", "depth2", "depth3", "total")
  rownames(area_per_sa_depth) <- c("511", "512", "513")
  
  ## sim_substrat_mean
  sim_substrat_mean <- matrix(NA, nrow=length(unique(sim_lobster_per_year_sa_depth$Year)), ncol=3)
  year_id <- unique(sim_lobster_per_year_sa_depth$Year)
  colnames(sim_substrat_mean) <- c("511", "512", "513")
  rownames(sim_substrat_mean) <- year_id
  for (i in 1:length(year_id)){
    sim_substrat_mean[i,1] <- area_per_sa_depth["511", "depth1"]/area_per_sa_depth["511", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["511", "depth2"]/area_per_sa_depth["511", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="21 to 40"& sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["511", "depth3"]/area_per_sa_depth["511", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="41 to 60"& sim_lobster_per_year_sa_depth$Year==year_id[i])] 
    
    sim_substrat_mean[i,2] <- area_per_sa_depth["512", "depth1"]/area_per_sa_depth["512", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["512", "depth2"]/area_per_sa_depth["512", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="21 to 40"& sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["512", "depth3"]/area_per_sa_depth["512", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="41 to 60"& sim_lobster_per_year_sa_depth$Year==year_id[i])] 
    
    sim_substrat_mean[i,3] <- area_per_sa_depth["513", "depth1"]/area_per_sa_depth["513", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["513", "depth2"]/area_per_sa_depth["513", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="21 to 40"& sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["513", "depth3"]/area_per_sa_depth["513", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="41 to 60"& sim_lobster_per_year_sa_depth$Year==year_id[i])] 
  }
  
  ## sim_substrat_var
  sim_lobster_per_year_sa_depth_site <- aggregate(vts_vtrap_data$sim_quantity, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata, vts_vtrap_data$SITE_ID), sum)
  colnames(sim_lobster_per_year_sa_depth_site) <- c("Year", "SA", "Depth", "Site", "Lob_Quantity") 
  
  sim_lobster_per_year_sa_depth_site$Trap_Quantity <- aggregate(vts_vtrap_data$TRAP_ID, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata, vts_vtrap_data$SITE_ID), length)$x
  
  sim_lobster_per_year_sa_depth_site$Quantity_per_Trap <- sim_lobster_per_year_sa_depth_site$Lob_Quantity/sim_lobster_per_year_sa_depth_site$Trap_Quantity
  
  sim_lobster_per_year_sa_depth$Ave_Lob_Trap <- sim_lobster_per_year_sa_depth$Lob_Quantity/sim_lobster_per_year_sa_depth$Trap_Quantity
  
  sim_lobster_per_year_sa_depth_site=merge(sim_lobster_per_year_sa_depth_site, sim_lobster_per_year_sa_depth[, c("Year", "SA", "Depth", "Ave_Lob_Trap")], by=c("Year", "SA", "Depth"))
  
  sim_lobster_per_year_sa_depth_site$Sub <- (sim_lobster_per_year_sa_depth_site$Ave_Lob_Trap - sim_lobster_per_year_sa_depth_site$Quantity_per_Trap)^2
  
  individual_value <- aggregate(sim_lobster_per_year_sa_depth_site$Sub, by=list(sim_lobster_per_year_sa_depth_site$Year, sim_lobster_per_year_sa_depth_site$SA, sim_lobster_per_year_sa_depth_site$Depth), sum)
  colnames(individual_value) <- c("Year", "SA", "Depth", "x")
  individual_value$individual_value <- individual_value$x/aggregate(sim_lobster_per_year_sa_depth_site$Site, by=list(sim_lobster_per_year_sa_depth_site$Year, sim_lobster_per_year_sa_depth_site$SA, sim_lobster_per_year_sa_depth_site$Depth), length)$x
  
  sim_substrat_var <- matrix(NA, nrow=length(unique(sim_lobster_per_year_sa_depth$Year)), ncol=3)
  colnames(sim_substrat_var) <- c("511", "512", "513")
  rownames(sim_substrat_var) <- year_id
  
  for (i in 1:length(year_id)){
    sim_substrat_var[i,1] <- 1/area_per_sa_depth["511", "total"]^2*((area_per_sa_depth["511", "depth1"]*(area_per_sa_depth["511", "depth1"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["511", "depth2"]*(area_per_sa_depth["511", "depth2"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["511", "depth3"]*(area_per_sa_depth["511", "depth3"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]))
    
    sim_substrat_var[i,2] <- 1/area_per_sa_depth["512", "total"]^2*((area_per_sa_depth["512", "depth1"]*(area_per_sa_depth["512", "depth1"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["512", "depth2"]*(area_per_sa_depth["512", "depth2"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["512", "depth3"]*(area_per_sa_depth["512", "depth3"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]))
    
    sim_substrat_var[i,3] <- 1/area_per_sa_depth["513", "total"]^2*((area_per_sa_depth["513", "depth1"]*(area_per_sa_depth["513", "depth1"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["513", "depth2"]*(area_per_sa_depth["513", "depth2"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["513", "depth3"]*(area_per_sa_depth["513", "depth3"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]))
  }
  
  jpeg(filename = "./plot/vts_lobster_substrat_mean_sim_q.jpeg", width=150, height=50, units = "mm", res = 600)
  par(mfrow=c(1,3))
  ylim_min <- min(na.omit(sim_substrat_mean))
  ylim_max <- max(na.omit(sim_substrat_mean))
  
  plot(year_id, sim_substrat_mean[,"513"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=1, cex=0.5, ylim=c(ylim_min, ylim_max))
  legend("topright", "513", bty="n")
  
  plot(year_id, sim_substrat_mean[,"512"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=1, cex=0.5, ylim=c(ylim_min, ylim_max))
  legend("topright", "512", bty="n")
  
  plot(year_id, sim_substrat_mean[,"511"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=1, cex=0.5, ylim=c(ylim_min, ylim_max))
  legend("topright", "511", bty="n")
  
  dev.off()
  
  fall_cor2 <- c()
  fall_cor2[1] <- cor(substrat_mean[,"511"], sim_substrat_mean[,"511"], use = "pairwise.complete.obs")
  fall_cor2[2] <- cor(substrat_mean[,"512"], sim_substrat_mean[,"512"], use = "pairwise.complete.obs")
  fall_cor2[3] <- cor(substrat_mean[,"513"], sim_substrat_mean[,"513"], use = "pairwise.complete.obs")
  fall_cor2
  #### population index ####
  polygon_points <- over(prediction_points, sa511_513)
  summary(polygon_points$Id)
  
  population_511_month_index <- c()
  population_512_month_index <- c()
  population_513_month_index <- c()
  
  for (i in 1:length(prediction_mean)){
    population_511_month_index[i] <- sum(prediction_mean[[i]][which(polygon_points$Id==511)], na.rm = T)
    population_512_month_index[i] <- sum(prediction_mean[[i]][which(polygon_points$Id==512)], na.rm = T)
    population_513_month_index[i] <- sum(prediction_mean[[i]][which(polygon_points$Id==513)], na.rm = T)
  }
  population_511_year_index <- rowMeans(matrix(population_511_month_index, ncol=3, byrow = T))
  population_511_year_index <- c(population_511_year_index[1:8], NA, population_511_year_index[9:10], NA)
  population_512_year_index <- rowMeans(matrix(population_512_month_index, ncol=3, byrow = T))
  population_512_year_index <- c(population_512_year_index[1:8], NA, population_512_year_index[9:10], NA)
  population_513_year_index <- rowMeans(matrix(population_513_month_index, ncol=3, byrow = T))
  population_513_year_index <- c(population_513_year_index[1:8], NA, population_513_year_index[9:10], NA)
  
  jpeg(filename = "./plot/pop_vts_lobster_substrat_mean.jpeg", width=175, height=60, units = "mm", res = 600)
  
  par(mfrow=c(1,3), mar=c(3.5,3.5,2,3.5))
  
  plot(x = year_id-0.13, y = population_513_year_index[1:length(year_id)], col = "deepskyblue3", type = "h", xlab = "", ylab="", main = "", lwd=3, xlim=c(2006, 2017))
  mtext("Simulated Lobster Population", side=2, line=2, col="deepskyblue3", cex=0.6)
  mtext("Year", side=1, line=2, cex=0.6)
  par(new = T)
  plot(x = year_id+0.13, y = sim_substrat_mean[1:length(year_id),"513"], col = "coral3", type = "h", xaxt = "n", yaxt = "n", xlab = "", ylab = "", lwd=3, xlim=c(2006, 2017))
  axis(4)
  mtext("VTS Abundance Index", side = 4, line = 2, col="coral3", cex=0.6)
  #legend("topleft", paste("513:\nCor = ",  round(cor(na.omit(sim_substrat_mean[,"513"]), na.omit(population_513_year_index)),2), sep=""), bty="n")
  
  plot(x = year_id-0.13, y = population_512_year_index[1:length(year_id)], col = "deepskyblue3", type = "h", xlab = "", ylab="", main = "", lwd=3, xlim=c(2006, 2017))
  mtext("Simulated Lobster Population", side=2, line=2, col="deepskyblue3", cex=0.6)
  mtext("Year", side=1, line=2, cex=0.6)
  par(new = T)
  plot(x = year_id+0.13, y = sim_substrat_mean[1:length(year_id),"512"], col = "coral3", type = "h", xaxt = "n", yaxt = "n", xlab = "", ylab = "", lwd=3, xlim=c(2006, 2017))
  axis(4)
  mtext("VTS Abundance Index", side = 4, line = 2, col="coral3", cex=0.6)
  #legend("topleft", paste("512:\nCor = ",  round(cor(na.omit(sim_substrat_mean[,"512"]), na.omit(population_512_year_index)),2), sep=""), bty="n")
  
  plot(x = year_id-0.13, y = population_511_year_index[1:length(year_id)], col = "deepskyblue3", type = "h", xlab = "", ylab="", main = "", lwd=3, xlim=c(2006, 2017))
  mtext("Simulated Lobster Population", side=2, line=2, col="deepskyblue3", cex=0.6)
  mtext("Year", side=1, line=2, cex=0.6)
  par(new = T)
  plot(x = year_id+0.13, y = sim_substrat_mean[1:length(year_id),"511"], col = "coral3", type = "h", xaxt = "n", yaxt = "n", xlab = "", ylab = "", lwd=3, xlim=c(2006, 2017))
  axis(4)
  mtext("VTS Abundance Index", side = 4, line = 2, col="coral3", cex=0.6)
  #legend("topleft", paste("511:\nCor = ",  round(cor(na.omit(sim_substrat_mean[,"511"]), na.omit(population_511_year_index)),2), sep=""), bty="n")
  dev.off()
  
  jpeg(filename = "./plot/pop_vts_lobster_substrat_mean2.jpeg", width=175, height=60, units = "mm", res = 300)
  
  par(mfrow=c(1,3), mar=c(3.5,3.5,2,3.5))
  
  plot(x = year_id, y = population_513_year_index[1:length(year_id)], col = "deepskyblue3", type = "h", xlab = "", ylab="", main = "", lwd=5, xlim=c(2006, 2017))
  mtext("Simulated Lobster Population", side=2, line=2, col="deepskyblue3", cex=0.6)
  mtext("Year", side=1, line=2, cex=0.6)
  par(new = T)
  plot(x = year_id, y = sim_substrat_mean[1:length(year_id),"513"], col = "coral3", type = "o", xaxt = "n", yaxt = "n", xlab = "", ylab = "", lwd=2, xlim=c(2006, 2017), pch=16)
  axis(4)
  mtext("VTS Abundance Index", side = 4, line = 2, col="coral3", cex=0.6)
  legend("topright", paste("513:\nCor = ",  round(cor(na.omit(sim_substrat_mean[,"513"]), na.omit(population_513_year_index)),2), sep=""), bty="n")
  
  plot(x = year_id, y = population_512_year_index[1:length(year_id)], col = "deepskyblue3", type = "h", xlab = "", ylab="", main = "", lwd=5, xlim=c(2006, 2017))
  mtext("Simulated Lobster Population", side=2, line=2, col="deepskyblue3", cex=0.6)
  mtext("Year", side=1, line=2, cex=0.6)
  par(new = T)
  plot(x = year_id, y = sim_substrat_mean[1:length(year_id),"512"], col = "coral3", type = "o", xaxt = "n", yaxt = "n", xlab = "", ylab = "", lwd=2, xlim=c(2006, 2017), pch=16)
  axis(4)
  mtext("VTS Abundance Index", side = 4, line = 2, col="coral3", cex=0.6)
  legend("topright", paste("512:\nCor = ",  round(cor(na.omit(sim_substrat_mean[,"512"]), na.omit(population_512_year_index)),2), sep=""), bty="n")
  
  plot(x = year_id, y = population_511_year_index[1:length(year_id)], col = "deepskyblue3", type = "h", xlab = "", ylab="", main = "", lwd=5, xlim=c(2006, 2017))
  mtext("Simulated Lobster Population", side=2, line=2, col="deepskyblue3", cex=0.6)
  mtext("Year", side=1, line=2, cex=0.6)
  par(new = T)
  plot(x = year_id, y = sim_substrat_mean[1:length(year_id),"511"], col = "coral3", type = "o", xaxt = "n", yaxt = "n", xlab = "", ylab = "", lwd=2, xlim=c(2006, 2017), pch=16)
  axis(4)
  mtext("VTS Abundance Index", side = 4, line = 2, col="coral3", cex=0.6)
  legend("topright", paste("511:\nCor = ",  round(cor(na.omit(sim_substrat_mean[,"511"]), na.omit(population_511_year_index)),2), sep=""), bty="n")
  dev.off()
} #### catchability with NAO, month, statistical area, and depth

if (FALSE){
  load("./output/temperature_raster_data.RData")
  mean_temperature <- dates
  temp<- c()
  for(i in 1:length(temperature_raster_data)){
    temp[i]<-mean(temperature_raster_data[[i]])
  }
  mean_temperature$temperature <- temp
  temperature_point <- c()
  for(i in 1:nrow(vts_vtrap_data)){
    if(vts_vtrap_data$month[i] == 9) temperature_point[i] <- mean_temperature$temperature[which(mean_temperature$y==vts_vtrap_data$year[i] & mean_temperature$m==8)]
    else temperature_point[i] <- mean_temperature$temperature[which(mean_temperature$y==vts_vtrap_data$year[i] & mean_temperature$m==vts_vtrap_data$month[i])]
    
  }
  vts_vtrap_data$temperature <- temperature_point
  vts_vtrap_data$catchability<- vts_vtrap_data$quantity/vts_vtrap_data$sim_quantity
  vts_vtrap_data[which(vts_vtrap_data$catchability>1),]
  
  temp_data <- na.omit(vts_vtrap_data)
  temp_data <- temp_data[which(temp_data$catchability<=1),]
  temp_data$month[(temp_data$month==9)] <- 8
  catchability_mean <- aggregate(temp_data$catchability, by=list(temp_data$temperature, temp_data$month, temp_data$STATAREA, temp_data$assigneddepthstrata), mean)
  catchability_sd <- aggregate(temp_data$catchability, by=list(temp_data$temperature, temp_data$month, temp_data$STATAREA, temp_data$assigneddepthstrata), sd)
  colnames(catchability_mean) <- c("temperature", "month", "STATAREA", "assigneddepthstrata", "mean")
  colnames(catchability_sd) <- c("temperature", "month", "STATAREA", "assigneddepthstrata", "sd")
  
  jpeg(filename = "./plot/catchability_year_month_strata.jpeg", width=160, height=150, units = "mm", res = 600)
  par(mfrow=c(2,2))
  boxplot(catchability_mean$mean~catchability_mean$temperature, xlab="Temperature", ylab="Catchability", ylim=c(0,0.3), pch=16, cex=0.5)
  boxplot(catchability_mean$mean~catchability_mean$month, xlab="Month", ylab="Catchability", ylim=c(0,0.3), pch=16, cex=0.5)
  boxplot(catchability_mean$mean~catchability_mean$STATAREA, xlab="Statistical Area", ylab="Catchability", ylim=c(0,0.3), pch=16, cex=0.5)
  boxplot(catchability_mean$mean~catchability_mean$assigneddepthstrata, xlab="Depth Strata", ylab="Catchability", ylim=c(0,0.3), xlim=c(1.5,4.5), pch=16, cex=0.5)
  dev.off()
  
  catchability_data <- vts_vtrap_data
  #catchability_data <- na.omit(catchability_data)
  
  catchability_data$month[(catchability_data$month==9)] <- 8
  
  merge_data <- merge(catchability_data, catchability_mean, by=c("temperature","month", "STATAREA", "assigneddepthstrata"))
  catchability_data$q_mean <- merge_data$mean
  merge_data <- merge(catchability_data, catchability_sd, by=c("temperature","month", "STATAREA", "assigneddepthstrata"))
  catchability_data$q_sd <- merge_data$sd
  
  
  simulated_vts_samples_q <- c()
  for(i in 1:length(simulated_vts_samples)){
    print(i)
    if(is.na(simulated_vts_samples[i])) simulated_vts_samples_q[i] <- NA
    else{
      simulated_vts_samples_q[i] <- simulated_vts_samples[i]*rnorm(1, mean=catchability_data$q_mean[i], sd=catchability_data$q_sd[i])
      if(simulated_vts_samples_q[i]<0) simulated_vts_samples_q[i]<-simulated_vts_samples[i]*catchability_data$q_mean[i]
    }
  }
  
  vts_vtrap_data$sim_quantity_q <- simulated_vts_samples_q
  
  catchability_data$month[(catchability_data$month==9)] <- 8
  
  #### vts simulated abundance index ####
  #vts_vtrap_data <- vts_vtrap_data[-which(vts_vtrap_data$year != 2014 & vts_vtrap_data$year != 2017 & is.na(vts_vtrap_data$sim_quantity)),]
  #vts_vtrap_data <- na.omit(vts_vtrap_data)
  summary(vts_vtrap_data)
  vts_vtrap_data <- na.omit(vts_vtrap_data)
  
  sim_lobster_per_year_sa_depth <- aggregate(vts_vtrap_data$sim_quantity, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata), sum)
  colnames(sim_lobster_per_year_sa_depth) <- c("Year", "SA", "Depth", "Lob_Quantity") 
  sim_lobster_per_year_sa_depth$Trap_Quantity <- aggregate(vts_vtrap_data$TRAP_ID, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata), length)$x
  sim_lobster_per_year_sa_depth$Site_Quantity <- aggregate(vts_vtrap_data$TRIP_ID, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata), function(x) length(unique(x)))$x
  summary(sim_lobster_per_year_sa_depth)
  sim_lobster_per_year_sa_depth$Ave_Lob_Trap <- sim_lobster_per_year_sa_depth$Lob_Quantity/sim_lobster_per_year_sa_depth$Trap_Quantity
  
  area_per_sa_depth <- as.data.frame(matrix(c(122, 82, 92, 296, 566, 395, 420, 1381, 315, 338, 198, 851), nrow=3, byrow=T))
  colnames(area_per_sa_depth) <- c("depth1", "depth2", "depth3", "total")
  rownames(area_per_sa_depth) <- c("511", "512", "513")
  
  ## sim_substrat_mean
  sim_substrat_mean <- matrix(NA, nrow=length(unique(sim_lobster_per_year_sa_depth$Year)), ncol=3)
  year_id <- unique(sim_lobster_per_year_sa_depth$Year)
  colnames(sim_substrat_mean) <- c("511", "512", "513")
  rownames(sim_substrat_mean) <- year_id
  for (i in 1:length(year_id)){
    sim_substrat_mean[i,1] <- area_per_sa_depth["511", "depth1"]/area_per_sa_depth["511", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["511", "depth2"]/area_per_sa_depth["511", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="21 to 40"& sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["511", "depth3"]/area_per_sa_depth["511", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="41 to 60"& sim_lobster_per_year_sa_depth$Year==year_id[i])] 
    
    sim_substrat_mean[i,2] <- area_per_sa_depth["512", "depth1"]/area_per_sa_depth["512", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["512", "depth2"]/area_per_sa_depth["512", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="21 to 40"& sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["512", "depth3"]/area_per_sa_depth["512", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="41 to 60"& sim_lobster_per_year_sa_depth$Year==year_id[i])] 
    
    sim_substrat_mean[i,3] <- area_per_sa_depth["513", "depth1"]/area_per_sa_depth["513", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["513", "depth2"]/area_per_sa_depth["513", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="21 to 40"& sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["513", "depth3"]/area_per_sa_depth["513", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="41 to 60"& sim_lobster_per_year_sa_depth$Year==year_id[i])] 
  }
  
  ## sim_substrat_var
  sim_lobster_per_year_sa_depth_site <- aggregate(vts_vtrap_data$sim_quantity, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata, vts_vtrap_data$SITE_ID), sum)
  colnames(sim_lobster_per_year_sa_depth_site) <- c("Year", "SA", "Depth", "Site", "Lob_Quantity") 
  
  sim_lobster_per_year_sa_depth_site$Trap_Quantity <- aggregate(vts_vtrap_data$TRAP_ID, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata, vts_vtrap_data$SITE_ID), length)$x
  
  sim_lobster_per_year_sa_depth_site$Quantity_per_Trap <- sim_lobster_per_year_sa_depth_site$Lob_Quantity/sim_lobster_per_year_sa_depth_site$Trap_Quantity
  
  sim_lobster_per_year_sa_depth$Ave_Lob_Trap <- sim_lobster_per_year_sa_depth$Lob_Quantity/sim_lobster_per_year_sa_depth$Trap_Quantity
  
  sim_lobster_per_year_sa_depth_site=merge(sim_lobster_per_year_sa_depth_site, sim_lobster_per_year_sa_depth[, c("Year", "SA", "Depth", "Ave_Lob_Trap")], by=c("Year", "SA", "Depth"))
  
  sim_lobster_per_year_sa_depth_site$Sub <- (sim_lobster_per_year_sa_depth_site$Ave_Lob_Trap - sim_lobster_per_year_sa_depth_site$Quantity_per_Trap)^2
  
  individual_value <- aggregate(sim_lobster_per_year_sa_depth_site$Sub, by=list(sim_lobster_per_year_sa_depth_site$Year, sim_lobster_per_year_sa_depth_site$SA, sim_lobster_per_year_sa_depth_site$Depth), sum)
  colnames(individual_value) <- c("Year", "SA", "Depth", "x")
  individual_value$individual_value <- individual_value$x/aggregate(sim_lobster_per_year_sa_depth_site$Site, by=list(sim_lobster_per_year_sa_depth_site$Year, sim_lobster_per_year_sa_depth_site$SA, sim_lobster_per_year_sa_depth_site$Depth), length)$x
  
  sim_substrat_var <- matrix(NA, nrow=length(unique(sim_lobster_per_year_sa_depth$Year)), ncol=3)
  colnames(sim_substrat_var) <- c("511", "512", "513")
  rownames(sim_substrat_var) <- year_id
  
  for (i in 1:length(year_id)){
    sim_substrat_var[i,1] <- 1/area_per_sa_depth["511", "total"]^2*((area_per_sa_depth["511", "depth1"]*(area_per_sa_depth["511", "depth1"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["511", "depth2"]*(area_per_sa_depth["511", "depth2"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["511", "depth3"]*(area_per_sa_depth["511", "depth3"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]))
    
    sim_substrat_var[i,2] <- 1/area_per_sa_depth["512", "total"]^2*((area_per_sa_depth["512", "depth1"]*(area_per_sa_depth["512", "depth1"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["512", "depth2"]*(area_per_sa_depth["512", "depth2"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["512", "depth3"]*(area_per_sa_depth["512", "depth3"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]))
    
    sim_substrat_var[i,3] <- 1/area_per_sa_depth["513", "total"]^2*((area_per_sa_depth["513", "depth1"]*(area_per_sa_depth["513", "depth1"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["513", "depth2"]*(area_per_sa_depth["513", "depth2"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["513", "depth3"]*(area_per_sa_depth["513", "depth3"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]))
  }
  
  jpeg(filename = "./plot/vts_lobster_substrat_mean_sim.jpeg", width=150, height=50, units = "mm", res = 600)
  par(mfrow=c(1,3))
  ylim_min <- min(na.omit(sim_substrat_mean))
  ylim_max <- max(na.omit(sim_substrat_mean))
  
  plot(year_id, sim_substrat_mean[,"513"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=1, cex=0.5, ylim=c(ylim_min, ylim_max))
  legend("topright", "513", bty="n")
  
  plot(year_id, sim_substrat_mean[,"512"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=1, cex=0.5, ylim=c(ylim_min, ylim_max))
  legend("topright", "512", bty="n")
  
  plot(year_id, sim_substrat_mean[,"511"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=1, cex=0.5, ylim=c(ylim_min, ylim_max))
  legend("topright", "511", bty="n")
  
  dev.off()
  
  if (FALSE) {
    sp6_fall9_cor <- c()
    sp6_fall9_cor[1] <- cor(substrat_mean[,"511"], sim_substrat_mean[,"511"], use = "pairwise.complete.obs")
    sp6_fall9_cor[2] <- cor(substrat_mean[,"512"], sim_substrat_mean[,"512"], use = "pairwise.complete.obs")
    sp6_fall9_cor[3] <- cor(substrat_mean[,"513"], sim_substrat_mean[,"513"], use = "pairwise.complete.obs")
    sp6_fall9_cor
  }
  
  
  fall_cor1 <- c()
  fall_cor1[1] <- cor(substrat_mean[,"511"], sim_substrat_mean[,"511"], use = "pairwise.complete.obs")
  fall_cor1[2] <- cor(substrat_mean[,"512"], sim_substrat_mean[,"512"], use = "pairwise.complete.obs")
  fall_cor1[3] <- cor(substrat_mean[,"513"], sim_substrat_mean[,"513"], use = "pairwise.complete.obs")
  fall_cor1
  
  
  #### Plot sim vts trends with consideration of q ####
  sim_lobster_per_year_sa_depth <- aggregate(vts_vtrap_data$sim_quantity_q, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata), sum)
  colnames(sim_lobster_per_year_sa_depth) <- c("Year", "SA", "Depth", "Lob_Quantity") 
  sim_lobster_per_year_sa_depth$Trap_Quantity <- aggregate(vts_vtrap_data$TRAP_ID, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata), length)$x
  sim_lobster_per_year_sa_depth$Site_Quantity <- aggregate(vts_vtrap_data$TRIP_ID, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata), function(x) length(unique(x)))$x
  summary(sim_lobster_per_year_sa_depth)
  sim_lobster_per_year_sa_depth$Ave_Lob_Trap <- sim_lobster_per_year_sa_depth$Lob_Quantity/sim_lobster_per_year_sa_depth$Trap_Quantity
  
  area_per_sa_depth <- as.data.frame(matrix(c(122, 82, 92, 296, 566, 395, 420, 1381, 315, 338, 198, 851), nrow=3, byrow=T))
  colnames(area_per_sa_depth) <- c("depth1", "depth2", "depth3", "total")
  rownames(area_per_sa_depth) <- c("511", "512", "513")
  
  ## sim_substrat_mean
  sim_substrat_mean <- matrix(NA, nrow=length(unique(sim_lobster_per_year_sa_depth$Year)), ncol=3)
  year_id <- unique(sim_lobster_per_year_sa_depth$Year)
  colnames(sim_substrat_mean) <- c("511", "512", "513")
  rownames(sim_substrat_mean) <- year_id
  for (i in 1:length(year_id)){
    sim_substrat_mean[i,1] <- area_per_sa_depth["511", "depth1"]/area_per_sa_depth["511", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["511", "depth2"]/area_per_sa_depth["511", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="21 to 40"& sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["511", "depth3"]/area_per_sa_depth["511", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="41 to 60"& sim_lobster_per_year_sa_depth$Year==year_id[i])] 
    
    sim_substrat_mean[i,2] <- area_per_sa_depth["512", "depth1"]/area_per_sa_depth["512", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["512", "depth2"]/area_per_sa_depth["512", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="21 to 40"& sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["512", "depth3"]/area_per_sa_depth["512", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="41 to 60"& sim_lobster_per_year_sa_depth$Year==year_id[i])] 
    
    sim_substrat_mean[i,3] <- area_per_sa_depth["513", "depth1"]/area_per_sa_depth["513", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["513", "depth2"]/area_per_sa_depth["513", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="21 to 40"& sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["513", "depth3"]/area_per_sa_depth["513", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="41 to 60"& sim_lobster_per_year_sa_depth$Year==year_id[i])] 
  }
  
  ## sim_substrat_var
  sim_lobster_per_year_sa_depth_site <- aggregate(vts_vtrap_data$sim_quantity, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata, vts_vtrap_data$SITE_ID), sum)
  colnames(sim_lobster_per_year_sa_depth_site) <- c("Year", "SA", "Depth", "Site", "Lob_Quantity") 
  
  sim_lobster_per_year_sa_depth_site$Trap_Quantity <- aggregate(vts_vtrap_data$TRAP_ID, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata, vts_vtrap_data$SITE_ID), length)$x
  
  sim_lobster_per_year_sa_depth_site$Quantity_per_Trap <- sim_lobster_per_year_sa_depth_site$Lob_Quantity/sim_lobster_per_year_sa_depth_site$Trap_Quantity
  
  sim_lobster_per_year_sa_depth$Ave_Lob_Trap <- sim_lobster_per_year_sa_depth$Lob_Quantity/sim_lobster_per_year_sa_depth$Trap_Quantity
  
  sim_lobster_per_year_sa_depth_site=merge(sim_lobster_per_year_sa_depth_site, sim_lobster_per_year_sa_depth[, c("Year", "SA", "Depth", "Ave_Lob_Trap")], by=c("Year", "SA", "Depth"))
  
  sim_lobster_per_year_sa_depth_site$Sub <- (sim_lobster_per_year_sa_depth_site$Ave_Lob_Trap - sim_lobster_per_year_sa_depth_site$Quantity_per_Trap)^2
  
  individual_value <- aggregate(sim_lobster_per_year_sa_depth_site$Sub, by=list(sim_lobster_per_year_sa_depth_site$Year, sim_lobster_per_year_sa_depth_site$SA, sim_lobster_per_year_sa_depth_site$Depth), sum)
  colnames(individual_value) <- c("Year", "SA", "Depth", "x")
  individual_value$individual_value <- individual_value$x/aggregate(sim_lobster_per_year_sa_depth_site$Site, by=list(sim_lobster_per_year_sa_depth_site$Year, sim_lobster_per_year_sa_depth_site$SA, sim_lobster_per_year_sa_depth_site$Depth), length)$x
  
  sim_substrat_var <- matrix(NA, nrow=length(unique(sim_lobster_per_year_sa_depth$Year)), ncol=3)
  colnames(sim_substrat_var) <- c("511", "512", "513")
  rownames(sim_substrat_var) <- year_id
  
  for (i in 1:length(year_id)){
    sim_substrat_var[i,1] <- 1/area_per_sa_depth["511", "total"]^2*((area_per_sa_depth["511", "depth1"]*(area_per_sa_depth["511", "depth1"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["511", "depth2"]*(area_per_sa_depth["511", "depth2"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["511", "depth3"]*(area_per_sa_depth["511", "depth3"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]))
    
    sim_substrat_var[i,2] <- 1/area_per_sa_depth["512", "total"]^2*((area_per_sa_depth["512", "depth1"]*(area_per_sa_depth["512", "depth1"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["512", "depth2"]*(area_per_sa_depth["512", "depth2"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["512", "depth3"]*(area_per_sa_depth["512", "depth3"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]))
    
    sim_substrat_var[i,3] <- 1/area_per_sa_depth["513", "total"]^2*((area_per_sa_depth["513", "depth1"]*(area_per_sa_depth["513", "depth1"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["513", "depth2"]*(area_per_sa_depth["513", "depth2"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="21 to 40" & sim_lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["513", "depth3"]*(area_per_sa_depth["513", "depth3"]-sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]/sim_lobster_per_year_sa_depth$Site_Quantity[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="41 to 60" & sim_lobster_per_year_sa_depth$Year==year_id[i])]))
  }
  
  jpeg(filename = "./plot/vts_lobster_substrat_mean_sim_q.jpeg", width=150, height=50, units = "mm", res = 600)
  par(mfrow=c(1,3))
  ylim_min <- min(na.omit(sim_substrat_mean))
  ylim_max <- max(na.omit(sim_substrat_mean))
  
  plot(year_id, sim_substrat_mean[,"513"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=1, cex=0.5, ylim=c(ylim_min, ylim_max))
  legend("topright", "513", bty="n")
  
  plot(year_id, sim_substrat_mean[,"512"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=1, cex=0.5, ylim=c(ylim_min, ylim_max))
  legend("topright", "512", bty="n")
  
  plot(year_id, sim_substrat_mean[,"511"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=1, cex=0.5, ylim=c(ylim_min, ylim_max))
  legend("topright", "511", bty="n")
  
  dev.off()
  
  fall_cor2 <- c()
  fall_cor2[1] <- cor(substrat_mean[,"511"], sim_substrat_mean[,"511"], use = "pairwise.complete.obs")
  fall_cor2[2] <- cor(substrat_mean[,"512"], sim_substrat_mean[,"512"], use = "pairwise.complete.obs")
  fall_cor2[3] <- cor(substrat_mean[,"513"], sim_substrat_mean[,"513"], use = "pairwise.complete.obs")
  fall_cor2
  #### population index ####
  polygon_points <- over(prediction_points, sa511_513)
  summary(polygon_points$Id)
  
  population_511_month_index <- c()
  population_512_month_index <- c()
  population_513_month_index <- c()
  
  for (i in 1:length(prediction_mean)){
    population_511_month_index[i] <- sum(prediction_mean[[i]][which(polygon_points$Id==511)], na.rm = T)
    population_512_month_index[i] <- sum(prediction_mean[[i]][which(polygon_points$Id==512)], na.rm = T)
    population_513_month_index[i] <- sum(prediction_mean[[i]][which(polygon_points$Id==513)], na.rm = T)
  }
  population_511_year_index <- rowMeans(matrix(population_511_month_index, ncol=3, byrow = T))
  population_511_year_index <- c(population_511_year_index[1:8], NA, population_511_year_index[9:10], NA)
  population_512_year_index <- rowMeans(matrix(population_512_month_index, ncol=3, byrow = T))
  population_512_year_index <- c(population_512_year_index[1:8], NA, population_512_year_index[9:10], NA)
  population_513_year_index <- rowMeans(matrix(population_513_month_index, ncol=3, byrow = T))
  population_513_year_index <- c(population_513_year_index[1:8], NA, population_513_year_index[9:10], NA)
  
  jpeg(filename = "./plot/pop_vts_lobster_substrat_mean.jpeg", width=175, height=60, units = "mm", res = 600)
  
  par(mfrow=c(1,3), mar=c(3.5,3.5,2,3.5))
  
  plot(x = year_id-0.13, y = population_513_year_index[1:length(year_id)], col = "deepskyblue3", type = "h", xlab = "", ylab="", main = "", lwd=3, xlim=c(2006, 2017))
  mtext("Simulated Lobster Population", side=2, line=2, col="deepskyblue3", cex=0.6)
  mtext("Year", side=1, line=2, cex=0.6)
  par(new = T)
  plot(x = year_id+0.13, y = sim_substrat_mean[1:length(year_id),"513"], col = "coral3", type = "h", xaxt = "n", yaxt = "n", xlab = "", ylab = "", lwd=3, xlim=c(2006, 2017))
  axis(4)
  mtext("VTS Abundance Index", side = 4, line = 2, col="coral3", cex=0.6)
  #legend("topleft", paste("513:\nCor = ",  round(cor(na.omit(sim_substrat_mean[,"513"]), na.omit(population_513_year_index)),2), sep=""), bty="n")
  
  plot(x = year_id-0.13, y = population_512_year_index[1:length(year_id)], col = "deepskyblue3", type = "h", xlab = "", ylab="", main = "", lwd=3, xlim=c(2006, 2017))
  mtext("Simulated Lobster Population", side=2, line=2, col="deepskyblue3", cex=0.6)
  mtext("Year", side=1, line=2, cex=0.6)
  par(new = T)
  plot(x = year_id+0.13, y = sim_substrat_mean[1:length(year_id),"512"], col = "coral3", type = "h", xaxt = "n", yaxt = "n", xlab = "", ylab = "", lwd=3, xlim=c(2006, 2017))
  axis(4)
  mtext("VTS Abundance Index", side = 4, line = 2, col="coral3", cex=0.6)
  #legend("topleft", paste("512:\nCor = ",  round(cor(na.omit(sim_substrat_mean[,"512"]), na.omit(population_512_year_index)),2), sep=""), bty="n")
  
  plot(x = year_id-0.13, y = population_511_year_index[1:length(year_id)], col = "deepskyblue3", type = "h", xlab = "", ylab="", main = "", lwd=3, xlim=c(2006, 2017))
  mtext("Simulated Lobster Population", side=2, line=2, col="deepskyblue3", cex=0.6)
  mtext("Year", side=1, line=2, cex=0.6)
  par(new = T)
  plot(x = year_id+0.13, y = sim_substrat_mean[1:length(year_id),"511"], col = "coral3", type = "h", xaxt = "n", yaxt = "n", xlab = "", ylab = "", lwd=3, xlim=c(2006, 2017))
  axis(4)
  mtext("VTS Abundance Index", side = 4, line = 2, col="coral3", cex=0.6)
  #legend("topleft", paste("511:\nCor = ",  round(cor(na.omit(sim_substrat_mean[,"511"]), na.omit(population_511_year_index)),2), sep=""), bty="n")
  dev.off()
  
  jpeg(filename = "./plot/pop_vts_lobster_substrat_mean2.jpeg", width=175, height=60, units = "mm", res = 300)
  
  par(mfrow=c(1,3), mar=c(3.5,3.5,2,3.5))
  
  plot(x = year_id, y = population_513_year_index[1:length(year_id)], col = "deepskyblue3", type = "h", xlab = "", ylab="", main = "", lwd=5, xlim=c(2006, 2017))
  mtext("Simulated Lobster Population", side=2, line=2, col="deepskyblue3", cex=0.6)
  mtext("Year", side=1, line=2, cex=0.6)
  par(new = T)
  plot(x = year_id, y = sim_substrat_mean[1:length(year_id),"513"], col = "coral3", type = "o", xaxt = "n", yaxt = "n", xlab = "", ylab = "", lwd=2, xlim=c(2006, 2017), pch=16)
  axis(4)
  mtext("VTS Abundance Index", side = 4, line = 2, col="coral3", cex=0.6)
  legend("topright", paste("513:\nCor = ",  round(cor(na.omit(sim_substrat_mean[,"513"]), na.omit(population_513_year_index)),2), sep=""), bty="n")
  
  plot(x = year_id, y = population_512_year_index[1:length(year_id)], col = "deepskyblue3", type = "h", xlab = "", ylab="", main = "", lwd=5, xlim=c(2006, 2017))
  mtext("Simulated Lobster Population", side=2, line=2, col="deepskyblue3", cex=0.6)
  mtext("Year", side=1, line=2, cex=0.6)
  par(new = T)
  plot(x = year_id, y = sim_substrat_mean[1:length(year_id),"512"], col = "coral3", type = "o", xaxt = "n", yaxt = "n", xlab = "", ylab = "", lwd=2, xlim=c(2006, 2017), pch=16)
  axis(4)
  mtext("VTS Abundance Index", side = 4, line = 2, col="coral3", cex=0.6)
  legend("topright", paste("512:\nCor = ",  round(cor(na.omit(sim_substrat_mean[,"512"]), na.omit(population_512_year_index)),2), sep=""), bty="n")
  
  plot(x = year_id, y = population_511_year_index[1:length(year_id)], col = "deepskyblue3", type = "h", xlab = "", ylab="", main = "", lwd=5, xlim=c(2006, 2017))
  mtext("Simulated Lobster Population", side=2, line=2, col="deepskyblue3", cex=0.6)
  mtext("Year", side=1, line=2, cex=0.6)
  par(new = T)
  plot(x = year_id, y = sim_substrat_mean[1:length(year_id),"511"], col = "coral3", type = "o", xaxt = "n", yaxt = "n", xlab = "", ylab = "", lwd=2, xlim=c(2006, 2017), pch=16)
  axis(4)
  mtext("VTS Abundance Index", side = 4, line = 2, col="coral3", cex=0.6)
  legend("topright", paste("511:\nCor = ",  round(cor(na.omit(sim_substrat_mean[,"511"]), na.omit(population_511_year_index)),2), sep=""), bty="n")
  dev.off()
} #### catchability with FVCOM temperature, month, statistical area, and depth

if (TRUE){
  
  load("./output/temperature_raster_data.RData")
  
  mean_temperature <- dates
  temp<- c()
  for(i in 1:length(temperature_raster_data)){
    temp[i]<-mean(temperature_raster_data[[i]])
  }
  mean_temperature$temperature <- round(temp, digits = 2)
  temperature_point <- c()
  for(i in 1:nrow(vts_vtrap_data)){
    if(vts_vtrap_data$month[i] == 9) temperature_point[i] <- mean_temperature$temperature[which(mean_temperature$y==vts_vtrap_data$year[i] & mean_temperature$m==8)]
    else temperature_point[i] <- mean_temperature$temperature[which(mean_temperature$y==vts_vtrap_data$year[i] & mean_temperature$m==vts_vtrap_data$month[i])]
    
  }
  vts_vtrap_data$temperature <- temperature_point
  q_mean <- matrix(NA, nrow = nrow(vts_vtrap_data), ncol=sim_num)
  q_sd <-  matrix(NA, nrow = nrow(vts_vtrap_data), ncol=sim_num)
  for(sim in 1:sim_num){
    print(sim)
    vts_vtrap_data$catchability<- vts_vtrap_data$quantity/simulated_vts_samples[,sim]
    #vts_vtrap_data[which(vts_vtrap_data$catchability>1),]
    temp_data <- na.omit(vts_vtrap_data)
    temp_data <- temp_data[which(temp_data$catchability<=1),]
    temp_data$month[(temp_data$month==9)] <- 8
    catchability_mean <- aggregate(temp_data$catchability, by=list(temp_data$temperature, temp_data$assigneddepthstrata), mean)
    catchability_sd <- aggregate(temp_data$catchability, by=list(temp_data$temperature,  temp_data$assigneddepthstrata), sd)
    colnames(catchability_mean) <- c("temperature", "assigneddepthstrata", "mean")
    colnames(catchability_sd) <- c("temperature", "assigneddepthstrata", "sd")
    
    catchability_data <- vts_vtrap_data
    
    catchability_data$month[(catchability_data$month==9)] <- 8
    
    merge_data <- merge(catchability_data, catchability_mean, by=c("temperature","assigneddepthstrata"))
    q_mean[,sim] <- merge_data[match(catchability_data$TRAP_ID,merge_data$TRAP_ID),]$mean
    merge_data <- merge(catchability_data, catchability_sd, by=c("temperature","assigneddepthstrata"))
    q_sd[,sim] <- merge_data[match(catchability_data$TRAP_ID,merge_data$TRAP_ID),]$sd
    if (sim==1) {
      jpeg(filename = "./plot/catchability1_year_month_strata.jpeg", width=160, height=150, units = "mm", res = 600)
      par(mfrow=c(2,2))
      boxplot(catchability_mean$mean~catchability_mean$temperature, xlab="Temperature", ylab="Catchability", ylim=c(0,0.3), pch=16, cex=0.5)
      #boxplot(catchability_mean$mean~catchability_mean$month, xlab="Month", ylab="Catchability", ylim=c(0,0.3), pch=16, cex=0.5)
      #boxplot(catchability_mean$mean~catchability_mean$STATAREA, xlab="Statistical Area", ylab="Catchability", ylim=c(0,0.3), pch=16, cex=0.5)
      boxplot(catchability_mean$mean~catchability_mean$assigneddepthstrata, xlab="Depth Strata", ylab="Catchability", ylim=c(0,0.3), xlim=c(1.5,4.5), pch=16, cex=0.5)
      dev.off()
    }
    if (sim==500) {
      jpeg(filename = "./plot/catchability500_year_month_strata.jpeg", width=160, height=150, units = "mm", res = 600)
      par(mfrow=c(2,2))
      boxplot(catchability_mean$mean~catchability_mean$temperature, xlab="Temperature", ylab="Catchability", ylim=c(0,0.3), pch=16, cex=0.5)
      #boxplot(catchability_mean$mean~catchability_mean$month, xlab="Month", ylab="Catchability", ylim=c(0,0.3), pch=16, cex=0.5)
      #boxplot(catchability_mean$mean~catchability_mean$STATAREA, xlab="Statistical Area", ylab="Catchability", ylim=c(0,0.3), pch=16, cex=0.5)
      boxplot(catchability_mean$mean~catchability_mean$assigneddepthstrata, xlab="Depth Strata", ylab="Catchability", ylim=c(0,0.3), xlim=c(1.5,4.5), pch=16, cex=0.5)
      dev.off()
    }
    if (sim==1000) {
      jpeg(filename = "./plot/catchability1000_year_month_strata.jpeg", width=160, height=150, units = "mm", res = 600)
      par(mfrow=c(2,2))
      boxplot(catchability_mean$mean~catchability_mean$temperature, xlab="Temperature", ylab="Catchability", ylim=c(0,0.3), pch=16, cex=0.5)
      #boxplot(catchability_mean$mean~catchability_mean$month, xlab="Month", ylab="Catchability", ylim=c(0,0.3), pch=16, cex=0.5)
      #boxplot(catchability_mean$mean~catchability_mean$STATAREA, xlab="Statistical Area", ylab="Catchability", ylim=c(0,0.3), pch=16, cex=0.5)
      boxplot(catchability_mean$mean~catchability_mean$assigneddepthstrata, xlab="Depth Strata", ylab="Catchability", ylim=c(0,0.3), xlim=c(1.5,4.5), pch=16, cex=0.5)
      dev.off()
    }
  }
  summary(q_mean)
  summary(q_sd)
  save(q_mean, q_sd, file="./output/catchability_mean_sd_1000.RData")
  
  if(TRUE){
    simulated_vts_samples_q <- matrix(NA, nrow=nrow(simulated_vts_samples), ncol=sim_num)
    
    for(i in 1:nrow(simulated_vts_samples)){
      print(i)
      if(is.na(simulated_vts_samples[i,])) simulated_vts_samples_q[i,] <- NA
      else{
        for (sim in 1:sim_num){
          simulated_vts_samples_q[i,sim] <- simulated_vts_samples[i,sim]*rnorm(1, mean=q_mean[i,sim], sd=q_sd[i,sim])
        }
        #simulated_vts_samples_q[i,] <- simulated_vts_samples[i,]*q_mean[i,]
        
        #if(simulated_vts_samples_q[i,]<0) simulated_vts_samples_q[i,]<-simulated_vts_samples[i,]*catchability_data$q_mean[i]
      }
    }
    
    save(simulated_vts_samples_q, file="./output/vts_simulated_samples_q.RData")
    
  }
  
} #### catchability with FVCOM temperature and depth
#### Population index ####
load("./output/population_1000.RData")
statistical_areas <-rgdal::readOGR("./data/gis/Statistical_Areas_2010.shp")
sa511_513 <- statistical_areas[which(statistical_areas@data$Id %in% c(511:513)),]
polygon_points <- over(prediction_points, sa511_513)
summary(polygon_points$Id)

population_511_month_index <- matrix(NA, nrow=length(population_1000), ncol=sim_num)
population_512_month_index <- matrix(NA, nrow=length(population_1000), ncol=sim_num)
population_513_month_index <- matrix(NA, nrow=length(population_1000), ncol=sim_num)

for (i in 1:length(population_1000)){
  population_511_month_index[i,] <- apply(population_1000[[i]][which(polygon_points$Id==511),], 2, sum, na.rm = T)
  population_512_month_index[i,] <- apply(population_1000[[i]][which(polygon_points$Id==512),],2, sum, na.rm = T)
  population_513_month_index[i,] <- apply(population_1000[[i]][which(polygon_points$Id==513),],2, sum, na.rm = T)
}


population_511_year_index_sim <- matrix(NA, nrow=length(c(2006:2016)), ncol=sim_num)
population_512_year_index_sim <- matrix(NA, nrow=length(c(2006:2016)), ncol=sim_num)
population_513_year_index_sim <- matrix(NA, nrow=length(c(2006:2016)), ncol=sim_num)
for (sim in 1:sim_num){
  population_511_year_index_sim[,sim] <- rowMeans(matrix(population_511_month_index[,sim], ncol=3, byrow = T))
  population_512_year_index_sim[,sim] <- rowMeans(matrix(population_512_month_index[,sim], ncol=3, byrow = T))
  population_513_year_index_sim[,sim] <- rowMeans(matrix(population_513_month_index[,sim], ncol=3, byrow = T))
}

population_511_year_index <- apply(population_511_year_index_sim,1,mean)
population_512_year_index <- apply(population_512_year_index_sim,1,mean)
population_513_year_index <- apply(population_513_year_index_sim,1,mean)

population_index <- cbind(population_511_year_index, population_512_year_index, population_513_year_index)
colnames(population_index) <- c("511", "512", "513")
rownames(population_index) <- c(2006:2016)

population_511_year_sd <- apply(population_511_year_index_sim,1,sd)
population_512_year_sd <- apply(population_512_year_index_sim,1,sd)
population_513_year_sd <- apply(population_513_year_index_sim,1,sd)

population_sd <- cbind(population_511_year_sd, population_512_year_sd, population_513_year_sd)
colnames(population_sd) <- c("511", "512", "513")
rownames(population_sd) <- c(2006:2016)

population_upper <- population_index+population_sd
population_lower <- population_index-population_sd

year_id <- c(2006:2016)

ylim=range(population_upper[1:length(year_id),], population_lower[1:length(year_id),])

jpeg(filename = "./plot/pop_mean_1000_2006_2016.jpeg", width=175, height=60, units = "mm", res = 600)

par(mfrow=c(1,3), mar=c(4,4,1,1))
#plot(year_id, population_index[1:length(year_id),"513"], pch=16, ylim=ylim_513, type="o", col="deepskyblue3", lty=1, xlab="Year", ylab="Abundance Index")
plot(year_id, population_index[1:length(year_id),"513"], pch=16, ylim=ylim,  col="black",  xlab="Year", ylab="Abundance Index", cex=0.5)
polygon(x=c(year_id, rev(year_id)), y=c(population_upper[1:length(year_id),"513"], rev(population_lower[1:length(year_id),"513"])), col=adjustcolor("deepskyblue3", alpha.f = 0.80), border = NA)
legend("topleft",  "513", bty="n",  cex=0.8)

#plot(year_id, population_index[1:length(year_id),"512"], pch=16, ylim=ylim_512, type="o", col="deepskyblue3", lty=1, xlab="Year", ylab="Abundance Index")
plot(year_id, population_index[1:length(year_id),"512"], pch=16, ylim=ylim,  col="black",  xlab="Year", ylab="Abundance Index", cex=0.5)
polygon(x=c(year_id, rev(year_id)), y=c(population_upper[1:length(year_id),"512"], rev(population_lower[1:length(year_id),"512"])), col=adjustcolor("deepskyblue3", alpha.f = 0.80), border = NA)
legend("topleft",  "512", bty="n",  cex=0.8)

#plot(year_id, population_index[1:length(year_id),"511"], pch=16, ylim=ylim_511, type="o", col="deepskyblue3", lty=1, xlab="Year", ylab="Abundance Index")
plot(year_id, population_index[1:length(year_id),"511"], pch=16, ylim=ylim,  col="black",  xlab="Year", ylab="Abundance Index", cex=0.5)
polygon(x=c(year_id, rev(year_id)), y=c(population_upper[1:length(year_id),"511"], rev(population_lower[1:length(year_id),"511"])), col=adjustcolor("deepskyblue3", alpha.f = 0.80), border = NA)
legend("topleft",  "511", bty="n",  cex=0.8)
dev.off()

#population_scale <- scale(population_index[1:length(year_id),])
year_num <- length(2006:2014)
population_scale <- (population_index[1:year_num,]-min(population_index[1:year_num,]))/diff(range(population_index[1:year_num,]))

save(population_scale, file="./output/population_indices_scaled_2006_2014.RData")
ylim=range(population_scale)
jpeg(filename = "./plot/pop_mean_1000_2006_2014_scale.jpeg", width=175, height=60, units = "mm", res = 600)

par(mfrow=c(1,3), mar=c(4,4,1,1))
plot(year_id[1:year_num], population_scale[1:year_num,"513"], pch=16, ylim=ylim, type="o", col="deepskyblue3", lty=1, xlab="Year", ylab="Abundance Index")
#polygon(x=c(year_id, rev(year_id)), y=c(population_upper[1:length(year_id),"513"], rev(population_lower[1:length(year_id),"513"])), col=adjustcolor("deepskyblue3", alpha.f = 0.20), border = NA)
legend("topleft",  "513", bty="n",  cex=0.8)

plot(year_id[1:year_num], population_scale[1:year_num,"512"], pch=16, ylim=ylim, type="o", col="deepskyblue3", lty=1, xlab="Year", ylab="Abundance Index")
#polygon(x=c(year_id, rev(year_id)), y=c(population_upper[1:length(year_id),"512"], rev(population_lower[1:length(year_id),"512"])), col=adjustcolor("deepskyblue3", alpha.f = 0.20), border = NA)
legend("topleft",  "512", bty="n",  cex=0.8)

plot(year_id[1:year_num], population_scale[1:year_num,"511"], pch=16, ylim=ylim, type="o", col="deepskyblue3", lty=1, xlab="Year", ylab="Abundance Index")
#polygon(x=c(year_id, rev(year_id)), y=c(population_upper[1:length(year_id),"511"], rev(population_lower[1:length(year_id),"511"])), col=adjustcolor("deepskyblue3", alpha.f = 0.20), border = NA)
legend("topleft",  "511", bty="n",  cex=0.8)
dev.off()

#### VTS population index ####
load("./output/simulated_vts_samples_1000.RData")

sim_pop_data <- vts_vtrap_data
na_id <- which(is.na(sim_pop_data$catchability))
simulated_vts_samples<- simulated_vts_samples[-na_id,]
sim_pop_data<- sim_pop_data[-na_id,]
#sim_pop_data <- na.omit(sim_pop_data)
sim_substrat_mean_list <- list()
for (sim in 1:sim_num){
  sim_lobster_per_year_sa_depth <- aggregate(simulated_vts_samples[,sim], by=list(sim_pop_data$year, sim_pop_data$STATAREA, sim_pop_data$assigneddepthstrata), sum)
  colnames(sim_lobster_per_year_sa_depth) <- c("Year", "SA", "Depth", "Lob_Quantity") 
  sim_lobster_per_year_sa_depth$Trap_Quantity <- aggregate(sim_pop_data$TRAP_ID, by=list(sim_pop_data$year, sim_pop_data$STATAREA, sim_pop_data$assigneddepthstrata), length)$x
  sim_lobster_per_year_sa_depth$Site_Quantity <- aggregate(sim_pop_data$TRIP_ID, by=list(sim_pop_data$year, sim_pop_data$STATAREA, sim_pop_data$assigneddepthstrata), function(x) length(unique(x)))$x
  summary(sim_lobster_per_year_sa_depth)
  sim_lobster_per_year_sa_depth$Ave_Lob_Trap <- sim_lobster_per_year_sa_depth$Lob_Quantity/sim_lobster_per_year_sa_depth$Trap_Quantity
  
  area_per_sa_depth <- as.data.frame(matrix(c(122, 82, 92, 296, 566, 395, 420, 1381, 315, 338, 198, 851), nrow=3, byrow=T))
  colnames(area_per_sa_depth) <- c("depth1", "depth2", "depth3", "total")
  rownames(area_per_sa_depth) <- c("511", "512", "513")
  
  ## sim_substrat_mean
  sim_substrat_mean <- matrix(NA, nrow=length(unique(sim_lobster_per_year_sa_depth$Year)), ncol=3)
  year_id <- unique(sim_lobster_per_year_sa_depth$Year)
  colnames(sim_substrat_mean) <- c("511", "512", "513")
  rownames(sim_substrat_mean) <- year_id
  for (i in 1:length(year_id)){
    sim_substrat_mean[i,1] <- area_per_sa_depth["511", "depth1"]/area_per_sa_depth["511", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["511", "depth2"]/area_per_sa_depth["511", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="21 to 40"& sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["511", "depth3"]/area_per_sa_depth["511", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="41 to 60"& sim_lobster_per_year_sa_depth$Year==year_id[i])] 
    
    sim_substrat_mean[i,2] <- area_per_sa_depth["512", "depth1"]/area_per_sa_depth["512", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["512", "depth2"]/area_per_sa_depth["512", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="21 to 40"& sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["512", "depth3"]/area_per_sa_depth["512", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="41 to 60"& sim_lobster_per_year_sa_depth$Year==year_id[i])] 
    
    sim_substrat_mean[i,3] <- area_per_sa_depth["513", "depth1"]/area_per_sa_depth["513", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["513", "depth2"]/area_per_sa_depth["513", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="21 to 40"& sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["513", "depth3"]/area_per_sa_depth["513", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="41 to 60"& sim_lobster_per_year_sa_depth$Year==year_id[i])] 
  }
  sim_substrat_mean_list[[sim]]<-sim_substrat_mean
}

mean_sim_substra_mean <- matrix(NA, nrow=length(year_id), ncol=3)
sd_sim_substra_mean <- matrix(NA, nrow=length(year_id), ncol=3)
for(i in 1:length(year_id)){
  for(j in 1:3){
    temp <- c()
    for(sim in 1:sim_num){
      temp[sim] <- sim_substrat_mean_list[[sim]][i,j]
    }
    mean_sim_substra_mean[i,j]<-mean(temp)
    sd_sim_substra_mean[i,j]<-sd(temp)
  }
}
colnames(mean_sim_substra_mean) <- c("511", "512", "513")
rownames(mean_sim_substra_mean) <- c(2006:2014)
colnames(sd_sim_substra_mean) <- c("511", "512", "513")
rownames(sd_sim_substra_mean) <- c(2006:2014)

sim_substrat_mean_upper <- mean_sim_substra_mean+sd_sim_substra_mean
sim_substrat_mean_lower <- mean_sim_substra_mean-sd_sim_substra_mean

ylim=range(sim_substrat_mean_upper, sim_substrat_mean_lower)
jpeg(filename = "./plot/vts_pop_mean_1000_2006_2014.jpeg", width=175, height=60, units = "mm", res = 600)

par(mfrow=c(1,3), mar=c(4,4,1,1))
plot(year_id, mean_sim_substra_mean[1:length(year_id),"513"], pch=16, ylim=ylim,  col="black",  xlab="Year", ylab="Abundance Index", cex=0.5, type="o")
polygon(x=c(year_id, rev(year_id)), y=c(sim_substrat_mean_upper[1:length(year_id),"513"], rev(sim_substrat_mean_lower[1:length(year_id),"513"])), col=adjustcolor("deepskyblue3", alpha.f = 0.50), border = NA)
legend("topleft",  "513", bty="n",  cex=0.8)


plot(year_id, mean_sim_substra_mean[1:length(year_id),"512"], pch=16, ylim=ylim,  col="black",  xlab="Year", ylab="Abundance Index", cex=0.5, type="o")
polygon(x=c(year_id, rev(year_id)), y=c(sim_substrat_mean_upper[1:length(year_id),"512"], rev(sim_substrat_mean_lower[1:length(year_id),"512"])), col=adjustcolor("deepskyblue3", alpha.f = 0.50), border = NA)
legend("topleft",  "512", bty="n",  cex=0.8)


plot(year_id, mean_sim_substra_mean[1:length(year_id),"511"], pch=16, ylim=ylim,  col="black",  xlab="Year", ylab="Abundance Index", cex=0.5, type="o")
polygon(x=c(year_id, rev(year_id)), y=c(sim_substrat_mean_upper[1:length(year_id),"511"], rev(sim_substrat_mean_lower[1:length(year_id),"511"])), col=adjustcolor("deepskyblue3", alpha.f = 0.50), border = NA)
legend("topleft",  "511", bty="n",  cex=0.8)
dev.off()

vts_sim_scale <- (mean_sim_substra_mean-min(mean_sim_substra_mean))/diff(range(mean_sim_substra_mean))

save(vts_sim_scale, file="./output/vts_sim_indices_scaled_2006_2014.RData")
ylim=range(vts_sim_scale)
jpeg(filename = "./plot/vts_sim_mean_1000_2006_2014_scale.jpeg", width=175, height=60, units = "mm", res = 600)

par(mfrow=c(1,3), mar=c(4,4,1,1))
plot(year_id, vts_sim_scale[1:length(year_id),"513"], pch=16, ylim=ylim,  col="black",  xlab="Year", ylab="Abundance Index", cex=0.5, type="o")
legend("topleft",  "513", bty="n",  cex=0.8)


plot(year_id, vts_sim_scale[1:length(year_id),"512"], pch=16, ylim=ylim,  col="black",  xlab="Year", ylab="Abundance Index", cex=0.5, type="o")
legend("topleft",  "512", bty="n",  cex=0.8)


plot(year_id, vts_sim_scale[1:length(year_id),"511"], pch=16, ylim=ylim,  col="black",  xlab="Year", ylab="Abundance Index", cex=0.5, type="o")
legend("topleft",  "511", bty="n",  cex=0.8)
dev.off()

#### vts_samples_with_consideration_of_q ####
load("./output/vts_simulated_samples_q.RData")

sim_pop_data <- vts_vtrap_data
na_id <- which(is.na(sim_pop_data$catchability))
simulated_vts_samples_q<- simulated_vts_samples_q[-na_id,]
sim_pop_data<- sim_pop_data[-na_id,]
#sim_pop_data <- na.omit(sim_pop_data)
sim_substrat_mean_list <- list()
for (sim in 1:sim_num){
  sim_lobster_per_year_sa_depth <- aggregate(simulated_vts_samples_q[,sim], by=list(sim_pop_data$year, sim_pop_data$STATAREA, sim_pop_data$assigneddepthstrata), sum)
  colnames(sim_lobster_per_year_sa_depth) <- c("Year", "SA", "Depth", "Lob_Quantity") 
  sim_lobster_per_year_sa_depth$Trap_Quantity <- aggregate(sim_pop_data$TRAP_ID, by=list(sim_pop_data$year, sim_pop_data$STATAREA, sim_pop_data$assigneddepthstrata), length)$x
  sim_lobster_per_year_sa_depth$Site_Quantity <- aggregate(sim_pop_data$TRIP_ID, by=list(sim_pop_data$year, sim_pop_data$STATAREA, sim_pop_data$assigneddepthstrata), function(x) length(unique(x)))$x
  summary(sim_lobster_per_year_sa_depth)
  sim_lobster_per_year_sa_depth$Ave_Lob_Trap <- sim_lobster_per_year_sa_depth$Lob_Quantity/sim_lobster_per_year_sa_depth$Trap_Quantity
  
  area_per_sa_depth <- as.data.frame(matrix(c(122, 82, 92, 296, 566, 395, 420, 1381, 315, 338, 198, 851), nrow=3, byrow=T))
  colnames(area_per_sa_depth) <- c("depth1", "depth2", "depth3", "total")
  rownames(area_per_sa_depth) <- c("511", "512", "513")
  
  ## sim_substrat_mean
  sim_substrat_mean <- matrix(NA, nrow=length(unique(sim_lobster_per_year_sa_depth$Year)), ncol=3)
  year_id <- unique(sim_lobster_per_year_sa_depth$Year)
  colnames(sim_substrat_mean) <- c("511", "512", "513")
  rownames(sim_substrat_mean) <- year_id
  for (i in 1:length(year_id)){
    sim_substrat_mean[i,1] <- area_per_sa_depth["511", "depth1"]/area_per_sa_depth["511", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["511", "depth2"]/area_per_sa_depth["511", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="21 to 40"& sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["511", "depth3"]/area_per_sa_depth["511", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="511" & sim_lobster_per_year_sa_depth$Depth=="41 to 60"& sim_lobster_per_year_sa_depth$Year==year_id[i])] 
    
    sim_substrat_mean[i,2] <- area_per_sa_depth["512", "depth1"]/area_per_sa_depth["512", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["512", "depth2"]/area_per_sa_depth["512", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="21 to 40"& sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["512", "depth3"]/area_per_sa_depth["512", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="512" & sim_lobster_per_year_sa_depth$Depth=="41 to 60"& sim_lobster_per_year_sa_depth$Year==year_id[i])] 
    
    sim_substrat_mean[i,3] <- area_per_sa_depth["513", "depth1"]/area_per_sa_depth["513", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="1 to 20" & sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["513", "depth2"]/area_per_sa_depth["513", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="21 to 40"& sim_lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["513", "depth3"]/area_per_sa_depth["513", "total"]*sim_lobster_per_year_sa_depth$Ave_Lob_Trap[which(sim_lobster_per_year_sa_depth$SA=="513" & sim_lobster_per_year_sa_depth$Depth=="41 to 60"& sim_lobster_per_year_sa_depth$Year==year_id[i])] 
  }
  sim_substrat_mean_list[[sim]]<-sim_substrat_mean
}

mean_sim_substra_mean_q <- matrix(NA, nrow=length(year_id), ncol=3)
sd_sim_substra_mean_q <- matrix(NA, nrow=length(year_id), ncol=3)
for(i in 1:length(year_id)){
  for(j in 1:3){
    temp <- c()
    for(sim in 1:sim_num){
      temp[sim] <- sim_substrat_mean_list[[sim]][i,j]
    }
    mean_sim_substra_mean_q[i,j]<-mean(temp)
    sd_sim_substra_mean_q[i,j]<-sd(temp)
  }
}
colnames(mean_sim_substra_mean_q) <- c("511", "512", "513")
rownames(mean_sim_substra_mean_q) <- c(2006:2014)
colnames(sd_sim_substra_mean_q) <- c("511", "512", "513")
rownames(sd_sim_substra_mean_q) <- c(2006:2014)

sim_substrat_mean_upper_q <- mean_sim_substra_mean_q+sd_sim_substra_mean_q
sim_substrat_mean_lower_q <- mean_sim_substra_mean_q-sd_sim_substra_mean_q

ylim=range(sim_substrat_mean_upper_q, sim_substrat_mean_lower_q)
jpeg(filename = "./plot/vts_mean_q_1000_2006_2014.jpeg", width=175, height=60, units = "mm", res = 600)

par(mfrow=c(1,3), mar=c(4,4,1,1))
plot(year_id, mean_sim_substra_mean_q[1:length(year_id),"513"], pch=16, ylim=ylim,  col="black",  xlab="Year", ylab="Abundance Index", cex=0.5, type="o")
polygon(x=c(year_id, rev(year_id)), y=c(sim_substrat_mean_upper_q[1:length(year_id),"513"], rev(sim_substrat_mean_lower_q[1:length(year_id),"513"])), col=adjustcolor("deepskyblue3", alpha.f = 0.50), border = NA)
legend("topleft",  "513", bty="n",  cex=0.8)


plot(year_id, mean_sim_substra_mean_q[1:length(year_id),"512"], pch=16, ylim=ylim,  col="black",  xlab="Year", ylab="Abundance Index", cex=0.5, type="o")
polygon(x=c(year_id, rev(year_id)), y=c(sim_substrat_mean_upper_q[1:length(year_id),"512"], rev(sim_substrat_mean_lower_q[1:length(year_id),"512"])), col=adjustcolor("deepskyblue3", alpha.f = 0.50), border = NA)
legend("topleft",  "512", bty="n",  cex=0.8)


plot(year_id, mean_sim_substra_mean_q[1:length(year_id),"511"], pch=16, ylim=ylim,  col="black",  xlab="Year", ylab="Abundance Index", cex=0.5, type="o")
polygon(x=c(year_id, rev(year_id)), y=c(sim_substrat_mean_upper_q[1:length(year_id),"511"], rev(sim_substrat_mean_lower_q[1:length(year_id),"511"])), col=adjustcolor("deepskyblue3", alpha.f = 0.50), border = NA)
legend("topleft",  "511", bty="n",  cex=0.8)
dev.off()

vts_q_scale <- (mean_sim_substra_mean_q-min(mean_sim_substra_mean_q))/diff(range(mean_sim_substra_mean_q))

save(vts_q_scale, file="./output/vts_q_indices_scaled_2006_2014.RData")
ylim=range(vts_q_scale)
jpeg(filename = "./plot/vts_mean_q_1000_2006_2014_scale.jpeg", width=175, height=60, units = "mm", res = 600)

par(mfrow=c(1,3), mar=c(4,4,1,1))
plot(year_id, vts_q_scale[1:length(year_id),"513"], pch=16, ylim=ylim,  col="black",  xlab="Year", ylab="Abundance Index", cex=0.5, type="o")
legend("topleft",  "513", bty="n",  cex=0.8)


plot(year_id, vts_q_scale[1:length(year_id),"512"], pch=16, ylim=ylim,  col="black",  xlab="Year", ylab="Abundance Index", cex=0.5, type="o")
legend("topleft",  "512", bty="n",  cex=0.8)


plot(year_id, vts_q_scale[1:length(year_id),"511"], pch=16, ylim=ylim,  col="black",  xlab="Year", ylab="Abundance Index", cex=0.5, type="o")
legend("topleft",  "511", bty="n",  cex=0.8)
dev.off()

#### Plot pop, vts available lobsters, and vts with q samples in one plot ####
real_vts_scale <- (substrat_mean-min(substrat_mean))/diff(range(substrat_mean))

ylim=range(population_scale, vts_sim_scale, vts_q_scale, real_vts_scale)
jpeg(filename = "./plot/scaled_indices_one_plot.jpeg", width=175, height=60, units = "mm", res = 600)

par(mfrow=c(1,3), mar=c(4,4,1,1))
plot(year_id, real_vts_scale[1:length(year_id),"513"], pch=1, ylim=ylim, col="black", type="o", lty=1, xlab="Year", ylab="Abundance Index")
lines(year_id, population_scale[1:length(year_id),"513"], pch=2, type="o", col="purple", lty=2)
lines(year_id, vts_sim_scale[1:length(year_id),"513"], pch=3, type="o", col="deepskyblue", lty=3)
lines(year_id, vts_q_scale[1:length(year_id),"513"], pch=4, type="o", col="coral", lty=4)
legend("topleft",  c("Observed VTS index", "Simulated Population Index", "Simulated VTS available lobsters index", "Simulated VTS with q index"), bty="n",  cex=0.7, pch=c(1, 2, 3, 4), lty=c(1,2,3,4), col=c("black", "purple", "deepskyblue", "coral"), title="513")

plot(year_id, real_vts_scale[1:length(year_id),"512"], pch=1, ylim=ylim, col="black", type="o", lty=1, xlab="Year", ylab="Abundance Index")
lines(year_id, population_scale[1:length(year_id),"512"], pch=2, type="o", col="purple", lty=2)
lines(year_id, vts_sim_scale[1:length(year_id),"512"], pch=3, type="o", col="deepskyblue", lty=3)
lines(year_id, vts_q_scale[1:length(year_id),"512"], pch=4, type="o", col="coral", lty=4)
legend("topleft",  "512", bty="n",  cex=0.7)

plot(year_id, real_vts_scale[1:length(year_id),"511"], pch=1, ylim=ylim, col="black", type="o", lty=1, xlab="Year", ylab="Abundance Index")
lines(year_id, population_scale[1:length(year_id),"511"], pch=2, type="o", col="purple", lty=2)
lines(year_id, vts_sim_scale[1:length(year_id),"511"], pch=3, type="o", col="deepskyblue", lty=3)
lines(year_id, vts_q_scale[1:length(year_id),"511"], pch=4, type="o", col="coral", lty=4)
legend("topleft",  "511", bty="n",  cex=0.7)
dev.off()
#### correlation check:2006-2014 ####
cor.test(population_index[as.character(year_id),"511"], mean_sim_substra_mean[,"511"])
cor.test(population_index[as.character(year_id),"512"], mean_sim_substra_mean[,"512"])
cor.test(population_index[as.character(year_id),"513"], mean_sim_substra_mean[,"513"])

cor.test(population_index[as.character(year_id),"511"], mean_sim_substra_mean_q[,"511"])
cor.test(population_index[as.character(year_id),"512"], mean_sim_substra_mean_q[,"512"])
cor.test(population_index[as.character(year_id),"513"], mean_sim_substra_mean_q[,"513"])

cor.test(mean_sim_substra_mean[,"511"], mean_sim_substra_mean_q[,"511"])
cor.test(mean_sim_substra_mean[,"512"], mean_sim_substra_mean_q[,"512"])
cor.test(mean_sim_substra_mean[,"513"], mean_sim_substra_mean_q[,"513"])

cor.test(substrat_mean[,"511"], mean_sim_substra_mean_q[,"511"])
cor.test(substrat_mean[,"512"], mean_sim_substra_mean_q[,"512"])
cor.test(substrat_mean[,"513"], mean_sim_substra_mean_q[,"513"])

cor(population_index[as.character(year_id),], mean_sim_substra_mean, method="pearson", use="pairwise.complete.obs")
cor(population_index[as.character(year_id),], mean_sim_substra_mean_q, method="pearson", use="pairwise.complete.obs")
cor(mean_sim_substra_mean, mean_sim_substra_mean_q, method="pearson", use="pairwise.complete.obs")

#### 2015 and 2016 observed index ####
sublegal_lobsters <- vts_lobster_data[which(vts_lobster_data$SAMPLE_LENGTH < 83 & vts_lobster_data$year > 2014 & vts_lobster_data$year < 2017 & (vts_lobster_data$TRAP_TYPE=="V" | vts_lobster_data$TRAP_TYPE=="v")),]

#lobster_per_trap <- aggregate(sublegal_lobsters$SAMPLE_SEQ_NO, by=list(sublegal_lobsters$TRAP_ID), length)

lobster_per_trap <- aggregate(sublegal_lobsters$SAMPLE_LENGTH, by=list(sublegal_lobsters$TRAP_ID, sublegal_lobsters$SET_OVER_DAYS), length)
lobster_per_trap$x <- round(lobster_per_trap$x/lobster_per_trap$Group.2)*3
lobster_per_trap <- as.data.frame(cbind(lobster_per_trap$Group.1, lobster_per_trap$x))
colnames(lobster_per_trap) <- c("Group.1", "x")

for (i in 1:nrow(vts_trap_data)){
  print(i)
  if (vts_trap_data$TRAP_ID[i] %in% lobster_per_trap$Group.1) vts_trap_data$quantity[i] <- lobster_per_trap$x[which(lobster_per_trap$Group.1==vts_trap_data$TRAP_ID[i])]
  else vts_trap_data$quantity[i] <- NA
}
summary(vts_trap_data)

validation_vts_vtrap_data <- vts_trap_data[which(vts_trap_data$year>2014 & vts_trap_data$year<2017 &(vts_trap_data$TRAP_TYPE=="V" |vts_trap_data$TRAP_TYPE=="v")),]
validation_vts_vtrap_data <- na.omit(validation_vts_vtrap_data)
write.csv(validation_vts_vtrap_data, "./data/vts/validation_vts_vtrap_data.csv")

plot_data <- as.data.frame(cbind(validation_vts_vtrap_data$LONGITUDE_DECIMAL, validation_vts_vtrap_data$LATITUDE_DECIMAL, validation_vts_vtrap_data$quantity))
colnames(plot_data) <- c("Longitude", "Latitude", "Y")


plotvar <- plot_data$Y
nclr=8
plotclr <- rev(brewer.pal(nclr,"RdYlBu"))
class <- classIntervals(plotvar, nclr, style="quantile")
class$brks = round(class$brks, digits=2)
colcode <- findColours(class, plotclr)

start_x <- range(lobster_catch_data$end_lon)[1]
end_x <- range(lobster_catch_data$end_lon)[2]
start_y <- range(lobster_catch_data$end_lat)[1]
end_y <- range(lobster_catch_data$end_lat)[2]
jpeg(filename = paste("./plot/validation_vts_lobster_quantity_map_2015_2016.jpeg", sep=""), width=100, height=50, units = "mm", res = 600)
layout(matrix(c(1,1,1,2), 1, 4, byrow = TRUE))
par(mar=c(4,4,1,1))
plot(plot_data$Longitude, plot_data$Latitude, pch=16, col=colcode, cex=0.3, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, xlab="Longitude", ylab="Latitude")
map(database = "worldHires", ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE, lwd=0.1)
box()
degAxis(1)
degAxis(2)
par(mar=c(0.5,0.5,0.5,0.5))
plot.new()
legend("left", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=0.7, bty="n", title="Lobster Density (#/trap)")
dev.off()

lobster_per_year_sa_depth <- aggregate(validation_vts_vtrap_data$quantity, by=list(validation_vts_vtrap_data$year, validation_vts_vtrap_data$STATAREA, validation_vts_vtrap_data$assigneddepthstrata), sum)
colnames(lobster_per_year_sa_depth) <- c("Year", "SA", "Depth", "Lob_Quantity") 

lobster_per_year_sa_depth$Trap_Quantity <- aggregate(validation_vts_vtrap_data$TRAP_ID, by=list(validation_vts_vtrap_data$year, validation_vts_vtrap_data$STATAREA, validation_vts_vtrap_data$assigneddepthstrata), length)$x

lobster_per_year_sa_depth$Site_Quantity <- aggregate(validation_vts_vtrap_data$SITE_ID, by=list(validation_vts_vtrap_data$year, validation_vts_vtrap_data$STATAREA, validation_vts_vtrap_data$assigneddepthstrata), function(x) length(unique(x)))$x
summary(lobster_per_year_sa_depth)

lobster_per_year_sa_depth$Ave_Lob_Trap <- lobster_per_year_sa_depth$Lob_Quantity/lobster_per_year_sa_depth$Trap_Quantity

area_per_sa_depth <- as.data.frame(matrix(c(122, 82, 92, 296, 566, 395, 420, 1381, 315, 338, 198, 851), nrow=3, byrow=T))
colnames(area_per_sa_depth) <- c("depth1", "depth2", "depth3", "total")
rownames(area_per_sa_depth) <- c("511", "512", "513")
## substrat_mean
validation_substrat_mean <- matrix(NA, nrow=length(unique(lobster_per_year_sa_depth$Year)), ncol=3)
year_id <- unique(lobster_per_year_sa_depth$Year)
colnames(validation_substrat_mean) <- c("511", "512", "513")
rownames(validation_substrat_mean) <- year_id
for (i in 1:length(year_id)){
  validation_substrat_mean[i,1] <- area_per_sa_depth["511", "depth1"]/area_per_sa_depth["511", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["511", "depth2"]/area_per_sa_depth["511", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="21 to 40"& lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["511", "depth3"]/area_per_sa_depth["511", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="41 to 60"& lobster_per_year_sa_depth$Year==year_id[i])] 
  
  validation_substrat_mean[i,2] <- area_per_sa_depth["512", "depth1"]/area_per_sa_depth["512", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["512", "depth2"]/area_per_sa_depth["512", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="21 to 40"& lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["512", "depth3"]/area_per_sa_depth["512", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="41 to 60"& lobster_per_year_sa_depth$Year==year_id[i])] 
  
  validation_substrat_mean[i,3] <- area_per_sa_depth["513", "depth1"]/area_per_sa_depth["513", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["513", "depth2"]/area_per_sa_depth["513", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="21 to 40"& lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["513", "depth3"]/area_per_sa_depth["513", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="41 to 60"& lobster_per_year_sa_depth$Year==year_id[i])] 
  
}

## substrat_variance
lobster_per_year_sa_depth_site <- aggregate(validation_vts_vtrap_data$quantity, by=list(validation_vts_vtrap_data$year, validation_vts_vtrap_data$STATAREA, validation_vts_vtrap_data$assigneddepthstrata, validation_vts_vtrap_data$SITE_ID), sum)
colnames(lobster_per_year_sa_depth_site) <- c("Year", "SA", "Depth", "Site", "Lob_Quantity") 

lobster_per_year_sa_depth_site$Trap_Quantity <- aggregate(validation_vts_vtrap_data$TRAP_ID, by=list(validation_vts_vtrap_data$year, validation_vts_vtrap_data$STATAREA, validation_vts_vtrap_data$assigneddepthstrata, validation_vts_vtrap_data$SITE_ID), length)$x

lobster_per_year_sa_depth_site$Quantity_per_Trap <- lobster_per_year_sa_depth_site$Lob_Quantity/lobster_per_year_sa_depth_site$Trap_Quantity

lobster_per_year_sa_depth_site=merge(lobster_per_year_sa_depth_site, lobster_per_year_sa_depth[, c("Year", "SA", "Depth", "Ave_Lob_Trap")], by=c("Year", "SA", "Depth"))

lobster_per_year_sa_depth_site$Sub <- (lobster_per_year_sa_depth_site$Ave_Lob_Trap - lobster_per_year_sa_depth_site$Quantity_per_Trap)^2

individual_value <- aggregate(lobster_per_year_sa_depth_site$Sub, by=list(lobster_per_year_sa_depth_site$Year, lobster_per_year_sa_depth_site$SA, lobster_per_year_sa_depth_site$Depth), sum)
colnames(individual_value) <- c("Year", "SA", "Depth", "x")
individual_value$individual_value <- individual_value$x/aggregate(lobster_per_year_sa_depth_site$Site, by=list(lobster_per_year_sa_depth_site$Year, lobster_per_year_sa_depth_site$SA, lobster_per_year_sa_depth_site$Depth), length)$x

validation_substrat_var <- matrix(NA, nrow=length(unique(lobster_per_year_sa_depth$Year)), ncol=3)
colnames(validation_substrat_var) <- c("511", "512", "513")
rownames(validation_substrat_var) <- year_id

for (i in 1:length(year_id)){
  validation_substrat_var[i,1] <- 1/area_per_sa_depth["511", "total"]^2*((area_per_sa_depth["511", "depth1"]*(area_per_sa_depth["511", "depth1"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["511", "depth2"]*(area_per_sa_depth["511", "depth2"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["511", "depth3"]*(area_per_sa_depth["511", "depth3"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])]))
  
  validation_substrat_var[i,2] <- 1/area_per_sa_depth["512", "total"]^2*((area_per_sa_depth["512", "depth1"]*(area_per_sa_depth["512", "depth1"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["512", "depth2"]*(area_per_sa_depth["512", "depth2"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["512", "depth3"]*(area_per_sa_depth["512", "depth3"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])]))
  
  validation_substrat_var[i,3] <- 1/area_per_sa_depth["513", "total"]^2*((area_per_sa_depth["513", "depth1"]*(area_per_sa_depth["513", "depth1"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["513", "depth2"]*(area_per_sa_depth["513", "depth2"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["513", "depth3"]*(area_per_sa_depth["513", "depth3"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])]))
}

jpeg(filename = "./plot/validation_vts_lobster_substrat_mean_real.jpeg", width=150, height=50, units = "mm", res = 600)
par(mfrow=c(1,3))
ylim_min <- min(validation_substrat_mean-validation_substrat_var)
ylim_max <- max(validation_substrat_mean+validation_substrat_var)

plot(year_id, validation_substrat_mean[,"513"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="black", lwd=0.5, cex=0.4, ylim=c(ylim_min, ylim_max))
polygon(x=c(year_id, rev(year_id)), y=c(validation_substrat_mean[,"513"]+validation_substrat_var[,"513"], rev(validation_substrat_mean[,"513"]-validation_substrat_var[,"513"])), col=adjustcolor("deepskyblue3", alpha.f = 0.50), border = NA)
legend("topleft", "513", bty="n")

plot(year_id, validation_substrat_mean[,"512"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="black", lwd=0.5, cex=0.4, ylim=c(ylim_min, ylim_max))
polygon(x=c(year_id, rev(year_id)), y=c(validation_substrat_mean[,"512"]+validation_substrat_var[,"512"], rev(validation_substrat_mean[,"512"]-validation_substrat_var[,"512"])), col=adjustcolor("deepskyblue3", alpha.f = 0.50), border = NA)
legend("topleft", "512", bty="n")

plot(year_id, validation_substrat_mean[,"511"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="black", lwd=0.5, cex=0.4, ylim=c(ylim_min, ylim_max))
polygon(x=c(year_id, rev(year_id)), y=c(validation_substrat_mean[,"511"]+validation_substrat_var[,"511"], rev(validation_substrat_mean[,"511"]-validation_substrat_var[,"511"])), col=adjustcolor("deepskyblue3", alpha.f = 0.50), border = NA)
legend("topleft", "511", bty="n")

dev.off()

#### Prediction of 2015 and 2016 vts with q index ####
depth_grid_plot <- read.csv("./output/grid_depth_data.csv")
prediction_points <- SpatialPoints(cbind(depth_grid_plot$lon[which(depth_grid_plot$fathom<0)], depth_grid_plot$lat[which(depth_grid_plot$fathom<0)]), proj4string=CRS("+proj=longlat +datum=NAD83 +no_defs")) 
vts_points <- SpatialPoints(cbind(validation_vts_vtrap_data$LONGITUDE_DECIMAL, validation_vts_vtrap_data$LATITUDE_DECIMAL), proj4string=CRS("+proj=longlat +datum=NAD83 +no_defs"))

if(TRUE){
  nearest_pred_points <- c()
  # for (i in 1:length(vts_points)){
  #   print(i)
  #   nearest_pred_points[i] <- which.min(gDistance(vts_points[i], prediction_points, byid=T))
  #   gc()
  # }
  nearest_pred_points <- sapply(1:length(vts_points), function(x) which.min(gDistance(vts_points[x], prediction_points, byid=T)))
  save(nearest_pred_points, file="./data/vts/validation_vts_nearest_prediction_point.RData")
  
  load("./data/vts/validation_vts_nearest_prediction_point.RData")
  dates$id <- 1:nrow(dates)
  simulated_vts_samples <- matrix(NA, nrow = length(vts_points), ncol=sim_num)
  for (i in 1:length(vts_points)){
    print(i)
    if(validation_vts_vtrap_data[i,"year"]==2017) simulated_vts_samples[i,] <- rep(NA, sim_num)
    else {
      if(validation_vts_vtrap_data[i,"month"]==9) time_id <- dates$id[which(dates$y==validation_vts_vtrap_data[i,"year"] & dates$m == 8)]
      else {
        time_id <- dates$id[which(dates$y==validation_vts_vtrap_data[i,"year"] & dates$m == validation_vts_vtrap_data[i,"month"])]
      }
      temp_value <- population_1000[[time_id]][nearest_pred_points[i],]
      if (is.na(temp_value)) simulated_vts_samples[i,]<- rep(NA, sim_num)
      else simulated_vts_samples[i,]<- temp_value
      
    }
  }
  summary(simulated_vts_samples)
  
  save(simulated_vts_samples, file="./output/validation_vts_simulated_samples.RData")
}

load("./output/validation_vts_simulated_samples.RData") #simulated_vts_samples
load("./output/temperature_raster_data.RData")
mean_temperature <- dates
temp<- c()
for(i in 1:length(temperature_raster_data)){
  temp[i]<-mean(temperature_raster_data[[i]])
}
mean_temperature$temperature <- round(temp, digits = 2)

temperature_point_match <- c()
for(i in 1:nrow(validation_vts_vtrap_data)){
  if(validation_vts_vtrap_data$month[i] == 9) temperature_point_match[i] <- mean_temperature$temperature[which(mean_temperature$y==validation_vts_vtrap_data$year[i] & mean_temperature$m==8)]
  else temperature_point_match[i] <- mean_temperature$temperature[which(mean_temperature$y==validation_vts_vtrap_data$year[i] & mean_temperature$m==validation_vts_vtrap_data$month[i])]
}
new_data <- validation_vts_vtrap_data
new_data$temperature <- temperature_point_match

temperature_point <- c()
for(i in 1:nrow(validation_vts_vtrap_data)){
  temp <- catchability_mean$temperature[which(catchability_mean$assigneddepthstrata == new_data$assigneddepthstrata[i])]
  temperature_point[i] <- temp[which.min(abs(new_data$temperature[i]-temp))]
}
validation_vts_vtrap_data$temperature <- temperature_point

temp_data <- validation_vts_vtrap_data
merge_data <- merge(temp_data, catchability_mean, by=c("temperature","assigneddepthstrata"))
for (i in 1:nrow(validation_vts_vtrap_data)){
  validation_vts_vtrap_data$q_mean[i] <- merge_data$mean[which(merge_data$TRAP_ID==validation_vts_vtrap_data$TRAP_ID[i])]
}

merge_data <- merge(temp_data, catchability_sd, by=c("temperature","assigneddepthstrata"))
for (i in 1:nrow(validation_vts_vtrap_data)){
  validation_vts_vtrap_data$q_sd[i] <- merge_data$sd[which(merge_data$TRAP_ID==validation_vts_vtrap_data$TRAP_ID[i])]
}


if(TRUE){
  validation_simulated_vts_samples_q <- matrix(NA, nrow = length(vts_points), ncol=sim_num)
  for(i in 1:nrow(simulated_vts_samples)){
    print(i)
    if(is.na(simulated_vts_samples[i,])) validation_simulated_vts_samples_q[i,] <- rep(NA, sim_num)
    else{
      validation_simulated_vts_samples_q[i,] <- simulated_vts_samples[i,]*rnorm(1, mean=validation_vts_vtrap_data$q_mean[i], sd=validation_vts_vtrap_data$q_sd[i])
      if(validation_simulated_vts_samples_q[i,]<0) validation_simulated_vts_samples_q[i,]<-simulated_vts_samples[i,]*validation_vts_vtrap_data$q_mean[i]
    }
  }
  
  save(validation_simulated_vts_samples_q, file="./output/validation_vts_simulated_samples_q.RData")
}
## calculate vts sim abundance q index 
load("./output/validation_vts_simulated_samples_q.RData") 
validation_sim_substrat_mean_list_q <- list()

for (sim in 1:sim_num){
  lobster_per_year_sa_depth <- aggregate(validation_simulated_vts_samples_q[,sim], by=list(validation_vts_vtrap_data$year, validation_vts_vtrap_data$STATAREA, validation_vts_vtrap_data$assigneddepthstrata), sum)
  colnames(lobster_per_year_sa_depth) <- c("Year", "SA", "Depth", "Lob_Quantity") 
  
  lobster_per_year_sa_depth$Trap_Quantity <- aggregate(validation_vts_vtrap_data$TRAP_ID, by=list(validation_vts_vtrap_data$year, validation_vts_vtrap_data$STATAREA, validation_vts_vtrap_data$assigneddepthstrata), length)$x
  
  lobster_per_year_sa_depth$Site_Quantity <- aggregate(validation_vts_vtrap_data$SITE_ID, by=list(validation_vts_vtrap_data$year, validation_vts_vtrap_data$STATAREA, validation_vts_vtrap_data$assigneddepthstrata), function(x) length(unique(x)))$x
  summary(lobster_per_year_sa_depth)
  
  lobster_per_year_sa_depth$Ave_Lob_Trap <- lobster_per_year_sa_depth$Lob_Quantity/lobster_per_year_sa_depth$Trap_Quantity
  
  area_per_sa_depth <- as.data.frame(matrix(c(122, 82, 92, 296, 566, 395, 420, 1381, 315, 338, 198, 851), nrow=3, byrow=T))
  colnames(area_per_sa_depth) <- c("depth1", "depth2", "depth3", "total")
  rownames(area_per_sa_depth) <- c("511", "512", "513")
  ## substrat_mean
  validation_sim_substrat_mean <- matrix(NA, nrow=length(unique(lobster_per_year_sa_depth$Year)), ncol=3)
  year_id <- unique(lobster_per_year_sa_depth$Year)
  colnames(validation_sim_substrat_mean) <- c("511", "512", "513")
  rownames(validation_sim_substrat_mean) <- year_id
  for (i in 1:length(year_id)){
    validation_sim_substrat_mean[i,1] <- area_per_sa_depth["511", "depth1"]/area_per_sa_depth["511", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["511", "depth2"]/area_per_sa_depth["511", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="21 to 40"& lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["511", "depth3"]/area_per_sa_depth["511", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="41 to 60"& lobster_per_year_sa_depth$Year==year_id[i])] 
    
    validation_sim_substrat_mean[i,2] <- area_per_sa_depth["512", "depth1"]/area_per_sa_depth["512", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["512", "depth2"]/area_per_sa_depth["512", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="21 to 40"& lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["512", "depth3"]/area_per_sa_depth["512", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="41 to 60"& lobster_per_year_sa_depth$Year==year_id[i])] 
    
    validation_sim_substrat_mean[i,3] <- area_per_sa_depth["513", "depth1"]/area_per_sa_depth["513", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["513", "depth2"]/area_per_sa_depth["513", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="21 to 40"& lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["513", "depth3"]/area_per_sa_depth["513", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="41 to 60"& lobster_per_year_sa_depth$Year==year_id[i])] 
    
  }
  
  validation_sim_substrat_mean_list_q[[sim]]<-validation_sim_substrat_mean
}

mean_validation_sim_substra_mean_q <- matrix(NA, nrow=length(year_id), ncol=3)
sd_validation_sim_substra_mean_q <- matrix(NA, nrow=length(year_id), ncol=3)
for(i in 1:length(year_id)){
  for(j in 1:3){
    temp <- c()
    for(sim in 1:sim_num){
      temp[sim] <- validation_sim_substrat_mean_list_q[[sim]][i,j]
    }
    mean_validation_sim_substra_mean_q[i,j]<-mean(temp)
    sd_validation_sim_substra_mean_q[i,j]<-sd(temp)
  }
}
colnames(mean_validation_sim_substra_mean_q) <- c("511", "512", "513")
rownames(mean_validation_sim_substra_mean_q) <- c(2015:2016)
colnames(sd_validation_sim_substra_mean_q) <- c("511", "512", "513")
rownames(sd_validation_sim_substra_mean_q) <- c(2015:2016)

sim_validation_substrat_mean_upper_q <- mean_validation_sim_substra_mean_q+sd_validation_sim_substra_mean_q
sim_validation_substrat_mean_lower_q <- mean_validation_sim_substra_mean_q-sd_validation_sim_substra_mean_q

ylim=range(sim_validation_substrat_mean_upper_q, sim_validation_substrat_mean_lower_q)
jpeg(filename = "./plot/validation_vts_pop_mean_1000_q_2015_2016.jpeg", width=175, height=60, units = "mm", res = 600)

par(mfrow=c(1,3), mar=c(4,4,1,1))
plot(year_id, mean_validation_sim_substra_mean_q[1:length(year_id),"513"], pch=16, ylim=ylim,  col="black",  xlab="Year", ylab="Abundance Index", cex=0.5, type="o")
polygon(x=c(year_id, rev(year_id)), y=c(sim_validation_substrat_mean_upper_q[1:length(year_id),"513"], rev(sim_validation_substrat_mean_lower_q[1:length(year_id),"513"])), col=adjustcolor("deepskyblue3", alpha.f = 0.50), border = NA)
legend("topleft",  "513", bty="n",  cex=0.8)


plot(year_id, mean_validation_sim_substra_mean_q[1:length(year_id),"512"], pch=16, ylim=ylim,  col="black",  xlab="Year", ylab="Abundance Index", cex=0.5, type="o")
polygon(x=c(year_id, rev(year_id)), y=c(sim_validation_substrat_mean_upper_q[1:length(year_id),"512"], rev(sim_validation_substrat_mean_lower_q[1:length(year_id),"512"])), col=adjustcolor("deepskyblue3", alpha.f = 0.50), border = NA)
legend("topleft",  "512", bty="n",  cex=0.8)


plot(year_id, mean_validation_sim_substra_mean_q[1:length(year_id),"511"], pch=16, ylim=ylim,  col="black",  xlab="Year", ylab="Abundance Index", cex=0.5, type="o")
polygon(x=c(year_id, rev(year_id)), y=c(sim_validation_substrat_mean_upper_q[1:length(year_id),"511"], rev(sim_validation_substrat_mean_lower_q[1:length(year_id),"511"])), col=adjustcolor("deepskyblue3", alpha.f = 0.50), border = NA)
legend("topleft",  "511", bty="n",  cex=0.8)
dev.off()

#### Prediction of 2015 and 2016 vts available lobsters index ####
load("./output/validation_vts_simulated_samples.RData") #simulated_vts_samples
validation_sim_substrat_mean_list <- list()

for (sim in 1:sim_num){
  lobster_per_year_sa_depth <- aggregate(simulated_vts_samples[,sim], by=list(validation_vts_vtrap_data$year, validation_vts_vtrap_data$STATAREA, validation_vts_vtrap_data$assigneddepthstrata), sum)
  colnames(lobster_per_year_sa_depth) <- c("Year", "SA", "Depth", "Lob_Quantity") 
  
  lobster_per_year_sa_depth$Trap_Quantity <- aggregate(validation_vts_vtrap_data$TRAP_ID, by=list(validation_vts_vtrap_data$year, validation_vts_vtrap_data$STATAREA, validation_vts_vtrap_data$assigneddepthstrata), length)$x
  
  lobster_per_year_sa_depth$Site_Quantity <- aggregate(validation_vts_vtrap_data$SITE_ID, by=list(validation_vts_vtrap_data$year, validation_vts_vtrap_data$STATAREA, validation_vts_vtrap_data$assigneddepthstrata), function(x) length(unique(x)))$x
  summary(lobster_per_year_sa_depth)
  
  lobster_per_year_sa_depth$Ave_Lob_Trap <- lobster_per_year_sa_depth$Lob_Quantity/lobster_per_year_sa_depth$Trap_Quantity
  
  area_per_sa_depth <- as.data.frame(matrix(c(122, 82, 92, 296, 566, 395, 420, 1381, 315, 338, 198, 851), nrow=3, byrow=T))
  colnames(area_per_sa_depth) <- c("depth1", "depth2", "depth3", "total")
  rownames(area_per_sa_depth) <- c("511", "512", "513")
  ## substrat_mean
  validation_sim_substrat_mean <- matrix(NA, nrow=length(unique(lobster_per_year_sa_depth$Year)), ncol=3)
  year_id <- unique(lobster_per_year_sa_depth$Year)
  colnames(validation_sim_substrat_mean) <- c("511", "512", "513")
  rownames(validation_sim_substrat_mean) <- year_id
  for (i in 1:length(year_id)){
    validation_sim_substrat_mean[i,1] <- area_per_sa_depth["511", "depth1"]/area_per_sa_depth["511", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["511", "depth2"]/area_per_sa_depth["511", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="21 to 40"& lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["511", "depth3"]/area_per_sa_depth["511", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="41 to 60"& lobster_per_year_sa_depth$Year==year_id[i])] 
    
    validation_sim_substrat_mean[i,2] <- area_per_sa_depth["512", "depth1"]/area_per_sa_depth["512", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["512", "depth2"]/area_per_sa_depth["512", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="21 to 40"& lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["512", "depth3"]/area_per_sa_depth["512", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="41 to 60"& lobster_per_year_sa_depth$Year==year_id[i])] 
    
    validation_sim_substrat_mean[i,3] <- area_per_sa_depth["513", "depth1"]/area_per_sa_depth["513", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["513", "depth2"]/area_per_sa_depth["513", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="21 to 40"& lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["513", "depth3"]/area_per_sa_depth["513", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="41 to 60"& lobster_per_year_sa_depth$Year==year_id[i])] 
    
  }
  
  validation_sim_substrat_mean_list[[sim]]<-validation_sim_substrat_mean
}


mean_validation_sim_substra_mean <- matrix(NA, nrow=length(year_id), ncol=3)
sd_validation_sim_substra_mean <- matrix(NA, nrow=length(year_id), ncol=3)
for(i in 1:length(year_id)){
  for(j in 1:3){
    temp <- c()
    for(sim in 1:sim_num){
      temp[sim] <- validation_sim_substrat_mean_list[[sim]][i,j]
    }
    mean_validation_sim_substra_mean[i,j]<-mean(temp)
    sd_validation_sim_substra_mean[i,j]<-sd(temp)
  }
}
colnames(mean_validation_sim_substra_mean) <- c("511", "512", "513")
rownames(mean_validation_sim_substra_mean) <- c(2015:2016)
colnames(sd_validation_sim_substra_mean) <- c("511", "512", "513")
rownames(sd_validation_sim_substra_mean) <- c(2015:2016)

sim_validation_substrat_mean_upper <- mean_validation_sim_substra_mean+sd_validation_sim_substra_mean
sim_validation_substrat_mean_lower <- mean_validation_sim_substra_mean-sd_validation_sim_substra_mean

ylim=range(sim_validation_substrat_mean_upper, sim_validation_substrat_mean_lower)
jpeg(filename = "./plot/validation_vts_pop_mean_1000_2015_2016.jpeg", width=175, height=60, units = "mm", res = 600)

par(mfrow=c(1,3), mar=c(4,4,1,1))
plot(year_id, mean_validation_sim_substra_mean[1:length(year_id),"513"], pch=16, ylim=ylim,  col="black",  xlab="Year", ylab="Abundance Index", cex=0.5, type="o")
polygon(x=c(year_id, rev(year_id)), y=c(sim_validation_substrat_mean_upper[1:length(year_id),"513"], rev(sim_validation_substrat_mean_lower[1:length(year_id),"513"])), col=adjustcolor("deepskyblue3", alpha.f = 0.50), border = NA)
legend("topleft",  "513", bty="n",  cex=0.8)


plot(year_id, mean_validation_sim_substra_mean[1:length(year_id),"512"], pch=16, ylim=ylim,  col="black",  xlab="Year", ylab="Abundance Index", cex=0.5, type="o")
polygon(x=c(year_id, rev(year_id)), y=c(sim_validation_substrat_mean_upper[1:length(year_id),"512"], rev(sim_validation_substrat_mean_lower[1:length(year_id),"512"])), col=adjustcolor("deepskyblue3", alpha.f = 0.50), border = NA)
legend("topleft",  "512", bty="n",  cex=0.8)


plot(year_id, mean_validation_sim_substra_mean[1:length(year_id),"511"], pch=16, ylim=ylim,  col="black",  xlab="Year", ylab="Abundance Index", cex=0.5, type="o")
polygon(x=c(year_id, rev(year_id)), y=c(sim_validation_substrat_mean_upper[1:length(year_id),"511"], rev(sim_validation_substrat_mean_lower[1:length(year_id),"511"])), col=adjustcolor("deepskyblue3", alpha.f = 0.50), border = NA)
legend("topleft",  "511", bty="n",  cex=0.8)
dev.off()



#### Scaled predicted indices in one plot ####
real_vts_scale_prediction <- (validation_substrat_mean-min(validation_substrat_mean))/diff(range(validation_substrat_mean))
pop_scale_prediction <- (population_index[c("2015","2016"),]-min(population_index[c("2015","2016"),]))/diff(range(population_index[c("2015","2016"),]))
vts_sim_scale_prediction <- (mean_validation_sim_substra_mean-min(mean_validation_sim_substra_mean))/diff(range(mean_validation_sim_substra_mean))
vts_q_scale_prediction <- (mean_validation_sim_substra_mean_q-min(mean_validation_sim_substra_mean_q))/diff(range(mean_validation_sim_substra_mean_q))

ylim=range(real_vts_scale_prediction, pop_scale_prediction, vts_sim_scale_prediction, vts_q_scale_prediction)
jpeg(filename = "./plot/scaled_indices_one_plot_prediction.jpeg", width=175, height=60, units = "mm", res = 600)

par(mfrow=c(1,3), mar=c(4,4,1,1))
plot(2015:2016, real_vts_scale_prediction[,"513"], pch=1, ylim=ylim, col="black", type="o", lty=1, xlab="Year", ylab="Abundance Index", axes=F)
axis(side=1, at=c(2015, 2016), labels = c(2015, 2016))
axis(side=2)
box()
lines(2015:2016, pop_scale_prediction[,"513"], pch=2, type="o", col="purple", lty=2)
lines(2015:2016, vts_sim_scale_prediction[,"513"], pch=3, type="o", col="deepskyblue", lty=3)
lines(2015:2016, vts_q_scale_prediction[,"513"], pch=4, type="o", col="coral", lty=4)
legend("topleft",  c("Observed VTS index", "Simulated Population Index", "Simulated VTS available lobsters index", "Simulated VTS with q index"), bty="n",  pch=c(1, 2, 3, 4), lty=c(1,2,3,4), col=c("black", "purple", "deepskyblue", "coral"), title="513", cex=0.7)

plot(2015:2016, real_vts_scale_prediction[,"512"], pch=1, ylim=ylim, col="black", type="o", lty=1, xlab="Year", ylab="Abundance Index", axes=F)
axis(side=1, at=c(2015, 2016), labels = c(2015, 2016))
axis(side=2)
box()
lines(2015:2016, pop_scale_prediction[,"512"], pch=2, type="o", col="purple", lty=2)
lines(2015:2016, vts_sim_scale_prediction[,"512"], pch=3, type="o", col="deepskyblue", lty=3)
lines(2015:2016, vts_q_scale_prediction[,"512"], pch=4, type="o", col="coral", lty=4)
legend("topleft",  "512", bty="n",  cex=0.7)

plot(2015:2016, real_vts_scale_prediction[,"511"], pch=1, ylim=ylim, col="black", type="o", lty=1, xlab="Year", ylab="Abundance Index", axes=F)
axis(side=1, at=c(2015, 2016), labels = c(2015, 2016))
axis(side=2)
box()
lines(2015:2016, pop_scale_prediction[,"511"], pch=2, type="o", col="purple", lty=2)
lines(2015:2016, vts_sim_scale_prediction[,"511"], pch=3, type="o", col="deepskyblue", lty=3)
lines(2015:2016, vts_q_scale_prediction[,"511"], pch=4, type="o", col="coral", lty=4)
legend("topleft",  "511", bty="n",  cex=0.7)
dev.off()

#### correlation check:2015-2016 ####
cor(population_index[c("2015","2016"),], mean_validation_sim_substra_mean, method="pearson", use="pairwise.complete.obs")
cor(population_index[c("2015","2016"),], mean_validation_sim_substra_mean_q, method="pearson", use="pairwise.complete.obs")
cor(validation_substrat_mean, mean_validation_sim_substra_mean_q, method="pearson", use="pairwise.complete.obs")

population_index_scale <- matrix(scale(c(population_index["2015",], population_index["2016",])), nrow=2, byrow=T)
colnames(population_index_scale) <- c("511", "512", "513")
rownames(population_index_scale) <- c(2015:2016)

population_upper_scale <- matrix((c(population_upper["2015",], population_upper["2016",])-mean(c(population_index["2015",], population_index["2016",])))/sd(c(population_index["2015",], population_index["2016",])-mean(c(population_index["2015",], population_index["2016",]))), nrow=2, byrow=T)
colnames(population_upper_scale) <- c("511", "512", "513")
rownames(population_upper_scale) <- c(2015:2016)

population_lower_scale <- matrix((c(population_lower["2015",], population_lower["2016",])-mean(c(population_index["2015",], population_index["2016",])))/sd(c(population_index["2015",], population_index["2016",])-mean(c(population_index["2015",], population_index["2016",]))), nrow=2, byrow=T)
colnames(population_lower_scale) <- c("511", "512", "513")
rownames(population_lower_scale) <- c(2015:2016)

mean_validation_sim_substra_mean_scale <- matrix(scale(c(mean_validation_sim_substra_mean[1,], mean_validation_sim_substra_mean[2,])), nrow=2, byrow=T)
colnames(mean_validation_sim_substra_mean_scale) <- c("511", "512", "513")
rownames(mean_validation_sim_substra_mean_scale) <- c(2015:2016)

mean_validation_sim_substra_mean_upper_scale <- matrix((c(sim_validation_substrat_mean_upper["2015",], sim_validation_substrat_mean_upper["2016",])-mean(c(mean_validation_sim_substra_mean["2015",], mean_validation_sim_substra_mean["2016",])))/sd(c(mean_validation_sim_substra_mean["2015",], mean_validation_sim_substra_mean["2016",])-mean(c(mean_validation_sim_substra_mean["2015",], mean_validation_sim_substra_mean["2016",]))), nrow=2, byrow=T)
colnames(mean_validation_sim_substra_mean_upper_scale) <- c("511", "512", "513")
rownames(mean_validation_sim_substra_mean_upper_scale) <- c(2015:2016)

mean_validation_sim_substra_mean_lower_scale <- matrix((c(sim_validation_substrat_mean_lower["2015",], sim_validation_substrat_mean_lower["2016",])-mean(c(mean_validation_sim_substra_mean["2015",], mean_validation_sim_substra_mean["2016",])))/sd(c(mean_validation_sim_substra_mean["2015",], mean_validation_sim_substra_mean["2016",])-mean(c(mean_validation_sim_substra_mean["2015",], mean_validation_sim_substra_mean["2016",]))), nrow=2, byrow=T)
colnames(mean_validation_sim_substra_mean_lower_scale) <- c("511", "512", "513")
rownames(mean_validation_sim_substra_mean_lower_scale) <- c(2015:2016)

mean_validation_sim_substra_mean_q_scale <- matrix(scale(c(mean_validation_sim_substra_mean_q[1,], mean_validation_sim_substra_mean_q[2,])), nrow=2, byrow=T)
colnames(mean_validation_sim_substra_mean_q_scale) <- c("511", "512", "513")
rownames(mean_validation_sim_substra_mean_q_scale) <- c(2015:2016)

mean_validation_sim_substra_mean_q_upper_scale <- matrix((c(sim_validation_substrat_mean_upper_q["2015",], sim_validation_substrat_mean_upper_q["2016",])-mean(c(mean_validation_sim_substra_mean_q["2015",], mean_validation_sim_substra_mean_q["2016",])))/sd(c(mean_validation_sim_substra_mean_q["2015",], mean_validation_sim_substra_mean_q["2016",])-mean(c(mean_validation_sim_substra_mean_q["2015",], mean_validation_sim_substra_mean_q["2016",]))), nrow=2, byrow=T)
colnames(mean_validation_sim_substra_mean_q_upper_scale) <- c("511", "512", "513")
rownames(mean_validation_sim_substra_mean_q_upper_scale) <- c(2015:2016)

mean_validation_sim_substra_mean_q_lower_scale <- matrix((c(sim_validation_substrat_mean_lower_q["2015",], sim_validation_substrat_mean_lower_q["2016",])-mean(c(mean_validation_sim_substra_mean_q["2015",], mean_validation_sim_substra_mean_q["2016",])))/sd(c(mean_validation_sim_substra_mean_q["2015",], mean_validation_sim_substra_mean_q["2016",])-mean(c(mean_validation_sim_substra_mean_q["2015",], mean_validation_sim_substra_mean_q["2016",]))), nrow=2, byrow=T)
colnames(mean_validation_sim_substra_mean_q_lower_scale) <- c("511", "512", "513")
rownames(mean_validation_sim_substra_mean_q_lower_scale) <- c(2015:2016)

validation_substrat_mean_scale <- matrix(scale(c(validation_substrat_mean[1,], validation_substrat_mean[2,])), nrow=2, byrow=T)
colnames(validation_substrat_mean_scale) <- c("511", "512", "513")
rownames(validation_substrat_mean_scale) <- c(2015:2016)

validation_substrat_mean_upper_scale <- matrix((c(validation_substrat_mean["2015",]+sqrt(validation_substrat_var["2015",]), validation_substrat_mean["2016",]+sqrt(validation_substrat_var["2016",]))-mean(c(validation_substrat_mean["2015",], validation_substrat_mean["2016",])))/sd(c(validation_substrat_mean["2015",], validation_substrat_mean["2016",])-mean(c(validation_substrat_mean["2015",], validation_substrat_mean["2016",]))), nrow=2, byrow=T)
colnames(validation_substrat_mean_upper_scale) <- c("511", "512", "513")
rownames(validation_substrat_mean_upper_scale) <- c(2015:2016)

validation_substrat_mean_lower_scale <- matrix((c(validation_substrat_mean["2015",]-sqrt(validation_substrat_var["2015",]), validation_substrat_mean["2016",]-sqrt(validation_substrat_var["2016",]))-mean(c(validation_substrat_mean["2015",], validation_substrat_mean["2016",])))/sd(c(validation_substrat_mean["2015",], validation_substrat_mean["2016",])-mean(c(validation_substrat_mean["2015",], validation_substrat_mean["2016",]))), nrow=2, byrow=T)
colnames(validation_substrat_mean_lower_scale) <- c("511", "512", "513")
rownames(validation_substrat_mean_lower_scale) <- c(2015:2016)


jpeg(filename = "./plot/scale_polygon_validation_pop_vtssim_vtsq_vts_2015_2016.jpeg", width=175, height=65, units = "mm", res = 600)

ylim=range(population_upper_scale, mean_validation_sim_substra_mean_upper_scale, mean_validation_sim_substra_mean_q_upper_scale, validation_substrat_mean_upper_scale, population_lower_scale, mean_validation_sim_substra_mean_lower_scale, mean_validation_sim_substra_mean_q_lower_scale, validation_substrat_mean_lower_scale)

par(mfrow=c(1,3), mar=c(4,4,1,1))
year_id=2015:2016
plot(year_id, population_index_scale[,"513"], pch=16, col="black", xlab="Year", ylab="Scaled Abundance Index", ylim=ylim, axes=F, type="o", lty=2)
polygon(x=c(year_id, rev(year_id)), y=c(population_upper_scale[,"513"], rev(population_lower_scale[,"513"])), col=adjustcolor("black", alpha.f = 0.20), border = NA)
lines(year_id, mean_validation_sim_substra_mean_scale[,"513"], pch=16, col="deepskyblue3", type="o", lty=2)
polygon(x=c(year_id, rev(year_id)), y=c(mean_validation_sim_substra_mean_upper_scale[,"513"], rev(mean_validation_sim_substra_mean_lower_scale[,"513"])), col=adjustcolor("deepskyblue3", alpha.f = 0.20), border = NA)
lines(year_id, mean_validation_sim_substra_mean_q_scale[,"513"], pch=16, col="coral3", type="o", lty=2)
polygon(x=c(year_id, rev(year_id)), y=c(mean_validation_sim_substra_mean_q_upper_scale[,"513"], rev(mean_validation_sim_substra_mean_q_lower_scale[,"513"])), col=adjustcolor("coral3", alpha.f = 0.20), border = NA)
lines(year_id, validation_substrat_mean_scale[,"513"], pch=16, col="darkgreen", type="o", lty=2)
polygon(x=c(year_id, rev(year_id)), y=c(validation_substrat_mean_upper_scale[,"513"], rev(validation_substrat_mean_lower_scale[,"513"])), col=adjustcolor("darkgreen", alpha.f = 0.20), border = NA)
legend("topleft", "513", bty="n", cex=0.8)
axis(1, at=year_id, labels = year_id)
axis(2)
box()

plot(year_id, population_index_scale[,"512"], pch=16, col="black", xlab="Year", ylab="Scaled Abundance Index", ylim=ylim, axes=F, type="o", lty=2)
polygon(x=c(year_id, rev(year_id)), y=c(population_upper_scale[,"512"], rev(population_lower_scale[,"512"])), col=adjustcolor("black", alpha.f = 0.20), border = NA)
lines(year_id, mean_validation_sim_substra_mean_scale[,"512"], pch=16, col="deepskyblue3", type="o", lty=2)
polygon(x=c(year_id, rev(year_id)), y=c(mean_validation_sim_substra_mean_upper_scale[,"512"], rev(mean_validation_sim_substra_mean_lower_scale[,"512"])), col=adjustcolor("deepskyblue3", alpha.f = 0.20), border = NA)
lines(year_id, mean_validation_sim_substra_mean_q_scale[,"512"], pch=16, col="coral3", type="o", lty=2)
polygon(x=c(year_id, rev(year_id)), y=c(mean_validation_sim_substra_mean_q_upper_scale[,"512"], rev(mean_validation_sim_substra_mean_q_lower_scale[,"512"])), col=adjustcolor("coral3", alpha.f = 0.20), border = NA)
lines(year_id, validation_substrat_mean_scale[,"512"], pch=16, col="darkgreen", type="o", lty=2)
polygon(x=c(year_id, rev(year_id)), y=c(validation_substrat_mean_upper_scale[,"512"], rev(validation_substrat_mean_lower_scale[,"512"])), col=adjustcolor("darkgreen", alpha.f = 0.20), border = NA)
legend("topleft", "512", bty="n", cex=0.8)
legend("bottomleft", legend=c("Sim Pop of Statistical Area", "Sim Pop at VTS station", "Sim Samples at VTS Station", "Real Samples at VTS Station"), lty=c(2,2,2,2), pch=c(16, 16, 16, 16), col=c("black", "deepskyblue3", "coral3", "darkgreen"), bty="n", cex=0.8)
axis(1, at=year_id, labels = year_id)
axis(2)
box()

plot(year_id, population_index_scale[,"511"], pch=16, col="black", xlab="Year", ylab="Scaled Abundance Index", ylim=ylim, axes=F, type="o", lty=2)
polygon(x=c(year_id, rev(year_id)), y=c(population_upper_scale[,"511"], rev(population_lower_scale[,"511"])), col=adjustcolor("black", alpha.f = 0.20), border = NA)
lines(year_id, mean_validation_sim_substra_mean_scale[,"511"], pch=16, col="deepskyblue3", type="o", lty=2)
polygon(x=c(year_id, rev(year_id)), y=c(mean_validation_sim_substra_mean_upper_scale[,"511"], rev(mean_validation_sim_substra_mean_lower_scale[,"511"])), col=adjustcolor("deepskyblue3", alpha.f = 0.20), border = NA)
lines(year_id, mean_validation_sim_substra_mean_q_scale[,"511"], pch=16, col="coral3", type="o", lty=2)
polygon(x=c(year_id, rev(year_id)), y=c(mean_validation_sim_substra_mean_q_upper_scale[,"511"], rev(mean_validation_sim_substra_mean_q_lower_scale[,"511"])), col=adjustcolor("coral3", alpha.f = 0.20), border = NA)
lines(year_id, validation_substrat_mean_scale[,"511"], pch=16, col="darkgreen", type="o", lty=2)
polygon(x=c(year_id, rev(year_id)), y=c(validation_substrat_mean_upper_scale[,"511"], rev(validation_substrat_mean_lower_scale[,"511"])), col=adjustcolor("darkgreen", alpha.f = 0.20), border = NA)
legend("topleft", "511", bty="n", cex=0.8)
axis(1, at=year_id, labels = year_id)
axis(2)
box()
dev.off()
