source('C:/Workspace_R/_define_parameters.R')

fleet <- 'Chile'
filename <- paste(directory_data, 'Catches/', fleet,'/mean_monthly_point.csv',sep="")
mean_monthly_point_Chile <- read.csv(filename)

fleet <- 'Netherlands'
filename <- paste(directory_data, 'Catches/', fleet,'/mean_monthly_point.csv',sep="")
mean_monthly_point_Netherlands <- read.csv(filename)

fleet <- 'Peru'
filename <- paste(directory_data, 'Catches/', fleet,'/mean_monthly_point.csv',sep="")
mean_monthly_point_Peru <- read.csv(filename)

x11()
plot(mean_monthly_point_Netherlands$Longitude, mean_monthly_point_Netherlands$Latitude, col="red", 
     type='l',xlim=c(-100,-70),ylim=c(-50,0), xlab="Longitude",ylab="Latitude", 
     main = "Mean monthly catches position by fishing fleet")
text(mean_monthly_point_Netherlands$Longitude, mean_monthly_point_Netherlands$Latitude, labels = mean_monthly_point_Netherlands$Month, cex= 0.7, offset = 10)
points(mean_monthly_point_Chile$Longitude, mean_monthly_point_Chile$Latitude, col="blue", type='l')
text(mean_monthly_point_Chile$Longitude, mean_monthly_point_Chile$Latitude, labels = mean_monthly_point_Chile$Month, cex= 0.7, offset = 10)
points(mean_monthly_point_Peru$Longitude, mean_monthly_point_Peru$Latitude, col="green", type='l')
text(mean_monthly_point_Peru$Longitude, mean_monthly_point_Peru$Latitude, labels = mean_monthly_point_Peru$Month, cex= 0.7, offset = 10)
lines(filename_shoreline)
legend("topright",c("Netherlands catches","Chilean catches","Peruvian catches"),col=c("red","blue","green"),lwd=2)
library(maps)
library(mapdata)
library(maptools)
ZEE <- readShapeLines("G:/_sauvegarde_Jeremie/_data/ZEE/World_EEZ_v7_2012_HR.shp")
lines(ZEE,xlim=c(-100,-70),ylim=c(-50,0),col="black")

