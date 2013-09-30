
source('C:/Workspace_R/_define_parameters.R')

library(ncdf)
library(gstat)
library(raster)
library(maptools)
data(wrld_simpl)

# Reading climato
file_climato_SST <- open.ncdf(paste(directory_data,'/Satellite/SST/9km/w1d_modis_sst2_9km_1m_20030101_20121231.qual1.avg.float.nc',sep=""))     
lon <- get.var.ncdf(file_climato_SST, "lon")   # coordinate variable
lat <- get.var.ncdf(file_climato_SST, "lat")   # coordinate variable
time <- get.var.ncdf(file_climato_SST, "time")
sst <- get.var.ncdf(file_climato_SST,"sst",start=c(1,1,1),count=c(dim(lon)[[1]],dim(lat)[[1]],dim(time)[[1]]))
#[-180 180] transformation to [-360 0]
indice_pivot <- length(lon)/2
sst2 <- sst[1:indice_pivot,,]
sst3 <- sst[(indice_pivot+1):dim(lon)[1],,]
sst[1:indice_pivot,,] <- sst3
sst[(indice_pivot+1):dim(lon)[1],,] <- sst2
lon[lon>0] <- lon[lon>0] - 360

## read chla climatology
file_climato_chla <- open.ncdf(paste(directory_data,'/Satellite/CHL/9km/w1d_modis_chla_9km_1m_20030101_20121231.R2012.avg.float.nc',sep=""))     
lon <- get.var.ncdf(file_climato_chla, "lon")   # coordinate variable
lat <- get.var.ncdf(file_climato_chla, "lat")   # coordinate variable
time <- get.var.ncdf(file_climato_chla, "time")
chla <- get.var.ncdf(file_climato_chla,"chla",start=c(1,1,1),count=c(dim(lon)[[1]],dim(lat)[[1]],dim(time)[[1]]))
#[-180 180] transformation to [-360 0]
chla2 <- chla[1:indice_pivot,,]
chla3 <- chla[(indice_pivot+1):dim(lon)[1],,]
chla[1:indice_pivot,,] <- chla3
chla[(indice_pivot+1):dim(lon)[1],,] <- chla2
lon[lon>0] <- lon[lon>0] - 360

# Load the data:

### reading main dataset
filename_global_dataset <- paste(directory_data,'data_with_all_environmental_parameters.csv',sep="")
dataset <- read.csv2(filename_global_dataset, sep=",", dec=".",header=T)
dataset <- subset(dataset, Year > 2002)

################ Generate predictions on current month data  
#glm prediction (relevant predictors)
formula = "presence_absence~SST+log(CHLA)+Latitude+Longitude"
glm.prediction <- glm(formula, dataset,family=binomial)
#*depth_isotherme*depth_isoO2_2*prod_prim, family=binomial)
# + bathy + position lat/lon ?
print(summary(glm.prediction))
coordinates(dataset) = ~Longitude+Latitude

#predictions by month
for (ind_month in min(dataset$Month):max(dataset$Month)) {
  
  dataset_month <- subset(dataset, Month == ind_month)
   
  ##### NEW ENVIRONMENTAL DATA 2003-2012
  
  #raster sst month
  m <- t(sst[,,ind_month])
  m <- m[nrow(m):1,]
  raster_sst_month <- raster(m,xmn = -360,xmx = 0, ymn = -90, ymx = 90,crs="+proj=longlat +datum=WGS84")
  
  #raster chla month
  m <- t(chla[,,ind_month])
  m <- m[nrow(m):1,]
  raster_chla_month <- raster(m,xmn = -360,xmx = 0, ymn = -90, ymx = 90,crs="+proj=longlat +datum=WGS84")
  
  ##raster coordinates month
  latitude <- seq(-90,90,1)
  longitude <- seq(-359.5,-0.5,1)
  
  latitude_matrix = matrix(NA, 180,360)
  for (i in 1:nrow(latitude_matrix)){
    latitude_matrix[i,] <- -latitude[i]
  }
  raster_latitude <- raster(latitude_matrix,xmn = -360,xmx = 0, ymn = -90, ymx = 90,crs="+proj=longlat +datum=WGS84")
    
  test_values_b = rep(longitude, each=length(latitude))
  longitude_matrix = matrix(test_values_b, 180,360)
  raster_longitude <- raster(longitude_matrix,xmn = -360,xmx = 0, ymn = -90, ymx = 90,crs="+proj=longlat +datum=WGS84")
  
  
  predictors_month <- stack(raster_sst_month,raster_chla_month,raster_latitude,raster_longitude) 
  names(predictors_month) = c("SST","CHLA","Latitude","Longitude")
  
  p <- predict(predictors_month,glm.prediction, type="response")  
   
  x11()
  plot(p, main = paste("Formula : ", formula, "\nMonth : ", ind_month, sep= ""),
       axes=TRUE, xlim=c(lon_min,lon_max), ylim=c(lat_min,lat_max))
  plot(wrld_simpl, add=TRUE)
  points(dataset_month, col='red',pch='.',cex=5)
  contour(predictors_month,add=TRUE)

}

