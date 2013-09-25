##############################################################################
# Program for coupling biological data with oceanographic data 
#-----------------------------------------------------------------------------
# INPUT :
# - biological dataset (either acoustic or catches) !!! HAVE TO BE SORTED !!!
# - Monthly 9°C isotherme climatology from WOA 2009 1 degree
# - Monthly 2ml/L dissolved oxygen climatology from WOA 2009 1 degree
# 
# OUTPUT :
# - biological dataset + oceanographic parameters
#
##############################################################################
rm(list=ls())
#loading libraries
source('C:/Workspace_R/_define_parameters.R')
library(ncdf)
library(sp)

if (data_type == "acoustic"){
  directory_dataset <- paste(directory_data, 'acoustic_scientific/', fleet, '/', sep="")
  filename_dataset <- paste(directory_dataset, 'acoustic_data_',year_start,'_',year_end,'.csv', sep="")
}
if (data_type == "acoustic_pro"){
  directory_dataset <- paste(directory_data, 'acoustic_fishing_vessels/', fleet, '/', sep="")
  filename_dataset <- paste(directory_dataset, 'acoustic_data_',year_start,'_',year_end,'.csv', sep="")
}
if (data_type == "catches"){
  directory_dataset = paste(directory_data, 'catches/', fleet, '/', sep="")
  filename_dataset <- paste(directory_dataset, 'catches_',year_start,'_',year_end,'.csv', sep="")  
}

# monthly climatology isotherme 9 degrees
file_isotherme_9 <- open.ncdf("E:/_data/Climato/WOA_2009/monthly/extract_WOA_isotherme.nc", verbose=TRUE)
lon_isotherme_9 <- get.var.ncdf(file_isotherme_9, "longitude")          # coordinate variable
lat_isotherme_9 <- get.var.ncdf(file_isotherme_9, "latitude")          # coordinate variable
isotherme_9 <- get.var.ncdf(file_isotherme_9, "isotherme_9")   # variable
### transformation longitude
lon_isotherme_9 <- lon_isotherme_9 - 360;

# monthly climatology isoO2 2ml/l
file_isoO2 <- open.ncdf("E:/_data/Climato/WOA_2009/monthly/extract_WOA_isoO2.nc", verbose=TRUE)
lon_isoO2 <- get.var.ncdf(file_isoO2, "longitude")          # coordinate variable
lat_isoO2 <- get.var.ncdf(file_isoO2, "latitude")          # coordinate variable
isoO2_2 <- get.var.ncdf(file_isoO2, "isoO2_2")   # variable
### transformation longitude
lon_isoO2 <- lon_isoO2 - 360;

#reading biological dataset
catches <- read.csv(filename, header=TRUE, sep=";", dec=".")
catches <- subset(catches, !is.na(Longitude)) #delete catches without georeference
catches$Longitude <- as.numeric(catches$Longitude)
catches$Latitude <- as.numeric(catches$Latitude)
catches$Month <- as.numeric(catches$Month)

catches_new <- as.data.frame(matrix(NA, nrow(catches),ncol(catches)))
colnames(catches_new) <- colnames(catches)
catches_new$depth_isotherme <- NA * dim(catches_new)[1]
catches_new$depth_isoO2_2 <- NA * dim(catches_new)[1]
 
indice_matrice <- 1 

#for each month
for (ind_month in 1:12) {
  
  #isotherme 9°C dataframe transposition
  isotherme_9_month <- isotherme_9[,,ind_month]
  
  #isoO2 2ml dataframe transposition
  isoO2_2_month <- isoO2_2[,,ind_month]
 
  #raster transformation
  m <- t(isotherme_9_month)
  m <- m[nrow(m):1,]
  raster_isotherme_9_month <- raster(m,xmn = -220.5, xmx = -69.5, ymn = -60.5, ymx = 5.5,crs="+proj=longlat +datum=WGS84")
  #plot(raster_isotherme_9_month)
  
  m <- t(isoO2_2_month)
  m <- m[nrow(m):1,]
  raster_isoO2_2_month <- raster(m,xmn = -220.5, xmx = -69.5, ymn = -60.5, ymx = 5.5,crs="+proj=longlat +datum=WGS84")
  #plot(raster_isoO2_2_month)
  
  catches_month <- subset(catches, Month == ind_month)
      
  if (dim(catches_month)[1]>0){
    
    catches_month$depth_isotherme <- NA * dim(catches_month)[1]
    catches_month$depth_isoO2_2 <- NA * dim(catches_month)[1]
    
    lon_lat_dataset <- data.frame(catches_month$Longitude,catches_month$Latitude)
    colnames(lon_lat_dataset) <- c("Longitude","Latitude")
    coordinates(lon_lat_dataset) <- ~Longitude+Latitude
      
    #extracting raster values
    catches_month$depth_isotherme <- extract(raster_isotherme_9_month,lon_lat_dataset)
    catches_month$depth_isoO2_2 <- extract(raster_isoO2_2_month,lon_lat_dataset)
        
    catches_new[indice_matrice:(indice_matrice+nrow(catches_month)-1),] <- catches_month   
    indice_matrice <- indice_matrice + nrow(catches_month)
    }       
}#end month

print(summary(catches_new$depth_isotherme))

summary(catches_new)
tapply(catches_new$depth_isotherme, list(catches_new$Month), min, na.rm=T)
tapply(catches_new$depth_isotherme, list(catches_new$Month), mean, na.rm=T)
tapply(catches_new$depth_isotherme, list(catches_new$Month), median, na.rm=T)
tapply(catches_new$depth_isotherme, list(catches_new$Month), max, na.rm=T)

if (length(unique(catches_new$depth_isoO2_2))>12) {
  print(summary(catches_new$depth_isoO2_2))
  tapply(catches_new$depth_isoO2_2, list(catches_new$Month), min, na.rm=T)
  tapply(catches_new$depth_isoO2_2, list(catches_new$Month), mean, na.rm=T)
  tapply(catches_new$depth_isoO2_2, list(catches_new$Month), median, na.rm=T)
  tapply(catches_new$depth_isoO2_2, list(catches_new$Month), max, na.rm=T)
}

##################### write matrix with all fields

if (data_type == "acoustic"){
  filenameout <- paste(directory_dataset,"With_Oceanographic_Data/acoustic_scientific_",fleet,"_",year_start,"_",year_end,"_with_oceanographic_data.csv",sep="")
  #dataset_satellite$Transect <- NULL;
}
if (data_type == "catches"){
  filenameout <- paste(directory_dataset,"With_Oceanographic_Data/catches_",year_start,"_",year_end,"_with_oceanographic_data.csv",sep="")
}

write.csv(catches_new, file=filenameout, row.names=FALSE, quote=FALSE)

