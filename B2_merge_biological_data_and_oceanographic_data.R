##############################################################################
# Program for coupling biological data with oceanographic data 
#-----------------------------------------------------------------------------
# INPUT :
# - biological dataset (either acoustic or catches) !!! HAVE TO BE SORTED !!!
# - Monthly 9°C isotherme climatology from WOA
# - Monthly 2ml/L dissolved oxygen climatology from WOA
# 
# OUTPUT :
# - biological dataset + oceanographic parameters
#
##############################################################################

#loading libraries
source('C:/Workspace_R/_define_parameters.R')
library(ncdf)
library(sp)

# monthly climatology isotherme 9 degrees
file_isotherme_9 <- open.ncdf("E:/_data/CLS_Data/extract_WOA_isotherme.nc", verbose=TRUE)
lon_isotherme_9 <- get.var.ncdf(file_isotherme_9, "longitude")          # coordinate variable
lat_isotherme_9 <- get.var.ncdf(file_isotherme_9, "latitude")          # coordinate variable
isotherme_9 <- get.var.ncdf(file_isotherme_9, "isotherme_9")   # variable
### transformation longitude
lon_isotherme_9 <- lon_isotherme_9 - 360;

# monthly climatology isoO2 2ml/l
file_isoO2 <- open.ncdf("E:/_data/CLS_Data/extract_WOA_isoO2.nc", verbose=TRUE)
lon_isoO2 <- get.var.ncdf(file_isoO2, "longitude")          # coordinate variable
lat_isoO2 <- get.var.ncdf(file_isoO2, "latitude")          # coordinate variable
isoO2_2 <- get.var.ncdf(file_isoO2, "isoO2_2")   # variable
### transformation longitude
lon_isoO2 <- lon_isoO2 - 360;

#reading biological dataset
catches <- read.csv(filename, header=TRUE, sep=";", dec=".")
catches$Longitude <- as.numeric(catches$Longitude)
catches$Latitude <- as.numeric(catches$Latitude)
catches$Month <- as.numeric(catches$Month)
catches$depth_isotherme <- NA * dim(catches)[1]
catches$depth_isoO2_2 <- NA * dim(catches)[1]
catches$Key <- paste(catches$Month, catches$Longitude, catches$Latitude, sep="")
 
#for each month
for (ind_month in 1:12) {
 
   #isotherme 9°C dataframe transposition
   isotherme_9_month <- isotherme_9[,,ind_month]
   data_frame_isotherme_9 <- as.data.frame(matrix(NA,dim(isotherme_9)[1]*dim(isotherme_9)[2],3))
   colnames(data_frame_isotherme_9) <- c("Longitude","Latitude","depth_iso_9")
   data_frame_isotherme_9$Longitude <- rep(lon_isotherme_9,dim(isotherme_9_month)[2])
   data_frame_isotherme_9$Latitude <- rep(lat_isotherme_9, each = dim(isotherme_9_month)[1],1)
   data_frame_isotherme_9$depth_iso_9 <- as.vector(isotherme_9_month)
   xy.isotherme_9 <- cbind(data_frame_isotherme_9$Longitude, data_frame_isotherme_9$Latitude)
   
   #isoO2 2ml dataframe transposition
   isoO2_2_month <- isoO2_2[,,ind_month]
   data_frame_isoO2_2 <- as.data.frame(matrix(NA,dim(isoO2_2)[1]*dim(isoO2_2)[2],3))
   colnames(data_frame_isoO2_2) <- c("Longitude","Latitude","depth_isoO2_2")
   data_frame_isoO2_2$Longitude <- rep(lon_isoO2,dim(isoO2_2_month)[2])
   data_frame_isoO2_2$Latitude <- rep(lat_isoO2, each = dim(isoO2_2_month)[1],1)
   data_frame_isoO2_2$depth_isoO2_2 <- as.vector(isoO2_2_month)
   xy.isoO2_2 <- cbind(data_frame_isoO2_2$Longitude, data_frame_isoO2_2$Latitude)   
   
   #CJM monthly catches selection
   if (dataset_id == "1") {
      catches_CJM_month <- subset(catches, CJM > 0 & Anch == 0 & Sard == 0 & Month == ind_month) 
   } else {
      catches_CJM_month <- subset(catches, CJM > 0 & Month == ind_month)
   }
   catches_CJM_month$Key <- paste(catches_CJM_month$Month, catches_CJM_month$Longitude, catches_CJM_month$Latitude, sep="")   
   pts_catches <- matrix(c(catches_CJM_month$Longitude,catches_CJM_month$Latitude), ncol = 2)
   
   if (dim(catches_CJM_month)[1]>0){
     
      catches_CJM_month$depth_isotherme <- NA * dim(catches_CJM_month)[1]
      catches_CJM_month$depth_isoO2_2 <- NA * dim(catches_CJM_month)[1]
      
      #for each catch
      for (i in 1:nrow(pts_catches)) {
         
          indice_matrice <- which(catches$Key == catches_CJM_month[i,]$Key)
       
          #find depth isotherme 9 
          indice_isotherme <- which(spDistsN1(xy.isotherme_9, pts_catches[i,], longlat = TRUE) == min(spDistsN1(xy.isotherme_9, pts_catches[i,], longlat = TRUE)))  
          catches_CJM_month[i,]$depth_isotherme <- data_frame_isotherme_9[min(indice_isotherme),]$depth_iso_9          
          catches[indice_matrice,]$depth_isotherme <- catches_CJM_month[i,]$depth_isotherme
          
          #find depth isoO2 2mL
          indice_isoO2_2 <- which(spDistsN1(xy.isoO2_2, pts_catches[i,], longlat = TRUE) == min(spDistsN1(xy.isoO2_2, pts_catches[i,], longlat = TRUE)))  
          catches_CJM_month[i,]$depth_isoO2_2 <- data_frame_isoO2_2[min(indice_isoO2_2),]$depth_isoO2_2
          catches[indice_matrice,]$depth_isoO2_2 <- catches_CJM_month[i,]$depth_isoO2_2
      }    
   } 
  
}#end month

print(summary(catches$depth_isotherme))
print(summary(catches$depth_isoO2_2))

summary(catches)
tapply(catches$depth_isotherme, list(catches$Month), min, na.rm=T)
tapply(catches$depth_isotherme, list(catches$Month), mean, na.rm=T)
tapply(catches$depth_isotherme, list(catches$Month), median, na.rm=T)
tapply(catches$depth_isotherme, list(catches$Month), max, na.rm=T)

tapply(catches$depth_isoO2_2, list(catches$Month), min, na.rm=T)
tapply(catches$depth_isoO2_2, list(catches$Month), mean, na.rm=T)
tapply(catches$depth_isoO2_2, list(catches$Month), median, na.rm=T)
tapply(catches$depth_isoO2_2, list(catches$Month), max, na.rm=T)

##################### write matrix with all fields
catches$Key <- NULL
filenameout <- paste(filename, 'with_oceanographic_data.csv', sep="")
write.csv(catches, file=filenameout, row.names=FALSE, quote=FALSE)


