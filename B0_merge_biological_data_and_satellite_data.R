##############################################################################
# Program for coupling biological data with satellite environmental data 
#-----------------------------------------------------------------------------
# INPUT :
# - biological dataset (either acoustic or catches) !!! HAVE TO BE SORTED !!!
# - SeaWiFS dataset
# - MODIS SST dataset
# - MODIS CHL-a dataset
# - AVHRR dataset
#
# OUTPUT :
# - biological dataset + environmental parameters + bathymetry ETOPO
#
# note : daily duplicated rows are deleted
#
##############################################################################

source('C:/Workspace_R/_define_parameters.R')
library(doBy)

######################## dataset data coupled with environmental data  ###################

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
dataset <- read.csv(filename_dataset, header=TRUE, sep=",", dec=".")
dataset_sorted <- orderBy(~Year+Month+Day+Longitude+Latitude, data=dataset)

# dataset$Longitude = as.numeric(dataset$Longitude)
# dataset$Latitude = as.numeric(dataset$Latitude)
# dataset$Year = as.numeric(dataset$Year)
# dataset$Month = as.numeric(dataset$Month)
# dataset$Day = as.numeric(dataset$Day)
# dataset$CJM = as.numeric(dataset$CJM)
# if (dataset_id == "1") {
#   dataset$SST = as.numeric(dataset$SST)
#   dataset$Anch = as.numeric(dataset$Anch)
#   dataset$Sard = as.numeric(dataset$Sard)
#   dataset$Cab = as.numeric(dataset$Cab)
# }

#dataset$Key <- paste(dataset$Year,dataset$Month, dataset$Day, round(dataset$Latitude,3), round(dataset$Longitude,3))

#deleting records without date
#dataset <- subset(dataset, !is.na(Longitude) & !is.na(Latitude) & !is.na(Year) & !is.na(Month) & !is.na(Day))

########## reading satellites data extracted

####### adding seawifs data
sensor <- 'seawifs'
parameter <- 'chla'
temporal_resolution <- '8day'
spatial_resolution <- '9km'
sensor_parameter <- paste(sensor,'.',parameter, sep="")
directory_dataset_satellite <- paste(directory_dataset, 'With_Satellite_Data/',temporal_resolution,'/',sep="")
file_satellite_parameter <- dir(directory_dataset_satellite,paste('*',sensor_parameter,'*',sep=""))
if (length(file_satellite_parameter)>0) { 
  satellite_parameters <- read.csv(paste(directory_dataset_satellite,file_satellite_parameter,sep=""), sep= "", header=FALSE, na.strings = c("NaN","********"),skip=12)
  colnames(satellite_parameters) =  c("id_record","year","month","day","latitude","longitude","bathy","seawifs.avg_r0","seawifs.avg_r1","seawifs.avg_r2","seawifs.min_r2","seawifs.max_r2","seawifs.std_r2", "seawifs.mode_r2", "data_file")
  satellite_parameters$id_record <- NULL
  satellite_parameters$data_file <- NULL
  #lon/lat : 4 decimal numbers
  #satellite_parameters$Key <- paste(satellite_parameters$year,satellite_parameters$month, satellite_parameters$day, round(satellite_parameters$latitude,3), round(satellite_parameters$longitude,3))
  #deleting duplicated rows
  #satellite_parameters_b <- unique(satellite_parameters)
  
  #merging process
  dataset_seawifs <- merge(dataset,satellite_parameters_b, all.x = TRUE)
  dataset_seawifs$year <- NULL
  dataset_seawifs$month <- NULL
  dataset_seawifs$day <- NULL
  dataset_seawifs$longitude <- NULL
  dataset_seawifs$latitude <- NULL
  #deleting duplicated rows
  dataset_seawifs_b <- unique(dataset_seawifs)
  
  str(dataset_seawifs)
  summary(dataset_seawifs)
}



####### adding modis chl data
if (year_end > 2000) {
temporal_resolution <- '1day'
if (dataset_id == "4" | dataset_id == "9" ){
   temporal_resolution <- '8day' 
}
spatial_resolution <- '4km'
sensor = 'modis'
parameter = 'chla'
sensor_parameter <- paste(sensor,'.',parameter, sep="")
directory_dataset_satellite <- paste(directory_dataset, 'With_Satellite_Data/',temporal_resolution,'/',sep="")
file_satellite_parameter <- dir(directory_dataset_satellite,paste('*',sensor_parameter,'*',sep=""))
if (length(file_satellite_parameter)>0) { 
  satellite_parameters <- read.csv(paste(directory_dataset_satellite,file_satellite_parameter,sep=""), sep= "", header=FALSE, na.strings = c("NaN","********"), skip=12)
  colnames(satellite_parameters) <-  c("id_record","year","month","day","latitude","longitude","bathy","modis.chla.nb_r0","modis.chla.avg_r0","modis.chla.nb_r1","modis.chla.avg_r1","modis.chla.nb_r2","modis.chla.avg_r2","modis.chla.nb_r3","modis.chla.avg_r3","modis.chla.min_r2","modis.chla.max_r2","modis.chla.std_r2", "modis.chla.mode_r2", "data_file")
  #colnames(satellite_parameters) <-  c("id_record","year","month","day","latitude","longitude","bathy","modis.chla.avg_r0","modis.chla.avg_r1","modis.chla.avg_r2","modis.chla.min_r2","modis.chla.max_r2","modis.chla.std_r2", "modis.chla.mode_r2", "data_file")  
  satellite_parameters$id_record <- NULL
  satellite_parameters$data_file <- NULL
  satellite_parameters_sorted <- orderBy(~year+month+day+longitude+latitude, data=satellite_parameters)
  #merging process
  dataset_modis_chl <-  data.frame(satellite_parameters_sorted, dataset$CJM)
  colnames(dataset_modis_chl) <- c(colnames(satellite_parameters_sorted),"CJM")
}

####### adding modis sst data
parameter <- 'sst'
sensor_parameter <- paste(sensor,'.',parameter, sep="")
directory_dataset_satellite <- paste(directory_dataset, 'With_Satellite_Data/',temporal_resolution,'/',sep="")
file_satellite_parameter <- dir(directory_dataset_satellite,paste('*',sensor_parameter,'*',sep=""))
if (length(file_satellite_parameter)>0) {
  satellite_parameters <- read.csv(paste(directory_dataset_satellite,file_satellite_parameter,sep=""), sep= "", header=FALSE, na.strings = c("NaN","********"), skip=12)
  colnames(satellite_parameters) <-  c("id_record","year","month","day","latitude","longitude","bathy","modis.sst.nb_r0","modis.sst.avg_r0","modis.sst.nb_r1","modis.sst.avg_r1","modis.sst.nb_r2","modis.sst.avg_r2","modis.sst.nb_r3","modis.sst.avg_r3","modis.sst.min_r2","modis.sst.max_r2","modis.sst.std_r2", "modis.sst.mode_r2", "data_file")
  #colnames(satellite_parameters) <-  c("id_record","year","month","day","latitude","longitude","bathy","modis.sst.avg_r0","modis.sst.avg_r1","modis.sst.avg_r2","modis.sst.min_r2","modis.sst.max_r2","modis.sst.std_r2", "modis.sst.mode_r2", "data_file")
  satellite_parameters$id_record <- NULL
  satellite_parameters$data_file <- NULL
  satellite_parameters_sorted <- orderBy(~year+month+day+longitude+latitude, data=satellite_parameters)
  #merging process
  dataset_modis_sst <-  data.frame(satellite_parameters_sorted, dataset$CJM)
  colnames(dataset_modis_sst) <- c(colnames(satellite_parameters_sorted),"CJM")
 
}
str(dataset_modis_chl)
summary(dataset_modis_chl)
str(dataset_modis_sst)
summary(dataset_modis_sst)
}

####### adding avhrr sst data
sensor <- 'avhrr'
parameter <- 'sst'
temporal_resolution <- '1day'
spatial_resolution <- '4km'
sensor_parameter <- paste(sensor,'.',parameter, sep="")
directory_dataset_satellite <- paste(directory_dataset, 'With_Satellite_Data/',temporal_resolution,'/',sep="")
file_satellite_parameter <- dir(directory_dataset_satellite,paste('*',sensor_parameter,'*',sep=""))
if (length(file_satellite_parameter)>0) {
  satellite_parameters <- read.csv(paste(directory_dataset_satellite,file_satellite_parameter,sep=""), sep= "", header=FALSE, na.strings = c("NaN","********","no data available for this date"), skip=10)
  colnames(satellite_parameters) <-  c("id_record","year","month","day","latitude","longitude","bathy","avhrr.avg_r0","avhrr.avg_r1","avhrr.avg_r2","avhrr.min_r2","avhrr.max_r2","avhrr.std_r2", "avhrr.mode_r2", "data_file")
  satellite_parameters$id_record <- NULL
  satellite_parameters$data_file <- NULL
  
  str(dataset_avhrr)
  summary(dataset_avhrr)
}




##########################
if (dataset_id == "1"){
  dataset <- orderBy(~ Key, dataset)
  dataset_seawifs_b <- orderBy(~ Key, dataset_seawifs_b)
  dataset_modis_chl_b <- orderBy(~ Key, dataset_modis_chl_b)
  dataset_modis_sst_b <- orderBy(~ Key, dataset_modis_sst_b)
  dataset_avhrr_b <- orderBy(~ Key, dataset_avhrr_b)
  dataset_satellite <- cbind(dataset_seawifs_b, 
                             dataset_modis_chl_b$modis.chla.avg_r0, dataset_modis_chl_b$modis.chla.avg_r1, dataset_modis_chl_b$modis.chla.avg_r2, dataset_modis_chl_b$modis.chla.min_r2, dataset_modis_chl_b$modis.chla.max_r2, dataset_modis_chl_b$modis.chla.std_r2, dataset_modis_chl_b$modis.chla.mode_r2,
                             dataset_modis_sst_b$modis.sst.avg_r0, dataset_modis_sst_b$modis.sst.avg_r1, dataset_modis_sst_b$modis.sst.avg_r2, dataset_modis_sst_b$modis.sst.min_r2, dataset_modis_sst_b$modis.sst.max_r2, dataset_modis_sst_b$modis.sst.std_r2, dataset_modis_sst_b$modis.sst.mode_r2, 
                             dataset_avhrr_b$avhrr.avg_r0, dataset_avhrr_b$avhrr.avg_r1, dataset_avhrr_b$avhrr.avg_r2, dataset_avhrr_b$avhrr.min_r2, dataset_avhrr_b$avhrr.max_r2, dataset_avhrr_b$avhrr.std_r2, dataset_avhrr_b$avhrr.mode_r2)
  colnames(dataset_satellite) =  c(colnames(dataset_seawifs_b),"modis.chla.avg_r0","modis.chla.avg_r1","modis.chla.avg_r2","modis.chla.min_r2","modis.chla.max_r2","modis.chla.std_r2","modis.chla.mode_r2",
                                   "modis.sst.avg_r0","modis.sst.avg_r1","modis.sst.avg_r2","modis.sst.min_r2","modis.sst.max_r2","modis.sst.std_r2","modis.sst.mode_r2",
                                   "avhrr.avg_r0","avhrr.avg_r1","avhrr.avg_r2","avhrr.min_r2","avhrr.max_r2","avhrr.std_r2","avhrr.mode_r2")
}

if (dataset_id == "3"){
  dataset_satellite <- cbind(dataset_seawifs_b, 
                             #dataset_modis_chl_b$modis.chla.avg_r0, dataset_modis_chl_b$modis.chla.avg_r1, dataset_modis_chl_b$modis.chla.avg_r2, dataset_modis_chl_b$modis.chla.min_r2, dataset_modis_chl_b$modis.chla.max_r2, dataset_modis_chl_b$modis.chla.std_r2, dataset_modis_chl_b$modis.chla.mode_r2,
                             #dataset_modis_sst_b$modis.sst.avg_r0, dataset_modis_sst_b$modis.sst.avg_r1, dataset_modis_sst_b$modis.sst.avg_r2, dataset_modis_sst_b$modis.sst.min_r2, dataset_modis_sst_b$modis.sst.max_r2, dataset_modis_sst_b$modis.sst.std_r2, dataset_modis_sst_b$modis.sst.mode_r2, 
                             dataset_avhrr_b$avhrr.avg_r0, dataset_avhrr_b$avhrr.avg_r1, dataset_avhrr_b$avhrr.avg_r2, dataset_avhrr_b$avhrr.min_r2, dataset_avhrr_b$avhrr.max_r2, dataset_avhrr_b$avhrr.std_r2, dataset_avhrr_b$avhrr.mode_r2)
  colnames(dataset_satellite) =  c(colnames(dataset_seawifs_b),
                                   "avhrr.avg_r0","avhrr.avg_r1","avhrr.avg_r2","avhrr.min_r2","avhrr.max_r2","avhrr.std_r2","avhrr.mode_r2")
}

#modis + avhrr
if (dataset_id == "4"){
  #sort data.frame before cbind
  
  dataset_satellite <- cbind(dataset, 
                             dataset_modis_chl_b$bathy,
                             dataset_modis_chl_b$modis.chla.avg_r0, dataset_modis_chl_b$modis.chla.avg_r1, dataset_modis_chl_b$modis.chla.avg_r2, dataset_modis_chl_b$modis.chla.min_r2, dataset_modis_chl_b$modis.chla.max_r2, dataset_modis_chl_b$modis.chla.std_r2, dataset_modis_chl_b$modis.chla.mode_r2,
                             dataset_modis_sst_b$modis.sst.avg_r0, dataset_modis_sst_b$modis.sst.avg_r1, dataset_modis_sst_b$modis.sst.avg_r2, dataset_modis_sst_b$modis.sst.min_r2, dataset_modis_sst_b$modis.sst.max_r2, dataset_modis_sst_b$modis.sst.std_r2, dataset_modis_sst_b$modis.sst.mode_r2, 
                             dataset_avhrr_b$avhrr.avg_r0, dataset_avhrr_b$avhrr.avg_r1, dataset_avhrr_b$avhrr.avg_r2, dataset_avhrr_b$avhrr.min_r2, dataset_avhrr_b$avhrr.max_r2, dataset_avhrr_b$avhrr.std_r2, dataset_avhrr_b$avhrr.mode_r2)
  
  colnames(dataset_satellite) <- c(colnames(dataset), 
                                   "Bathymetry",
                                   "modis.chla.avg_r0","modis.chla.avg_r1","modis.chla.avg_r2","modis.chla.min_r2","modis.chla.max_r2","modis.chla.std_r2","modis.chla.mode_r2",
                                   "modis.sst.avg_r0","modis.sst.avg_r1","modis.sst.avg_r2","modis.sst.min_r2","modis.sst.max_r2","modis.sst.std_r2","modis.sst.mode_r2",
                                   "avhrr.avg_r0","avhrr.avg_r1","avhrr.avg_r2","avhrr.min_r2","avhrr.max_r2","avhrr.std_r2","avhrr.mode_r2")
}

#MODIS only
if (dataset_id == "6" | dataset_id == "9"){  
  dataset_satellite <- data.frame(dataset_modis_sst[,1:ncol(dataset_modis_sst)-1], dataset_modis_chl[,8:ncol(dataset_modis_chl)])
  #dataset_satellite$bathy <- as.numeric(dataset_satellite$bathy)
  #dataset_satellite <- subset(dataset_satellite, modis.sst.nb_r0 %in% c("1") )
}

##################### subset dataset with data and position only
dataset_satellite <- subset(dataset_satellite, !is.na(latitude) & !is.na(longitude) &!is.na(year))


##################### write matrix with all fields

if (data_type == "acoustic"){
  filenameout <- paste(directory_dataset,"/With_Satellite_Data/acoustic_scientific_",fleet,"_",year_start,"_",year_end,"_with_satellite_data.csv",sep="")
  dataset_satellite$Transect <- NULL;
}
if (data_type == "catches"){
  filenameout <- paste(directory_dataset,"/With_Satellite_Data/catches_",year_start,"_",year_end,"_with_satellite_data.csv",sep="")
}


###### checking file
summary(dataset_satellite)
str(dataset_satellite)

if (dataset_id == "1"){
  head(subset(dataset_satellite, avhrr.avg_r0 > 0 & modis.sst.avg_r0 >0 & modis.chla.avg_r0 > 0 & seawifs.avg_r0 > 0))
}
if (dataset_id == "3"){
  head(subset(dataset_satellite, avhrr.avg_r0 > 0 & seawifs.avg_r0 > 0))
}
if (dataset_id == "6"){
  head(subset(dataset_satellite, modis.chla.avg_r0 > 0 & modis.sst.avg_r0 > 0))
}
write.csv(dataset_satellite, file=filenameout, row.names = FALSE, quote=FALSE)

