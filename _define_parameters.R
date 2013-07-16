######################################################
######## Define parameters
#-----------------------------------------------------
# zone
# dataset_id
######################################################

rm(list = ls())
cl = colours()

directory_data = 'G:/_sauvegarde_Jeremie/_data/'
#directory_data = '/home/jhabasque/'
specie = 'Jack Mackerel' # 1 - Jack Mackerel
catches_unit = 'tons' # 1=tons

dataset_id = '1' #, 1= Acoustic data IMARPE
                 #  2= Acoustic data TASA
                 #  3= Acoustic data IFOP
                 #  4= Catches from Netherlands
                 #  5= Catches from Russia
                 #  6= Catches from Peru

if (dataset_id == 1) {
  zone = 1
  dataset = "Acoustic data collected by Peruvian scientific surveys"
  year_start = 1983
  year_end = 2011
  fleet = 'Peru'
  data_type = 'acoustic'
  ### sensors and parameters definition
  sensors_parameters = c("seawifs","modis.chla","modis.sst","avhrr")
  sensor = c("SeaWiFS","MODIS","MODIS","AVHRR")
  temporal_resolution = c("8day","1day","8day","1day")
  spatial_resolution = c("9km","4km","4km","4km")
  #sensors_unit = c("mg/m3","mg/m3","°C","°C")
  sensors_unit = c("mg/m3","mg/m3","C","C")
  parameter = c("CHL-a","CHL-a","SST","SST")
}
if (dataset_id == 2) {
  zone = 1
  fleet = 'Peru'
  data_type = 'acoustic_pro'
}
if (dataset_id == 3) {
  zone = 2 
  dataset = "Acoustic data collected by Chilean scientific surveys"
  year_start = 1997
  year_end = 1999
  fleet = 'Chile'
  data_type = 'acoustic'
  ### sensors and parameters definition
  sensors_parameters = c("seawifs","avhrr")
  sensor = c("SeaWiFS","AVHRR")
  temporal_resolution = c("8day","1day")
  spatial_resolution = c("9km","4km")
  #sensors_unit = c("mg/m3","°C")
  sensors_unit = c("mg/m3","C")
  parameter = c("CHL-a","SST")
}
if (dataset_id == 4) {
  zone = 4
  dataset = "Catches data collected by Dutch fishing fleet"
  year_start = 2005
  year_end = 2011
  fleet = 'Netherlands'
  data_type = 'catches'
}
if (dataset_id == 6) {
  zone = 1
  dataset = "Catches data collected by Peruvian fishing fleet"
  year_start = 2011
  year_end = 2013
  fleet = 'Peru'
  data_type = 'catches'
  sensors_parameters = c("modis.chla","modis.sst")
  sensor = c("MODIS","MODIS")
  temporal_resolution = c("1day","1day")
  spatial_resolution = c("4km","4km")
  sensors_unit = c("mg/m3","°C")
  parameter = c("CHL-a","SST")
}

#geographical parameters
if (zone == 1) {
  zone_name = 'Peru'  
  lat_min = -20
  lat_max = -5
  lon_min = -82
  lon_max = -70
}
if (zone == 2) {
  zone_name = 'Chile'  
  lat_min = -50
  lat_max = -18
  lon_min = -85
  lon_max = -70
}
if (zone == 3) {
  zone_name = 'sudamerica'  
  lat_min = -60
  lat_max = 5
  lon_min = -200
  lon_max = -70
}
if (zone == 4) {
  zone_name = 'sudamerica'  
  lat_min = -60
  lat_max = 5
  lon_min = -200
  lon_max = -70
}

#study area
filename_shoreline <- read.table(paste(directory_data,'Shorelines/gshhs_crude.txt',sep=''));
filename_shoreline$V1 <- filename_shoreline$V1 -360;
#x11()
#plot(filename_shoreline$V1,filename_shoreline$V2, xlim=c(lon_min,lon_max), ylim=c(lat_min,lat_max), xlab='Longitude', ylab ='Latitude', main="Study area", type='l')

filename_shelfbreak = paste(directory_data, 'Bathymetry/ShelfBreakPosition.csv', sep="")
ShelfBreakPosition <- read.csv(filename_shelfbreak, header=TRUE, sep=";", dec=".",colClasses = "character")


