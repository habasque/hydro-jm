##############################################################################
# Program of monthly catches, acoustic data and habitat limits mapping 
#-----------------------------------------------------------------------------
# INPUT :
# - SST AVHRR monthly climatology 18km - 1982-2011
# - CHL MODIS monthly climatology 18km - 2003-2011
# - Dutch catches 2005-2011
# - Acoustic scientific Chile 1997-1999
# - Acoustic scientific Peru 1983-2011
# - Peru catches 2011-2013
# - Chilean catches 2010-2011
##############################################################################

source('C:/Workspace_R/_define_parameters.R')

#define habitat parameter limits
lowest_temperature <- 9
highest_temperature <- 26
lowest_chla <- 0.07
lowest_chla_Peru <- 0.2

#loading libraries
library(ncdf)
library(fields)
library(RColorBrewer)

#loading data OSCAR climato file 
#file_climato_OSCAR <- open.ncdf(paste(directory_data,'/Satellite/OSCAR/oscar-filter-monthly-51e7f5eb5ed19.nc',sep=""))     
#lon <- get.var.ncdf(file_climato_OSCAR, "lon")   # coordinate variable
#lat <- get.var.ncdf(file_climato_OSCAR, "lat")   # coordinate variable
#time <- get.var.ncdf(file_climato_OSCAR, "time")
#depth <- get.var.ncdf(file_climato_OSCAR, "depth")
#u <- get.var.ncdf(file_climato_OSCAR,"u",start=c(1,1,1,1),count=c(dim(lon)[[1]],dim(lat)[[1]],dim(time)[[1]],dim(depth)[[1]])

#[-180 180] transformation to [-360 0]
sst2 <- sst[1:1080,,]
sst3 <- sst[1081:dim(lon)[1],,]
sst[1:1080,,] <- sst3
sst[1081:dim(lon)[1],,] <- sst2


#loading SST AVHRR climato file 
file_climato_SST <- open.ncdf(paste(directory_data,'/Satellite/SST/18km/w18_avhrr_sst2_18km_1m_19820101_20111231.pfv52v20fv10.qual4.float.nc',sep=""))     
lon <- get.var.ncdf(file_climato_SST, "lon")   # coordinate variable
lat <- get.var.ncdf(file_climato_SST, "lat")   # coordinate variable
time <- get.var.ncdf(file_climato_SST, "time")
sst <- get.var.ncdf(file_climato_SST,"sst",start=c(1,1,1),count=c(dim(lon)[[1]],dim(lat)[[1]],dim(time)[[1]]))

#[-180 180] transformation to [-360 0]
sst2 <- sst[1:1080,,]
sst3 <- sst[1081:dim(lon)[1],,]
sst[1:1080,,] <- sst3
sst[1081:dim(lon)[1],,] <- sst2

#loading CHLA MODIS 
file_climato_CHL <- open.ncdf(paste(directory_data,'/Satellite/CHL/18km/w18_modis_chla_18km_1m_20030101_20111231.R2012.0.seawifs-corrected.0.50.avg.float.nc',sep=""))
lon <- get.var.ncdf(file_climato_CHL, "lon")          # coordinate variable
lat <- get.var.ncdf(file_climato_CHL, "lat")          # coordinate variable
time <- get.var.ncdf(file_climato_CHL, "time")
chla <- get.var.ncdf(file_climato_CHL,"chla",start=c(1,1,1),count=c(dim(lon)[[1]],dim(lat)[[1]],dim(time)[[1]]))

#[-180 180] transformation to [-360 0]
chla2 <- chla[1:1080,,]
chla3 <- chla[1081:dim(lon)[1],,]
chla[1:1080,,] <- chla3
chla[1081:dim(lon)[1],,] <- chla2

lon[lon>0] <- lon[lon>0] - 360
lon <- sort(lon)

##### creating a presence/absence matrix
species_presence <- array(NA, dim=c(dim(sst)[[1]], dim(sst)[[2]], dim(sst)[[3]] ))
species_presence[sst<lowest_temperature | sst>highest_temperature] <- 0
species_presence[sst>=lowest_temperature & sst<=highest_temperature] <- 1
species_presence[chla<lowest_chla] <- 0   

# filename Dutch catches
fleet = 'Netherlands'
year_start <- 2005
year_end <- 2011
filename_catches <- paste(directory_data, 'Catches/', fleet, '/Catches_',year_start,'_',year_end,'_with_ship.csv', sep="")
catches <- read.csv(filename_catches, header=TRUE, sep=";", dec=".",colClasses = "character")
catches$Longitude <- as.numeric(catches$Longitude)
catches$Latitude <- as.numeric(catches$Latitude)
catches$Month <- as.numeric(catches$Month)
catches$Year <- as.numeric(catches$Year)

# filename Chilean catches
fleet = 'Chile'
year_start <- 2010
year_end <- 2011
filename_catches <- paste(directory_data, 'Catches/', fleet, '/Catches_',year_start,'_',year_end,'.csv', sep="")
catches_Chile <- read.csv(filename_catches, header=TRUE, sep=";", dec=".",colClasses = "character")
catches_Chile$Longitude <- as.numeric(catches_Chile$Longitude)
catches_Chile$Latitude <- as.numeric(catches_Chile$Latitude)
catches_Chile$Month <- as.numeric(catches_Chile$Month)
catches_Chile$Year <- as.numeric(catches_Chile$Year)


# filename international catches month
fleet <- 'International'
year_start <- 1975
year_end <- 2012
filename_catches_international <- paste(directory_data, 'Catches/', fleet, '/Catches_',year_start,'_',year_end,'.csv', sep="")
catches_international <- read.csv2(filename_catches_international, header=TRUE, dec=".")

# filename international catches season
fleet <- 'International'
year_start <- 1975
year_end <- 2012
filename_catches_international_season <- paste(directory_data, 'Catches/', fleet, '/Catches_',year_start,'_',year_end,'_season.csv', sep="")
catches_international <- read.csv2(filename_catches_international_season, header=TRUE, dec=".")


# filename acoustic scientific Chile 1997-1999
fleet = 'Chile'
year_start <- 1997
year_end <- 1999
filename_acoustic <- paste(directory_data, 'Acoustic_Scientific/', fleet, '/acoustic_ubm_',year_start,'_',year_end,'.csv', sep="")
acoustic_data_Chile = read.csv2(filename_acoustic, header=TRUE, sep=";", dec=".",colClasses = "character")
acoustic_data_Chile$Longitude <- as.numeric(acoustic_data_Chile$Longitud)
acoustic_data_Chile$Latitude <- as.numeric(acoustic_data_Chile$Latitud)
acoustic_data_Chile$CJM <- as.numeric(acoustic_data_Chile$CJM)
acoustic_data_Chile$Month <- as.numeric(acoustic_data_Chile$Month)
acoustic_data_Chile$Year <- as.numeric(acoustic_data_Chile$Year)

# filename acoustic scientific Peru 1983-2011
fleet = 'Peru' # Peru
year_start <- 1983
year_end <- 2011
filename_acoustic_Peru = paste(directory_data, 'Acoustic_Scientific/', fleet, '/acoustic_data_',year_start,'_',year_end,'.csv', sep="")
acoustic_data_Peru = read.csv(filename_acoustic_Peru, header=TRUE, sep=";", dec=".",colClasses = "character")
acoustic_data_Peru$Longitude = as.numeric(acoustic_data_Peru$Longitude)
acoustic_data_Peru$Latitude = as.numeric(acoustic_data_Peru$Latitude)
acoustic_data_Peru$CJM = as.numeric(acoustic_data_Peru$CJM)
acoustic_data_Peru$Month <- as.numeric(acoustic_data_Peru$Month)
acoustic_data_Peru$Year <- as.numeric(acoustic_data_Peru$Year)

#artisanal catches Peru 1997-2010
year_start <- 1997
year_end <- 2010
filename_artisanal_catches_Peru = paste(directory_data, 'Catches/', fleet, '/Artisanales/jurel_',year_start,'_',year_end,'.csv', sep="")
artisanal_catches_Peru = read.csv(filename_artisanal_catches_Peru, header=TRUE, sep=";", dec=".",colClasses = "character")
artisanal_catches_Peru$Longitude = as.numeric(artisanal_catches_Peru$Longitude)
artisanal_catches_Peru$Latitude = as.numeric(artisanal_catches_Peru$Latitude)
artisanal_catches_Peru$Month <- as.numeric(artisanal_catches_Peru$Month)
artisanal_catches_Peru$Year <- as.numeric(artisanal_catches_Peru$Year)

#catches Peru 2011-2013
year_start <- 2011
year_end <- 2013
filename_catches_Peru = paste(directory_data, 'Catches/', fleet, '/catches_',year_start,'_',year_end,'.csv', sep="")
catches_Peru = read.csv(filename_catches_Peru, header=TRUE, sep=";", dec=".",colClasses = "character")
catches_Peru$Longitude = as.numeric(catches_Peru$Longitude)
catches_Peru$Latitude = as.numeric(catches_Peru$Latitude)
catches_Peru$Month <- as.numeric(catches_Peru$Month)
catches_Peru$Year <- as.numeric(catches_Peru$Year)

filename_prospection_TASA <- paste(directory_data, 'Acoustic_Fishing_Vessels/Prospeccion_Jurel_T55_T58/positions_prospeccion_TASA.csv', sep="")
prospection_TASA <- read.csv(filename_prospection_TASA, header=TRUE, sep=";", dec=".",colClasses = "character")
prospection_TASA$Longitude <- as.numeric(prospection_TASA$Longitude)
prospection_TASA$Latitude <- as.numeric(prospection_TASA$Latitude)
prospection_TASA$Year <- as.numeric(prospection_TASA$Year)
prospection_TASA$Month <- as.numeric(prospection_TASA$Month)

filename_oxycline_Peru <- paste(directory_data, 'Acoustic_Scientific/', fleet, '/Oxycline/oxycline_2008_02_04.csv', sep="")
oxycline_Peru <- read.csv(filename_oxycline_Peru, header=TRUE, sep=";", dec=".",colClasses = "character")
oxycline_Peru$Longitude <- as.numeric(oxycline_Peru$Longitude)
oxycline_Peru$Latitude <- as.numeric(oxycline_Peru$Latitude)
oxycline_Peru$Year <- as.numeric(oxycline_Peru$Year)
oxycline_Peru$Month <- as.numeric(oxycline_Peru$Month)
oxycline_Peru$Depth_oxycline <- as.numeric(oxycline_Peru$Depth_oxycline)
oxycline_Peru <- subset(oxycline_Peru, !is.na(Latitude) & !is.na(Depth_oxycline))

#figure global
x11(width=15,height=8,pointsize=12)
#catches international
plot(catches_international$Longitude, catches_international$Latitude, col ='white',
     xlim=c(lon_min,lon_max), ylim=c(lat_min,lat_max), xlab='Longitude', ylab ='Latitude', 
     main=paste("Catches and acoustic data points from 1983 to 2013"))
#catches Netherlands
catches_CJM_Netherlands <- subset(catches, CJM > 0)
points(catches_CJM_Netherlands$Longitude, catches_CJM_Netherlands$Latitude, col = 'green')
#acoustic Chile
acoustic_CJM_Chile <- subset(acoustic_data_Chile, CJM > 0) 
points(acoustic_CJM_Chile$Longitude, acoustic_CJM_Chile$Latitude, col ='red',pch='.')
#acoustic Peru
acoustic_CJM_Peru <- subset(acoustic_data_Peru, CJM > 0) 
points(acoustic_CJM_Peru$Longitude, acoustic_CJM_Peru$Latitude, col ='blue',pch='.')
#catches Peru 2011-2013
catches_Peru <- subset(catches_Peru, CJM > 0) 
points(catches_Peru$Longitude, catches_Peru$Latitude, col ='darkgreen',pch='.')
#catches Chile 2010-2011
points(catches_Chile$Longitude, catches_Chile$Latitude, col ='black')
#shoreline
lines(filename_shoreline)

legend("top", c("Netherlands catches 2005-2011",                 
                "Published data from IFOP acoustic surveys 1997-1999",
                "Published data from IMARPE acoustic surveys 1983-2011", 
                "Peruvian catches 2011-2013",
                "Chilean catches 2010-2011"),
                lwd = 2, col = c("green","red","blue","darkgreen","black"))

# for each month
for (ind_month in 1:12) {
  
  
  sst_month <- sst[,,ind_month]
  #par(new=TRUE)
  #contour(lon,lat,sst_month, levels = lowest_temperature,col="blue",xlim=c(lon_min,lon_max), ylim=c(lat_min,lat_max))
  #par(new=TRUE)
  #contour(lon,lat,sst_month, levels = highest_temperature,col="red",xlim=c(lon_min,lon_max), ylim=c(lat_min,lat_max))
  
  chla_month <- chla[,,ind_month]
  #par(new=TRUE)
  #contour(lon,lat,chla_month, levels = lowest_chla,col="darkgreen",xlim=c(lon_min,lon_max), ylim=c(lat_min,lat_max))
  #par(new=TRUE)
  #contour(lon,lat,chla_month, levels = lowest_chla_Peru,col="darkgreen",xlim=c(lon_min,lon_max), ylim=c(lat_min,lat_max))
  
  #shoreline
  lines(filename_shoreline)
  
  #shelfbreak
  if (zone == 1) {
    lines(ShelfBreakPosition)
  }
  
  #ZEE (marineregions.org)
  #library(maps)
  #library(mapdata)
  #library(maptools)
  #ZEE <- readShapeLines("E:/_sauvegarde_Jeremie/_data/ZEE/World_EEZ_v7_2012_HR.shp")
  #x11()
  #plot(0,0,xlim=c(-90, -70), ylim=c(-60,10),type="n", main="Exemple")
  #lines(ZEE,ylim=c(-60,10),xlim=c(-90,-70),col="blue")
  #prospection_TASA_month <- subset(prospection_TASA, Month == ind_month)   
  #points(prospection_TASA_month$Longitude, prospection_TASA_month$Latitude, col ='blue',pch='.')
  
  species_presence_month <- species_presence[,,ind_month]
  species_presence_pacific <- species_presence_month[lon >= lon_min & lon <= lon_max,lat >= lat_min & lat <= lat_max]
  chla_month_pacific <- chla_month[lon >= lon_min & lon <= lon_max,lat >= lat_min & lat <= lat_max]
  sst_month_pacific <- sst_month[lon >= lon_min & lon <= lon_max,lat >= lat_min & lat <= lat_max]  
  lon_pacific <- lon[lon>= lon_min & lon <= lon_max]
  lat_pacific <- lat[lat>= lat_min & lat <= lat_max]
  y_model <- matrix(NaN,c(dim(chla_month_pacific)[[1]]))
  for (i in 1:length(chla_month_pacific)){
    y_model[i] <- 9 + ((450*chla_month_pacific[i])/(1+(26*chla_month_pacific[i])))
  }
  
  species_presence_pacific[sst_month_pacific > y_model] <- 0
  
  # plot
  x11(width=15,height=8,pointsize=12)
  image(lon_pacific,lat_pacific,species_presence_pacific, col=rev(brewer.pal(9,"RdBu")), xlab='Longitude', ylab ='Latitude', 
        main= paste("Catches and acoustic data points from 1983 to 2011 vs horizontal habitat limits - Month ", ind_month,
                    "\nSST >= 9°C & SST<=26°C, CHLa >= 0.07 mg/m3 and interaction model between SST and CHLa", sep=""))
 
  
  #oxycline depth
  #img_oxycline <- as.image(Z=oxycline_Peru$Depth_oxycline, ind=data.frame(oxycline_Peru$Longitude,oxycline_Peru$Latitude))
  #img_smooth <- image.smooth(img)
  #image.plot(img_smooth,ylim=c(lat_min,lat_max),xlim=c(lon_min,lon_max))
  #image.plot(img_oxycline,ylim=c(lat_min,lat_max),xlim=c(lon_min,lon_max),legend.lab="Depth(meters)",main="Map of oxycline depth (IMARPE survey 2008-02/04) and TASA 55/58 prospection track 12-24th may 2008")
  #contour(img_oxycline,add=T)
  
  #catches selection by month
  catches_CJM_Netherlands <- subset(catches, CJM > 0 & Month == ind_month)  
  points(catches_CJM_Netherlands$Longitude, catches_CJM_Netherlands$Latitude, col ='green')
  #plot(catches_CJM_Netherlands$Longitude, catches_CJM_Netherlands$Latitude, col ='green',
  #     xlim=c(lon_min,lon_max), ylim=c(lat_min,lat_max), xlab='Longitude', ylab ='Latitude', 
  #     main=paste("Catches and acoustic data points from 1983 to 2011 vs habitat limits - Month ", ind_month, sep=""))#,pch='.'
  
  #track Dutch fleet
  #colors <- c("red","blue","green","orange","grey","darkgreen")
  #listeMonth <- unique(catches$Month)
  #for (i in 1:length(listeMonth)) {
  #   data_Month <- subset(catches, Month == ind_month)
  #   listeShip <- unique(data_Month$Ship)
  #   for (j in 1:length(listeShip)) {
  #     data_Month_Ship <- subset(data_Month, Ship == listeShip[j])
  #     points(data_Month_Ship$Longitude, data_Month_Ship$Latitude, type='l', col=colors[j])
  #   }  
  #}
  
  #catches international
  #catches_CJM_international <- subset(catches_international, Month == ind_month) 
  #points(catches_CJM_international$Longitude, catches_CJM_international$Latitude, pch='.', col = 'lightgrey')
  
  
  #acoustic Chile by month
  acoustic_CJM_Chile_month <- subset(acoustic_data_Chile, CJM > 0 & Month == ind_month) 
  points(acoustic_CJM_Chile_month$Longitude, acoustic_CJM_Chile_month$Latitude, col ='pink',pch='.')
  
  #acoustic Peru by month
  acoustic_CJM_Peru_month <- subset(acoustic_data_Peru, CJM > 0 & Month == ind_month) 
  points(acoustic_CJM_Peru_month$Longitude, acoustic_CJM_Peru_month$Latitude, col ='orange',pch='.')
  
  #catches Peru by month
  catches_CJM_Peru_month <- subset(catches_Peru, CJM > 0 & Month == ind_month) 
  points(catches_CJM_Peru_month$Longitude, catches_CJM_Peru_month$Latitude, col ='black',pch='.')

  #catches Chile by month
  catches_CJM_Chile_month <- subset(catches_Chile, Month == ind_month) 
  points(catches_CJM_Chile_month$Longitude, catches_CJM_Chile_month$Latitude, col ='yellow')
  
  # filename acoustic fishing vessels Peru
  #fleet = 'Peru' # Peru, Chile, Netherlands
  #year_start = 2011
  #year_end = 2012
  #filename_acoustic = paste(directory_data, 'Logbooks_Peche/acoustic_data_TASA_',year_start,'_',year_end,'.csv', sep="")
  #acoustic_data_TASA = read.csv(filename_acoustic, header=TRUE, sep=",", dec=".",colClasses = "character")
  #acoustic_data_TASA$Longitud = as.numeric(acoustic_data_TASA$Longitud)
  #acoustic_data_TASA$Latitud = as.numeric(acoustic_data_TASA$Latitud)
  #acoustic_CJM_TASA = subset(acoustic_data_TASA, Pesca_grande > 0)
  #points(acoustic_CJM_TASA$Longitud, acoustic_CJM_TASA$Latitud, col ='black')
  
  legend("top", c("Netherlands catches 2005-2011", "Published data from IMARPE acoustic surveys 1983-2011", "Published data from IFOP acoustic surveys 1997-1999","Peruvian catches 2011-2013","Chilean catches 2010-2011"), lwd = 2, col = c("green","orange","pink","black","yellow"))
  #legend("top", c("Netherlands catches 2005-2011", "Published data from IMARPE acoustic surveys 1983-2011", "Published data from IFOP acoustic surveys 1997-1999","Peruvian catches 2011-2013","Lowest temperature limit (9°C isoline from AVHRR monthly climatology)", "Highest temperature limit (26°C isoline from AVHRR monthly climatology)","CHL 0.07 mg/m3 isoline (MODIS monthly climatology)"), lwd = 2, col = c("green","orange","pink","black","blue","red","darkgreen"))
  #legend("top", c("Netherlands catches 2005-2011","Lowest temperature limit (9°C isoline from WOA monthly climatology)", "Highest temperature limit (26°C isoline from WOA monthly climatology)","CHL 0.07 mg/m3 isoline (SeaWIFS mean climatology)"), lwd = 2, col = c("red","blue","green","orange","darkgreen"))
  
  #save figure
  filename_figure = paste(directory_data, 'Catches_And_Habitat_Limits_Month_',ind_month, '.png', sep=""); 
  savePlot(filename_figure,type=c("png"));
  
}

# for each season
for (ind_season in 1:4) {
  
  x11(width=15,height=8,pointsize=12)
   
  #catches selection by month
  catches_CJM_Netherlands <- subset(catches, CJM > 0 & Season == ind_season)  
  plot(catches_CJM_Netherlands$Longitude, catches_CJM_Netherlands$Latitude, col ='green',
       xlim=c(lon_min,lon_max), ylim=c(lat_min,lat_max), xlab='Longitude', ylab ='Latitude', 
       main=paste("Catches and acoustic data points from 1983 to 2011 vs habitat limits - Season ", ind_season, sep=""))#,pch='.'
    
  #acoustic Chile by month
  acoustic_CJM_Chile_season <- subset(acoustic_data_Chile, CJM > 0 & Season == ind_season) 
  points(acoustic_CJM_Chile_season$Longitude, acoustic_CJM_Chile_season$Latitude, col ='pink',pch='.')
  
  #acoustic Peru by month
  acoustic_CJM_Peru_season <- subset(acoustic_data_Peru, CJM > 0 & Season == ind_season) 
  points(acoustic_CJM_Peru_season$Longitude, acoustic_CJM_Peru_season$Latitude, col ='orange')
    
  sst_month <- sst[,,ind_month]
  par(new=TRUE)
  contour(lon,lat,sst_month, levels = lowest_temperature,col="blue",xlim=c(lon_min,lon_max), ylim=c(lat_min,lat_max))
  par(new=TRUE)
  contour(lon,lat,sst_month, levels = highest_temperature,col="red",xlim=c(lon_min,lon_max), ylim=c(lat_min,lat_max))
  
  chla_month <- chla[,,ind_month]
  par(new=TRUE)
  contour(lon,lat,chla_month, levels = lowest_chla,col="darkgreen",xlim=c(lon_min,lon_max), ylim=c(lat_min,lat_max))
  #par(new=TRUE)
  #contour(lon,lat,chla_month, levels = lowest_chla_Peru,col="darkgreen",xlim=c(lon_min,lon_max), ylim=c(lat_min,lat_max))
  
  #shoreline
  lines(filename_shoreline)
  
  legend("top", c("Netherlands catches 2005-2011", "Published data from IMARPE acoustic surveys 1983-2011", "Published data from IFOP acoustic surveys 1997-1999","Lowest temperature limit (9°C isoline from AVHRR monthly climatology)", "Highest temperature limit (26°C isoline from AVHRR monthly climatology)","CHL 0.07 mg/m3 isoline (MODIS monthly climatology)"), lwd = 2, col = c("green","orange","pink","blue","red","darkgreen"))
  #legend("top", c("Netherlands catches 2005-2011","Lowest temperature limit (9°C isoline from WOA monthly climatology)", "Highest temperature limit (26°C isoline from WOA monthly climatology)","CHL 0.07 mg/m3 isoline (SeaWIFS mean climatology)"), lwd = 2, col = c("red","blue","green","orange","darkgreen"))
  
  #save figure
  filename_figure = paste(directory_data, 'Catches/', fleet, '/Figures/Catches_And_Habitat_Limits_Season_',ind_season, '.png', sep=""); 
  savePlot(filename_figure,type=c("png"));
}
