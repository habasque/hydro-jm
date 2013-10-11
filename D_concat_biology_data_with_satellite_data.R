######################################################
### Biology with satellite data concatenation program
#-----------------------------------------------------
### Inputs:
### - acoustic data from IMARPE 1983-2011
### - acoustic data from IFOP 1997-1999
### - catches data from Dutch fleet 2005-2011
### - catches data from Peruvian fleet 2011-2013
### - catches data from Chilean fleet 2007-2012
### OUTPUT :
### - all_biology_data_with_satellite_data.csv
######################################################

source('C:/Workspace_R/_define_parameters.R')

col1 = "black"
col2 = "red"
col3 = "blue"
col4 = "green"

#read matrix Peru
fleet <- 'Peru'
year_start <- 1983
year_end <- 2011
directory_acoustic_with_satellite <- paste(directory_data, 'Acoustic_Scientific/', fleet, '/With_Satellite_Data/', sep="")
filename_acoustic_satellite_peru <- paste(directory_acoustic_with_satellite, "acoustic_scientific_",fleet,"_",year_start,"_",year_end,"_with_satellite_data.csv",sep="")
acoustic_satellite_peru <- read.csv(filename_acoustic_satellite_peru, sep=",", dec=".",header=T)
str(acoustic_satellite_peru)

#read matrix Chile
fleet <- 'Chile'
year_start <- 1997
year_end <- 1999
directory_acoustic_with_satellite <- paste(directory_data, 'Acoustic_Scientific/', fleet, '/With_Satellite_Data/', sep="")
#directory_acoustic_with_satellite <- paste(directory_data, 'Acoustic_Scientific/', fleet, sep="")
filename_acoustic_satellite <- paste(directory_acoustic_with_satellite, "acoustic_ubm_",fleet,"_",year_start,"_",year_end,"_with_satellite_data.csv",sep="")
#filename_acoustic_satellite <- paste(directory_acoustic_with_satellite, "/acoustic_ubm_",year_start,"_",year_end,".csv",sep="")
acoustic_satellite_chile <- read.csv(filename_acoustic_satellite, sep=";", dec=".",header=T)
str(acoustic_satellite_chile)

#read matrix Netherlands
fleet <- 'Netherlands'
year_start <- 2005
year_end <- 2011
directory_catches_with_satellite <- paste(directory_data, 'Catches/', fleet, '/With_Satellite_Data/', sep="")
filename_catches_netherlands <- paste(directory_catches_with_satellite, "catches_",year_start,"_",year_end,"_with_satellite_data.csv",sep="")
catches_netherlands <- read.csv(filename_catches_netherlands, sep=",", dec=".",header=T)
str(catches_netherlands)

#read matrix catches Peru
fleet <- 'Peru'
year_start <- 2011
year_end <- 2013
directory_catches_with_satellite <- paste(directory_data, 'Catches/', fleet, '/With_Satellite_Data/', sep="")
filename_catches_peru <- paste(directory_catches_with_satellite, "catches_",year_start,"_",year_end,"_with_satellite_data.csv",sep="")
catches_peru <- read.csv(filename_catches_peru, sep=";", dec=".",header=T)
str(catches_peru)

#read matrix catches Chile
fleet <- 'Chile'
year_start <- 2007
year_end <- 2012
directory_catches_with_satellite <- paste(directory_data, 'Catches/', fleet, '/With_Satellite_Data/', sep="")
filename_catches_chile <- paste(directory_catches_with_satellite, "Catches_",year_start,"_",year_end,"_with_satellite_data.csv",sep="")
catches_chile <- read.csv(filename_catches_chile, sep=",", dec=".",header=T)
str(catches_chile)


######################### selecting jack mackerel records

acoustic_satellite_peru_cjm <- subset(acoustic_satellite_peru, Anch == 0 & Sard == 0 & CJM >= 0)
acoustic_satellite_chile_cjm <- subset(acoustic_satellite_chile, CJM >= 0)
catches_netherlands_cjm <- subset(catches_netherlands, CJM >= 0)
catches_peru_cjm <- subset(catches_peru, CJM >= 0)
catches_chile_cjm <- subset(catches_chile, CJM >= 0)

######################### combining datasets

# seawifs.avg_r2 and avhrr.avg_r2
acoustic_satellite_peru_cjm$dataset_id <- 1
dataset_peru_fields <- data.frame(acoustic_satellite_peru_cjm$dataset_id, acoustic_satellite_peru_cjm$Year, acoustic_satellite_peru_cjm$Month, acoustic_satellite_peru_cjm$Day, acoustic_satellite_peru_cjm$Latitude, acoustic_satellite_peru_cjm$Longitude, acoustic_satellite_peru_cjm$bathy, acoustic_satellite_peru_cjm$CJM, acoustic_satellite_peru_cjm$seawifs.avg_r2, acoustic_satellite_peru_cjm$avhrr.avg_r2)
colnames(dataset_peru_fields) <- c("dataset_id","Year","Month","Day","Latitude","Longitude","Bathy","CJM","CHLA","SST")

# seawifs.avg_r2 and avhrr.avg_r2
acoustic_satellite_chile_cjm$dataset_id <- 3
dataset_chile_fields <- data.frame(acoustic_satellite_chile_cjm$dataset_id, acoustic_satellite_chile_cjm$Year, acoustic_satellite_chile_cjm$Month, acoustic_satellite_chile_cjm$Day, acoustic_satellite_chile_cjm$Latitude, acoustic_satellite_chile_cjm$Longitude, acoustic_satellite_chile_cjm$bathy , acoustic_satellite_chile_cjm$CJM, acoustic_satellite_chile_cjm$seawifs.avg_r2, acoustic_satellite_chile_cjm$avhrr.avg_r2)
colnames(dataset_chile_fields) <- c("dataset_id","Year","Month","Day","Latitude","Longitude","Bathy","CJM","CHLA","SST")

#modis.chla.avg_r2 and modis.sst.avg_r2
catches_netherlands_cjm$dataset_id <- 4
dataset_netherlands_fields <- data.frame(catches_netherlands_cjm$dataset_id, catches_netherlands_cjm$Year, catches_netherlands_cjm$Month, catches_netherlands_cjm$Day, catches_netherlands_cjm$Latitude, catches_netherlands_cjm$Longitude, catches_netherlands_cjm$Bathymetry, catches_netherlands_cjm$CJM, catches_netherlands_cjm$modis.chla.avg_r2, catches_netherlands_cjm$modis.sst.avg_r2)
colnames(dataset_netherlands_fields) <- c("dataset_id","Year","Month","Day","Latitude","Longitude","Bathy","CJM","CHLA","SST")

#modis.chla.avg_r2 and modis.sst.avg_r0
catches_peru_cjm$dataset_id <- 6
dataset_catches_peru_fields <- data.frame(catches_peru_cjm$dataset_id, catches_peru_cjm$Year, catches_peru_cjm$Month, catches_peru_cjm$Day, catches_peru_cjm$Latitude, catches_peru_cjm$Longitude, catches_peru_cjm$Bathymetry, catches_peru_cjm$CJM, catches_peru_cjm$modis.chla.avg_r2, catches_peru_cjm$modis.sst.avg_r0)
colnames(dataset_catches_peru_fields) <- c("dataset_id","Year","Month","Day","Latitude","Longitude","Bathy","CJM","CHLA","SST")

#modis.chla.avg_r2 and modis.sst.avg_r2
catches_chile_cjm$dataset_id <- 9
dataset_catches_chile_fields <- data.frame(catches_chile_cjm$dataset_id, catches_chile_cjm$year, catches_chile_cjm$month, catches_chile_cjm$day, catches_chile_cjm$latitude, catches_chile_cjm$longitude, catches_chile_cjm$bathy, catches_chile_cjm$CJM, catches_chile_cjm$modis.chla.avg_r2, catches_chile_cjm$modis.sst.avg_r2)
colnames(dataset_catches_chile_fields) <- c("dataset_id","Year","Month","Day","Latitude","Longitude","Bathy","CJM","CHLA","SST")


######################### concatenation records of the 4 datasets
dataset_global <- rbind(dataset_peru_fields, rbind(dataset_chile_fields,rbind(dataset_netherlands_fields,rbind(dataset_catches_peru_fields,dataset_catches_chile_fields))))
write.csv(dataset_global, file=paste(directory_data,'all_biology_data_with_satellite_data.csv',sep=""),row.names=FALSE,quote=FALSE)


tapply(dataset_global$dataset_id, list(dataset_global$dataset_id), function(x) length(x))


x11()
plot(dataset_cjm_model_netherlands$Longitude, dataset_cjm_model_netherlands$Latitude, xlim=c(lon_min,lon_max), ylim=c(lat_min,lat_max), xlab='Longitude', ylab ='Latitude', main=paste("Catches and acoustic data points from 1983 to 2013"), col="red")
points(dataset_cjm_model_peru$Longitude, dataset_cjm_model_peru$Latitude, col ='blue',pch='.')
points(dataset_cjm_model_chile$Longitude, dataset_cjm_model_chile$Latitude, col ='green',pch='.')
points(dataset_cjm_model_catches_peru$Longitude, dataset_cjm_model_catches_peru$Latitude, col ='orange',pch='.')
legend("top", c("Netherlands catches 2005-2011", 
                "Published data from IMARPE acoustic surveys 1983-2011", 
                "Published data from IFOP acoustic surveys 1997-1999",
                "Peruvian catches 2011-2013"), 
               lwd = 2, col = c("red","blue","green"))
#shoreline
lines(filename_shoreline)

######################### presence/absence field
dataset_cjm_model$presence_absence <- recode(dataset_cjm_model$CJM, '0=0; else = 1', as.factor.result=FALSE)

x11()
hist(dataset_cjm_model$presence_absence, main="Presence/absence distribution", xlab = "Presence/Absence")
summary(dataset_cjm_model)

presence_absence_peru <- subset(dataset_cjm_model, dataset_id == 1)
hist_peru <- table(presence_absence_peru$presence_absence)
x11()
barplot(hist_peru, main="Presence/absence distribution - Peru", xlab = "Presence/Absence", col ="yellow")

presence_absence_chile <- subset(dataset_cjm_model, dataset_id == 3)
hist_chile <- table(presence_absence_chile$presence_absence)
x11()
barplot(hist_chile, main="Presence/absence distribution - Chile", xlab = "Presence/Absence", col ="yellow")

presence_absence_netherlands <- subset(dataset_cjm_model, dataset_id == 4)
hist_netherlands <- table(presence_absence_netherlands$presence_absence)
x11()
barplot(hist_netherlands, main="Presence/absence distribution - Catches Netherlands", xlab = "Presence/Absence", col ="yellow")

presence_absence_catches_peru <- subset(dataset_cjm_model, dataset_id == 6)
hist_catches_peru <- table(presence_absence_catches_peru$presence_absence)
x11()
barplot(hist_catches_peru, main="Presence/absence distribution - Catches Peru", xlab = "Presence/Absence", col ="yellow")

######################### records with empty CHLA et SST values are deleted
dataset_cjm_model_sans_na <- subset(dataset_cjm_model, !is.na(CHLA) & !is.na(SST))
summary(dataset_cjm_model_sans_na)

#number of data by dataset
tapply(dataset_cjm_model_sans_na$dataset_id, list(dataset_cjm_model_sans_na$dataset_id), function(x) length(x))

######################### plots SST vs CHLA presence/absence

x11()
plot(log(dataset_cjm_model_sans_na$CHLA+1), dataset_cjm_model_sans_na$SST,xlim=c(0,4), main="SST vs CHLA", xlab="log(CHLA)", ylab="SST")
 
# plots presence
dataset_cjm_model_sans_na_presence <- subset(dataset_cjm_model_sans_na, presence_absence ==1)
x11()
plot(log(dataset_cjm_model_sans_na_presence$CHLA+1), dataset_cjm_model_sans_na_presence$SST,xlim=c(0,3), ylim=c(9,28), main="SST vs CHLA - presence", xlab="log(CHLA)", ylab="SST")

dataset_cjm_model_sans_na_presence_peru <- subset(dataset_cjm_model_sans_na_presence, dataset_id == 1)
par(new=TRUE)
plot(log(dataset_cjm_model_sans_na_presence_peru$CHLA+1), dataset_cjm_model_sans_na_presence_peru$SST,xlim=c(0,3), ylim=c(9,28), col="red", xlab="log(CHLA)", ylab="SST")

dataset_cjm_model_sans_na_presence_chile <- subset(dataset_cjm_model_sans_na_presence, dataset_id == 2)
par(new=TRUE)
plot(log(dataset_cjm_model_sans_na_presence_chile$CHLA+1), dataset_cjm_model_sans_na_presence_chile$SST,xlim=c(0,3), ylim=c(9,28), col="blue", xlab="log(CHLA)", ylab="SST")
legend("topright", legend=c("Peru","Chile","Netherlands"), lwd = 2, col= c("red","blue","black"))

# plots absence
dataset_cjm_model_sans_na_absence <- subset(dataset_cjm_model_sans_na, presence_absence == 0)
x11()
plot(log(dataset_cjm_model_sans_na_absence$CHLA+1), dataset_cjm_model_sans_na_absence$SST,xlim=c(0,3), ylim=c(9,28), main="SST vs CHLA - absence", xlab="log(CHLA)", ylab="SST")

dataset_cjm_model_sans_na_absence_peru <- subset(dataset_cjm_model_sans_na_absence, dataset_id == 1)
par(new=TRUE)
plot(log(dataset_cjm_model_sans_na_absence_peru$CHLA+1), dataset_cjm_model_sans_na_absence_peru$SST,xlim=c(0,3), ylim=c(9,28), col="red", xlab="log(CHLA)", ylab="SST")

dataset_cjm_model_sans_na_absence_chile <- subset(dataset_cjm_model_sans_na_absence, dataset_id == 3)
par(new=TRUE)
plot(log(dataset_cjm_model_sans_na_absence_chile$CHLA+1), dataset_cjm_model_sans_na_absence_chile$SST,xlim=c(0,3), ylim=c(9,28), col="blue", xlab="log(CHLA)", ylab="SST")
legend("topright", legend=c("Peru","Chile","Netherlands"), lwd = 2, col= c("red","blue","black"))

######################### variables distribution
x11()
hist(log(dataset_cjm_model_sans_na$CHLA+1),nclass=100, xlab="log(CHL-a + 1)", main="CHLA-a distribution")
x11()
hist(dataset_cjm_model_sans_na$SST,nclass=100,xlab="SST", main="SST distribution")


# check all positions
plot(dataset_cjm_model_sans_na$Longitude, dataset_cjm_model_sans_na$Latitude,pch='.')


######################### Writing global dataset

write.csv(dataset_cjm_model_sans_na, file=paste(directory_data,'all_biology_data_with_satellite_data.csv',sep=""),row.names=FALSE,quote=FALSE)
str(dataset_cjm_model_sans_na)
summary(dataset_cjm_model_sans_na)

tapply(dataset_cjm_model_sans_na$dataset_id, list(dataset_cjm_model_sans_na$dataset_id,dataset_cjm_model_sans_na$presence_absence), function(x) length(x))

