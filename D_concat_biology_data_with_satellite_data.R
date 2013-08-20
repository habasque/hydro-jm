######################################################
### Biology with satellite data concatenation program
#-----------------------------------------------------
### Inputs:
### - acoustic data from IMARPE 1983-2011
### - acoustic data from IFOP 1997-1999
### - catches data from Dutch fleet 2005-2011
### - catches data from Peruvian fleet 2011-2013
### OUTPUT :
### - all_biology_data_with_satellite_data.csv
######################################################

source('C:/Workspace_R/_define_parameters.R')

library(tweedie)
library(scatterplot3d)
library(car)
library(quantreg)
library(MASS)
library(splines)

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
catches_netherlands <- read.csv(filename_catches_netherlands, sep=";", dec=".",header=T)
str(catches_netherlands)

#read matrix catches Peru
fleet <- 'Peru'
year_start <- 2011
year_end <- 2013
directory_catches_with_satellite <- paste(directory_data, 'Catches/', fleet, '/With_Satellite_Data/', sep="")
filename_catches_peru <- paste(directory_catches_with_satellite, "catches_",year_start,"_",year_end,"_with_satellite_data.csv",sep="")
catches_peru <- read.csv(filename_catches_peru, sep=";", dec=".",header=T)
str(catches_peru)

######################### selecting jack mackerel records
dataset_cjm_model_peru <- subset(acoustic_satellite_peru, Anch == 0 & Sard == 0 & CJM >= 0)
dataset_cjm_model_chile <- subset(acoustic_satellite_chile, CJM >= 0)
dataset_cjm_model_netherlands <- subset(catches_netherlands, CJM >= 0)
dataset_cjm_model_catches_peru <- subset(catches_peru, CJM >= 0)

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

######################### fields selection (1 field CJM, 1 field for CHL-a, 1 field for SST)
dataset_cjm_model_peru$dataset_id <- 1
dataset_cjm_model_peru_fields <- data.frame(dataset_cjm_model_peru$dataset_id, dataset_cjm_model_peru$Year, dataset_cjm_model_peru$Month, dataset_cjm_model_peru$Day, dataset_cjm_model_peru$Latitude, dataset_cjm_model_peru$Longitude, dataset_cjm_model_peru$CJM, dataset_cjm_model_peru$seawifs.avg_r0, dataset_cjm_model_peru$avhrr.avg_r0)
colnames(dataset_cjm_model_peru_fields) <- c("dataset_id","Year","Month","Day","Latitude","Longitude","CJM","CHLA","SST")

dataset_cjm_model_chile$dataset_id <- 3
dataset_cjm_model_chile_fields <- data.frame(dataset_cjm_model_chile$dataset_id, dataset_cjm_model_chile$Year, dataset_cjm_model_chile$Month, dataset_cjm_model_chile$Day, dataset_cjm_model_chile$Latitude, dataset_cjm_model_chile$Longitude, dataset_cjm_model_chile$CJM, dataset_cjm_model_chile$seawifs.avg_r0, dataset_cjm_model_chile$avhrr.avg_r0)
colnames(dataset_cjm_model_chile_fields) <- c("dataset_id","Year","Month","Day","Latitude","Longitude","CJM","CHLA","SST")

dataset_cjm_model_netherlands$dataset_id <- 4
dataset_cjm_model_netherlands_fields <- data.frame(dataset_cjm_model_netherlands$dataset_id, dataset_cjm_model_netherlands$Year, dataset_cjm_model_netherlands$Month, dataset_cjm_model_netherlands$Day, dataset_cjm_model_netherlands$Latitude, dataset_cjm_model_netherlands$Longitude, dataset_cjm_model_netherlands$CJM, dataset_cjm_model_netherlands$modis.chla.avg_r0, dataset_cjm_model_netherlands$avhrr.avg_r0)
colnames(dataset_cjm_model_netherlands_fields) <- c("dataset_id","Year","Month","Day","Latitude","Longitude","CJM","CHLA","SST")

dataset_cjm_model_catches_peru$dataset_id <- 6
dataset_cjm_model_catches_peru_fields <- data.frame(dataset_cjm_model_catches_peru$dataset_id, dataset_cjm_model_catches_peru$Year, dataset_cjm_model_catches_peru$Month, dataset_cjm_model_catches_peru$Day, dataset_cjm_model_catches_peru$Latitude, dataset_cjm_model_catches_peru$Longitude, dataset_cjm_model_catches_peru$CJM, dataset_cjm_model_catches_peru$modis.chla.avg_r0, dataset_cjm_model_catches_peru$modis.sst.avg_r0)
colnames(dataset_cjm_model_catches_peru_fields) <- c("dataset_id","Year","Month","Day","Latitude","Longitude","CJM","CHLA","SST")

######################### concatenation records of the 4 datasets
dataset_cjm_model <- rbind(dataset_cjm_model_peru_fields, rbind(dataset_cjm_model_chile_fields,rbind(dataset_cjm_model_netherlands_fields,dataset_cjm_model_catches_peru_fields)))

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
