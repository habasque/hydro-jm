###D3_concat_biology_data_with_environmental_data

rm(list=ls())
source('C:/Workspace_R/_define_parameters.R')
library(doBy)
library(car)

oceanographic_file <- read.csv("E:/_data/all_biology_data_with_oceanographic_data.csv")
satellite_file <- read.csv("E:/_data/all_biology_data_with_satellite_data.csv")

oceanographic_file_sorted <- orderBy(~Year+Month+Day+Longitude+Latitude, data=oceanographic_file)
satellite_file_sorted <- orderBy(~Year+Month+Day+Longitude+Latitude, data=satellite_file)

all_data <- data.frame(satellite_file_sorted,oceanographic_file_sorted$depth_isotherme,oceanographic_file_sorted$depth_isoO2_2,oceanographic_file_sorted$prod_prim)
colnames(all_data) <- c(colnames(satellite_file_sorted),"depth_isotherme","depth_isoO2_2","prod_prim")
filenameout <- paste(directory_data,"all_biology_data_with_environmental_data.csv",sep="")
write.csv(all_data, file=filenameout, row.names=FALSE, quote=FALSE)

tapply(all_data$dataset_id, list(all_data$dataset_id), function(x) length(x))

################## DATA WITH ALL ENVIRONMENTAL PARAMETERS (WITHOUT O2)

data_with_all_parameters <- subset(all_data, !is.na(depth_isotherme) & !is.na(depth_isoO2_2) & !is.na(SST) & !is.na(CHLA))
tapply(data_with_all_parameters$dataset_id, list(data_with_all_parameters$dataset_id), function(x) length(x))
data_with_all_parameters$presence_absence <- recode(data_with_all_parameters$CJM, '0=0; else = 1', as.factor.result=FALSE)

filenameout <- paste(directory_data,"data_with_all_environmental_parameters.csv",sep="")
write.csv(data_with_all_parameters, file=filenameout, row.names=FALSE, quote=FALSE)







