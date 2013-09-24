######################################################
######## NCEP iso O2 period comparison
#-----------------------------------------------------
# periode1 : 1980-1990
# periode2 : 2000-2008
######################################################

rm(list=ls())

source('C:/Workspace_R/_define_parameters.R')

#lecture des données mensuelles NCEP - 2 degrees resolution
library(ncdf)
library(rworldmap)
data(countriesLow)

nlevel_contour <- 30

for (ind_month in 1:length(month.name)) {

  month <- month.name[ind_month]

#########################################
periode1 <- "1980-1990"
directory_profiles <- paste('E:/_data/isoO2_2_NCEP/',periode1,'/',month,"/", sep="")
list_files = dir(directory_profiles,'*.nc')
nb_files_old = length(list_files)

variable_tot_old <- matrix(0,76,33)
nb_values <- matrix(0,76,33)
  
# for each profile
for (indice_file in 1:nb_files_old) {
  name_file = list_files[indice_file]
  file_isoline = open.ncdf(paste(directory_profiles,name_file,sep=""))
  lon = get.var.ncdf(file_isoline, "longitude")          # coordinate variable
  lat = get.var.ncdf(file_isoline, "latitude")          # coordinate variable
  variable = get.var.ncdf(file_isoline, "isoO2_2")   # variable
  variable[is.na(variable)] = 0;
  nb_values[variable>0] = nb_values[variable > 0] + 1;
  variable_tot_old = variable_tot_old + variable
  ### transformation longitude
  lon = lon - 360;
  close.ncdf(file_isoline)
}

variable_mean_old <- matrix(0,76,33)
variable_mean_old <- variable_tot_old / nb_values
#variable_mean_old[is.na(variable_mean_old)] <- 1
#variable_mean_old[nb_values>0] <- variable_tot_old[nb_values>0] / nb_values[nb_values>0]
 
# x11()
# filled.contour(lon,lat,variable_mean_old, xlim = c(-110,-70), ylim = c(-40, 0),zlim = range(c(0,200), finite = TRUE),
#                color = terrain.colors, asp = 1, nlevels = nlevel_contour, 
#                key.title = title(main = "Depth\n(meters)"),
#                plot.axes = {axis(1); axis(2) ; 
#                             contour(lon,lat,variable_mean_old, add = T, lwd = 1.5,nlevels = nlevel_contour);
#                             plot(countriesLow,add=T,col="white")
#                             },
#                main=paste("1985-1995 (",month,") NCEP data 2 degrees resolution \n Isodepth O2 2mL/L",sep=""), xlab = "Longitude", ylab="Latitude")

#########################################
periode2 <- "2000-2008"
directory_profiles <- paste('E:/_data/isoO2_2_NCEP/',periode2,'/',month, "/",sep="")
list_files = dir(directory_profiles,'*.nc')
nb_files_new = length(list_files)

variable_tot_new <- matrix(0,76,33)
nb_values_new <- matrix(0,76,33)

# for each profile
for (indice_file in 1:nb_files_new) {
  name_file = list_files[indice_file]
  file_isoline = open.ncdf(paste(directory_profiles,name_file,sep=""))
  lon = get.var.ncdf(file_isoline, "longitude")          # coordinate variable
  lat = get.var.ncdf(file_isoline, "latitude")          # coordinate variable
  variable = get.var.ncdf(file_isoline, "isoO2_2")   # variable
  variable[is.na(variable)] = 0;
  nb_values_new[variable>0] = nb_values_new[variable > 0] + 1;
  variable_tot_new = variable_tot_new + variable
  ### transformation longitude
  lon = lon - 360;
  close.ncdf(file_isoline)
}

#nb_values_new[nb_values_new<length(list_files)] <- 0
#variable_tot_new[nb_values_new<length(list_files)] <- 0
variable_mean_new <- variable_tot_new / nb_values_new

# x11()
# filled.contour(lon,lat,variable_mean_new, xlim = c(-110,-70), ylim = c(-40, 0),zlim = range(c(0,200), finite = TRUE),
#                color = terrain.colors, asp = 1, nlevels = nlevel_contour, 
#                key.title = title(main = "Depth\n(meters)"),
#                plot.axes = {axis(1); axis(2);  
#                             contour(lon,lat,variable_mean_new, add = T, lwd = 1.5,nlevels = nlevel_contour);
#                             plot(countriesLow,add=T,col="white")},
#                main=paste(periode2," (",month,") NCEP data 2 degrees resolution \n Isodepth O2 2mL/L",sep=""), xlab = "Longitude", ylab="Latitude")

##########################################
# difference, compression ?
 
variable_diff_old_new <- variable_mean_old - variable_mean_new
variable_diff_old_new[is.na(variable_diff_old_new)] <- 0
  
x11()
filled.contour(lon,lat,variable_diff_old_new, 
               nlevels=nlevel_contour,
               xlim = c(-110,-70), ylim = c(-40, 0),zlim=c(-100,100),
               color = terrain.colors, 
               key.title = title(main = "Depth\n(meters)"),
               plot.axes = {axis(1); axis(2); 
                            contour(lon,lat,variable_diff_old_new,add = T, lwd = 1.5,nlevels=nlevel_contour);
                            plot(countriesLow,add=T,col="white")
                            },
               main=paste("Isodepth O2 2mL/L evolution\n", periode1," - ",periode2," (",month.name[ind_month],") \n NCEP data - 2 degrees resolution",sep=""),
               xlab = "Longitude", ylab="Latitude")

indice_compression <- which(variable_diff_old_new >0)
summary(variable_mean_new[indice_compression]) #depth in the 2000's

}

#hist(variable_mean_new[indice_compression],50)
  
  