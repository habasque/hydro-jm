##################################################
###### Analysis program for catches data 
# INPUT : 
# - dataset_satellite issu de B0 (replace , by . for numeric fields)
# OUTPUT : 
# - catches distribution
##################################################

source('C:/Workspace_R/_define_parameters.R')

library(tweedie)
library(mgcv)#package gam
library(scatterplot3d)
library(ggplot2)

col1 = "black"
col2 = "red"
col3 = "blue"
col4 = "green"

directory_figures <- paste(directory_data, 'Catches/', fleet,'/figures',sep="")
                                          
#read matrix
directory_catches_with_satellite <- paste(directory_data, 'Catches/', fleet, '/With_Satellite_Data/', sep="")
filename_catches_satellite <- paste(directory_catches_with_satellite, 'catches_2011_2013_with_satellite_data.csv', sep="")
catches_satellite <- read.csv2(filename_catches_satellite, sep=";", dec=".",header=T)

#fields format
catches_satellite$Longitude = as.numeric(catches_satellite$Longitude)
catches_satellite$Latitude = as.numeric(catches_satellite$Latitude)
catches_satellite$Year = as.numeric(catches_satellite$Year)
catches_satellite$Month = as.numeric(catches_satellite$Month)
catches_satellite$Day = as.numeric(catches_satellite$Day)
catches_satellite$CJM = as.numeric(catches_satellite$CJM)
#catches_satellite$Anch = as.numeric(catches_satellite$Anch)
#catches_satellite$Sard = as.numeric(catches_satellite$Sard)
#catches_satellite$Cab = as.numeric(catches_satellite$Cab)
catches_satellite$SST = as.numeric(catches_satellite$SST)

catches_satellite$Bathymetry = as.numeric(catches_satellite$Bathymetry)
#catches_satellite$DC = as.numeric(catches_satellite$DC)
#catches_satellite$DSB = as.numeric(catches_satellite$DSB)
#catches_satellite$Depth = as.numeric(catches_satellite$Depth)

if ("seawifs" %in% sensors_parameters){
catches_satellite$seawifs.avg_r0 = as.numeric(catches_satellite$seawifs.avg_r0)
catches_satellite$seawifs.avg_r1 = as.numeric(catches_satellite$seawifs.avg_r1)
catches_satellite$seawifs.avg_r2 = as.numeric(catches_satellite$seawifs.avg_r2)
catches_satellite$seawifs.std_r2 = as.numeric(catches_satellite$seawifs.std_r2)
catches_satellite$seawifs.mode_r2 = as.numeric(catches_satellite$seawifs.mode_r2)
catches_satellite$seawifs.min_r2 = as.numeric(catches_satellite$seawifs.min_r2)
catches_satellite$seawifs.max_r2 = as.numeric(catches_satellite$seawifs.max_r2)
}
if ("avhrr" %in% sensors_parameters){
catches_satellite$avhrr.avg_r0 = as.numeric(catches_satellite$avhrr.avg_r0)
catches_satellite$avhrr.avg_r1 = as.numeric(catches_satellite$avhrr.avg_r1)
catches_satellite$avhrr.avg_r2 = as.numeric(catches_satellite$avhrr.avg_r2)
catches_satellite$avhrr.std_r2 = as.numeric(catches_satellite$avhrr.std_r2)
catches_satellite$avhrr.mode_r2 = as.numeric(catches_satellite$avhrr.mode_r2)
catches_satellite$avhrr.min_r2 = as.numeric(catches_satellite$avhrr.min_r2)
catches_satellite$avhrr.max_r2 = as.numeric(catches_satellite$avhrr.max_r2)
}

catches_satellite$modis.sst.avg_r0 = as.numeric(catches_satellite$modis.sst.avg_r0)
catches_satellite$modis.sst.avg_r1 = as.numeric(catches_satellite$modis.sst.avg_r1)
catches_satellite$modis.sst.avg_r2 = as.numeric(catches_satellite$modis.sst.avg_r2)
catches_satellite$modis.sst.std_r2 = as.numeric(catches_satellite$modis.sst.std_r2)
catches_satellite$modis.sst.mode_r2 = as.numeric(catches_satellite$modis.sst.mode_r2)
catches_satellite$modis.sst.min_r2 = as.numeric(catches_satellite$modis.sst.min_r2)
catches_satellite$modis.sst.max_r2 = as.numeric(catches_satellite$modis.sst.max_r2)

catches_satellite$modis.chla.avg_r0 = as.numeric(catches_satellite$modis.chla.avg_r0)
catches_satellite$modis.chla.avg_r1 = as.numeric(catches_satellite$modis.chla.avg_r1)
catches_satellite$modis.chla.avg_r2 = as.numeric(catches_satellite$modis.chla.avg_r2)
catches_satellite$modis.chla.std_r2 = as.numeric(catches_satellite$modis.chla.std_r2)
catches_satellite$modis.chla.mode_r2 = as.numeric(catches_satellite$modis.chla.mode_r2)
catches_satellite$modis.chla.min_r2 = as.numeric(catches_satellite$modis.chla.min_r2)
catches_satellite$modis.chla.max_r2 = as.numeric(catches_satellite$modis.chla.max_r2)

######## Jack Mackerel selection

#catches_CJM = subset(catches_satellite, Anch == 0 & Sard == 0 & CJM > 0)
catches_CJM <- subset(catches_satellite, CJM > 0)

#mapa de las posiciones por ano/mes
x11(width=15,height=8,pointsize=12)
p <-qplot(Longitude, Latitude, data=catches_CJM, facets=Year~Month,xlim=c(-82,-70),ylim =c(-20,-5),xlab = 'Longitude', ylab = 'Latitude',main="Catches positions 2011-2013", col="red")
p + theme_bw()
p + geom_line(data = filename_shoreline, colour="black", size = 1)
savePlot(filename=paste(directory_figures,"/catches_year_month.png",sep=""),type =c("png"))


### parameter statistics
for (i in 1:length(sensors_parameters)){
  
  field <- paste("catches_CJM$",sensors_parameters[i],".avg_r0",sep="")
  dataset_sensor <- subset(catches_CJM, !is.na(eval(parse(text=field))))
  
  # outliers deletion
  outliers <- 0
  if (parameter[i] == 'SST') {    
    diff_avgr0_avgr2 <- eval(parse(text=paste("catches_CJM$",sensors_parameters[i],".avg_r2",sep=""))) - eval(parse(text=paste("catches_CJM$",sensors_parameters[i],".avg_r0",sep="")))
    outliers <- subset(catches_CJM, eval(parse(text=paste(sensors_parameters[i],".std_r2",sep=""))) >2 | abs(diff_avgr0_avgr2) >2)
    print(paste("Number of outliers ", dim(outliers)[1], sep=""))
    catches_CJM <- subset(catches_CJM, !Key %in% unique(outliers$Key))
  }  
  
  summary(eval(parse(text=paste("catches_CJM$",sensors_parameters[i],".avg_r0",sep=""))))
  summary(eval(parse(text=paste("catches_CJM$",sensors_parameters[i],".avg_r1",sep=""))))
  summary(eval(parse(text=paste("catches_CJM$",sensors_parameters[i],".avg_r2",sep=""))))
  
  field <- paste("catches_CJM$",sensors_parameters[i],".avg_r0",sep="")
  tapply(eval(parse(text=field))[!is.na(eval(parse(text=field)))], catches_CJM$Month[!is.na(eval(parse(text=field)))], min, na.rm =T)
  tapply(eval(parse(text=field))[!is.na(eval(parse(text=field)))], catches_CJM$Month[!is.na(eval(parse(text=field)))], max, na.rm =T)
  tapply(eval(parse(text=field))[!is.na(eval(parse(text=field)))], catches_CJM$Year[!is.na(eval(parse(text=field)))], min, na.rm =T)
  tapply(eval(parse(text=field))[!is.na(eval(parse(text=field)))], catches_CJM$Year[!is.na(eval(parse(text=field)))], max, na.rm =T)
  
  tapply(eval(parse(text=field))[!is.na(eval(parse(text=field)))], catches_CJM$Year[!is.na(eval(parse(text=field)))], function(x) length(unique(x)))
  tapply(eval(parse(text=field))[!is.na(eval(parse(text=field)))], catches_CJM$Month[!is.na(eval(parse(text=field)))], function(x) length(unique(x)))
  
  #quantiles
  quantile(eval(parse(text=field))[!is.na(eval(parse(text=field)))], probs=c(0.025))
  quantile(eval(parse(text=field))[!is.na(eval(parse(text=field)))], probs=c(0.975))
  
  listeMonth = unique(dataset_sensor$Month)
  for (i_month in 1:length(listeMonth)) {
    CJM_month = subset(dataset_sensor, as.numeric(Month) == as.numeric(listeMonth[i_month]))
    print(listeMonth[i_month])
    field <- paste("CJM_month$",sensors_parameters[i],".avg_r0",sep="")
    print(quantile(eval(parse(text=field))[!is.na(eval(parse(text=field)))], probs=c(0.025)))
    print(quantile(eval(parse(text=field))[!is.na(eval(parse(text=field)))], probs=c(0.975)))
  }
  
  listeYear = unique(dataset_sensor$Year)
  for (i_year in 1:length(listeYear)) {
    CJM_year = subset(dataset_sensor, as.numeric(Year) == as.numeric(listeYear[i_year]))
    print(listeYear[i_year])
    field <- paste("CJM_year$",sensors_parameters[i],".avg_r0",sep="")
    print(quantile(eval(parse(text=field))[!is.na(eval(parse(text=field)))], probs=c(0.025)))
    print(quantile(eval(parse(text=field))[!is.na(eval(parse(text=field)))], probs=c(0.975)))
  }
  
  #catches distribution in relation with sensor parameter
  field <- paste("catches_CJM$",sensors_parameters[i],".avg_r0",sep="")
  year_start <- min(unique(dataset_sensor$Year))
  year_end <- max(unique(dataset_sensor$Year))
  x11()
  hist(eval(parse(text=field)),100,main = paste(dataset,"\nDistribution ", parameter[i], " ", sensor[i], " for CJM presence - ", year_start, "-", year_end, " \n", temporal_resolution[i]," - ", spatial_resolution[i], " resolution",sep=""), xlab = paste(parameter[i], "(",sensors_unit[i],")" ,sep=""), ylab ="Number of fishing sets", col = col1, sub = paste("Number of outliers deleted : ", dim(outliers)[1]))
  
  x11()
  field <- paste("catches_CJM$",sensors_parameters[i],".avg_r0",sep="")
  plot(eval(parse(text=field)), log(catches_CJM$CJM+1), main = paste(dataset,"\nCJM catches (log) in relation with ", parameter[i], " ", sensor[i], " - ", year_start, "-", year_end, "\n", temporal_resolution[i]," - ", spatial_resolution[i], " resolution",sep=""), xlab = paste(parameter[i], "(",sensors_unit[i],")" ,sep=""), ylab ="Log(Catches (in tons))", col = col1, sub = paste("Number of outliers deleted : ", dim(outliers)[1]))
  field <- paste("catches_CJM$",sensors_parameters[i],".avg_r1",sep="")
  points(eval(parse(text=field)), log(catches_CJM$CJM+1), col = col2)
  field <- paste("catches_CJM$",sensors_parameters[i],".avg_r2",sep="")
  points(eval(parse(text=field)), log(catches_CJM$CJM+1), col = col3)
  if (parameter[i] == 'SST' & !is.null(catches_CJM$CJM)) {
    points(dataset_sensor$SST, log(dataset_sensor$CJM+1), col = col4)
    legend("topright", c("avg rad=0", "avg rad=1", "avg rad=2", paste(parameter[i], " in-situ",sep="")), lwd = 2, col = c(col1, col2, col3, col4))  
  } else {
    legend("topright", c("avg rad=0", "avg rad=1", "avg rad=2"), lwd = 2, col = c(col1, col2, col3))  
  }
}

###################################################################
################ Modelisation using all Jack Mackerel data points
###################################################################

#seawifs data + all jack mackerel catches (successfull or not)
dataset_cjm_model <- subset(catches_satellite, !is.na(seawifs.avg_r0) & (Anch == 0 & Sard == 0 & CJM >= 0))
dataset_cjm_model$presence_absence = matrix(0, dim(dataset_cjm_model)[1])
dataset_cjm_model$presence_absence[dataset_cjm_model$CJM>0] = 1

#spline
x11()
#plot(dataset_cjm_model$seawifs.avg_r0, log(dataset_cjm_model$CJM+1))
#par(new=TRUE) 
splineJackMackerel <- smooth.spline(dataset_cjm_model$seawifs.avg_r0, log(dataset_cjm_model$CJM+1), df = 5)
plot(splineJackMackerel, type="l",xlab= 'CHL-a' , ylab = 'log(Sa)')

############ gam univariate CJM ~ CHL-a

#gam on presence/absence field with binomial family
f1 <- formula(presence_absence~s(seawifs.avg_r0))
gam_presence_absence_binomial <- gam(as.factor(presence_absence)~s(seawifs.avg_r0), data=dataset_cjm_model,na.action=na.exclude, family=binomial)
x11()
plot(gam_presence_absence_binomial, se=T, main ="GAM on presence/absence field, family binomial")
x11()
plot(gam_presence_absence_binomial, se=T, xlim=c(0,1))

#gam tweedie
out <- tweedie.profile(log(dataset_cjm_model$CJM+1)~dataset_cjm_model$seawifs.avg_r0,p.vec=seq(1.1, 1.4, length=9))
pa <- out$p.max #distribution found by tweedie
gam_tweedie <- gam(log(CJM+1)~s(seawifs.avg_r0), data=dataset_cjm_model,na.action=na.exclude, family=Tweedie(pa,link="log"), control = gam.control(irls.reg=0.0,mgcv.tol=1e-6,mgcv.half=15))
x11()
plot(gam_tweedie, se=T, main ="GAM, family tweedie")
x11()
plot(gam_tweedie, se=T, xlim=c(0,5))

############ gam multivariate CJM ~ CHL-a + SST
field_chla <- "modis.chla.avg_r2"
field_sst <- "avhrr.avg_r2"
dataset_cjm_model_multi <- subset(catches_satellite, !is.na(eval(parse(text=field_chla))) & !is.na(eval(parse(text=field_sst))) & (CJM >= 0))

#3D representation
#x11()
#s3d <- scatterplot3d(dataset_cjm_model_multi$seawifs.avg_r0, dataset_cjm_model_multi$avhrr.avg_r0, log(dataset_cjm_model_multi$CJM+1), xlab="CHL-a SeaWiFS", ylab="SST AVHRR", zlab="Jack Mackerel Sa")

gam3d <- gam(log(CJM+1)~s(modis.chla.avg_r2, avhrr.avg_r2), data=dataset_cjm_model_multi)
x11()
vis.gam(gam3d,ticktype="detailed",theta=-35)
x11()
vis.gam(gam3d, view=c("modis.chla.avg_r2","avhrr.avg_r2"),plot.type="contour",color="topo")

#gam3db <- gam(log(CJM+1)~s(avhrr.avg_r0,seawifs.avg_r0), data=dataset_cjm_model_multi)
#x11()
#vis.gam(gam3db,ticktype="detailed")

#gam tweedie
pa <- 1.27
f1 <- formula(log(CJM+1) ~ s(seawifs.avg_r0) + s(avhrr.avg_r0))
gam_tweedie_CHLA_SST <- gam(f1, data=dataset_cjm_model_multi,na.action=na.exclude, family=Tweedie(pa,link="log"), control = gam.control(irls.reg=0.0,mgcv.tol=1e-6,mgcv.half=15))
x11()
plot(gam_tweedie_CHLA_SST, se=T)
x11()
plot(gam_tweedie_CHLA_SST, se=T, xlim=c(0,1))

#zero-inflated model : CJM ~ CHL-a + SST
library(pscl) # a placer ici car sinon conflit avec library mgcv
f1 <- formula(round(CJM) ~ seawifs.avg_r0 + avhrr.avg_r0) 
zip1 <- zeroinfl(f1, dist = "poisson", link = "logit", data=dataset_cjm_model_multi)
summary(zip1)
nb1 <- zeroinfl(f1, dist = "negbin", link = "logit", data=dataset_cjm_model_multi)
summary(nb1)
#library(lmtest)
#lrtest(zip1,nb1)

#x11()
#plot(zinf,se=T)

#hurdle model
hurd <- hurdle(presence_absence ~ s(seawifs.avg_r0) + s(avhrr.avg_r0), data=dataset_cjm_model_multi)
summary(hurd)

#glm
model_lm = glm(log(dataset_cjm_model_multi$CJM+1)~dataset_cjm_model_multi$seawifs.avg_r0+dataset_cjm_model_multi$avhrr.avg_r0);
summary(model_lm)
x11()
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(model_lm)      # Residuals, Fitted, ...
par(opar)


########### 
#if (parameter == 'sst') {
# Parameter difference between sensor and fleet
# catches_CJM$SST_IMARPE_MODIS_difference = catches_CJM$avg_r0 - catches_CJM$SST
#  x11()
#  hist(catches_CJM$SST_IMARPE_MODIS_difference, 100, main="SST difference between MODIS and fleet measurement", xlab = "SST (Deg °C)")

#  x11()
#  plot(catches_CJM$SST, type='l', ylim=c(14,30), main = "SST measured by MODIS and by IMARPE - 1983-2008", ylab = "SST")
#  par(ann=FALSE,new=TRUE) 
#  plot(catches_CJM$avg_r0, type='l', col='red', ylim=c(14,30))
#  legend("topright", c("SST IMARPE", "SST MODIS avg_r0"), lwd = 2, col = c(col1, col2))
#}