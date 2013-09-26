######################################################
### Exploratory program for catches data
#-----------------------------------------------------
### Input format :
###       - year
###       - month 
###       - day
###       - time (optional)
###       - lat/lon
###       - catches
###       - SST (optional)
######################################################

source('C:/Workspace_R/_define_parameters.R')
library(sp)
library(ggplot2)

# filename catches 
filename_catches <- paste(directory_data, 'Catches/', fleet, '/Catches_',year_start,'_',year_end,'.csv', sep="")
catches <- read.csv(filename_catches, header=TRUE, sep=";", dec=".")
str(catches)
summary(catches)

#Adding distance to coast
catches$DistanceToCoast <- NaN * dim(catches)[1]
filename_shoreline_b <- subset(filename_shoreline, V2 <10 & V2 >-60 & V1 > -220 & V1 < -70)
xy.coast <- cbind(filename_shoreline_b$V1, filename_shoreline_b$V2)[!is.na(filename_shoreline_b$V1), ]
pts <- matrix(c(catches$Longitude,catches$Latitude), ncol = 2)
for (i in 1:nrow(pts)) {
  catches[i,]$DistanceToCoast <- min(spDistsN1(xy.coast, pts[i,], longlat = TRUE))
}

#Adding distance to shelfbreak
catches$DistanceToShelfbreak <- NaN * dim(catches)[1]
filename_shelfbreak <- subset(ShelfBreakPosition, Latitude <10 & Latitude >-60 &  Longitude> -220 & Longitude < -70)
xy.shelfbreak <- cbind(filename_shelfbreak$Longitude, filename_shelfbreak$Latitude)[!is.na(filename_shelfbreak$Longitude), ]
pts <- matrix(c(catches$Longitude,catches$Latitude), ncol = 2)
for (i in 1:nrow(pts)) {
  catches[i,]$DistanceToShelfbreak <- min(spDistsN1(xy.shelfbreak, pts[i,], longlat = TRUE))
}

################### Jack Mackerel selection ###################
catches_CJM <- subset(catches, CJM > 0)

#plot rond proportionnel a CJM
x11()
#qplot(Longitude, Latitude,data=catches_CJM, facets=Year~.,colour=CJM)+ scale_colour_gradient(low="red", high="blue")
qplot(Longitude, Latitude,data=catches_CJM, facets=Year~Month, main="Chilean catches distribution 2007-2012")

#number of data by year/month
tapply(catches_CJM$CJM, list(catches_CJM$Year, catches_CJM$Month), function(x) length(x))
tapply(catches_CJM$CJM, list(catches_CJM$Year), function(x) length(x))
tapply(catches_CJM$CJM, list(catches_CJM$Month), function(x) length(x))

#catches positions by year*month
x11()
qplot(Longitude,Latitude,data=catches_CJM, facets=Year~Month,
      main="Catches positions by year / month",
      xlab="Longitude", ylab="Latitude")
savePlot(filename=paste(directory_data, 'Catches/', fleet,'/Figures/catches_positions_by_year_month.png', sep=""),type =c("png"))       


ds_lon <- ddply(catches_CJM, .(Month), summarise, mean = mean(Longitude))
colnames(ds_lon) = c("Month","Longitude")
ds_lat <- ddply(catches_CJM, .(Month), summarise, mean = mean(Latitude))
colnames(ds_lat) = c("Month","Latitude")
mean_monthly_point <- merge(ds_lon,ds_lat)
filenameout <- paste(directory_data, 'Catches/', fleet,'/mean_monthly_point.csv',sep="")
write.csv(mean_monthly_point, file=filenameout,row.names=FALSE)

x11()
p <- qplot(Longitude,Latitude,data=catches_CJM, facets=Year~Month,
      main="Catches positions by year / month",
      xlab="Longitude", ylab="Latitude")
p + geom_point(data = mean_monthly_point,col="red")


#total catches by year/month
a <- tapply(catches_CJM$CJM, list(catches_CJM$Year, catches_CJM$Month), sum)
b <- as.data.frame(matrix(NA,dim(a)[1]*dim(a)[2],3))
colnames(b) = c("Year","Month","CJM")
b$CJM = as.vector(a)
b$Year = rep(dimnames(a)[[1]],dim(a)[2])
b$Month = rep(dimnames(a)[[2]], each = dim(a)[1],1)
#library(doBy)
b <- orderBy(~Year, b)
bsansNA <- subset(b, !is.na(CJM))
bsansNA$CJM <- as.numeric(bsansNA$CJM)
bsansNA$Year <- as.numeric(bsansNA$Year)
bsansNA$Month <- as.numeric(bsansNA$Month)

interaction.plot(bsansNA$Year, bsansNA$Month, bsansNA$CJM, fun=mean, 
                 col=cl[seq(550,(550+(4*(length(split(bsansNA,bsansNA$Month))-1))),by=4)],
                 trace.label = "Month", xlab ="year", ylab = "CJM Catches")
x11()
qplot(sum(CJM[!is.na(CJM)]),data=catches_CJM, facets=Year~Month,binwidth = 0.5,
      main="Catches by year / month",
      xlab="", ylab="Total catches")

# map by year
nbGraphParPage = 1
nb_groupe = ceiling(length(unique(catches_CJM$Year))/nbGraphParPage);
indice_debut = 1;
indice_fin = nbGraphParPage;
nbYear = length(unique(catches_CJM$Year))
listeYear = unique(catches_CJM$Year)

for (i in 1:nb_groupe) {
    listeYearGroupe = listeYear[indice_debut:indice_fin]
    listeYearGroupe = listeYearGroupe[!is.na(listeYearGroupe)]
    x11()
    par(mfrow = c(ceiling(length(listeYearGroupe)/2),1), cex=0.4, ps=24)
    for (j in 1:length(listeYearGroupe)) {
       captures_Year = subset(catches_CJM, Year == listeYearGroupe[j])
       captures_Year$Catches = as.numeric(captures_Year$Catches)
       captures_Year = subset(captures_Year, Catches > 0)
       captures_Year$Catches = log(captures_Year$Catches)
       plot(captures_Year$Longitude,captures_Year$Latitude, col = catches_CJM$Catches, main = paste('Catches ', fleet, ' - ', specie, ' \n ', listeYearGroupe[j], sep =''), xlim= c(lon_min,lon_max),ylim = c(lat_min,lat_max), xlab='Longitude', ylab ='Latitude')
       #symbols(captures_Year$Longitude, captures_Year$Latitude, captures_Year$Quantity/10000, add= TRUE,lwd=3)
       lines(coast)
       savePlot(filename=paste(directory_data, 'Catches/', fleet,'/Figures/catches_',listeYearGroupe[j],'.png', sep=""),type =c("png"))       
    }
    indice_debut = indice_debut + nbGraphParPage;
    indice_fin = indice_fin + nbGraphParPage;
}

#distance to coast
x11()
hist(catches_CJM$DistanceToCoast,n=100, main = paste(dataset, " - CJM \nDistance to coast"), xlab="Distance to coast (kms)")
#by month
listeMonth <- unique(catches_CJM$Month)
for (i in 1:length(listeMonth)) {
  CJM_month <- subset(catches_CJM, as.numeric(Month) == as.numeric(listeMonth[i]))
  print(listeMonth[i])
  print(summary(CJM_month$DistanceToCoast))
  print(quantile(CJM_month$DistanceToCoast, probs=c(0.025)))
  print(quantile(CJM_month$DistanceToCoast, probs=c(0.975)))
}
a <- tapply(catches_CJM$DistanceToCoast, list(catches_CJM$Month), min)
distmin <- as.data.frame(matrix(NA,dim(a)[1],2))
colnames(distmin) = c("Month","DistanceToCoast")
distmin$Month = rep(dimnames(a)[[1]])
distmin$DistanceToCoast = as.vector(a,"numeric")

a <- tapply(catches_CJM$DistanceToCoast, list(catches_CJM$Month), max)
distmax <- as.data.frame(matrix(NA,dim(a)[1],2))
colnames(distmax) <- c("Month","DistanceToCoastMax")
distmax$Month = rep(dimnames(a)[[1]])
distmax$DistanceToCoastMax = as.vector(a,"numeric")


x11()
plot(distmin$Month, distmin$DistanceToCoast, type= 'l', main = paste(dataset, " - CJM \nDistance to coast minimum by month "), xlab = "Month")
par(new=TRUE)
plot(distmax$Month, distmax$DistanceToCoast, type= 'l')
#by year/month
tapply(catches_CJM$DistanceToCoast, list(catches_CJM$Year, catches_CJM$Month)), min)
tapply(catches_CJM$DistanceToCoast, list(catches_CJM$Year, catches_CJM$Month)), max)

#distance to shelfbreak


### catches histogram
x11()
hist(catches_CJM$CJM,100, xlab=paste('Catches (', catches_unit, ')', sep=""), main = paste(specie," - Catches distribution", sep=""))
savePlot(filename=paste(directory_data, 'Catches/', fleet,'/Figures/Catches_distribution.png', sep=""),type =c("png"))   

####################################################################################################
### SST histogram (optional)
if (length(unique(catches_CJM$SST))) {
   x11()
   hist(catches_CJM$SST,100, xlab ='SST', main = paste(specie," - SST distribution", sep=""))
   savePlot(filename=paste(directory_data, 'Catches/', fleet,'/Figures/SST_distribution.png', sep=""),type =c("png"))   
}

### SST statistics
summary(catches_CJM$SST[!is.na(catches_CJM$SST)])
tapply(catches_CJM$SST[!is.na(catches_CJM$SST)], catches_CJM$Month[!is.na(catches_CJM$SST)], min, na.rm =T)
tapply(catches_CJM$SST[!is.na(catches_CJM$SST)], catches_CJM$Month[!is.na(catches_CJM$SST)], max, na.rm =T)
tapply(catches_CJM$SST[!is.na(catches_CJM$SST)], catches_CJM$Year[!is.na(catches_CJM$SST)], min, na.rm =T)
tapply(catches_CJM$SST[!is.na(catches_CJM$SST)], catches_CJM$Year[!is.na(catches_CJM$SST)], max, na.rm =T)

#quantiles
quantile(catches_CJM$SST[!is.na(catches_CJM$SST)], probs=c(0.025))
quantile(catches_CJM$SST[!is.na(catches_CJM$SST)], probs=c(0.975))

listeMonth = unique(catches_CJM$Month[!is.na(catches_CJM$SST)])
for (i in 1:length(listeMonth)) {
  test_CJM_month = subset(catches_CJM, as.numeric(Month) == as.numeric(listeMonth[i]))
  print(listeMonth[i])
  print(quantile(test_CJM_month$SST[!is.na(test_CJM_month$SST)], probs=c(0.025)))
  print(quantile(test_CJM_month$SST[!is.na(test_CJM_month$SST)], probs=c(0.975)))
}

### SST vs catches
x11()
plot(catches_CJM$SST, log(catches_CJM$Catches), main = paste("Catches (log) ", specie, ' in relation with SST\n', fleet, ' - ', year_start,' - ',year_end ,sep=""), xlab ="SST (Deg °C)", ylab ="log(Catches)", col = 'black')
savePlot(filename=paste(directory_data, 'Catches/', fleet,'/Figures/SST_vs_catches.png', sep=""),type =c("png"))


################### NO...Jack Mackerel selection ###################
catches_no_CJM = subset(catches, Catches == 0)

### SST no Jack Mackerel
if (length(unique(catches_no_CJM$SST))) {
  x11()
  hist(catches_no_CJM$SST,100, xlab ='SST', main = paste("SST distribution without ", specie, sep=""))
  savePlot(filename=paste(directory_data, 'Catches/', fleet,'/Figures/SST_distribution_Without',specie,'.png', sep=""),type =c("png"))   
}


