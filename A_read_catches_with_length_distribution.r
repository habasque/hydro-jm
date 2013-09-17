######################################################
### Exploratory program for catches data with length distribution
#-----------------------------------------------------
### Input format :
###       - year
###       - month 
###       - day
###       - time (optional)
###       - lat/lon
###       - catches
###       - SST (optional)
###       - length distribution
######################################################

source('C:/Workspace_R/_define_parameters.R')

# filename catches 
filename_catches <- paste(directory_data, 'Catches/', fleet, '/Catches_',year_start,'_',year_end,'_with_length_distribution.csv', sep="")
catches <- read.csv(filename_catches, header=TRUE, sep=";", dec=".")

# size frequency computation by year
size <- seq(15,60,1)


liste_year <- seq(2007,2013,1) 
prct_size_year <- data.frame(year = rep(liste_year, each = length(size)),size,freq=0)

#x11()
#par(mfrow = c(2, 1),cex=0.3)

indice_matrice <- 1 
for (i in 1:length(liste_year)) {
   current_year <- subset(catches, Year == liste_year[i])
   if (nrow(current_year)>0) {
   sum_number_by_size <- lapply(current_year[,9:length(current_year)], sum, na.rm=TRUE)
   sum_number_total <- sum(current_year[,9:length(current_year)],na.rm=TRUE)
   if (sum_number_total>0) {
      prct_by_size <- as.data.frame(sum_number_by_size) / sum_number_total
      #prct_by_size_tot[i,]<- prct_by_size 
      prct_values <- as.data.frame(t(prct_by_size))
      rownames(prct_values) <- NULL
      colnames(prct_values) <- c("prct")
      prct_size_year[indice_matrice:(indice_matrice+length(size)-1),]$freq <- prct_values$prct
      #x11()
      #plot(size, prct_by_size,type='l', ylab="Frequency (%)", xlab="Size (cm)",
      #     main=paste("Year - ", liste_year[i], sep=""))
   }
   }
   indice_matrice <- indice_matrice + length(size) 
}

#tentative ggplot
#ggplot(data=prct_size_year, aes(x=size,y=freq)  
#       + facet_grid(year~size))
x11()
qplot(size, freq, data=prct_size_year, 
      facets=year~., 
      geom="line",
      type='l',
      ylab="Frequency (%)", xlab="Size (cm)", 
      main = "Catches from dutch fleet - 2007-2013 - Length distribution by year")



