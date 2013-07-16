######################################################
### Modelling horizontal habitat with environmental parameters
#-----------------------------------------------------
# INPUT: global dataset of CJM presence (output of D_concat_biology_data_with_satellite_data)
#        - 16008 occurrences
######################################################
source('C:/Workspace_R/_define_parameters.R')

library(quantreg)
library(doBy)

### reading main dataset
filename_acoustic_satellite_peru <- paste(directory_data,'all_biology_data_with_satellite_data.csv',sep="")
dataset_cjm_model_sans_na <- read.csv2(filename_acoustic_satellite_peru, sep=",", dec=".",header=T)

dataset_cjm_model_sans_na_presence <- subset(dataset_cjm_model_sans_na, presence_absence ==1)
dataset_cjm_model_sans_na_absence <- subset(dataset_cjm_model_sans_na, presence_absence ==0)

######################### Fonction y(x) <- ax / (1 + bx)
x11()
plot(dataset_cjm_model_sans_na_presence$CHLA,dataset_cjm_model_sans_na_presence$SST,xlab = "CHLA", ylab = "SST", xlim=c(0,5),ylim=c(9,30), 
     main="South Pacific area - All datasets - 1983-2011 - CJM catches / acoustic detections\nCHL-a*SST interaction\nFormule=y[i] <- ((750*x[i])/(1+(max_SST*x[i]))")
x <- seq(min(dataset_cjm_model_sans_na_presence$CHLA),5,by=0.1)
min_CHLA <- 0.07
max_SST <- 26
for (i in 1:length(x)){
  y[i] <- ((750*x[i])/(1+(max_SST*x[i])))
}
points(x,y,col="red",type='l')

######################### Fonction y(x) <- a/b * tanh(bx)
x11()
plot(dataset_cjm_model_sans_na_presence$CHLA,dataset_cjm_model_sans_na_presence$SST,xlab = "CHLA", ylab = "SST", xlim=c(0,5),ylim=c(9,30), 
     main="South Pacific area - All datasets - 1983-2011 - CJM catches / acoustic detections\nCHL-a*SST interaction\nFormule=y[i] <- ((750*x[i])/(1+(max_SST*x[i]))")
x <- seq(min(dataset_cjm_model_sans_na_presence$CHLA),5,by=0.1)
min_CHLA <- 0.07
max_SST <- 25
for (i in 1:length(x)){
  y[i] <- (700/max_SST)*(tanh(max_SST*x[i]))
}
points(x,y,col="red",type='l')



######################### Quantile regression
x11()
plot(dataset_cjm_model_sans_na_presence$CHLA,dataset_cjm_model_sans_na_presence$SST,xlab = "CHLA", ylab = "SST", xlim=c(0,5),ylim=c(9,30), main="CHL-a*SST interaction")
#points(dataset_cjm_model_sans_na_absence$CHLA, dataset_cjm_model_sans_na_absence$SST, col="orange")
X <- model.matrix(dataset_cjm_model_sans_na_presence$SST ~ bs(dataset_cjm_model_sans_na_presence$CHLA, df=15))
fit <- rq(SST ~ bs(CHLA, df=15), tau=0.95, data=dataset_cjm_model_sans_na_presence)
SST.fit <- X %*% fit$coef
points(dataset_cjm_model_sans_na_presence$CHLA,SST.fit,col="red")
fit <- rq(SST ~ bs(CHLA, df=15), tau=0.01, data=dataset_cjm_model_sans_na_presence)
SST.fit <- X %*% fit$coef
points(dataset_cjm_model_sans_na_presence$CHLA,SST.fit,col="blue")


######################### test rqss
dataset_cjm_model_sans_na_presence_sorted <- orderBy(~CHLA, dataset_cjm_model_sans_na_presence)
x11()
plot(dataset_cjm_model_sans_na_presence_sorted$CHLA,dataset_cjm_model_sans_na_presence_sorted$SST,
     xlab = "CHLA", ylab = "SST", xlim=c(0,2),ylim=c(9,30), main="CHL-A*SST interaction - Quantile regression tx = 5% and 95%")
#
fit <- rqss(SST ~ qss(CHLA, constraint="N"), tau = 0.95, data=dataset_cjm_model_sans_na_presence_sorted)
lines(smooth.spline(unique(dataset_cjm_model_sans_na_presence_sorted$CHLA[-1]), fit$coef[1] + fit$coef[-1]), col="blue")
#
fit2 <- rqss(SST ~ qss(CHLA, constraint="N"), tau = 0.05, data=dataset_cjm_model_sans_na_presence_sorted)
lines(smooth.spline(unique(dataset_cjm_model_sans_na_presence_sorted$CHLA[-1]), fit2$coef[1] + fit2$coef[-1]), col="black")
#
lines(c(0.047,0.0471),c(min(fit2$coef[1] + fit2$coef[-1]),min(fit$coef[1] + fit$coef[-1])),col="red")
lines(quantile(dataset_cjm_model_sans_na_presence_sorted$CHLA,probs=c(0.01)))

######################### modelisation glm multivariate CJM ~ CHLA + SST + CHLA*SST
dataset_cjm_model_sans_na <- subset(dataset_cjm_model_sans_na, CHLA <8)
model_glm <- glm(presence_absence ~ CHLA + SST + CHLA*SST, family=binomial(link="logit"),data=dataset_cjm_model_sans_na)
summary(model_glm)
anova(model_glm,test="Chisq")
x11()
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(model_glm)

#### prediction
newCHLA <- seq(min(dataset_cjm_model_sans_na$CHLA),max(dataset_cjm_model_sans_na$CHLA),le=10)
newSST  <- seq(min(dataset_cjm_model_sans_na$SST),max(dataset_cjm_model_sans_na$SST),le=10)
newData <- expand.grid(CHLA=newCHLA, SST=newSST)
head(newData)
newresult <- predict(model_glm,newData,type="response")
test <- cbind(newData, newresult)

newresult <- matrix(newresult,nrow=10,ncol=10,byrow=T)
newresult[1:5,1:5]
x11()
persp(newCHLA,newSST,newresult, ticktype = "detailed", xlab="CHLA",ylab="SST",theta=45,phi=30)
points(trans3d(dataset_cjm_model_sans_na$CHLA,dataset_cjm_model_sans_na$SST,dataset_cjm_model_sans_na$presence_absence,auxi),pch=19)

#### gam binomial
gam3d <- gam(presence_absence ~ s(CHLA, SST), data=dataset_cjm_model_sans_na, family=binomial(link="logit"))
x11()
vis.gam(gam3d,ticktype="detailed")




#test <- subset(dataset_cjm_model_sans_na_presence_sorted, SST<15)


######################### test nlrq
x11()
plot(dataset_cjm_model_sans_na_presence$CHLA,dataset_cjm_model_sans_na_presence$SST,xlab = "CHLA", ylab = "SST",type="n", xlim=c(0,3),ylim=c(9,30))
# fit first a nonlinear least-square regression
Dat.nls <- nls(SST ~ SSlogis(CHLA, Asym, mid, scal), data=dataset_cjm_model_sans_na_presence); Dat.nls
lines(1:25, predict(Dat.nls, newdata=list(x=1:25)), col=1)
# then fit the median using nlrq
Dat.nlrq <- nlrq(SST ~ SSlogis(CHLA, Asym, mid, scal), data=dataset_cjm_model_sans_na_presence, tau=0.5, trace=TRUE)
lines(1:25, predict(Dat.nlrq, newdata=list(x=1:25)), col=2)

######################## test smooth spline
x11()
plot(dataset_cjm_model_sans_na_presence$CHLA, dataset_cjm_model_sans_na_presence$SST)
lines(smooth.spline(dataset_cjm_model_sans_na_presence$CHLA, dataset_cjm_model_sans_na_presence$SST), col='red', lwd=3)

######################## test nls
#x11()
#plot(dataset_cjm_model_sans_na_presence$CHLA, dataset_cjm_model_sans_na_presence$SST)
#r <- nls(dataset_cjm_model_sans_na_presence$SST ~ f(dataset_cjm_model_sans_na_presence$CHLA))

######################## test SSasymp
#lines(dataset_cjm_model_sans_na_presence$CHLA,SSasymp(dataset_cjm_model_sans_na_presence$CHLA))

######################## test lowess
#lines(lowess(dataset_cjm_model_sans_na_presence$CHLA,dataset_cjm_model_sans_na_presence$SST),col="red")

######################## test locpoly
#bw <- 0.5
#lines( locpoly(dataset_cjm_model_sans_na_presence$CHLA,dataset_cjm_model_sans_na_presence$SST,degree=0, bandwidth=bw), col='red' )

######################## test lprq
#r <- lprq(dataset_cjm_model_sans_na_presence$CHLA,dataset_cjm_model_sans_na_presence$SST,h=4, tau=0.99)
#lines(r$xx, r$fv, col="blue", lwd=3)

