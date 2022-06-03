
library(maps)
library(mapdata)
library(maptools)  #for shapefiles
library(scales)  #for transparency
library(ggplot2)
library(devtools)
library(dplyr)
library(stringr)
library(ggmap) #need to cite ggmap
library(Hmisc)
library(dplR)
library(stats)
library(RColorBrewer)

library(ncdf4)

monarch_ts <- read.csv("MonarchTimeSeries.csv")
monarch_ts1 <- monarch_ts[which(monarch_ts$Year>=2002 & monarch_ts$Year<=2019),]
#detrend each index
monarch_ts_detrend <- data.frame(pracma::detrend(as.matrix(monarch_ts1)))

JN_ROOST_8 <- monarch_ts_detrend$JN_ROOST_8

JN_ROOST_9 <- monarch_ts_detrend$JN_ROOST_9
#################################################################
## CRU TMIN

library(raster)

a <- nc_open("~/Downloads/cru_ts4.04.1901.2019.tmn.dat.nc")
dset <- raster("~/Downloads/cru_ts4.04.1901.2019.tmn.dat.nc")
proj4string(dset)

print(dset)
plot(dset) #rotate(dset) shifts from 0-360 to -180 to 180
df <- as.data.frame(dset, xy = TRUE)


data_time <- 1901:2019
#1901:2019 monthly
year <- matrix(rep(1901:2019,12),nrow = length(1901:2019),ncol = 12)
year <- as.vector(t(year))
month <- rep(1:12,length(1901:2019))#repeat 1:12 116 times
#repeat 1901:2019 12 times and make an array for the year vector
time <- cbind(year,month)

xdim <- round(a$dim[[2]]$vals, digits = 5)# lon
ydim <- round(a$dim[[3]]$vals, digits = 5) # lat
zdim <- a$dim[[1]]$vals # time
xs <- which(xdim == -124.75)
ys <-  which(ydim == 15.25) #allows us to crop to the overwintering acreage 
zs <- which(zdim == 380) 

tmp <- a$var[[1]]

ptm <- proc.time()   
Clim_var.nc<- ncvar_get(a, tmp,start = c(xs,ys,zs), count = c(120, 110,length(zdim)))    
proc.time() - ptm
dim(Clim_var.nc)[1]
dim(Clim_var.nc)[2]
dim(Clim_var.nc)[3]


longitude <- xdim[c(xs:(xs+119))]
latitude <- ydim[c(ys:(ys+109))]

Clim_var.nc<- Clim_var.nc[,,which(time[,1] >=2002 & time[,1] <=2019)]
time <- time[which(time[,1] >=2002 & time[,1] <=2019),]

#################################################################

png("fig_mex_corr_monthlytemp_tmin.png",8,13,
    units = "in",res = 600, pointsize=20, family= "helvetica")
#par(mfrow=c(1,2), tcl=-0.5, family="serif", mai=c(0,0,0,0),mar = c(0, 0, 0, 0))
par(mfrow=c(4,3))#,mar = c(0, 0, 0, 0))



mon <- Clim_var.nc[,,time[,2]==1]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1detrend[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8),legend = F)
text(-65,15, "JAN",adj = c(1,0),cex = 1.5)

plot(rotate3, legend.only = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8),legend.width=1, legend.shrink=1,legend.args=list(text=c(expression(paste(rho,"\n, p<0.1"))),side=3, line=.25, cex=1), axis.args = list(cex.axis = 1),horiz=F)



mon <- Clim_var.nc[,,time[,2]==2]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1detrend[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "FEB",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==3]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "MAR",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==4]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "APR",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==5]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "MAY",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==6]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "JUN",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==7]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "JUL",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==8]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "AUG",adj = c(1,0),cex = 1.5)



mon <- Clim_var.nc[,,time[,2]==9]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "SEP",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==10]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "OCT",adj = c(1,0),cex = 1.5)


dev.off()

#################################################################

png("fig_roost8_corr_monthlytemp_tmin.png",8,13,
    units = "in",res = 600, pointsize=20, family= "helvetica")
#par(mfrow=c(1,2), tcl=-0.5, family="serif", mai=c(0,0,0,0),mar = c(0, 0, 0, 0))
par(mfrow=c(4,3))#,mar = c(0, 0, 0, 0))



mon <- Clim_var.nc[,,time[,2]==1]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1detrend[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8),legend = F)
text(-65,15, "JAN",adj = c(1,0),cex = 1.5)

plot(rotate3, legend.only = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8),legend.width=1, legend.shrink=1,legend.args=list(text=c(expression(paste(rho,"\n, p<0.1"))),side=3, line=.25, cex=1), axis.args = list(cex.axis = 1),horiz=F)



mon <- Clim_var.nc[,,time[,2]==2]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1detrend[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "FEB",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==3]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "MAR",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==4]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "APR",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==5]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "MAY",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==6]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "JUN",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==7]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "JUL",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==8]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "AUG",adj = c(1,0),cex = 1.5)



mon <- Clim_var.nc[,,time[,2]==9]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "SEP",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==10]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "OCT",adj = c(1,0),cex = 1.5)


dev.off()


#################################################################

png("fig_roost9_corr_monthlytemp_tmin.png",8,13,
    units = "in",res = 600, pointsize=20, family= "helvetica")
#par(mfrow=c(1,2), tcl=-0.5, family="serif", mai=c(0,0,0,0),mar = c(0, 0, 0, 0))
par(mfrow=c(4,3))#,mar = c(0, 0, 0, 0))



mon <- Clim_var.nc[,,time[,2]==1]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1detrend[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8),legend = F)
text(-65,15, "JAN",adj = c(1,0),cex = 1.5)

plot(rotate3, legend.only = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8),legend.width=1, legend.shrink=1,legend.args=list(text=c(expression(paste(rho,"\n, p<0.1"))),side=3, line=.25, cex=1), axis.args = list(cex.axis = 1),horiz=F)



mon <- Clim_var.nc[,,time[,2]==2]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1detrend[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "FEB",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==3]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "MAR",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==4]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "APR",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==5]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "MAY",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==6]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "JUN",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==7]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "JUL",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==8]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "AUG",adj = c(1,0),cex = 1.5)



mon <- Clim_var.nc[,,time[,2]==9]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "SEP",adj = c(1,0),cex = 1.5)

dev.off()
#################################################################

png("fig_roost10_corr_monthlytemp_tmin.png",8,13,
    units = "in",res = 600, pointsize=20, family= "helvetica")
#par(mfrow=c(1,2), tcl=-0.5, family="serif", mai=c(0,0,0,0),mar = c(0, 0, 0, 0))
par(mfrow=c(4,3))#,mar = c(0, 0, 0, 0))



mon <- Clim_var.nc[,,time[,2]==1]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1detrend[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8),legend = F)
text(-65,15, "JAN",adj = c(1,0),cex = 1.5)

plot(rotate3, legend.only = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8),legend.width=1, legend.shrink=1,legend.args=list(text=c(expression(paste(rho,"\n, p<0.1"))),side=3, line=.25, cex=1), axis.args = list(cex.axis = 1),horiz=F)



mon <- Clim_var.nc[,,time[,2]==2]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1detrend[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "FEB",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==3]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "MAR",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==4]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "APR",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==5]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "MAY",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==6]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "JUN",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==7]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "JUL",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==8]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "AUG",adj = c(1,0),cex = 1.5)



mon <- Clim_var.nc[,,time[,2]==9]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "SEP",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==10]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "OCT",adj = c(1,0),cex = 1.5)


dev.off()

#################################################################
## CRU TMAX

library(raster)

a <- nc_open("~/Downloads/cru_ts4.04.1901.2019.tmx.dat.nc")
dset <- raster("~/Downloads/cru_ts4.04.1901.2019.tmx.dat.nc")
proj4string(dset)

print(dset)
plot(dset) #rotate(dset) shifts from 0-360 to -180 to 180
df <- as.data.frame(dset, xy = TRUE)


data_time <- 1901:2019
#1901:2019 monthly
year <- matrix(rep(1901:2019,12),nrow = length(1901:2019),ncol = 12)
year <- as.vector(t(year))
month <- rep(1:12,length(1901:2019))#repeat 1:12 116 times
#repeat 1901:2019 12 times and make an array for the year vector
time <- cbind(year,month)

xdim <- round(a$dim[[2]]$vals, digits = 5)# lon
ydim <- round(a$dim[[3]]$vals, digits = 5) # lat
zdim <- a$dim[[1]]$vals # time
xs <- which(xdim == -124.75)
ys <-  which(ydim == 15.25) #allows us to crop to the overwintering acreage 
zs <- which(zdim == 380) 

tmp <- a$var[[1]]

ptm <- proc.time()   
Clim_var.nc<- ncvar_get(a, tmp,start = c(xs,ys,zs), count = c(120, 110,length(zdim)))    
proc.time() - ptm
dim(Clim_var.nc)[1]
dim(Clim_var.nc)[2]
dim(Clim_var.nc)[3]


longitude <- xdim[c(xs:(xs+119))]
latitude <- ydim[c(ys:(ys+109))]

Clim_var.nc<- Clim_var.nc[,,which(time[,1] >=2002 & time[,1] <=2019)]
time <- time[which(time[,1] >=2002 & time[,1] <=2019),]

#################################################################

png("fig_mex_corr_monthlytemp_tmax.png",8,13,
    units = "in",res = 600, pointsize=20, family= "helvetica")
#par(mfrow=c(1,2), tcl=-0.5, family="serif", mai=c(0,0,0,0),mar = c(0, 0, 0, 0))
par(mfrow=c(4,3))#,mar = c(0, 0, 0, 0))



mon <- Clim_var.nc[,,time[,2]==1]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1detrend[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8),legend = F)
text(-65,15, "JAN",adj = c(1,0),cex = 1.5)

plot(rotate3, legend.only = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8),legend.width=1, legend.shrink=1,legend.args=list(text=c(expression(paste(rho,"\n, p<0.1"))),side=3, line=.25, cex=1), axis.args = list(cex.axis = 1),horiz=F)



mon <- Clim_var.nc[,,time[,2]==2]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1detrend[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "FEB",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==3]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "MAR",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==4]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "APR",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==5]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "MAY",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==6]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "JUN",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==7]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "JUL",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==8]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "AUG",adj = c(1,0),cex = 1.5)



mon <- Clim_var.nc[,,time[,2]==9]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "SEP",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==10]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "OCT",adj = c(1,0),cex = 1.5)


dev.off()

#################################################################

png("fig_roost8_corr_monthlytemp_tmax.png",8,13,
    units = "in",res = 600, pointsize=20, family= "helvetica")
#par(mfrow=c(1,2), tcl=-0.5, family="serif", mai=c(0,0,0,0),mar = c(0, 0, 0, 0))
par(mfrow=c(4,3))#,mar = c(0, 0, 0, 0))



mon <- Clim_var.nc[,,time[,2]==1]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1detrend[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8),legend = F)
text(-65,15, "JAN",adj = c(1,0),cex = 1.5)

plot(rotate3, legend.only = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8),legend.width=1, legend.shrink=1,legend.args=list(text=c(expression(paste(rho,"\n, p<0.1"))),side=3, line=.25, cex=1), axis.args = list(cex.axis = 1),horiz=F)



mon <- Clim_var.nc[,,time[,2]==2]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1detrend[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "FEB",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==3]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "MAR",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==4]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "APR",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==5]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "MAY",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==6]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "JUN",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==7]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "JUL",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==8]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "AUG",adj = c(1,0),cex = 1.5)



mon <- Clim_var.nc[,,time[,2]==9]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "SEP",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==10]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "OCT",adj = c(1,0),cex = 1.5)


dev.off()


#################################################################

png("fig_roost9_corr_monthlytemp_tmax.png",8,13,
    units = "in",res = 600, pointsize=20, family= "helvetica")
#par(mfrow=c(1,2), tcl=-0.5, family="serif", mai=c(0,0,0,0),mar = c(0, 0, 0, 0))
par(mfrow=c(4,3))#,mar = c(0, 0, 0, 0))



mon <- Clim_var.nc[,,time[,2]==1]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1detrend[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8),legend = F)
text(-65,15, "JAN",adj = c(1,0),cex = 1.5)

plot(rotate3, legend.only = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8),legend.width=1, legend.shrink=1,legend.args=list(text=c(expression(paste(rho,"\n, p<0.1"))),side=3, line=.25, cex=1), axis.args = list(cex.axis = 1),horiz=F)



mon <- Clim_var.nc[,,time[,2]==2]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1detrend[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "FEB",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==3]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "MAR",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==4]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "APR",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==5]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "MAY",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==6]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "JUN",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==7]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "JUL",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==8]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "AUG",adj = c(1,0),cex = 1.5)



mon <- Clim_var.nc[,,time[,2]==9]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "SEP",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==10]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "OCT",adj = c(1,0),cex = 1.5)


dev.off()
#################################################################

png("fig_roost10_corr_monthlytemp_tmax.png",8,13,
    units = "in",res = 600, pointsize=20, family= "helvetica")
#par(mfrow=c(1,2), tcl=-0.5, family="serif", mai=c(0,0,0,0),mar = c(0, 0, 0, 0))
par(mfrow=c(4,3))#,mar = c(0, 0, 0, 0))



mon <- Clim_var.nc[,,time[,2]==1]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1detrend[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8),legend = F)
text(-65,15, "JAN",adj = c(1,0),cex = 1.5)

plot(rotate3, legend.only = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8),legend.width=1, legend.shrink=1,legend.args=list(text=c(expression(paste(rho,"\n, p<0.1"))),side=3, line=.25, cex=1), axis.args = list(cex.axis = 1),horiz=F)



mon <- Clim_var.nc[,,time[,2]==2]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1detrend[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "FEB",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==3]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "MAR",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==4]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "APR",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==5]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "MAY",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==6]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "JUN",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==7]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "JUL",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==8]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "AUG",adj = c(1,0),cex = 1.5)



mon <- Clim_var.nc[,,time[,2]==9]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "SEP",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==10]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "OCT",adj = c(1,0),cex = 1.5)


dev.off()

#################################################################
## CRU PRECIP

library(raster)

a <- nc_open("~/Downloads/cru_ts4.04.1901.2019.pre.dat.nc")
dset <- raster("~/Downloads/cru_ts4.04.1901.2019.pre.dat.nc")
proj4string(dset)

print(dset)
plot(dset) #rotate(dset) shifts from 0-360 to -180 to 180
df <- as.data.frame(dset, xy = TRUE)


data_time <- 1901:2019
#1901:2019 monthly
year <- matrix(rep(1901:2019,12),nrow = length(1901:2019),ncol = 12)
year <- as.vector(t(year))
month <- rep(1:12,length(1901:2019))#repeat 1:12 116 times
#repeat 1901:2019 12 times and make an array for the year vector
time <- cbind(year,month)

xdim <- round(a$dim[[2]]$vals, digits = 5)# lon
ydim <- round(a$dim[[3]]$vals, digits = 5) # lat
zdim <- a$dim[[1]]$vals # time
xs <- which(xdim == -124.75)
ys <-  which(ydim == 15.25) #allows us to crop to the overwintering acreage 
zs <- which(zdim == 380) 

tmp <- a$var[[1]]

ptm <- proc.time()   
Clim_var.nc<- ncvar_get(a, tmp,start = c(xs,ys,zs), count = c(120, 110,length(zdim)))    
proc.time() - ptm
dim(Clim_var.nc)[1]
dim(Clim_var.nc)[2]
dim(Clim_var.nc)[3]


longitude <- xdim[c(xs:(xs+119))]
latitude <- ydim[c(ys:(ys+109))]

Clim_var.nc<- Clim_var.nc[,,which(time[,1] >=2002 & time[,1] <=2019)]
time <- time[which(time[,1] >=2002 & time[,1] <=2019),]

#################################################################

png("fig_mex_corr_monthlytemp_prec.png",8,13,
    units = "in",res = 600, pointsize=20, family= "helvetica")
#par(mfrow=c(1,2), tcl=-0.5, family="serif", mai=c(0,0,0,0),mar = c(0, 0, 0, 0))
par(mfrow=c(4,3))#,mar = c(0, 0, 0, 0))



mon <- Clim_var.nc[,,time[,2]==1]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1detrend[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8),legend = F)
text(-65,15, "JAN",adj = c(1,0),cex = 1.5)

plot(rotate3, legend.only = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8),legend.width=1, legend.shrink=1,legend.args=list(text=c(expression(paste(rho,"\n, p<0.1"))),side=3, line=.25, cex=1), axis.args = list(cex.axis = 1),horiz=F)



mon <- Clim_var.nc[,,time[,2]==2]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1detrend[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "FEB",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==3]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "MAR",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==4]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "APR",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==5]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "MAY",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==6]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "JUN",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==7]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "JUL",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==8]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "AUG",adj = c(1,0),cex = 1.5)



mon <- Clim_var.nc[,,time[,2]==9]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "SEP",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==10]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],mexicoareadetrend_roost$MexicoArea,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "OCT",adj = c(1,0),cex = 1.5)


dev.off()

#################################################################

png("fig_roost8_corr_monthlytemp_prec.png",8,13,
    units = "in",res = 600, pointsize=20, family= "helvetica")
#par(mfrow=c(1,2), tcl=-0.5, family="serif", mai=c(0,0,0,0),mar = c(0, 0, 0, 0))
par(mfrow=c(4,3))#,mar = c(0, 0, 0, 0))



mon <- Clim_var.nc[,,time[,2]==1]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1detrend[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8),legend = F)
text(-65,15, "JAN",adj = c(1,0),cex = 1.5)

plot(rotate3, legend.only = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8),legend.width=1, legend.shrink=1,legend.args=list(text=c(expression(paste(rho,"\n, p<0.1"))),side=3, line=.25, cex=1), axis.args = list(cex.axis = 1),horiz=F)



mon <- Clim_var.nc[,,time[,2]==2]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1detrend[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "FEB",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==3]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "MAR",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==4]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "APR",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==5]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "MAY",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==6]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "JUN",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==7]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "JUL",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==8]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "AUG",adj = c(1,0),cex = 1.5)



mon <- Clim_var.nc[,,time[,2]==9]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "SEP",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==10]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_8,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "OCT",adj = c(1,0),cex = 1.5)


dev.off()


#################################################################

png("fig_roost9_corr_monthlytemp_prec.png",8,13,
    units = "in",res = 600, pointsize=20, family= "helvetica")
#par(mfrow=c(1,2), tcl=-0.5, family="serif", mai=c(0,0,0,0),mar = c(0, 0, 0, 0))
par(mfrow=c(4,3))#,mar = c(0, 0, 0, 0))



mon <- Clim_var.nc[,,time[,2]==1]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1detrend[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8),legend = F)
text(-65,15, "JAN",adj = c(1,0),cex = 1.5)

plot(rotate3, legend.only = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8),legend.width=1, legend.shrink=1,legend.args=list(text=c(expression(paste(rho,"\n, p<0.1"))),side=3, line=.25, cex=1), axis.args = list(cex.axis = 1),horiz=F)



mon <- Clim_var.nc[,,time[,2]==2]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1detrend[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "FEB",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==3]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "MAR",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==4]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "APR",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==5]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "MAY",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==6]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "JUN",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==7]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "JUL",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==8]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "AUG",adj = c(1,0),cex = 1.5)



mon <- Clim_var.nc[,,time[,2]==9]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "SEP",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==10]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_9,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "OCT",adj = c(1,0),cex = 1.5)


dev.off()
#################################################################

png("fig_roost10_corr_monthlytemp_prec.png",8,13,
    units = "in",res = 600, pointsize=20, family= "helvetica")
#par(mfrow=c(1,2), tcl=-0.5, family="serif", mai=c(0,0,0,0),mar = c(0, 0, 0, 0))
par(mfrow=c(4,3))#,mar = c(0, 0, 0, 0))



mon <- Clim_var.nc[,,time[,2]==1]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1detrend[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8),legend = F)
text(-65,15, "JAN",adj = c(1,0),cex = 1.5)

plot(rotate3, legend.only = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8),legend.width=1, legend.shrink=1,legend.args=list(text=c(expression(paste(rho,"\n, p<0.1"))),side=3, line=.25, cex=1), axis.args = list(cex.axis = 1),horiz=F)



mon <- Clim_var.nc[,,time[,2]==2]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1detrend[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "FEB",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==3]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "MAR",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==4]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "APR",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==5]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "MAY",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==6]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "JUN",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==7]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "JUL",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==8]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "AUG",adj = c(1,0),cex = 1.5)



mon <- Clim_var.nc[,,time[,2]==9]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "SEP",adj = c(1,0),cex = 1.5)


mon <- Clim_var.nc[,,time[,2]==10]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(18, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
require(pracma)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

## Make Correlations w Monarchs

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1df)){
  rho[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],JN_ROOST_10,type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] <0.1){
    rho1[i] <- rho[i] # significant correlation values
    #mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "OCT",adj = c(1,0),cex = 1.5)


dev.off()

#################################################################
