#correlations between monarch overwintering numbers and average temperature (threshold of 12-32C)

library(maps)
library(mapdata)
library(maptools)  #for shapefiles
library(scales)  #for transparency
library(ggplot2)
library(devtools)
library(dplyr)
library(stringr)
library(ggmap) #need to cite ggmap

library(dplR)
library(stats)

mexicoarea <- read.table('Butterflies.txt',header = T)
#mexicoarea <- mexicoarea[1:19,]#2012
mexicoarea <- mexicoarea[1:25,]

#detrend(mexicoarea,make.plot = T) 
#mexicoareadetrend <- dplR::detrend(mexicoarea,make.plot = T, method = c("ModNegExp")) # detrended using modified neg exp
#mexicoareadetrend <- detrend(mexicoarea,make.plot = T, method = c("Spline")) # detrended using modified neg exp
mexicoareadetrend <- matrix(NA,nrow = length(1994:2018),ncol = 2)
mexicoareadetrend[,2] <- lm(mexicoarea$MexicoArea~mexicoarea$Year)$residuals
mexicoareadetrend[,1] <- 1994:2018 #1994:2012
mexicoareadetrend <- data.frame(mexicoareadetrend)
colnames(mexicoareadetrend) <- c("Year", "MexicoArea")
# are there trends in mean or variance for the jet longitudes?

mexicoareadetrend
#######################################################################################################
#Temperature Berkeley 

data_time <- 1994:2018
#1901:2018 monthly
year <- matrix(rep(1994:2018,12),nrow = length(1994:2018),ncol = 12)
year <- as.vector(t(year))
month <- rep(1:12,length(1994:2018))#repeat 1:12 116 times
#repeat 1901:2018 12 times and make an array for the year vector
#crop to 1994 (jan or feb)
time <- cbind(year,month)
time <- subset(time,year >=1994)
#itime <- cbind(time,year>=1994)

#don't know how to subset the time variable to 1994
#1117
library(ncdf4)
a <- nc_open("Berkeley_TAVG_LatLong1.nc")
#nc_disp

xdim <- round(a$dim[[1]]$vals, digits = 5)# lon
ydim <- round(a$dim[[2]]$vals, digits = 5) # lat
zdim <- a$dim[[3]]$vals # time

xs <- which(xdim == -125.5)
ys <-  which(ydim == 14.5) #allows us to crop to the overwintering acreage 
zs <- 2929#which(zdim == 1994.042) # Berkeley is labeled as year
#zdim[which(zdim <= 2019 & zdim>= 1994)]

tmp <- a$var[[1]]

ptm <- proc.time()   
Clim_var.nc<- ncvar_get(a, tmp,start = c(xs,ys,zs), count = c(61, 57,300))    
proc.time() - ptm
dim(Clim_var.nc)[1]
dim(Clim_var.nc)[2]
dim(Clim_var.nc)[3]


longitude <- xdim[c(xs:(xs+61-1))]
latitude <- ydim[c(ys:(ys+57-1))]

###########################################################
png("figS1_monarch_corr_monthlytemp.png",8,13,
    units = "in",res = 600, pointsize=20, family= "helvetica")
#par(mfrow=c(1,2), tcl=-0.5, family="serif", mai=c(0,0,0,0),mar = c(0, 0, 0, 0))
par(mfrow=c(4,3),mar = c(0, 0, 0, 0))

###########################################################

mon <- Clim_var.nc[,,time[,2]==1]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(25, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
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
  rho[i]<- rcorr(mon1detrend[,i],mexicoareadetrend[,2],type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],mexicoareadetrend[,2],type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1detrend)){
  if (is.na(pval[i]) | pval[i] < 0.05){
    rho1[i] <- rho[i] # significant correlation values
    mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
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
text(-65,15, "JAN",adj = c(1,0),cex = 1.5)
plot(rotate3, add = T, col = rev(brewer.pal(9,"RdBu")))
######################################################################################################################

mon <- Clim_var.nc[,,time[,2]==2]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(25, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
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
  rho[i]<- rcorr(mon1detrend[,i],mexicoareadetrend[,2],type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],mexicoareadetrend[,2],type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1detrend)){
  if (is.na(pval[i]) | pval[i] < 0.05){
    rho1[i] <- rho[i] # significant correlation values
    mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
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
text(-65,15, "FEB",adj = c(1,0),cex = 1.5)
plot(rotate3, add = T, col = rev(brewer.pal(9,"RdBu")))
######################################################################################################################

mon <- Clim_var.nc[,,time[,2]==3]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(25, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
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
  rho[i]<- rcorr(mon1detrend[,i],mexicoareadetrend[,2],type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],mexicoareadetrend[,2],type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1detrend)){
  if (is.na(pval[i]) | pval[i] < 0.05){
    rho1[i] <- rho[i] # significant correlation values
    mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
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
text(-65,15, "MAR",adj = c(1,0),cex = 1.5)
plot(rotate3, add = T, col = rev(brewer.pal(9,"RdBu")))
######################################################################################################################

mon <- Clim_var.nc[,,time[,2]==4]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(25, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
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
  rho[i]<- rcorr(mon1detrend[,i],mexicoareadetrend[,2],type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],mexicoareadetrend[,2],type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1detrend)){
  if (is.na(pval[i]) | pval[i] < 0.05){
    rho1[i] <- rho[i] # significant correlation values
    mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
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
text(-65,15, "APR",adj = c(1,0),cex = 1.5)
plot(rotate3, add = T, col = rev(brewer.pal(9,"RdBu")))
######################################################################################################################

mon <- Clim_var.nc[,,time[,2]==5]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(25, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
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
  rho[i]<- rcorr(mon1detrend[,i],mexicoareadetrend[,2],type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],mexicoareadetrend[,2],type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1detrend)){
  if (is.na(pval[i]) | pval[i] < 0.05){
    rho1[i] <- rho[i] # significant correlation values
    mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
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
text(-65,15, "MAY",adj = c(1,0),cex = 1.5)
plot(rotate3, add = T, col = rev(brewer.pal(9,"RdBu")))
######################################################################################################################

mon <- Clim_var.nc[,,time[,2]==6]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(25, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
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
  rho[i]<- rcorr(mon1detrend[,i],mexicoareadetrend[,2],type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],mexicoareadetrend[,2],type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1detrend)){
  if (is.na(pval[i]) | pval[i] < 0.05){
    rho1[i] <- rho[i] # significant correlation values
    mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
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
text(-65,15, "JUN",adj = c(1,0),cex = 1.5)
plot(rotate3, add = T, col = rev(brewer.pal(9,"RdBu")))
######################################################################################################################

mon <- Clim_var.nc[,,time[,2]==7]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(25, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
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
  rho[i]<- rcorr(mon1detrend[,i],mexicoareadetrend[,2],type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],mexicoareadetrend[,2],type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1detrend)){
  if (is.na(pval[i]) | pval[i] < 0.05){
    rho1[i] <- rho[i] # significant correlation values
    mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
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
text(-65,15, "JUL",adj = c(1,0),cex = 1.5)
plot(rotate3, add = T, col = rev(brewer.pal(9,"RdBu")))
######################################################################################################################

mon <- Clim_var.nc[,,time[,2]==8]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(25, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
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
  rho[i]<- rcorr(mon1detrend[,i],mexicoareadetrend[,2],type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],mexicoareadetrend[,2],type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1detrend)){
  if (is.na(pval[i]) | pval[i] < 0.05){
    rho1[i] <- rho[i] # significant correlation values
    mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
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
text(-65,15, "AUG",adj = c(1,0),cex = 1.5)
plot(rotate3, add = T, col = rev(brewer.pal(9,"RdBu")))

######################################################################################################################

mon <- Clim_var.nc[,,time[,2]==9]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(25, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
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
  rho[i]<- rcorr(mon1detrend[,i],mexicoareadetrend[,2],type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],mexicoareadetrend[,2],type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1detrend)){
  if (is.na(pval[i]) | pval[i] < 0.05){
    rho1[i] <- rho[i] # significant correlation values
    mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
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
text(-65,15, "SEP",adj = c(1,0),cex = 1.5)
plot(rotate3, add = T, col = rev(brewer.pal(9,"RdBu")))
######################################################################################################################

mon <- Clim_var.nc[,,time[,2]==10]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(25, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
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
  rho[i]<- rcorr(mon1detrend[,i],mexicoareadetrend[,2],type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],mexicoareadetrend[,2],type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1detrend)){
  if (is.na(pval[i]) | pval[i] < 0.05){
    rho1[i] <- rho[i] # significant correlation values
    mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
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
text(-65,15, "OCT",adj = c(1,0),cex = 1.5)
plot(rotate3, add = T, col = rev(brewer.pal(9,"RdBu")))
######################################################################################################################

mon <- Clim_var.nc[,,time[,2]==11]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(25, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
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
  rho[i]<- rcorr(mon1detrend[,i],mexicoareadetrend[,2],type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],mexicoareadetrend[,2],type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1detrend)){
  if (is.na(pval[i]) | pval[i] < 0.05){
    rho1[i] <- rho[i] # significant correlation values
    mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
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
text(-65,15, "NOV",adj = c(1,0),cex = 1.5)
plot(rotate3, add = T, col = rev(brewer.pal(9,"RdBu")))
######################################################################################################################

mon <- Clim_var.nc[,,time[,2]==12]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(25, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
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
  rho[i]<- rcorr(mon1detrend[,i],mexicoareadetrend[,2],type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],mexicoareadetrend[,2],type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1detrend)){
  if (is.na(pval[i]) | pval[i] < 0.05){
    rho1[i] <- rho[i] # significant correlation values
    mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
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
text(-65,15, "DEC",adj = c(1,0),cex = 1.5)
plot(rotate3, add = T, col = rev(brewer.pal(9,"RdBu")))



dev.off()

############################################################################################################################################################################
######################################################################################
######################################################################################
######################################################################################

cM <- colMeans(mon1df1, na.rm = T)
summary(cM)
#cM <- colMeans(mon1df)


# we remove pixels (make NA) if the column means cM are above/below our temperature thresholds: Sept: above 29 deg C and below 12; August: above 32 and below 12.) 

mintemp <- 12
maxtemp <- 32

#if (month1 == 8){
#  maxtemp <- 32
#}

#if (month1 == 9){
#  maxtemp <- 29
#}

for (i in 1: length(cM)){
  if (is.na(cM[i]) | cM[i] < mintemp) {
    rho1[i] <- NA
    mon1df1[,i] <- NA
  }
  if (is.na(cM[i]) | cM[i] > maxtemp) {
    rho1[i] <- NA
    mon1df1[,i] <- NA
  }
}


#hist(as.data.frame(cM))
# truncate the mon1df1 pixels

mon1df1 <- as.matrix(mon1df1[yer,]-cM) #temperature anomalies 2003
#mon1df1 <- as.matrix(mon1df1[9,]-cM) #temperature anomalies 2002

dim(mon1df1) <- c(length(latitude),length(longitude))
mon1df1 <- as.data.frame(mon1df1)
colnames(mon1df1) <- longitude
rownames(mon1df1) <- latitude

#plot mon1df for one year high monarch pop
#plot mon1df for one year low monarch pop


#Subset initial dataset by significant rho.


#par(mfrow=c(1,2), tcl=-0.5, family="serif", mai=c(0,0,0,0),mar = c(0, 0, 0, 0))
#par(mar = c(0, 0, 0, 0))


#reds <- brewer.pal(n = 7, name = 'Reds')#the color bar is still off here.

#par(mar=rep(4,4))
mon1df1 <- as.matrix(mon1df1)
rotate3 <- raster(mon1df1[nrow(mon1df1):1,])
extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))
#plot(rotate3,main="August 2003",col=reds,xlab="",ylab="") #

#map("world",add=T,lwd=2)
#m <- map("worldHires","Canada", xlim=c(-150,max(longitude)),ylim=c(min(latitude),max(latitude)), fill=F,lwd = 2, add = F)  #plot the region of Canada
#map("worldHires","USA", xlim=c(-150,max(longitude)),ylim=c(min(latitude),max(latitude)), fill=F, add = T,lwd = 2)  #plot the region of US
#map("worldHires","Mexico", xlim=c(-150,max(longitude)),ylim=c(min(latitude),max(latitude)), fill=F, add = T,lwd = 2)#plot the region of Mexico
#dev.off()

m <- rotate3 #

