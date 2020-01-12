#Figure 2. Jet stream position impacts environmental temperatures. The 5 most northern (southern) jet streams correspond with warmer (cooler) temperatures around the climatological mean, with the average temperatures of the 5 years shown here for pixels which were significantly different, determined using a Student’s t-test to compare the mean temperatures for the 5 northernmost (top) and 5 southernmost (bottom) jet streams by each longitude for each month. We removed pixels where average temperatures over the 1994-2012 time period were outside of the 12-32C range optimal for monarch survival and development. 

# First- correlation test.
# Detrend both jet stream and climate data

library(matrixStats)
library(maps)
library(mapdata)
library(maptools)  #for shapefiles
library(scales)  #for transparency
library(ggplot2)
library(devtools)
library(dplyr)
library(stringr)
library(ggmap) #need to cite ggmap
library(matrixStats) #colMedians
library(dplR)
library(stats)
library(Hmisc)
library(RColorBrewer)

lon <- as.integer(seq(235-360,295-360,2.5))

# import and detrend jet

n <- read.table("NHJ_position_global_1948jan-2019sep_ncepncar.txt")
colnames(n) <- seq(0,357.5,2.5)
jet1 <- n[2:853,]
# import and detrend climate data
#Temperature CRU 


data_time <- 1994:2018
#1901:2018 monthly
year <- matrix(rep(1994:2018,12),nrow = length(1994:2018),ncol = 12)
year <- as.vector(t(year))
month <- rep(1:12,length(1994:2018))#repeat 1:12 116 times
#repeat 1901:2018 12 times and make an array for the year vector
#crop to 1994 (jan or feb)
time <- cbind(year,month)
time <- subset(time,year >=1994)

#don't know how to subset the time variable to 1994
#1117
library(ncdf4)
a <- nc_open("cru_ts4.03.1901.2018.tmp.dat.nc")

#nc_disp

xdim <- round(a$dim[[1]]$vals, digits = 5)# lon
ydim <- round(a$dim[[2]]$vals, digits = 5) # lat
zdim <- a$dim[[3]]$vals # time

xs <- which(xdim == -124.75)
ys <-  which(ydim == 15.25) #allows us to crop to the overwintering acreage 
zs <- which(zdim == 34348) # #know this from looking up the time variable 1117
which(ydim == 69.75)-ys+1
which(xdim == -65.25)-xs+1

tmp <- a$var[[1]]

ptm <- proc.time()   
Clim_var.nc<- ncvar_get(a, tmp,start = c(xs,ys,zs), count = c(120, 110,300))    
proc.time() - ptm
dim(Clim_var.nc)[1]
dim(Clim_var.nc)[2]
dim(Clim_var.nc)[3]


longitude <- xdim[c(xs:(xs+119))]
latitude <- ydim[c(ys:(ys+109))]


rb <- rev(brewer.pal(n = 8, name = 'RdBu'))

#png("fig2_jet_temp_corr.png",8,13,
png("fig2_jet_temp_corr.png",13,8,
    units = "in",res = 600, pointsize=20, family= "helvetica")
par(mfrow=c(3,2),mar = c(0, 0, 0, 0))
#par(mfrow=c(1,2),mar = c(0, 0, 0, 0))
##############################################################################################################################################################################################################################################################################################################################################################################################################################################


month <- 2

mon <- Clim_var.nc[,,time[,2]==month]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
#dim(mon1)<- c(25, length(latitude)*length(longitude)) 
#mon1df <- as.data.frame(mon1)
#require(pracma)
#mon1detrend<- pracma::detrend(as.matrix(mon1df))
#mon1detrend<-as.data.frame(mon1detrend)

west <- -130
east <- -65
#subset mon1 by longitudes for jet correlation
mon2 <- mon1[,,which(longitude>west & east>longitude)]
long_2 <- longitude[which(longitude>west & east>longitude)]
dim(mon2)<- c(25, length(latitude)*length(long_2)) 
mon2detrend<- data.frame(pracma::detrend(as.matrix(mon2)))

jet <- jet1[seq(month, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
jet <- jet[47:71,]#1994-2018: 47:71
rownames(jet) <- 1994:2018
jet <- lapply(jet, as.character)
jet <- lapply(jet, as.numeric)
jet <- as.data.frame(jet)

jet_long <- seq(-125,-65,2.5)
jet_corr <- data.frame(rowMeans(jet[,which(jet_long>=west & jet_long<=east)]))
#average jet for 90 to 70


##

# regrid temp to match jet longitude width of 2.5
# I don't know how to regrid 3d raster objects?

#dim(mon1detrend) <- c(length(data_time),length(latitude),length(longitude)) #not sure why this doesn't work?

##  Correlations
#make a rho and pval to find significant correlations.
rho <- NA[length(mon2detrend)]
pval <- NA[length(mon2detrend)]
# 
for (i in 1:length(mon2detrend)){
  rho[i]<- rcorr(mon2detrend[,i],jet[,which(jet_long==-75)],type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon2detrend[,i],jet[,which(jet_long==-75)],type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon2detrend)){
  if (is.na(pval[i])==F & pval[i] < 0.1){
    rho1[i] <- 1 #rho[i] # significant correlation values
    mon1df1[,i] <- mon2detrend[,i] #climate time series of significant pixels
  }
}


library(raster)
#changed rho1 to rho
rho2 <- as.matrix(rho) #pval
dim(rho2) <- c(length(latitude), length(long_2))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- long_2
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate4 <- raster(rho2[nrow(rho2):1,])
extent(rotate4) <- extent(c(min(long_2),max(long_2),min(latitude),max(latitude)))

library(raster)
#changed rho1 to rho
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude), length(long_2))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- long_2
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(long_2),max(long_2),min(latitude),max(latitude)))
test <- rasterToPoints(rotate3)
pva2<- rotate3
rot2 <- rotate4 

countries = c("Canada","USA","Cuba","Puerto Rico","Haiti", "Dominican Republic",
              "Mexico", "Guatemala")

map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

#map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
#map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
#plot(rotate4, add = T, col = rev(brewer.pal(9,"RdBu")))
plot(rotate4, add = T, col = rb, breaks = c(-0.8,-0.6,-.4,-0.2,0,.2,.4,.6,.8),   xlab="",ylab="",horizontal = F, legend = F)
#plot(rotate4, add = F, axes = T, col = rb, breaks = c(-0.8,-0.6,-.4,-0.2,0,.2,.4,.6,.8),   xlab="",ylab="",horizontal = F, legend = F)

map("worldHires",countries, xlim=c(-122,-68),ylim=c(16,69), fill=F,lwd = 1, add = T)  #plot the region of Canada

points(test[,1],test[,2], pch=4, col="black",cex=0.001)  
text(-65,15, "FEB",adj = c(1,0),cex = 1.5)

##############################################################################################################################################################################################################################################################################################################################################################################################################################################
month <- 3

mon <- Clim_var.nc[,,time[,2]==month]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
#dim(mon1)<- c(25, length(latitude)*length(longitude)) 
#mon1df <- as.data.frame(mon1)
#require(pracma)
#mon1detrend<- pracma::detrend(as.matrix(mon1df))
#mon1detrend<-as.data.frame(mon1detrend)

west <- -130 #-120
east <- -65 #-110
#subset mon1 by longitudes for jet correlation
mon2 <- mon1[,,which(longitude>west & east>longitude)]
long_2 <- longitude[which(longitude>west & east>longitude)]
dim(mon2)<- c(25, length(latitude)*length(long_2)) 
mon2detrend<- data.frame(pracma::detrend(as.matrix(mon2)))

jet <- jet1[seq(month, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
jet <- jet[47:71,]#1994-2018: 47:71
rownames(jet) <- 1994:2018
jet <- lapply(jet, as.character)
jet <- lapply(jet, as.numeric)
jet <- as.data.frame(jet)

jet_long <- seq(-125,-65,2.5)
jet_corr <- data.frame(rowMeans(jet[,which(jet_long>=west & jet_long<=east)]))
#average jet for 90 to 70


##

# regrid temp to match jet longitude width of 2.5
# I don't know how to regrid 3d raster objects?

#dim(mon1detrend) <- c(length(data_time),length(latitude),length(longitude)) #not sure why this doesn't work?

##  Correlations
#make a rho and pval to find significant correlations.
rho <- NA[length(mon2detrend)]
pval <- NA[length(mon2detrend)]
# 
for (i in 1:length(mon2detrend)){
  rho[i]<- rcorr(mon2detrend[,i],jet[,which(jet_long==-75)],type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon2detrend[,i],jet[,which(jet_long==-75)],type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon2detrend)){
  if (is.na(pval[i])==F & pval[i] < 0.1){
    rho1[i] <- 1 #rho[i] # significant correlation values
    mon1df1[,i] <- mon2detrend[,i] #climate time series of significant pixels
  }
}


library(raster)
#changed rho1 to rho
rho2 <- as.matrix(rho) #pval
dim(rho2) <- c(length(latitude), length(long_2))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- long_2
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate4 <- raster(rho2[nrow(rho2):1,])
extent(rotate4) <- extent(c(min(long_2),max(long_2),min(latitude),max(latitude)))

library(raster)
#changed rho1 to rho
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude), length(long_2))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- long_2
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(long_2),max(long_2),min(latitude),max(latitude)))
test <- rasterToPoints(rotate3)
pva3 <- rotate3
rot3 <- rotate4

countries = c("Canada","USA","Cuba","Puerto Rico","Haiti", "Dominican Republic",
              "Mexico", "Guatemala")

map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

#map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
#map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
#plot(rotate4, add = T, col = rev(brewer.pal(9,"RdBu")))
plot(rotate4, add = T, col = rb, breaks = c(-0.8,-0.6,-.4,-0.2,0,.2,.4,.6,.8),   xlab="",ylab="",horizontal = F, legend = F)
#plot(rotate4, add = F, axes = T, col = rb, breaks = c(-0.8,-0.6,-.4,-0.2,0,.2,.4,.6,.8),   xlab="",ylab="",horizontal = F, legend = F)

map("worldHires",countries, xlim=c(-120,-70),ylim=c(15,70), fill=F,lwd = 1, add = T)  #plot the region of Canada

points(test[,1],test[,2], pch=4, col="black",cex=0.001)  
text(-65,15, "MAR",adj = c(1,0),cex = 1.5)
##############################################################################################################################################################################################################################################################################################################################################################################################################################################
## Left off 12/30 

plot(rotate4, legend.only = T,col =rb, breaks = c(-0.8,-0.6,-.4,-0.2,0,.2,.4,.6,.8),legend.width=1, legend.shrink=1,legend.args=list(text='r', side=3, line=.25, cex=1), axis.args = list(cex.axis = 1),horiz=F)
dev.off()
##############################################################################################################################################################################################################################################################################################################################################################################################################################################
month <- 6

mon <- Clim_var.nc[,,time[,2]==month]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
#dim(mon1)<- c(25, length(latitude)*length(longitude)) 
#mon1df <- as.data.frame(mon1)
#require(pracma)
#mon1detrend<- pracma::detrend(as.matrix(mon1df))
#mon1detrend<-as.data.frame(mon1detrend)

west <- -105
east <- -75
#subset mon1 by longitudes for jet correlation
mon2 <- mon1[,,which(longitude>west & east>longitude)]
long_2 <- longitude[which(longitude>west & east>longitude)]
dim(mon2)<- c(25, length(latitude)*length(long_2)) 
mon2detrend<- data.frame(pracma::detrend(as.matrix(mon2)))

jet <- jet1[seq(month, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
jet <- jet[47:71,]#1994-2018: 47:71
rownames(jet) <- 1994:2018
jet <- lapply(jet, as.character)
jet <- lapply(jet, as.numeric)
jet <- as.data.frame(jet)

jet_long <- seq(-125,-65,2.5)
jet_corr <- data.frame(rowMeans(jet[,which(jet_long>=west & jet_long<=east)]))
#average jet for 90 to 70


##

# regrid temp to match jet longitude width of 2.5
# I don't know how to regrid 3d raster objects?

#dim(mon1detrend) <- c(length(data_time),length(latitude),length(longitude)) #not sure why this doesn't work?

##  Correlations
#make a rho and pval to find significant correlations.
rho <- NA[length(mon2detrend)]
pval <- NA[length(mon2detrend)]
# 
for (i in 1:length(mon2detrend)){
  rho[i]<- rcorr(mon2detrend[,i],jet[,which(jet_long==-75)],type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon2detrend[,i],jet[,which(jet_long==-75)],type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon2detrend)){
  if (is.na(pval[i]) | pval[i] < 0.1){
    rho1[i] <- rho[i] # significant correlation values
    mon1df1[,i] <- mon2detrend[,i] #climate time series of significant pixels
  }
}

library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude), length(long_2))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- long_2
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(long_2),max(long_2),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
text(-65,15, "JUN",adj = c(1,0),cex = 1.5)
plot(rotate3, add = T, col = rb, breaks = c(-0.8,-0.6,-.4,-0.2,0,.2,.4,.6,.8),   xlab="",ylab="",horizontal = F, legend = F)
##############################################################################################################################################################################################################################################################################################################################################################################################################################################
mon <- Clim_var.nc[,,time[,2]==8]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
#dim(mon1)<- c(25, length(latitude)*length(longitude)) 
#mon1df <- as.data.frame(mon1)
#require(pracma)
#mon1detrend<- pracma::detrend(as.matrix(mon1df))
#mon1detrend<-as.data.frame(mon1detrend)

west <- -90
east <- -65
#subset mon1 by longitudes for jet correlation
mon2 <- mon1[,,which(longitude>west & east>longitude)]
long_2 <- longitude[which(longitude>west & east>longitude)]
dim(mon2)<- c(25, length(latitude)*length(long_2)) 
mon2detrend<- data.frame(pracma::detrend(as.matrix(mon2)))

jet <- jet1[seq(8, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
jet <- jet[47:71,]#1994-2018: 47:71
rownames(jet) <- 1994:2018
jet <- lapply(jet, as.character)
jet <- lapply(jet, as.numeric)
jet <- as.data.frame(jet)

jet_long <- seq(-125,-65,2.5)
jet_corr <- data.frame(rowMeans(jet[,which(jet_long>=west & jet_long<=east)]))
#average jet for 90 to 70


##

# regrid temp to match jet longitude width of 2.5
# I don't know how to regrid 3d raster objects?

#dim(mon1detrend) <- c(length(data_time),length(latitude),length(longitude)) #not sure why this doesn't work?

##  Correlations
#make a rho and pval to find significant correlations.
rho <- NA[length(mon2detrend)]
pval <- NA[length(mon2detrend)]
# 
for (i in 1:length(mon2detrend)){
  rho[i]<- rcorr(mon2detrend[,i],jet[,which(jet_long==-75)],type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon2detrend[,i],jet[,which(jet_long==-75)],type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon2detrend)){
  if (is.na(pval[i]) | pval[i] < 0.05){
    rho1[i] <- rho[i] # significant correlation values
    mon1df1[,i] <- mon2detrend[,i] #climate time series of significant pixels
  }
}

library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude), length(long_2))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- long_2
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(long_2),max(long_2),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
text(-65,15, "AUG",adj = c(1,0),cex = 1.5)
plot(rotate3 , add = T, col = rb, breaks = c(-0.8,-0.6,-.4,-0.2,0,.2,.4,.6,.8),   xlab="",ylab="",horizontal = F, legend = F)

##############################################################################################################################################################################################################################################################################################################################################################################################################################################
month <- 9

mon <- Clim_var.nc[,,time[,2]==month]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
#dim(mon1)<- c(25, length(latitude)*length(longitude)) 
#mon1df <- as.data.frame(mon1)
#require(pracma)
#mon1detrend<- pracma::detrend(as.matrix(mon1df))
#mon1detrend<-as.data.frame(mon1detrend)

west <- -110
east <- -90
#subset mon1 by longitudes for jet correlation
mon2 <- mon1[,,which(longitude>west & east>longitude)]
long_2 <- longitude[which(longitude>west & east>longitude)]
dim(mon2)<- c(25, length(latitude)*length(long_2)) 
mon2detrend<- data.frame(pracma::detrend(as.matrix(mon2)))

jet <- jet1[seq(month, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
jet <- jet[47:71,]#1994-2018: 47:71
rownames(jet) <- 1994:2018
jet <- lapply(jet, as.character)
jet <- lapply(jet, as.numeric)
jet <- as.data.frame(jet)

jet_long <- seq(-125,-65,2.5)
jet_corr <- data.frame(rowMeans(jet[,which(jet_long>=west & jet_long<=east)]))
#average jet for 90 to 70


##

# regrid temp to match jet longitude width of 2.5
# I don't know how to regrid 3d raster objects?

#dim(mon1detrend) <- c(length(data_time),length(latitude),length(longitude)) #not sure why this doesn't work?

##  Correlations
#make a rho and pval to find significant correlations.
rho <- NA[length(mon2detrend)]
pval <- NA[length(mon2detrend)]
# 
for (i in 1:length(mon2detrend)){
  rho[i]<- rcorr(mon2detrend[,i],jet[,which(jet_long==-75)],type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon2detrend[,i],jet[,which(jet_long==-75)],type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon2detrend)){
  if (is.na(pval[i]) | pval[i] < 0.05){
    rho1[i] <- rho[i] # significant correlation values
    mon1df1[,i] <- mon2detrend[,i] #climate time series of significant pixels
  }
}

library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude), length(long_2))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- long_2
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(long_2),max(long_2),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
text(-65,15, "SEP",adj = c(1,0),cex = 1.5)
plot(rotate3, add = T, col = rb, breaks = c(-0.8,-0.6,-.4,-0.2,0,.2,.4,.6,.8),   xlab="",ylab="",horizontal = F, legend = F)
##############################################################################################################################################################################################################################################################################################################################################################################################################################################
month <- 10

mon <- Clim_var.nc[,,time[,2]==month]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
#dim(mon1)<- c(25, length(latitude)*length(longitude)) 
#mon1df <- as.data.frame(mon1)
#require(pracma)
#mon1detrend<- pracma::detrend(as.matrix(mon1df))
#mon1detrend<-as.data.frame(mon1detrend)

west <- -125
east <- -110
#subset mon1 by longitudes for jet correlation
mon2 <- mon1[,,which(longitude>west & east>longitude)]
long_2 <- longitude[which(longitude>west & east>longitude)]
dim(mon2)<- c(25, length(latitude)*length(long_2)) 
mon2detrend<- data.frame(pracma::detrend(as.matrix(mon2)))

jet <- jet1[seq(month, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
jet <- jet[47:71,]#1994-2018: 47:71
rownames(jet) <- 1994:2018
jet <- lapply(jet, as.character)
jet <- lapply(jet, as.numeric)
jet <- as.data.frame(jet)

jet_long <- seq(-125,-65,2.5)
jet_corr <- data.frame(rowMeans(jet[,which(jet_long>=west & jet_long<=east)]))
#average jet for 90 to 70


##

# regrid temp to match jet longitude width of 2.5
# I don't know how to regrid 3d raster objects?

#dim(mon1detrend) <- c(length(data_time),length(latitude),length(longitude)) #not sure why this doesn't work?

##  Correlations
#make a rho and pval to find significant correlations.
rho <- NA[length(mon2detrend)]
pval <- NA[length(mon2detrend)]
# 
for (i in 1:length(mon2detrend)){
  rho[i]<- rcorr(mon2detrend[,i],jet[,which(jet_long==-75)],type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon2detrend[,i],jet[,which(jet_long==-75)],type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon2detrend)){
  if (is.na(pval[i]) | pval[i] < 0.1){
    rho1[i] <- rho[i] # significant correlation values
    mon1df1[,i] <- mon2detrend[,i] #climate time series of significant pixels
  }
}

library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude), length(long_2))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- long_2
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(long_2),max(long_2),min(latitude),max(latitude)))


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
text(-65,15, "OCT",adj = c(1,0),cex = 1.5)
plot(rotate3, add = T, col = rb, breaks = c(-0.8,-0.6,-.4,-0.2,0,.2,.4,.6,.8),   xlab="",ylab="",horizontal = F, legend = F)

dev.off()
