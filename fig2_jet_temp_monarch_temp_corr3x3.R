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
rb <- rev(brewer.pal(n = 8, name = 'RdBu'))

# import and detrend jet

n <- read.table("NHJ_position_global_1948jan-2019sep_ncepncar.txt")
colnames(n) <- seq(0,357.5,2.5)
jet1 <- n[2:853,]

# need to still subset by month

# import and detrend climate data

# Temperature CRU 


data_time <- 1994:2018
#1901:2018 monthly
year <- matrix(rep(1994:2018,12),nrow = length(1994:2018),ncol = 12)
year <- as.vector(t(year))
month <- rep(1:12,length(1994:2018))#repeat 1:12 116 times
#repeat 1901:2018 12 times and make an array for the year vector
#crop to 1994 (jan or feb)
time <- cbind(year,month)
time <- subset(time,year >=1994)

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

png("fig2_ASO_jet_temp_corr_spearman_0.1.png",8,13,
    units = "in",res = 600, pointsize=20, family= "helvetica")
par(mfrow=c(3,1))#,mar = c(0, 0, 0, 0))
##############################################################################################################################################################################################################################################################################################################################################################################################################################################
month <- 8

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
jetdetrend <- pracma::detrend(as.matrix(jet))

jet_long <- seq(-125,-65,2.5)
#jet_corr <- matrix(rowMeans(jetdetrend[,which(jet_long>=-78 & jet_long<=-72.5)]))
#jet_corr <- matrix(rowMeans(jetdetrend[,which(jet_long>=-108 & jet_long<=-72.5)]))
jet_corr <- matrix(rowMeans(jetdetrend[,which(jet_long>=-102.5 & jet_long<=-72.5)]))

#average jet for 90 to 70


rho <- NA[length(mon2detrend)]
pval <- NA[length(mon2detrend)]
# 
for (i in 1:length(mon2detrend)){
  rho[i]<- rcorr(mon2detrend[,i],jet_corr,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon2detrend[,i],jet_corr,type = c("spearman"))$P[1,2]
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


countries = c("Canada","USA","Cuba","Puerto Rico","Haiti", "Dominican Republic",
              "Mexico", "Guatemala")

map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()

plot(rotate3, add = T, col = rb, breaks = c(-0.8,-0.6,-.4,-0.2,0,.2,.4,.6,.8),   xlab="",ylab="",horizontal = F, legend = F)

map("worldHires",countries, xlim=c(-122,-68),ylim=c(16,69), fill=F,lwd = 1, add = T)  #plot the region of Canada

lon_left <- -105
lon_right <- -70

polygon.x <- c(seq(-125,lon_left,by = 2.5),rev(seq(-125,lon_left,by = 2.5)))
polygon.y <- c(rep(15,length(polygon.x)/2),rep(70,length(polygon.x)/2))
polygon(x=polygon.x, y=polygon.y, col=adjustcolor("gray", alpha.f=0.7), border=F) #red positive correlation 

polygon.x <- c(seq(lon_right,-65,by = 2.5),rev(seq(lon_right,-65,by = 2.5)))
polygon.y <- c(rep(15,length(polygon.x)/2),rep(70,length(polygon.x)/2))
polygon(x=polygon.x, y=polygon.y, col=adjustcolor("gray", alpha.f=0.7), border=F) #red positive correlation 

text(-65,15, "AUG",adj = c(1,0),cex = 1.5)

##############################################################################################################################################################################################################################################################################################################################################################################################################################################
png("fig2_S_jet_temp_corr_spearman_0.1.png",13,8,
    units = "in",res = 600, pointsize=20, family= "helvetica")
par(mfrow=c(3,1))#,mar = c(0, 0, 0, 0))

# ##############################################################################################################################################################################################################################################################################################################################################################################################################################################
## Dipole - Central 
month <- 9

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
jetdetrend <- pracma::detrend(as.matrix(jet))

jet_long <- seq(-125,-65,2.5)
jet_corr <- as.matrix(rowMeans(jetdetrend[,which(jet_long>=-105 & jet_long<=-97.5)]))
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
  rho[i]<- rcorr(mon2detrend[,i],jet_corr,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon2detrend[,i], jet_corr,type = c("spearman"))$P[1,2]
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


countries = c("Canada","USA","Cuba","Puerto Rico","Haiti", "Dominican Republic",
              "Mexico", "Guatemala")

map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()

plot(rotate3, add = T, col = rb, breaks = c(-0.8,-0.6,-.4,-0.2,0,.2,.4,.6,.8),   xlab="",ylab="",horizontal = F, legend = F)

map("worldHires",countries, xlim=c(-122,-68),ylim=c(16,69), fill=F,lwd = 1, add = T)  #plot the region of Canada

lon_left <- -107.5
lon_right <- -95

polygon.x <- c(seq(-125,lon_left,by = 2.5),rev(seq(-125,lon_left,by = 2.5)))
polygon.y <- c(rep(15,length(polygon.x)/2),rep(70,length(polygon.x)/2))
polygon(x=polygon.x, y=polygon.y, col=adjustcolor("gray", alpha.f=0.7), border=F) #red positive correlation 

polygon.x <- c(seq(lon_right,-65,by = 2.5),rev(seq(lon_right,-65,by = 2.5)))
polygon.y <- c(rep(15,length(polygon.x)/2),rep(70,length(polygon.x)/2))
polygon(x=polygon.x, y=polygon.y, col=adjustcolor("gray", alpha.f=0.7), border=F) #red positive correlation 

text(-65,15, "SEP_C",adj = c(1,0),cex = 1.5)
#text(-65,15, "SEP",adj = c(1,0),cex = 1.5)
#dev.off()

##############################################################################################################################################################################################################################################################################################################################################################################################################################################
# ## Dipole east is only significant with pearsons correlation - positively correlated w monarch migration.


# ## Dipole - East
# month <- 9
# 
# mon <- Clim_var.nc[,,time[,2]==month]
# mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
# #dim(mon1)<- c(25, length(latitude)*length(longitude))
# #mon1df <- as.data.frame(mon1)
# #require(pracma)
# #mon1detrend<- pracma::detrend(as.matrix(mon1df))
# #mon1detrend<-as.data.frame(mon1detrend)
# 
# west <- -130
# east <- -65
# #subset mon1 by longitudes for jet correlation
# mon2 <- mon1[,,which(longitude>west & east>longitude)]
# long_2 <- longitude[which(longitude>west & east>longitude)]
# dim(mon2)<- c(25, length(latitude)*length(long_2))
# mon2detrend<- data.frame(pracma::detrend(as.matrix(mon2)))
# 
# jet <- jet1[seq(month, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
# jet <- jet[47:71,]#1994-2018: 47:71
# rownames(jet) <- 1994:2018
# jet <- lapply(jet, as.character)
# jet <- lapply(jet, as.numeric)
# jet <- as.data.frame(jet)
# jetdetrend <- pracma::detrend(as.matrix(jet))
# 
# jet_long <- seq(-125,-65,2.5)
# jet_corr <- as.matrix(rowMeans(jetdetrend[,which(jet_long>=-85 & jet_long<=-80)]))
# 
# #dim(mon1detrend) <- c(length(data_time),length(latitude),length(longitude)) #not sure why this doesn't work?
# 
# ##  Correlations
# #make a rho and pval to find significant correlations.
# rho <- NA[length(mon2detrend)]
# pval <- NA[length(mon2detrend)]
# #
# for (i in 1:length(mon2detrend)){
#   rho[i]<- rcorr(mon2detrend[,i],jet_corr,type = c("spearman"))$r[1,2]
#   pval[i]<- rcorr(mon2detrend[,i], jet_corr,type = c("spearman"))$P[1,2]
# }
# 
# rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
# mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))
# 
# for (i in 1:length(mon2detrend)){
#   if (is.na(pval[i]) | pval[i] < 0.1){
#     rho1[i] <- rho[i] # significant correlation values
#     mon1df1[,i] <- mon2detrend[,i] #climate time series of significant pixels
#   }
# }
# 
# library(raster)
# rho2 <- as.matrix(rho1)
# dim(rho2) <- c(length(latitude), length(long_2))
# rho2 <- as.data.frame(rho2)
# colnames(rho2) <- long_2
# rownames(rho2) <- latitude
# rho2 <- as.matrix(rho2)
# rotate3 <- raster(rho2[nrow(rho2):1,])
# #rotate3 <- raster(t(flip(rho2, 1)))
# #image(t(flip(x, 2)))
# 
# extent(rotate3) <- extent(c(min(long_2),max(long_2),min(latitude),max(latitude)))
# 
# 
# countries = c("Canada","USA","Cuba","Puerto Rico","Haiti", "Dominican Republic",
#               "Mexico", "Guatemala")
# 
# map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
# map.axes()
# 
# plot(rotate3, add = T, col = rb, breaks = c(-0.8,-0.6,-.4,-0.2,0,.2,.4,.6,.8),   xlab="",ylab="",horizontal = F, legend = F)
# 
# map("worldHires",countries, xlim=c(-122,-68),ylim=c(16,69), fill=F,lwd = 1, add = T)  #plot the region of Canada
# 
# lon_left <- -87.5
# lon_right <- -77.5
# 
# polygon.x <- c(seq(-125,lon_left,by = 2.5),rev(seq(-125,lon_left,by = 2.5)))
# polygon.y <- c(rep(15,length(polygon.x)/2),rep(70,length(polygon.x)/2))
# polygon(x=polygon.x, y=polygon.y, col=adjustcolor("gray", alpha.f=0.7), border=F) #red positive correlation
# 
# polygon.x <- c(seq(lon_right,-65,by = 2.5),rev(seq(lon_right,-65,by = 2.5)))
# polygon.y <- c(rep(15,length(polygon.x)/2),rep(70,length(polygon.x)/2))
# polygon(x=polygon.x, y=polygon.y, col=adjustcolor("gray", alpha.f=0.7), border=F) #red positive correlation
# 
# text(-65,15, "SEP_E",adj = c(1,0),cex = 1.5)
##############################################################################################################################################################################################################################################################################################################################################################################################################################################
month <- 10

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
jetdetrend <- pracma::detrend(as.matrix(jet))

jet_long <- seq(-125,-65,2.5)
jet_corr <- matrix(rowMeans(jetdetrend[,which(jet_long>=-123 & jet_long<=-113)]))
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
  rho[i]<- rcorr(mon2detrend[,i],jet_corr,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon2detrend[,i],jet_corr,type = c("spearman"))$P[1,2]
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


countries = c("Canada","USA","Cuba","Puerto Rico","Haiti", "Dominican Republic",
              "Mexico", "Guatemala")

map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()

plot(rotate3, add = T, col = rb, breaks = c(-0.8,-0.6,-.4,-0.2,0,.2,.4,.6,.8),   xlab="",ylab="",horizontal = F, legend = F)

map("worldHires",countries, xlim=c(-122,-68),ylim=c(16,69), fill=F,lwd = 1, add = T)  #plot the region of Canada

lon_left <- -122.5
lon_right <- -112.5

polygon.x <- c(seq(-125,lon_left,by = 2.5),rev(seq(-125,lon_left,by = 2.5)))
polygon.y <- c(rep(15,length(polygon.x)/2),rep(70,length(polygon.x)/2))
polygon(x=polygon.x, y=polygon.y, col=adjustcolor("gray", alpha.f=0.7), border=F) #red positive correlation 

polygon.x <- c(seq(lon_right,-65,by = 2.5),rev(seq(lon_right,-65,by = 2.5)))
polygon.y <- c(rep(15,length(polygon.x)/2),rep(70,length(polygon.x)/2))
polygon(x=polygon.x, y=polygon.y, col=adjustcolor("gray", alpha.f=0.7), border=F) #red positive correlation 

text(-65,15, "OCT",adj = c(1,0),cex = 1.5)

dev.off()

##############################################################################################################################################
## Monarch Temp
png("fig2_ASO_monarch_temp_corr_linear_spearman_0.1.png",8,13,
    units = "in",res = 600, pointsize=20, family= "helvetica")
par(mfrow=c(3,1))#,mar = c(0, 0, 0, 0))

mexicoarea <- read.table('Butterflies.txt',header = T)
mexicoarea[,1] <- 1994:2018

mexicoareadetrend <- as.matrix(pracma::detrend(mexicoarea[,2]))
#mexicoareadetrend <- as.matrix(detrend(mexicoarea,make.plot = T, method = c("Spline"))[,2]) # detrended using modified neg exp
#mexicoareadetrend <- detrend(mexicoarea,make.plot = T, method = c("Spline")) # 

##################################################################################################################################################
month <- 8

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

##  Correlations
#make a rho and pval to find significant correlations.
rho <- NA[length(mon2detrend)]
pval <- NA[length(mon2detrend)]
# 
for (i in 1:length(mon2detrend)){
  rho[i]<- rcorr(mon2detrend[,i],mexicoareadetrend,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon2detrend[,i],mexicoareadetrend,type = c("spearman"))$P[1,2]
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


countries = c("Canada","USA","Cuba","Puerto Rico","Haiti", "Dominican Republic",
              "Mexico", "Guatemala")

map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()

plot(rotate3, add = T, col = rb, breaks = c(-0.8,-0.6,-.4,-0.2,0,.2,.4,.6,.8),   xlab="",ylab="",horizontal = F, legend = F)

map("worldHires",countries, xlim=c(-122,-68),ylim=c(16,69), fill=F,lwd = 1, add = T)  #plot the region of Canada

lon_left <- -105
lon_right <- -70

polygon.x <- c(seq(-125,lon_left,by = 2.5),rev(seq(-125,lon_left,by = 2.5)))
polygon.y <- c(rep(15,length(polygon.x)/2),rep(70,length(polygon.x)/2))
polygon(x=polygon.x, y=polygon.y, col=adjustcolor("gray", alpha.f=0.7), border=F) #red positive correlation 

polygon.x <- c(seq(lon_right,-65,by = 2.5),rev(seq(lon_right,-65,by = 2.5)))
polygon.y <- c(rep(15,length(polygon.x)/2),rep(70,length(polygon.x)/2))
polygon(x=polygon.x, y=polygon.y, col=adjustcolor("gray", alpha.f=0.7), border=F) #red positive correlation 

text(-65,15, "AUG",adj = c(1,0),cex = 1.5)

plot(rotate3, legend.only = T,col =rb, breaks = c(-0.8,-0.6,-.4,-0.2,0,.2,.4,.6,.8),legend.width=1, legend.shrink=1,legend.args=list(text=c(expression(paste(rho,"\n, p<0.1"))),side=3, line=.25, cex=1), axis.args = list(cex.axis = 1),horiz=F)

#plot(rotate3, legend.only = T,col =rb, breaks = c(-0.8,-0.6,-.4,-0.2,0,.2,.4,.6,.8),legend.width=1, legend.shrink=1,legend.args=list(text=c(expression(italic("r"))),side=3, line=.25, cex=1), axis.args = list(cex.axis = 1),horiz=F)


##############################################################################################################################################################################################################################################################################################################################################################################################################################################
month <- 9

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


##  Correlations
#make a rho and pval to find significant correlations.
rho <- NA[length(mon2detrend)]
pval <- NA[length(mon2detrend)]
# 
for (i in 1:length(mon2detrend)){
  rho[i]<- rcorr(mon2detrend[,i],mexicoareadetrend,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon2detrend[,i],mexicoareadetrend,type = c("spearman"))$P[1,2]
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


countries = c("Canada","USA","Cuba","Puerto Rico","Haiti", "Dominican Republic",
              "Mexico", "Guatemala")

map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()

plot(rotate3, add = T, col = rb, breaks = c(-0.8,-0.6,-.4,-0.2,0,.2,.4,.6,.8),   xlab="",ylab="",horizontal = F, legend = F)

map("worldHires",countries, xlim=c(-122,-68),ylim=c(16,69), fill=F,lwd = 1, add = T)  #plot the region of Canada

lon_left <- -107.5
lon_right <- -95

polygon.x <- c(seq(-125,lon_left,by = 2.5),rev(seq(-125,lon_left,by = 2.5)))
polygon.y <- c(rep(15,length(polygon.x)/2),rep(70,length(polygon.x)/2))
polygon(x=polygon.x, y=polygon.y, col=adjustcolor("gray", alpha.f=0.7), border=F) #red positive correlation 

polygon.x <- c(seq(lon_right,-65,by = 2.5),rev(seq(lon_right,-65,by = 2.5)))
polygon.y <- c(rep(15,length(polygon.x)/2),rep(70,length(polygon.x)/2))
polygon(x=polygon.x, y=polygon.y, col=adjustcolor("gray", alpha.f=0.7), border=F) #red positive correlation 

text(-65,15, "SEP",adj = c(1,0),cex = 1.5)
##############################################################################################################################################################################################################################################################################################################################################################################################################################################
month <- 10

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

##  Correlations
#make a rho and pval to find significant correlations.
rho <- NA[length(mon2detrend)]
pval <- NA[length(mon2detrend)]
# 
for (i in 1:length(mon2detrend)){
  rho[i]<- rcorr(mon2detrend[,i],mexicoareadetrend,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon2detrend[,i],mexicoareadetrend,type = c("spearman"))$P[1,2]
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


countries = c("Canada","USA","Cuba","Puerto Rico","Haiti", "Dominican Republic",
              "Mexico", "Guatemala")

map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()

plot(rotate3, add = T, col = rb, breaks = c(-0.8,-0.6,-.4,-0.2,0,.2,.4,.6,.8),   xlab="",ylab="",horizontal = F, legend = F)

map("worldHires",countries, xlim=c(-122,-68),ylim=c(16,69), fill=F,lwd = 1, add = T)  #plot the region of Canada

lon_left <- -122.5
lon_right <- -112.5

polygon.x <- c(seq(-125,lon_left,by = 2.5),rev(seq(-125,lon_left,by = 2.5)))
polygon.y <- c(rep(15,length(polygon.x)/2),rep(70,length(polygon.x)/2))
polygon(x=polygon.x, y=polygon.y, col=adjustcolor("gray", alpha.f=0.7), border=F) #red positive correlation 

polygon.x <- c(seq(lon_right,-65,by = 2.5),rev(seq(lon_right,-65,by = 2.5)))
polygon.y <- c(rep(15,length(polygon.x)/2),rep(70,length(polygon.x)/2))
polygon(x=polygon.x, y=polygon.y, col=adjustcolor("gray", alpha.f=0.7), border=F) #red positive correlation 

text(-65,15, "OCT",adj = c(1,0),cex = 1.5)


dev.off()
############################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
png("fig2_ASO_monarch_temp_corr_linear_pearson_0.1.png",8,13,
    units = "in",res = 600, pointsize=20, family= "helvetica")
par(mfrow=c(3,1))#,mar = c(0, 0, 0, 0))

mexicoarea <- read.table('Butterflies.txt',header = T)
mexicoarea[,1] <- 1994:2018

mexicoareadetrend <- as.matrix(pracma::detrend(mexicoarea[,2]))
#mexicoareadetrend <- as.matrix(detrend(mexicoarea,make.plot = T, method = c("Spline"))[,2]) # detrended using modified neg exp
#mexicoareadetrend <- detrend(mexicoarea,make.plot = T, method = c("Spline")) # 

##################################################################################################################################################
month <- 8

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

##  Correlations
#make a rho and pval to find significant correlations.
rho <- NA[length(mon2detrend)]
pval <- NA[length(mon2detrend)]
# 
for (i in 1:length(mon2detrend)){
  rho[i]<- rcorr(mon2detrend[,i],mexicoareadetrend,type = c("pearson"))$r[1,2]
  pval[i]<- rcorr(mon2detrend[,i],mexicoareadetrend,type = c("pearson"))$P[1,2]
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


countries = c("Canada","USA","Cuba","Puerto Rico","Haiti", "Dominican Republic",
              "Mexico", "Guatemala")

map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()

plot(rotate3, add = T, col = rb, breaks = c(-0.8,-0.6,-.4,-0.2,0,.2,.4,.6,.8),   xlab="",ylab="",horizontal = F, legend = F)

map("worldHires",countries, xlim=c(-122,-68),ylim=c(16,69), fill=F,lwd = 1, add = T)  #plot the region of Canada

lon_left <- -110
lon_right <- -70

polygon.x <- c(seq(-125,lon_left,by = 2.5),rev(seq(-125,lon_left,by = 2.5)))
polygon.y <- c(rep(15,length(polygon.x)/2),rep(70,length(polygon.x)/2))
polygon(x=polygon.x, y=polygon.y, col=adjustcolor("gray", alpha.f=0.7), border=F) #red positive correlation 

polygon.x <- c(seq(lon_right,-65,by = 2.5),rev(seq(lon_right,-65,by = 2.5)))
polygon.y <- c(rep(15,length(polygon.x)/2),rep(70,length(polygon.x)/2))
polygon(x=polygon.x, y=polygon.y, col=adjustcolor("gray", alpha.f=0.7), border=F) #red positive correlation 

polygon.x <- c(seq(-102.5,-95,by = 2.5),rev(seq(-102.5,-95,by = 2.5)))
polygon.y <- c(rep(15,length(polygon.x)/2),rep(70,length(polygon.x)/2))
polygon(x=polygon.x, y=polygon.y, col=adjustcolor("gray", alpha.f=0.7), border=F) #red positive correlation 

polygon.x <- c(seq(-82.5,-77.5,by = 2.5),rev(seq(-82.5,-77.5,by = 2.5)))
polygon.y <- c(rep(15,length(polygon.x)/2),rep(70,length(polygon.x)/2))
polygon(x=polygon.x, y=polygon.y, col=adjustcolor("gray", alpha.f=0.7), border=F) #red positive correlation 

text(-65,15, "AUG",adj = c(1,0),cex = 1.5)

#plot(rotate3, legend.only = T,col =rb, breaks = c(-0.8,-0.6,-.4,-0.2,0,.2,.4,.6,.8),legend.width=1, legend.shrink=1,legend.args=list(text=c(expression(paste(rho))),side=3, line=.25, cex=1), axis.args = list(cex.axis = 1),horiz=F)

plot(rotate3, legend.only = T,col =rb, breaks = c(-0.8,-0.6,-.4,-0.2,0,.2,.4,.6,.8),legend.width=1, legend.shrink=1,legend.args=list(text=c(expression(italic("r"))),side=3, line=.25, cex=1), axis.args = list(cex.axis = 1),horiz=F)


##############################################################################################################################################################################################################################################################################################################################################################################################################################################
month <- 9

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


##  Correlations
#make a rho and pval to find significant correlations.
rho <- NA[length(mon2detrend)]
pval <- NA[length(mon2detrend)]
# 
for (i in 1:length(mon2detrend)){
  rho[i]<- rcorr(mon2detrend[,i],mexicoareadetrend,type = c("pearson"))$r[1,2]
  pval[i]<- rcorr(mon2detrend[,i],mexicoareadetrend,type = c("pearson"))$P[1,2]
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


countries = c("Canada","USA","Cuba","Puerto Rico","Haiti", "Dominican Republic",
              "Mexico", "Guatemala")

map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()

plot(rotate3, add = T, col = rb, breaks = c(-0.8,-0.6,-.4,-0.2,0,.2,.4,.6,.8),   xlab="",ylab="",horizontal = F, legend = F)

map("worldHires",countries, xlim=c(-122,-68),ylim=c(16,69), fill=F,lwd = 1, add = T)  #plot the region of Canada

lon_left <- -120
lon_right <- -77.5

polygon.x <- c(seq(-125,lon_left,by = 2.5),rev(seq(-125,lon_left,by = 2.5)))
polygon.y <- c(rep(15,length(polygon.x)/2),rep(70,length(polygon.x)/2))
polygon(x=polygon.x, y=polygon.y, col=adjustcolor("gray", alpha.f=0.7), border=F) #red positive correlation 

polygon.x <- c(seq(lon_right,-65,by = 2.5),rev(seq(lon_right,-65,by = 2.5)))
polygon.y <- c(rep(15,length(polygon.x)/2),rep(70,length(polygon.x)/2))
polygon(x=polygon.x, y=polygon.y, col=adjustcolor("gray", alpha.f=0.7), border=F) #red positive correlation 

polygon.x <- c(seq(-112.5,-87.5,by = 2.5),rev(seq(-112.5,-87.5,by = 2.5)))
polygon.y <- c(rep(15,length(polygon.x)/2),rep(70,length(polygon.x)/2))
polygon(x=polygon.x, y=polygon.y, col=adjustcolor("gray", alpha.f=0.7), border=F) #red positive correlation 


text(-65,15, "SEP",adj = c(1,0),cex = 1.5)
##############################################################################################################################################################################################################################################################################################################################################################################################################################################
month <- 10

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

##  Correlations
#make a rho and pval to find significant correlations.
rho <- NA[length(mon2detrend)]
pval <- NA[length(mon2detrend)]
# 
for (i in 1:length(mon2detrend)){
  rho[i]<- rcorr(mon2detrend[,i],mexicoareadetrend,type = c("pearson"))$r[1,2]
  pval[i]<- rcorr(mon2detrend[,i],mexicoareadetrend,type = c("pearson"))$P[1,2]
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


countries = c("Canada","USA","Cuba","Puerto Rico","Haiti", "Dominican Republic",
              "Mexico", "Guatemala")

map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()

plot(rotate3, add = T, col = rb, breaks = c(-0.8,-0.6,-.4,-0.2,0,.2,.4,.6,.8),   xlab="",ylab="",horizontal = F, legend = F)

map("worldHires",countries, xlim=c(-122,-68),ylim=c(16,69), fill=F,lwd = 1, add = T)  #plot the region of Canada

lon_left <- -122.5
lon_right <- -112.5

polygon.x <- c(seq(-125,lon_left,by = 2.5),rev(seq(-125,lon_left,by = 2.5)))
polygon.y <- c(rep(15,length(polygon.x)/2),rep(70,length(polygon.x)/2))
polygon(x=polygon.x, y=polygon.y, col=adjustcolor("gray", alpha.f=0.7), border=F) #red positive correlation 

polygon.x <- c(seq(lon_right,-65,by = 2.5),rev(seq(lon_right,-65,by = 2.5)))
polygon.y <- c(rep(15,length(polygon.x)/2),rep(70,length(polygon.x)/2))
polygon(x=polygon.x, y=polygon.y, col=adjustcolor("gray", alpha.f=0.7), border=F) #red positive correlation 

text(-65,15, "OCT",adj = c(1,0),cex = 1.5)


dev.off()

###################################################################################################################################
## For 3rd Column Monarch Jet correlation



n <- read.table("NHJ_position_global_1948jan-2019sep_ncepncar.txt")
colnames(n) <- seq(0,357.5,2.5)
jet1 <- n[2:853,]

mexicoarea <- read.table('Butterflies.txt',header = T)
mexicoarea <- mexicoarea[1:25,]
mexicoareadetrend <- matrix(NA,nrow = length(1994:2018),ncol = 2)
mexicoareadetrend[,2] <- lm(mexicoarea$MexicoArea~mexicoarea$Year)$residuals
mexicoareadetrend[,1] <- 1994:2018 #1994:2012
mexicoareadetrend <- data.frame(mexicoareadetrend)
colnames(mexicoareadetrend) <- c("Year", "MexicoArea")

# August
month <- 8
jet <- jet1[seq(month, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
jet <- jet[47:71,]#1994-2018: 47:71
rownames(jet) <- 1994:2018
jet <- lapply(jet, as.character)
jet <- lapply(jet, as.numeric)
jet <- as.data.frame(jet)
jetdetrend <- pracma::detrend(as.matrix(jet))
jet_long <- seq(-125,-65,2.5)

jet_corr <- matrix(rowMeans(jetdetrend[,which(jet_long>=-102.5 & jet_long<=-72.5)]))

rcorr(mexicoareadetrend$MexicoArea,jet_corr, type = "spearman")

#Sept
month <- 9
jet <- jet1[seq(month, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
jet <- jet[47:71,]#1994-2018: 47:71
rownames(jet) <- 1994:2018
jet <- lapply(jet, as.character)
jet <- lapply(jet, as.numeric)
jet <- as.data.frame(jet)
jetdetrend <- pracma::detrend(as.matrix(jet))
jet_long <- seq(-125,-65,2.5)

#jet_corr <- matrix(rowMeans(jetdetrend[,which(jet_long>=-120 & jet_long<=-97.5)]))
#rcorr(mexicoareadetrend$MexicoArea,jet_corr, type = "spearman")

jet_corr <- matrix(rowMeans(jetdetrend[,which(jet_long>=-105 & jet_long<=-97.5)]))
rcorr(mexicoareadetrend$MexicoArea,jet_corr, type = "spearman")$r[1,2]
#plot(1994:2018,-scale(mexicoareadetrend$MexicoArea), type = "l")
#lines(1994:2018,scale(matrix(rowMeans(jet[,which(jet_long>=-105 & jet_long<=-97.5)]))), col = "red")

jet_corr <- matrix(rowMeans(jetdetrend[,which(jet_long>=-120& jet_long<=-112.5)]))
rcorr(mexicoareadetrend$MexicoArea,jet_corr, type = "spearman")

#jet_corr <- matrix(rowMeans(jetdetrend[,which(jet_long>=-85& jet_long<=-80)]))
#rcorr(mexicoareadetrend$MexicoArea,jet_corr, type = "spearman")

map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()

plot(rotate3, add = T, col = rb, breaks = c(-0.8,-0.6,-.4,-0.2,0,.2,.4,.6,.8),   xlab="",ylab="",horizontal = F, legend = F)

map("worldHires",countries, xlim=c(-122,-68),ylim=c(16,69), fill=F,lwd = 1, add = T)  #plot the region of Canada

lon_left <- -107.5
lon_right <- -95

polygon.x <- c(seq(-125,lon_left,by = 2.5),rev(seq(-125,lon_left,by = 2.5)))
polygon.y <- c(rep(15,length(polygon.x)/2),rep(70,length(polygon.x)/2))
polygon(x=polygon.x, y=polygon.y, col=adjustcolor("gray", alpha.f=0.7), border=F) #red positive correlation 

polygon.x <- c(seq(lon_right,-65,by = 2.5),rev(seq(lon_right,-65,by = 2.5)))
polygon.y <- c(rep(15,length(polygon.x)/2),rep(70,length(polygon.x)/2))
polygon(x=polygon.x, y=polygon.y, col=adjustcolor("gray", alpha.f=0.7), border=F) #red positive correlation 

text(-65,15, "SEP",adj = c(1,0),cex = 1.5)
#

month <- 10
jet <- jet1[seq(month, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
jet <- jet[47:71,]#1994-2018: 47:71
rownames(jet) <- 1994:2018
jet <- lapply(jet, as.character)
jet <- lapply(jet, as.numeric)
jet <- as.data.frame(jet)
jetdetrend <- pracma::detrend(as.matrix(jet))
jet_long <- seq(-125,-65,2.5)

jet_corr <- matrix(rowMeans(jetdetrend[,which(jet_long>=-122.5 & jet_long<=-100)]))
rcorr(mexicoareadetrend$MexicoArea,jet_corr, type = "spearman")

jet_corr <- matrix(rowMeans(jetdetrend[,which(jet_long>=-122.5 & jet_long<=-112.5)]))
rcorr(mexicoareadetrend$MexicoArea,jet_corr, type = "spearman")

jet_corr <- matrix(rowMeans(jetdetrend[,which(jet_long>=-120 & jet_long<=-112.5)]))
rcorr(mexicoareadetrend$MexicoArea,jet_corr, type = "spearman")

jet_corr <- matrix(rowMeans(jetdetrend[,which(jet_long>=-117.5 & jet_long<=-112.5)]))
rcorr(mexicoareadetrend$MexicoArea,jet_corr, type = "spearman")

## plot the column, but could be cool to also plot the time series of these groupings with the monarch time series! color progression for seasonal advance?



#######################################################################################################################################################################################################
## ## First column August, Sept October regions 
############################################################################
## August
#jet <- read.table("NHJ_USGS/position_aug_20thcentury.txt") #rownames(jet) #1871-2012 
#jet <- jet[124:142,which(seq(0,358,2)>=235 & seq(0,358,2)<=295)] #subset 1994-2012, subset -125 to -65 or 235 to 295

png("fig2_ASO_jet_boundaries_spearman.png",8,13,
    units = "in",res = 600, pointsize=20, family= "helvetica")
par(mfrow=c(3,1))#,mar = c(0, 0, 0, 0))

jet <- jet1[seq(8, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
jet <- jet[47:71,]#1994-2018: 47:71
rownames(jet) <- 1994:2018
jet <- lapply(jet, as.character)
jet <- lapply(jet, as.numeric)
jet <- as.data.frame(jet)
jet <- jet[order(mexicoareadetrend[,2]),]

map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico

#Aug: -107.5, -87.5 -85, -75, -72.5 positive correlation 
lon_left <- -105
lon_right <- -70

polygon.x <- c(seq(lon_left,lon_right,by = 2.5),rev(seq(lon_left,lon_right,by = 2.5)))
polygon.y <- c(colMins(as.matrix(jet[which(seq(-125,-65,2.5)<=lon_right & seq(-125,-65,2.5)>= lon_left)])),
               rev(colMaxs(as.matrix(jet[which(seq(-125,-65,2.5)<=lon_right & seq(-125,-65,2.5)>= lon_left)]))))

polygon(x=polygon.x, y=polygon.y, col=adjustcolor("#B2182B", alpha.f=0.4), border=F) 

lon_left <- -105
lon_right <- -70

polygon.x <- c(seq(-125,lon_left,by = 2.5),rev(seq(-125,lon_left,by = 2.5)))
polygon.y <- c(rep(15,length(polygon.x)/2),rep(70,length(polygon.x)/2))
polygon(x=polygon.x, y=polygon.y, col=adjustcolor("gray", alpha.f=0.7), border=F) #red positive correlation 

polygon.x <- c(seq(lon_right,-65,by = 2.5),rev(seq(lon_right,-65,by = 2.5)))
polygon.y <- c(rep(15,length(polygon.x)/2),rep(70,length(polygon.x)/2))
polygon(x=polygon.x, y=polygon.y, col=adjustcolor("gray", alpha.f=0.7), border=F) #red positive correlation 
text(-65,15, "AUG",adj = c(1,0),cex = 1.5)

############################################################################
## September
#jet <- read.table("NHJ_USGS/position_sep_20thcentury.txt") #rownames(jet) #1871-2012 
#jet <- jet[124:142,which(seq(0,358,2)>=235 & seq(0,358,2)<=295)] #subset 1994-2012, subset -125 to -65 or 235 to 295
jet <- jet1[seq(9, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
jet <- jet[47:71,]#1994-2018: 47:71
rownames(jet) <- 1994:2018
jet <- lapply(jet, as.character)
jet <- lapply(jet, as.numeric)
jet <- as.data.frame(jet)
jet <- jet[order(mexicoareadetrend[,2]),]

map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico

#Sep: neg correlation

lon_left <- -107.5
lon_right <- -95

polygon.x <- c(seq(lon_left,lon_right,by = 2.5),rev(seq(lon_left,lon_right,by = 2.5)))
polygon.y <- c(colMins(as.matrix(jet[which(seq(-125,-65,2.5)<=lon_right & seq(-125,-65,2.5)>= lon_left)])),
               rev(colMaxs(as.matrix(jet[which(seq(-125,-65,2.5)<=lon_right & seq(-125,-65,2.5)>= lon_left)]))))

polygon(x=polygon.x, y=polygon.y, col=adjustcolor("#2166AC", alpha.f=0.4), border=F) 

lon_left <- -107.5
lon_right <- -95

polygon.x <- c(seq(-125,lon_left,by = 2.5),rev(seq(-125,lon_left,by = 2.5)))
polygon.y <- c(rep(15,length(polygon.x)/2),rep(70,length(polygon.x)/2))
polygon(x=polygon.x, y=polygon.y, col=adjustcolor("gray", alpha.f=0.7), border=F) #red positive correlation 

polygon.x <- c(seq(lon_right,-65,by = 2.5),rev(seq(lon_right,-65,by = 2.5)))
polygon.y <- c(rep(15,length(polygon.x)/2),rep(70,length(polygon.x)/2))
polygon(x=polygon.x, y=polygon.y, col=adjustcolor("gray", alpha.f=0.7), border=F) #red positive correlation 
text(-65,15, "SEP",adj = c(1,0),cex = 1.5)



############################################################################
## October
#jet <- read.table("NHJ_USGS/position_oct_20thcentury.txt") #rownames(jet) #1871-2012 
#jet <- jet[124:142,which(seq(0,358,2)>=235 & seq(0,358,2)<=295)] #subset 1994-2012, subset -125 to -65 or 235 to 295
jet <- jet1[seq(10, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
jet <- jet[47:71,]#1994-2018: 47:71
rownames(jet) <- 1994:2018
jet <- lapply(jet, as.character)
jet <- lapply(jet, as.numeric)
jet <- as.data.frame(jet)
jet <- jet[order(mexicoareadetrend[,2]),]

map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico

#Oct: -122.5 pos correlation -115 -112.5 pos
lon_left <- -122.5
lon_right <- -112.5

polygon.x <- c(seq(lon_left,lon_right,by = 2.5),rev(seq(lon_left,lon_right,by = 2.5)))
polygon.y <- c(colMins(as.matrix(jet[which(seq(-125,-65,2.5)<=lon_right & seq(-125,-65,2.5)>= lon_left)])),
               rev(colMaxs(as.matrix(jet[which(seq(-125,-65,2.5)<=lon_right & seq(-125,-65,2.5)>= lon_left)]))))

polygon(x=polygon.x, y=polygon.y, col=adjustcolor("#B2182B", alpha.f=0.4), border=F) 


lon_left <- -122.5
lon_right <- -112.5

polygon.x <- c(seq(-125,lon_left,by = 2.5),rev(seq(-125,lon_left,by = 2.5)))
polygon.y <- c(rep(15,length(polygon.x)/2),rep(70,length(polygon.x)/2))
polygon(x=polygon.x, y=polygon.y, col=adjustcolor("gray", alpha.f=0.7), border=F) #red positive correlation 

polygon.x <- c(seq(lon_right,-65,by = 2.5),rev(seq(lon_right,-65,by = 2.5)))
polygon.y <- c(rep(15,length(polygon.x)/2),rep(70,length(polygon.x)/2))
polygon(x=polygon.x, y=polygon.y, col=adjustcolor("gray", alpha.f=0.7), border=F) #red positive correlation 

text(-65,15, "OCT",adj = c(1,0),cex = 1.5)
dev.off()


########################################################################################################################################################################################################################################################################################################################################################################################################################################################################
png("fig2_SEP_W.png",8,13,
    units = "in",res = 600, pointsize=20, family= "helvetica")
par(mfrow=c(3,1))#,mar = c(0, 0, 0, 0))



map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico

lon_left <- -120
lon_right <- -112.5

polygon.x <- c(seq(lon_left,lon_right,by = 2.5),rev(seq(lon_left,lon_right,by = 2.5)))
polygon.y <- c(colMins(as.matrix(jet[which(seq(-125,-65,2.5)<=lon_right & seq(-125,-65,2.5)>= lon_left)])),
               rev(colMaxs(as.matrix(jet[which(seq(-125,-65,2.5)<=lon_right & seq(-125,-65,2.5)>= lon_left)]))))

polygon(x=polygon.x, y=polygon.y, col=adjustcolor("#2166AC", alpha.f=0.4), border=F) 

lon_left <- -120
lon_right <- -112.5

polygon.x <- c(seq(-125,lon_left,by = 2.5),rev(seq(-125,lon_left,by = 2.5)))
polygon.y <- c(rep(15,length(polygon.x)/2),rep(70,length(polygon.x)/2))
polygon(x=polygon.x, y=polygon.y, col=adjustcolor("gray", alpha.f=0.7), border=F) #red positive correlation 

polygon.x <- c(seq(lon_right,-65,by = 2.5),rev(seq(lon_right,-65,by = 2.5)))
polygon.y <- c(rep(15,length(polygon.x)/2),rep(70,length(polygon.x)/2))
polygon(x=polygon.x, y=polygon.y, col=adjustcolor("gray", alpha.f=0.7), border=F) #red positive correlation 
text(-65,15, "SEP_W",adj = c(1,0),cex = 1.5)

## Dipole - Western
month <- 9

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
jetdetrend <- pracma::detrend(as.matrix(jet))

jet_long <- seq(-125,-65,2.5)
jet_corr <- as.matrix(rowMeans(jetdetrend[,which(jet_long>=-120 & jet_long<=-113)]))
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
  rho[i]<- rcorr(mon2detrend[,i],jet_corr,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon2detrend[,i],jet_corr,type = c("spearman"))$P[1,2]
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


countries = c("Canada","USA","Cuba","Puerto Rico","Haiti", "Dominican Republic",
              "Mexico", "Guatemala")

map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()

plot(rotate3, add = T, col = rb, breaks = c(-0.8,-0.6,-.4,-0.2,0,.2,.4,.6,.8),   xlab="",ylab="",horizontal = F, legend = F)

map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = T)  #plot the region of Canada

lon_left <- -120
lon_right <- -112.5

polygon.x <- c(seq(-125,lon_left,by = 2.5),rev(seq(-125,lon_left,by = 2.5)))
polygon.y <- c(rep(15,length(polygon.x)/2),rep(70,length(polygon.x)/2))
polygon(x=polygon.x, y=polygon.y, col=adjustcolor("gray", alpha.f=0.7), border=F) #red positive correlation 

polygon.x <- c(seq(lon_right,-65,by = 2.5),rev(seq(lon_right,-65,by = 2.5)))
polygon.y <- c(rep(15,length(polygon.x)/2),rep(70,length(polygon.x)/2))
polygon(x=polygon.x, y=polygon.y, col=adjustcolor("gray", alpha.f=0.7), border=F) #red positive correlation

text(-65,15, "SEP_W",adj = c(1,0),cex = 1.5)

month <- 9

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


##  Correlations
#make a rho and pval to find significant correlations.
rho <- NA[length(mon2detrend)]
pval <- NA[length(mon2detrend)]
# 
for (i in 1:length(mon2detrend)){
  rho[i]<- rcorr(mon2detrend[,i],mexicoareadetrend,type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon2detrend[,i],mexicoareadetrend,type = c("spearman"))$P[1,2]
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


countries = c("Canada","USA","Cuba","Puerto Rico","Haiti", "Dominican Republic",
              "Mexico", "Guatemala")

map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()

plot(rotate3, add = T, col = rb, breaks = c(-0.8,-0.6,-.4,-0.2,0,.2,.4,.6,.8),   xlab="",ylab="",horizontal = F, legend = F)

map("worldHires",countries, xlim=c(-122,-68),ylim=c(16,69), fill=F,lwd = 1, add = T)  #plot the region of Canada

lon_left <- -120
lon_right <- -112.5

polygon.x <- c(seq(-125,lon_left,by = 2.5),rev(seq(-125,lon_left,by = 2.5)))
polygon.y <- c(rep(15,length(polygon.x)/2),rep(70,length(polygon.x)/2))
polygon(x=polygon.x, y=polygon.y, col=adjustcolor("gray", alpha.f=0.7), border=F) #red positive correlation 

polygon.x <- c(seq(lon_right,-65,by = 2.5),rev(seq(lon_right,-65,by = 2.5)))
polygon.y <- c(rep(15,length(polygon.x)/2),rep(70,length(polygon.x)/2))
polygon(x=polygon.x, y=polygon.y, col=adjustcolor("gray", alpha.f=0.7), border=F) #red positive correlation

text(-65,15, "SEP_W",adj = c(1,0),cex = 1.5)
##

dev.off()