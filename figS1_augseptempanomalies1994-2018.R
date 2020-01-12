#detrend temperature anomalies? don't know if I should detrend jet... 

#insert jet 
n <- read.table("NHJ_position_global_1948jan-2019sep_ncepncar.txt")
colnames(n) <- seq(0,357.5,2.5)
jet1 <- n[2:853,]

#add loess filter
lon <- as.integer(seq(235-360,295-360,2.5))


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
library(raster)


lon <- as.integer(seq(235-360,295-360,2.5))
rb <- rev(brewer.pal(n = 8, name = 'RdBu'))

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

year <- 1994:2018
png("figS1_ASOtempanom+loess_19941998.png",8,13,
    units = "in",res = 600, pointsize=20, family= "helvetica")
par(mfrow=c(5,3),mar = c(0, 0, 0, 0))
for (i in 1:length(1994:1998)){
month <-8   
mon <- Clim_var.nc[,,time[,2]==month] # August
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(length(data_time), length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)
mon1detrend<- pracma::detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

  rho2 <- as.matrix(mon1detrend[i,])
  dim(rho2) <- c(length(latitude), length(longitude))
  rho2 <- as.data.frame(rho2)
  colnames(rho2) <- longitude
  rownames(rho2) <- latitude
  rho2 <- as.matrix(rho2)
  rotate3 <- raster(rho2[nrow(rho2):1,])
  extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))

  jet <- jet1[seq(month, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
  jet <- jet[47:71,]#1994-2018: 47:71
  rownames(jet) <- 1994:2018
  jet <- lapply(jet, as.character)
  jet <- lapply(jet, as.numeric)
  jet <- as.data.frame(jet)
  jetdetrend <- pracma::detrend(as.matrix(jet))
  
  
  
  countries = c("Canada","USA","Cuba","Puerto Rico","Haiti", "Dominican Republic",
                "Mexico", "Guatemala")
  
  map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
  map.axes()
  title(main = paste( year[i], ": ", mexicoarea[i,2]), line = .5)
  rb <- rev(brewer.pal(n = 8, name = 'RdBu'))
  plot(rotate3, add = T, col = rb, breaks = c(-6,-4.5,-3,-1.5,0,1.5,3,4.5,6),   xlab="",ylab="",horizontal = F,legend = T,legend.args=list(text=c(expression(paste("  " ,degree,"C"))),side=3, line=.25, cex=1))

  map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = T)  #plot the region of Canada
  text(-65,15, "AUG",adj = c(1,0),cex = 1.5)
  lines(lon,loess(as.numeric(jet[i,])~lon)$fitted, lwd = 5)
#  lines(lon,loess(as.numeric(jet[i,])~lon, family = "symmetric")$fitted, lwd = 5, col = "red")
  
  month <- 9
  mon <- Clim_var.nc[,,time[,2]==month] # Sept
  mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
  dim(mon1)<- c(length(data_time), length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
  mon1df <- as.data.frame(mon1)
  mon1detrend<- pracma::detrend(as.matrix(mon1df))
  mon1detrend<-as.data.frame(mon1detrend)
  
  rho2 <- as.matrix(mon1detrend[i,])
  dim(rho2) <- c(length(latitude), length(longitude))
  #rho2[rho2< -4] <- -4
  rho2 <- as.data.frame(rho2)
  colnames(rho2) <- longitude
  rownames(rho2) <- latitude
  rho2 <- as.matrix(rho2)
  rotate3 <- raster(rho2[nrow(rho2):1,])
  extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))
  
  jet <- jet1[seq(month, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
  jet <- jet[47:71,]#1994-2018: 47:71
  rownames(jet) <- 1994:2018
  jet <- lapply(jet, as.character)
  jet <- lapply(jet, as.numeric)
  jet <- as.data.frame(jet)
  jetdetrend <- pracma::detrend(as.matrix(jet))
  
  
  
  countries = c("Canada","USA","Cuba","Puerto Rico","Haiti", "Dominican Republic",
                "Mexico", "Guatemala")
  
  map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
  map.axes()
  rb <- rev(brewer.pal(n = 8, name = 'RdBu'))
  plot(rotate3, add = T, col = rb, breaks = c(-6,-4.5,-3,-1.5,0,1.5,3,4.5,6),   xlab="",ylab="",horizontal = F, legend =F)#,legend = T,legend.args=list(text=c(expression(paste("  " ,degree,"C"))),side=3, line=.25, cex=1))
  map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = T)  #plot the region of Canada
  text(-65,15, "SEP",adj = c(1,0),cex = 1.5)
  lines(lon,loess(as.numeric(jet[i,])~lon)$fitted, lwd = 5)
#  lines(lon,loess(as.numeric(jet[i,])~lon, family = "symmetric")$fitted, lwd = 5, col = "red")
  
  month <- 10
  mon <- Clim_var.nc[,,time[,2]==month] # Sept
  mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
  dim(mon1)<- c(length(data_time), length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
  mon1df <- as.data.frame(mon1)
  mon1detrend<- pracma::detrend(as.matrix(mon1df))
  mon1detrend<-as.data.frame(mon1detrend)
  
  rho2 <- as.matrix(mon1detrend[i,])
  dim(rho2) <- c(length(latitude), length(longitude))
  #rho2[rho2< -4] <- -4
  rho2 <- as.data.frame(rho2)
  colnames(rho2) <- longitude
  rownames(rho2) <- latitude
  rho2 <- as.matrix(rho2)
  rotate3 <- raster(rho2[nrow(rho2):1,])
  extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))
  
  jet <- jet1[seq(month, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
  jet <- jet[47:71,]#1994-2018: 47:71
  rownames(jet) <- 1994:2018
  jet <- lapply(jet, as.character)
  jet <- lapply(jet, as.numeric)
  jet <- as.data.frame(jet)
  jetdetrend <- pracma::detrend(as.matrix(jet))
  
  countries = c("Canada","USA","Cuba","Puerto Rico","Haiti", "Dominican Republic",
                "Mexico", "Guatemala")
  
  map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
  map.axes()
  rb <- rev(brewer.pal(n = 8, name = 'RdBu'))
  plot(rotate3, add = T, col = rb, breaks = c(-6,-4.5,-3,-1.5,0,1.5,3,4.5,6),   xlab="",ylab="",horizontal = F, legend =F)#,legend = T,legend.args=list(text=c(expression(paste("  " ,degree,"C"))),side=3, line=.25, cex=1))
  map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = T)  #plot the region of Canada
  text(-65,15, "OCT",adj = c(1,0),cex = 1.5)
  lines(lon,loess(as.numeric(jet[i,])~lon)$fitted, lwd = 5)
#  lines(lon,loess(as.numeric(jet[i,])~lon, family = "symmetric")$fitted, lwd = 5, col = "red")
  
}
dev.off()

png("figS1_ASOtempanom+loess_19992003.png",8,13,
    units = "in",res = 600, pointsize=20, family= "helvetica")
par(mfrow=c(5,3),mar = c(0, 0, 0, 0))
for (i in 1:length(1999:2003)){
  i <- i + 5
  
  month <-8   
  mon <- Clim_var.nc[,,time[,2]==month] # August
  mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
  dim(mon1)<- c(length(data_time), length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
  mon1df <- as.data.frame(mon1)
  mon1detrend<- pracma::detrend(as.matrix(mon1df))
  mon1detrend<-as.data.frame(mon1detrend)
  
  rho2 <- as.matrix(mon1detrend[i,])
  dim(rho2) <- c(length(latitude), length(longitude))
  rho2 <- as.data.frame(rho2)
  colnames(rho2) <- longitude
  rownames(rho2) <- latitude
  rho2 <- as.matrix(rho2)
  rotate3 <- raster(rho2[nrow(rho2):1,])
  extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))
  
  jet <- jet1[seq(month, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
  jet <- jet[47:71,]#1994-2018: 47:71
  rownames(jet) <- 1994:2018
  jet <- lapply(jet, as.character)
  jet <- lapply(jet, as.numeric)
  jet <- as.data.frame(jet)
  jetdetrend <- pracma::detrend(as.matrix(jet))
  
  
  
  countries = c("Canada","USA","Cuba","Puerto Rico","Haiti", "Dominican Republic",
                "Mexico", "Guatemala")
  
  map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
  map.axes()
  title(main = paste( year[i], ": ", mexicoarea[i,2]), line = .5)
  rb <- rev(brewer.pal(n = 8, name = 'RdBu'))
  plot(rotate3, add = T, col = rb, breaks = c(-6,-4.5,-3,-1.5,0,1.5,3,4.5,6),   xlab="",ylab="",horizontal = F, legend =F)#,legend = T,legend.args=list(text=c(expression(paste("  " ,degree,"C"))),side=3, line=.25, cex=1))
  
  map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = T)  #plot the region of Canada
  text(-65,15, "AUG",adj = c(1,0),cex = 1.5)
  lines(lon,loess(as.numeric(jet[i,])~lon)$fitted, lwd = 5)
#  lines(lon,loess(as.numeric(jet[i,])~lon, family = "symmetric")$fitted, lwd = 5, col = "red")
  
  month <- 9
  mon <- Clim_var.nc[,,time[,2]==month] # Sept
  mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
  dim(mon1)<- c(length(data_time), length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
  mon1df <- as.data.frame(mon1)
  mon1detrend<- pracma::detrend(as.matrix(mon1df))
  mon1detrend<-as.data.frame(mon1detrend)
  
  rho2 <- as.matrix(mon1detrend[i,])
  dim(rho2) <- c(length(latitude), length(longitude))
  #rho2[rho2< -4] <- -4
  rho2 <- as.data.frame(rho2)
  colnames(rho2) <- longitude
  rownames(rho2) <- latitude
  rho2 <- as.matrix(rho2)
  rotate3 <- raster(rho2[nrow(rho2):1,])
  extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))
  
  jet <- jet1[seq(month, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
  jet <- jet[47:71,]#1994-2018: 47:71
  rownames(jet) <- 1994:2018
  jet <- lapply(jet, as.character)
  jet <- lapply(jet, as.numeric)
  jet <- as.data.frame(jet)
  jetdetrend <- pracma::detrend(as.matrix(jet))
  
  
  
  countries = c("Canada","USA","Cuba","Puerto Rico","Haiti", "Dominican Republic",
                "Mexico", "Guatemala")
  
  map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
  map.axes()
  rb <- rev(brewer.pal(n = 8, name = 'RdBu'))
  plot(rotate3, add = T, col = rb, breaks = c(-6,-4.5,-3,-1.5,0,1.5,3,4.5,6),   xlab="",ylab="",horizontal = F, legend =F)#,legend = T,legend.args=list(text=c(expression(paste("  " ,degree,"C"))),side=3, line=.25, cex=1))
  map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = T)  #plot the region of Canada
  text(-65,15, "SEP",adj = c(1,0),cex = 1.5)
  lines(lon,loess(as.numeric(jet[i,])~lon)$fitted, lwd = 5)
#  lines(lon,loess(as.numeric(jet[i,])~lon, family = "symmetric")$fitted, lwd = 5, col = "red")
  
  month <- 10
  mon <- Clim_var.nc[,,time[,2]==month] # Sept
  mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
  dim(mon1)<- c(length(data_time), length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
  mon1df <- as.data.frame(mon1)
  mon1detrend<- pracma::detrend(as.matrix(mon1df))
  mon1detrend<-as.data.frame(mon1detrend)
  
  rho2 <- as.matrix(mon1detrend[i,])
  dim(rho2) <- c(length(latitude), length(longitude))
  #rho2[rho2< -4] <- -4
  rho2 <- as.data.frame(rho2)
  colnames(rho2) <- longitude
  rownames(rho2) <- latitude
  rho2 <- as.matrix(rho2)
  rotate3 <- raster(rho2[nrow(rho2):1,])
  extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))
  
  jet <- jet1[seq(month, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
  jet <- jet[47:71,]#1994-2018: 47:71
  rownames(jet) <- 1994:2018
  jet <- lapply(jet, as.character)
  jet <- lapply(jet, as.numeric)
  jet <- as.data.frame(jet)
  jetdetrend <- pracma::detrend(as.matrix(jet))
  
  countries = c("Canada","USA","Cuba","Puerto Rico","Haiti", "Dominican Republic",
                "Mexico", "Guatemala")
  
  map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
  map.axes()
  rb <- rev(brewer.pal(n = 8, name = 'RdBu'))
  plot(rotate3, add = T, col = rb, breaks = c(-6,-4.5,-3,-1.5,0,1.5,3,4.5,6),   xlab="",ylab="",horizontal = F, legend =F)#,legend = T,legend.args=list(text=c(expression(paste("  " ,degree,"C"))),side=3, line=.25, cex=1))
  map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = T)  #plot the region of Canada
  text(-65,15, "OCT",adj = c(1,0),cex = 1.5)
  lines(lon,loess(as.numeric(jet[i,])~lon)$fitted, lwd = 5)
#  lines(lon,loess(as.numeric(jet[i,])~lon, family = "symmetric")$fitted, lwd = 5, col = "red")
  
}
dev.off()

png("figS1_ASOtempanom+loess_20042008.png",8,13,
    units = "in",res = 600, pointsize=20, family= "helvetica")
par(mfrow=c(5,3),mar = c(0, 0, 0, 0))
for (i in 1:length(2004:2008)){
  i <- i + 10
  
  month <-8   
  mon <- Clim_var.nc[,,time[,2]==month] # August
  mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
  dim(mon1)<- c(length(data_time), length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
  mon1df <- as.data.frame(mon1)
  mon1detrend<- pracma::detrend(as.matrix(mon1df))
  mon1detrend<-as.data.frame(mon1detrend)
  
  rho2 <- as.matrix(mon1detrend[i,])
  dim(rho2) <- c(length(latitude), length(longitude))
  rho2 <- as.data.frame(rho2)
  colnames(rho2) <- longitude
  rownames(rho2) <- latitude
  rho2 <- as.matrix(rho2)
  rotate3 <- raster(rho2[nrow(rho2):1,])
  extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))
  
  jet <- jet1[seq(month, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
  jet <- jet[47:71,]#1994-2018: 47:71
  rownames(jet) <- 1994:2018
  jet <- lapply(jet, as.character)
  jet <- lapply(jet, as.numeric)
  jet <- as.data.frame(jet)
  jetdetrend <- pracma::detrend(as.matrix(jet))
  
  
  
  countries = c("Canada","USA","Cuba","Puerto Rico","Haiti", "Dominican Republic",
                "Mexico", "Guatemala")
  
  map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
  map.axes()
  title(main = paste( year[i], ": ", mexicoarea[i,2]), line = .5)
  rb <- rev(brewer.pal(n = 8, name = 'RdBu'))
  plot(rotate3, add = T, col = rb, breaks = c(-6,-4.5,-3,-1.5,0,1.5,3,4.5,6),   xlab="",ylab="",horizontal = F, legend =F)#,legend = T,legend.args=list(text=c(expression(paste("  " ,degree,"C"))),side=3, line=.25, cex=1))
  
  map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = T)  #plot the region of Canada
  text(-65,15, "AUG",adj = c(1,0),cex = 1.5)
  lines(lon,loess(as.numeric(jet[i,])~lon)$fitted, lwd = 5)
#  lines(lon,loess(as.numeric(jet[i,])~lon, family = "symmetric")$fitted, lwd = 5, col = "red")
  
  month <- 9
  mon <- Clim_var.nc[,,time[,2]==month] # Sept
  mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
  dim(mon1)<- c(length(data_time), length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
  mon1df <- as.data.frame(mon1)
  mon1detrend<- pracma::detrend(as.matrix(mon1df))
  mon1detrend<-as.data.frame(mon1detrend)
  
  rho2 <- as.matrix(mon1detrend[i,])
  dim(rho2) <- c(length(latitude), length(longitude))
  #rho2[rho2< -4] <- -4
  rho2 <- as.data.frame(rho2)
  colnames(rho2) <- longitude
  rownames(rho2) <- latitude
  rho2 <- as.matrix(rho2)
  rotate3 <- raster(rho2[nrow(rho2):1,])
  extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))
  
  jet <- jet1[seq(month, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
  jet <- jet[47:71,]#1994-2018: 47:71
  rownames(jet) <- 1994:2018
  jet <- lapply(jet, as.character)
  jet <- lapply(jet, as.numeric)
  jet <- as.data.frame(jet)
  jetdetrend <- pracma::detrend(as.matrix(jet))
  
  
  
  countries = c("Canada","USA","Cuba","Puerto Rico","Haiti", "Dominican Republic",
                "Mexico", "Guatemala")
  
  map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
  map.axes()
  rb <- rev(brewer.pal(n = 8, name = 'RdBu'))
  plot(rotate3, add = T, col = rb, breaks = c(-6,-4.5,-3,-1.5,0,1.5,3,4.5,6),   xlab="",ylab="",horizontal = F, legend =F)#,legend = T,legend.args=list(text=c(expression(paste("  " ,degree,"C"))),side=3, line=.25, cex=1))
  map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = T)  #plot the region of Canada
  text(-65,15, "SEP",adj = c(1,0),cex = 1.5)
  lines(lon,loess(as.numeric(jet[i,])~lon)$fitted, lwd = 5)
#  lines(lon,loess(as.numeric(jet[i,])~lon, family = "symmetric")$fitted, lwd = 5, col = "red")
  
  month <- 10
  mon <- Clim_var.nc[,,time[,2]==month] # Sept
  mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
  dim(mon1)<- c(length(data_time), length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
  mon1df <- as.data.frame(mon1)
  mon1detrend<- pracma::detrend(as.matrix(mon1df))
  mon1detrend<-as.data.frame(mon1detrend)
  
  rho2 <- as.matrix(mon1detrend[i,])
  dim(rho2) <- c(length(latitude), length(longitude))
  #rho2[rho2< -4] <- -4
  rho2 <- as.data.frame(rho2)
  colnames(rho2) <- longitude
  rownames(rho2) <- latitude
  rho2 <- as.matrix(rho2)
  rotate3 <- raster(rho2[nrow(rho2):1,])
  extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))
  
  jet <- jet1[seq(month, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
  jet <- jet[47:71,]#1994-2018: 47:71
  rownames(jet) <- 1994:2018
  jet <- lapply(jet, as.character)
  jet <- lapply(jet, as.numeric)
  jet <- as.data.frame(jet)
  jetdetrend <- pracma::detrend(as.matrix(jet))
  
  countries = c("Canada","USA","Cuba","Puerto Rico","Haiti", "Dominican Republic",
                "Mexico", "Guatemala")
  
  map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
  map.axes()
  rb <- rev(brewer.pal(n = 8, name = 'RdBu'))
  plot(rotate3, add = T, col = rb, breaks = c(-6,-4.5,-3,-1.5,0,1.5,3,4.5,6),   xlab="",ylab="",horizontal = F, legend =F)#,legend = T,legend.args=list(text=c(expression(paste("  " ,degree,"C"))),side=3, line=.25, cex=1))
  map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = T)  #plot the region of Canada
  text(-65,15, "OCT",adj = c(1,0),cex = 1.5)
  lines(lon,loess(as.numeric(jet[i,])~lon)$fitted, lwd = 5)
#  lines(lon,loess(as.numeric(jet[i,])~lon, family = "symmetric")$fitted, lwd = 5, col = "red")
  
}
dev.off()

png("figS1_ASOtempanom+loess_20092013.png",8,13,
    units = "in",res = 600, pointsize=20, family= "helvetica")
par(mfrow=c(5,3),mar = c(0, 0, 0, 0))
for (i in 1:length(2009:2013)){
  i <- i + 15
  
  month <-8   
  mon <- Clim_var.nc[,,time[,2]==month] # August
  mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
  dim(mon1)<- c(length(data_time), length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
  mon1df <- as.data.frame(mon1)
  mon1detrend<- pracma::detrend(as.matrix(mon1df))
  mon1detrend<-as.data.frame(mon1detrend)
  
  rho2 <- as.matrix(mon1detrend[i,])
  dim(rho2) <- c(length(latitude), length(longitude))
  rho2 <- as.data.frame(rho2)
  colnames(rho2) <- longitude
  rownames(rho2) <- latitude
  rho2 <- as.matrix(rho2)
  rotate3 <- raster(rho2[nrow(rho2):1,])
  extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))
  
  jet <- jet1[seq(month, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
  jet <- jet[47:71,]#1994-2018: 47:71
  rownames(jet) <- 1994:2018
  jet <- lapply(jet, as.character)
  jet <- lapply(jet, as.numeric)
  jet <- as.data.frame(jet)
  jetdetrend <- pracma::detrend(as.matrix(jet))
  
  countries = c("Canada","USA","Cuba","Puerto Rico","Haiti", "Dominican Republic",
                "Mexico", "Guatemala")
  
  map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
  map.axes()
  title(main = paste( year[i], ": ", mexicoarea[i,2]), line = .5)
  rb <- rev(brewer.pal(n = 8, name = 'RdBu'))
  plot(rotate3, add = T, col = rb, breaks = c(-6,-4.5,-3,-1.5,0,1.5,3,4.5,6),   xlab="",ylab="",horizontal = F, legend =F)#,legend = T,legend.args=list(text=c(expression(paste("  " ,degree,"C"))),side=3, line=.25, cex=1))
  
  map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = T)  #plot the region of Canada
  text(-65,15, "AUG",adj = c(1,0),cex = 1.5)
  lines(lon,loess(as.numeric(jet[i,])~lon)$fitted, lwd = 5)
#  lines(lon,loess(as.numeric(jet[i,])~lon, family = "symmetric")$fitted, lwd = 5, col = "red")
  
  month <- 9
  mon <- Clim_var.nc[,,time[,2]==month] # Sept
  mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
  dim(mon1)<- c(length(data_time), length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
  mon1df <- as.data.frame(mon1)
  mon1detrend<- pracma::detrend(as.matrix(mon1df))
  mon1detrend<-as.data.frame(mon1detrend)
  
  rho2 <- as.matrix(mon1detrend[i,])
  dim(rho2) <- c(length(latitude), length(longitude))
  #rho2[rho2< -4] <- -4
  rho2 <- as.data.frame(rho2)
  colnames(rho2) <- longitude
  rownames(rho2) <- latitude
  rho2 <- as.matrix(rho2)
  rotate3 <- raster(rho2[nrow(rho2):1,])
  extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))
  
  jet <- jet1[seq(month, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
  jet <- jet[47:71,]#1994-2018: 47:71
  rownames(jet) <- 1994:2018
  jet <- lapply(jet, as.character)
  jet <- lapply(jet, as.numeric)
  jet <- as.data.frame(jet)
  jetdetrend <- pracma::detrend(as.matrix(jet))
  
  
  
  countries = c("Canada","USA","Cuba","Puerto Rico","Haiti", "Dominican Republic",
                "Mexico", "Guatemala")
  
  map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
  map.axes()
  rb <- rev(brewer.pal(n = 8, name = 'RdBu'))
  plot(rotate3, add = T, col = rb, breaks = c(-6,-4.5,-3,-1.5,0,1.5,3,4.5,6),   xlab="",ylab="",horizontal = F, legend =F)#,legend = T,legend.args=list(text=c(expression(paste("  " ,degree,"C"))),side=3, line=.25, cex=1))
  map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = T)  #plot the region of Canada
  text(-65,15, "SEP",adj = c(1,0),cex = 1.5)
  lines(lon,loess(as.numeric(jet[i,])~lon)$fitted, lwd = 5)
#  lines(lon,loess(as.numeric(jet[i,])~lon, family = "symmetric")$fitted, lwd = 5, col = "red")
  
  month <- 10
  mon <- Clim_var.nc[,,time[,2]==month] # Sept
  mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
  dim(mon1)<- c(length(data_time), length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
  mon1df <- as.data.frame(mon1)
  mon1detrend<- pracma::detrend(as.matrix(mon1df))
  mon1detrend<-as.data.frame(mon1detrend)
  
  rho2 <- as.matrix(mon1detrend[i,])
  dim(rho2) <- c(length(latitude), length(longitude))
  #rho2[rho2< -4] <- -4
  rho2 <- as.data.frame(rho2)
  colnames(rho2) <- longitude
  rownames(rho2) <- latitude
  rho2 <- as.matrix(rho2)
  rotate3 <- raster(rho2[nrow(rho2):1,])
  extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))
  
  jet <- jet1[seq(month, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
  jet <- jet[47:71,]#1994-2018: 47:71
  rownames(jet) <- 1994:2018
  jet <- lapply(jet, as.character)
  jet <- lapply(jet, as.numeric)
  jet <- as.data.frame(jet)
  jetdetrend <- pracma::detrend(as.matrix(jet))
  
  countries = c("Canada","USA","Cuba","Puerto Rico","Haiti", "Dominican Republic",
                "Mexico", "Guatemala")
  
  map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
  map.axes()
  rb <- rev(brewer.pal(n = 8, name = 'RdBu'))
  plot(rotate3, add = T, col = rb, breaks = c(-6,-4.5,-3,-1.5,0,1.5,3,4.5,6),   xlab="",ylab="",horizontal = F, legend =F)#,legend = T,legend.args=list(text=c(expression(paste("  " ,degree,"C"))),side=3, line=.25, cex=1))
  map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = T)  #plot the region of Canada
  text(-65,15, "OCT",adj = c(1,0),cex = 1.5)
  lines(lon,loess(as.numeric(jet[i,])~lon)$fitted, lwd = 5)
#  lines(lon,loess(as.numeric(jet[i,])~lon, family = "symmetric")$fitted, lwd = 5, col = "red")
  
}
dev.off()

png("figS1_ASOtempanom+loess_20142018.png",8,13,
    units = "in",res = 600, pointsize=20, family= "helvetica")
par(mfrow=c(5,3),mar = c(0, 0, 0, 0))
for (i in 1:length(2014:2018)){
  i <- i + 20
  month <-8   
  mon <- Clim_var.nc[,,time[,2]==month] # August
  mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
  dim(mon1)<- c(length(data_time), length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
  mon1df <- as.data.frame(mon1)
  mon1detrend<- pracma::detrend(as.matrix(mon1df))
  mon1detrend<-as.data.frame(mon1detrend)
  
  rho2 <- as.matrix(mon1detrend[i,])
  dim(rho2) <- c(length(latitude), length(longitude))
  rho2 <- as.data.frame(rho2)
  colnames(rho2) <- longitude
  rownames(rho2) <- latitude
  rho2 <- as.matrix(rho2)
  rotate3 <- raster(rho2[nrow(rho2):1,])
  extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))
  
  jet <- jet1[seq(month, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
  jet <- jet[47:71,]#1994-2018: 47:71
  rownames(jet) <- 1994:2018
  jet <- lapply(jet, as.character)
  jet <- lapply(jet, as.numeric)
  jet <- as.data.frame(jet)
  jetdetrend <- pracma::detrend(as.matrix(jet))
  
  
  
  countries = c("Canada","USA","Cuba","Puerto Rico","Haiti", "Dominican Republic",
                "Mexico", "Guatemala")
  
  map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
  map.axes()
  title(main = paste( year[i], ": ", mexicoarea[i,2]), line = .5)
  rb <- rev(brewer.pal(n = 8, name = 'RdBu'))
  plot(rotate3, add = T, col = rb, breaks = c(-6,-4.5,-3,-1.5,0,1.5,3,4.5,6),   xlab="",ylab="",horizontal = F, legend =F)#,legend = T,legend.args=list(text=c(expression(paste("  " ,degree,"C"))),side=3, line=.25, cex=1))
  
  map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = T)  #plot the region of Canada
  text(-65,15, "AUG",adj = c(1,0),cex = 1.5)
  lines(lon,loess(as.numeric(jet[i,])~lon)$fitted, lwd = 5)
#  lines(lon,loess(as.numeric(jet[i,])~lon, family = "symmetric")$fitted, lwd = 5, col = "red")
  
  month <- 9
  mon <- Clim_var.nc[,,time[,2]==month] # Sept
  mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
  dim(mon1)<- c(length(data_time), length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
  mon1df <- as.data.frame(mon1)
  mon1detrend<- pracma::detrend(as.matrix(mon1df))
  mon1detrend<-as.data.frame(mon1detrend)
  
  rho2 <- as.matrix(mon1detrend[i,])
  dim(rho2) <- c(length(latitude), length(longitude))
  #rho2[rho2< -4] <- -4
  rho2 <- as.data.frame(rho2)
  colnames(rho2) <- longitude
  rownames(rho2) <- latitude
  rho2 <- as.matrix(rho2)
  rotate3 <- raster(rho2[nrow(rho2):1,])
  extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))
  
  jet <- jet1[seq(month, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
  jet <- jet[47:71,]#1994-2018: 47:71
  rownames(jet) <- 1994:2018
  jet <- lapply(jet, as.character)
  jet <- lapply(jet, as.numeric)
  jet <- as.data.frame(jet)
  jetdetrend <- pracma::detrend(as.matrix(jet))
  
  
  
  countries = c("Canada","USA","Cuba","Puerto Rico","Haiti", "Dominican Republic",
                "Mexico", "Guatemala")
  
  map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
  map.axes()
  rb <- rev(brewer.pal(n = 8, name = 'RdBu'))
  plot(rotate3, add = T, col = rb, breaks = c(-6,-4.5,-3,-1.5,0,1.5,3,4.5,6),   xlab="",ylab="",horizontal = F, legend =F)#,legend = T,legend.args=list(text=c(expression(paste("  " ,degree,"C"))),side=3, line=.25, cex=1))
  map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = T)  #plot the region of Canada
  text(-65,15, "SEP",adj = c(1,0),cex = 1.5)
  lines(lon,loess(as.numeric(jet[i,])~lon)$fitted, lwd = 5)
#  lines(lon,loess(as.numeric(jet[i,])~lon, family = "symmetric")$fitted, lwd = 5, col = "red")
  
  month <- 10
  mon <- Clim_var.nc[,,time[,2]==month] # Sept
  mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
  dim(mon1)<- c(length(data_time), length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
  mon1df <- as.data.frame(mon1)
  mon1detrend<- pracma::detrend(as.matrix(mon1df))
  mon1detrend<-as.data.frame(mon1detrend)
  
  rho2 <- as.matrix(mon1detrend[i,])
  dim(rho2) <- c(length(latitude), length(longitude))
  #rho2[rho2< -4] <- -4
  rho2 <- as.data.frame(rho2)
  colnames(rho2) <- longitude
  rownames(rho2) <- latitude
  rho2 <- as.matrix(rho2)
  rotate3 <- raster(rho2[nrow(rho2):1,])
  extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))
  
  jet <- jet1[seq(month, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
  jet <- jet[47:71,]#1994-2018: 47:71
  rownames(jet) <- 1994:2018
  jet <- lapply(jet, as.character)
  jet <- lapply(jet, as.numeric)
  jet <- as.data.frame(jet)
  jetdetrend <- pracma::detrend(as.matrix(jet))
  
  countries = c("Canada","USA","Cuba","Puerto Rico","Haiti", "Dominican Republic",
                "Mexico", "Guatemala")
  
  map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
  map.axes()
  rb <- rev(brewer.pal(n = 8, name = 'RdBu'))
  plot(rotate3, add = T, col = rb, breaks = c(-6,-4.5,-3,-1.5,0,1.5,3,4.5,6),   xlab="",ylab="",horizontal = F, legend =F)#, #legend = T,legend.args=list(text=c(expression(paste("  " ,degree,"C"))),side=3, line=.25, cex=1))
  map("worldHires",countries, xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = T)  #plot the region of Canada
  text(-65,15, "OCT",adj = c(1,0),cex = 1.5)
  lines(lon,loess(as.numeric(jet[i,])~lon)$fitted, lwd = 5)
#  lines(lon,loess(as.numeric(jet[i,])~lon, family = "symmetric")$fitted, lwd = 5, col = "red")
  
}
dev.off()
