# columns: yrmin; ymax; index(AugustRoost; SeptemberRoost; MexicoRoost; jet8as; jet9ms); month(Aug,Sep); climvar(tmn, tmx, pre, ndvi, uwnd, vwnd); latitude; longitude; r; p #; wilksp0.1; wilksp0.5

# could potentially test spearman versus pearson- number of relationships different?

# for Wilks, I could constrain to eastern North America (>-110E) and constrain latitudes to 55N and 30S

# read in indices
source('scripts/read_indices.R')

library(maps)
library(mapdata)
library(maptools)  #for shapefiles
library(scales)  #for transparency
library(ggplot2)
library(devtools)
library(dplyr)
library(stringr)
library(Hmisc)
library(dplR)
library(stats)
library(RColorBrewer)

library(ncdf4)
corcolglobal <- rev(c("#B2182B", 
                      "#D6604D",
                      "#F4A582",
                      "#FFFFFF",
                      "#FFFFFF",
                      "#92C5DE",
                      "#4393C3",
                      "#2166AC"))

global_alpha <- 0.1


  
#################################################################
## CRU

library(raster)

a <- nc_open(paste("data/raw/cru_ts4.06.1901.2021.",climvar[1],".dat.nc", sep = ""))
dset <- raster(paste("data/raw/cru_ts4.06.1901.2021.",climvar[1],".dat.nc", sep = ""))
proj4string(dset)

print(dset)
plot(dset) #rotate(dset) shifts from 0-360 to -180 to 180
df <- as.data.frame(dset, xy = TRUE)


data_time <- 1901:yrmax
#1901:yrmax monthly
year <- matrix(rep(1901:yrmax,12),nrow = length(1901:yrmax),ncol = 12)
year <- as.vector(t(year))
month <- rep(1:12,length(1901:yrmax))#repeat 1:12 116 times
#repeat 1901:yrmax 12 times and make an array for the year vector
time <- cbind(year,month)

xdim <- round(a$dim[[1]]$vals, digits = 5)# lon
ydim <- round(a$dim[[2]]$vals, digits = 5) # lat
zdim <- a$dim[[3]]$vals # time
xs <- which(xdim == -110.25) #-124.75 which(xdim == -65.75)
ys <-  which(ydim == 29.75) #15.25 (>-110E) and constrain latitudes to 55N and 30S

zs <- which(zdim == 380) 

tmp <- a$var[[1]]

ptm <- proc.time()   
Clim_var.nc<- ncvar_get(a, tmp,start = c(xs,ys,zs), count = c(89, 51,length(zdim)))    
proc.time() - ptm
dim(Clim_var.nc)[1]
dim(Clim_var.nc)[2]
dim(Clim_var.nc)[3]


longitude <- xdim[c(xs:(xs+88))]
latitude <- ydim[c(ys:(ys+50))]

Clim_var.nc<- Clim_var.nc[,,which(time[,1] >=yrmin & time[,1] <=yrmax)]
time <- time[which(time[,1] >=yrmin & time[,1] <=yrmax),]

#################################################################


mon <- Clim_var.nc[,,time[,2]==8]
mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(length(yrmin:yrmax), length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
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
  rho[i]<- rcorr(mon1detrend[,i],mexicoarea, type = "pearson")$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],mexicoarea, type = "pearson")$P[1,2]
}
# 
# #create the global threshold based on the non-NA values
# x <- 2*global_alpha*rank(na.omit(pval))/length(na.omit(pval))
# y <- na.omit(pval)
# pnona <- na.omit(pval) < 2*global_alpha*rank(na.omit(pval))/length(na.omit(pval))
# #but preserve the NA values for plotting
# d <- t(pval)
# d[!is.na(d)] <- t(pnona)[!is.na(t(pnona))]
# pvalglobal <- as.numeric(t(d))


# add pixels for global significance threshold

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i] < 0.1){ # if (is.na(pval[i]) | pvalglobal[i] > 0){
    rho1[i] <- rho[i] # significant correlation values
  }
}
# now plot significant r values, stored as rho1
library(raster)
rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude

##### save to file
rho2 <- cbind(latitude,data.frame(rho2))
library(tidyr)
rho2 <- pivot_longer(data.frame(rho2),cols = 2:90)
rho2$name <- as.numeric(gsub('X.', '-', rho2$name))
rho2$index <- index[3]
rho2$climvar  <- climvar[1]
rho2$month <- 8
rho2$yrmin <- yrmin
rho2$yrmax <- yrmax
colnames(rho2) <- c("latitude","longitude","r,p<0.1","index","climvar","month","yrmin","yrmax")


#######################
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
plot(rotate3)
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))

map("worldHires",c("Canada", "USA","Mexico"), xlim=c(-125,-65),ylim=c(15,55), fill=F,lwd = 1, add = F)  
map.axes()

plot(rotate3, add = T, col = corcolglobal, breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8),legend = F)
map("worldHires", c("Canada", "USA","Mexico"), xlim=c(-125,-65),ylim=c(15,55), fill=F,lwd = 1, add = T) 
#legend(-65,15, "JAN", bg = "white", adj = 0.2) #box.col = NA
text(-65,15, "AUG",adj = c(1,0),cex = 1.5, bg = "white")

plot(rotate3, legend.only = T, col = corcolglobal, breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8),legend.width=1, legend.shrink=1,legend.args=list(text=c(expression(paste(rho,"\n, p<0.1"))),side=3, line=.25, cex=1), axis.args = list(cex.axis = 1),horiz=F)


#create the global threshold based on the non-NA values
#pv_cutoff = 2*global_alpha*my_rank/n_total)
#[33] 0.106789915 0.074118072 0.038570452 
pval < 2*global_alpha*rank(pval)/length(pval) #
pnona <- na.omit(pval) < 2*global_alpha*rank(na.omit(pval))/length(na.omit(pval))
#but preserve the NA values for plotting
d <- t(pval)
d[!is.na(d)] <- t(pnona)[!is.na(t(pnona))]
pvalglobal <- as.numeric(t(d))

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
#mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon1df)){
  if (is.na(pval[i]) | pval[i]< pvalglobal[i]){
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


map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,60), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,60), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,60), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, legend = F, add = T, col = corcolglobal, breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
text(-65,15, "AUG",adj = c(1,0),cex = 1.5)


