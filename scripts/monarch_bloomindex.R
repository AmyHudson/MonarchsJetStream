# put all 1994-2019 first bloom index together



############
#NPN First Bloom

#USA National Phenology Network. 2017. Spring Indices, Historical Annual (1981-Previous Year) -  First Bloom - Spring Index, Years 1994-2019. Region: 49.9375,-66.4791667,24.0625,-125.0208333. https://data.usanpn.org/geoserver-request-builder?service=wcs&layer=si-x:average_bloom_prism&year=2019&format=application/x-netcdf&projection=4269 . USA-NPN, Tucson, Arizona, USA. Data set accessed 2020-7-31. http://dx.doi.org/10.5066/F7XD0ZRK


library(ncdf4)
library(raster)
#install.packages("abind")
library(abind)

a <- nc_open("data/si-x_average_bloom_prism_1994.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_bloom_prism')))

latitude <- a$dim$lat$vals
longitude <- a$dim$lon$vals

bloom <- array(numeric(),c(length(a$dim$lon$vals),length(a$dim$lat$vals),0)) 
bloom <- abind(bloom, c, along = 3)

a <- nc_open("data/si-x_average_bloom_prism_1995.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_bloom_prism')))
 
bloom <- abind(bloom, c, along = 3)

a <- nc_open("data/si-x_average_bloom_prism_1996.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_bloom_prism')))
 
bloom <- abind(bloom, c, along = 3)

a <- nc_open("data/si-x_average_bloom_prism_1997.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_bloom_prism')))
 
bloom <- abind(bloom, c, along = 3)

a <- nc_open("data/si-x_average_bloom_prism_1998.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_bloom_prism')))
 
bloom <- abind(bloom, c, along = 3)

a <- nc_open("data/si-x_average_bloom_prism_1999.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_bloom_prism')))
 
bloom <- abind(bloom, c, along = 3)

a <- nc_open("data/si-x_average_bloom_prism_2000.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_bloom_prism')))
 
bloom <- abind(bloom, c, along = 3)

a <- nc_open("data/si-x_average_bloom_prism_2001.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_bloom_prism')))
 
bloom <- abind(bloom, c, along = 3)

a <- nc_open("data/si-x_average_bloom_prism_2002.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_bloom_prism')))
 
bloom <- abind(bloom, c, along = 3)

a <- nc_open("data/si-x_average_bloom_prism_2003.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_bloom_prism')))
 
bloom <- abind(bloom, c, along = 3)

a <- nc_open("data/si-x_average_bloom_prism_2004.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_bloom_prism')))
 
bloom <- abind(bloom, c, along = 3)

a <- nc_open("data/si-x_average_bloom_prism_2005.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_bloom_prism')))
 
bloom <- abind(bloom, c, along = 3)

a <- nc_open("data/si-x_average_bloom_prism_2006.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_bloom_prism')))
 
bloom <- abind(bloom, c, along = 3)

a <- nc_open("data/si-x_average_bloom_prism_2007.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_bloom_prism')))
 
bloom <- abind(bloom, c, along = 3)

a <- nc_open("data/si-x_average_bloom_prism_2008.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_bloom_prism')))
 
bloom <- abind(bloom, c, along = 3)

a <- nc_open("data/si-x_average_bloom_prism_2009.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_bloom_prism')))
 
bloom <- abind(bloom, c, along = 3)

a <- nc_open("data/si-x_average_bloom_prism_2010.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_bloom_prism')))
 
bloom <- abind(bloom, c, along = 3)

a <- nc_open("data/si-x_average_bloom_prism_2011.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_bloom_prism')))
 
bloom <- abind(bloom, c, along = 3)

a <- nc_open("data/si-x_average_bloom_prism_2012.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_bloom_prism')))
 
bloom <- abind(bloom, c, along = 3)

a <- nc_open("data/si-x_average_bloom_prism_2013.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_bloom_prism')))
 
bloom <- abind(bloom, c, along = 3)

a <- nc_open("data/si-x_average_bloom_prism_2014.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_bloom_prism')))
 
bloom <- abind(bloom, c, along = 3)

a <- nc_open("data/si-x_average_bloom_prism_2015.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_bloom_prism')))
 
bloom <- abind(bloom, c, along = 3)

a <- nc_open("data/si-x_average_bloom_prism_2016.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_bloom_prism')))
 
bloom <- abind(bloom, c, along = 3)

a <- nc_open("data/si-x_average_bloom_prism_2017.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_bloom_prism')))
 
bloom <- abind(bloom, c, along = 3)

a <- nc_open("data/si-x_average_bloom_prism_2018.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_bloom_prism')))
 
bloom <- abind(bloom, c, along = 3)

a <- nc_open("data/si-x_average_bloom_prism_2019.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_bloom_prism')))
 
bloom <- abind(bloom, c, along = 3)

# read in monarch time series

library(Hmisc)
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
install.packages("RColorBrewer")
library(RColorBrewer)
library(raster)

mexicoarea <- read.table('Butterflies1994-2019.txt',header = T)
#mexicoarea <- mexicoarea[1:19,]#2012
mexicoarea <- mexicoarea[1:26,]
mexicoarea[,1] <- 1994:2019
mexicoareadetrend <- matrix(NA,nrow = length(1994:2019),ncol = 2)
mexicoareadetrend[,2] <- lm(mexicoarea$MexicoArea~mexicoarea$Year)$residuals
mexicoareadetrend[,1] <- 1994:2019 #1994:2012
mexicoareadetrend <- data.frame(mexicoareadetrend)
colnames(mexicoareadetrend) <- c("Year", "MexicoArea")


# correlate monarch time series and bloom


mon1 <- aperm(bloom,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(26, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
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
#rotate3 <- raster(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))

png("mexicomonarch_corr_bloom.png",13,8,
    units = "in",res = 600, pointsize=20, family= "helvetica")

#par(mfrow=c(4,3))#,mar = c(0, 0, 0, 0))

map("worldHires","Canada", xlim=c(-125,-65),ylim=c(25,50), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(25,50), fill=F, add = T,lwd = 1)  #plot the region of US
map("state", xlim=c(-125,-65),ylim=c(25,50), fill=F, add = T,lwd = 1)  #plot the region of US

map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(25,50), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8),legend = F)
text(-65,25, "Bloom",adj = c(1,0),cex = 1.5)

plot(rotate3, legend.only = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8),legend.width=1, legend.shrink=1,legend.args=list(text=c(expression(paste("r \np<0.1"))),side=3, line=.25, cex=1), axis.args = list(cex.axis = 1),horiz=F)
dev.off()


############
#NPN First Leaf

#USA National Phenology Network. 2017. Spring Indices, Historical Annual (1981-Previous Year) -  First Bloom - Spring Index, Years 1994-2019. Region: 49.9375,-66.4791667,24.0625,-125.0208333. https://data.usanpn.org/geoserver-request-builder?service=wcs&layer=si-x:average_bloom_prism&year=2019&format=application/x-netcdf&projection=4269 . USA-NPN, Tucson, Arizona, USA. Data set accessed 2020-7-31. http://dx.doi.org/10.5066/F7XD0ZRK


library(ncdf4)
library(raster)
#install.packages("abind")
library(abind)

a <- nc_open("data/si-x_average_leaf_prism_1994.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_leaf_prism')))

latitude <- a$dim$lat$vals
longitude <- a$dim$lon$vals

leaf <- array(numeric(),c(length(a$dim$lon$vals),length(a$dim$lat$vals),0)) 
leaf <- abind(leaf, c, along = 3)

a <- nc_open("data/si-x_average_leaf_prism_1995.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_leaf_prism')))

leaf <- abind(leaf, c, along = 3)

a <- nc_open("data/si-x_average_leaf_prism_1996.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_leaf_prism')))

leaf <- abind(leaf, c, along = 3)

a <- nc_open("data/si-x_average_leaf_prism_1997.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_leaf_prism')))

leaf <- abind(leaf, c, along = 3)

a <- nc_open("data/si-x_average_leaf_prism_1998.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_leaf_prism')))

leaf <- abind(leaf, c, along = 3)

a <- nc_open("data/si-x_average_leaf_prism_1999.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_leaf_prism')))

leaf <- abind(leaf, c, along = 3)

a <- nc_open("data/si-x_average_leaf_prism_2000.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_leaf_prism')))

leaf <- abind(leaf, c, along = 3)

a <- nc_open("data/si-x_average_leaf_prism_2001.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_leaf_prism')))

leaf <- abind(leaf, c, along = 3)

a <- nc_open("data/si-x_average_leaf_prism_2002.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_leaf_prism')))

leaf <- abind(leaf, c, along = 3)

a <- nc_open("data/si-x_average_leaf_prism_2003.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_leaf_prism')))

leaf <- abind(leaf, c, along = 3)

a <- nc_open("data/si-x_average_leaf_prism_2004.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_leaf_prism')))

leaf <- abind(leaf, c, along = 3)

a <- nc_open("data/si-x_average_leaf_prism_2005.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_leaf_prism')))

leaf <- abind(leaf, c, along = 3)

a <- nc_open("data/si-x_average_leaf_prism_2006.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_leaf_prism')))

leaf <- abind(leaf, c, along = 3)

a <- nc_open("data/si-x_average_leaf_prism_2007.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_leaf_prism')))

leaf <- abind(leaf, c, along = 3)

a <- nc_open("data/si-x_average_leaf_prism_2008.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_leaf_prism')))

leaf <- abind(leaf, c, along = 3)

a <- nc_open("data/si-x_average_leaf_prism_2009.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_leaf_prism')))

leaf <- abind(leaf, c, along = 3)

a <- nc_open("data/si-x_average_leaf_prism_2010.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_leaf_prism')))

leaf <- abind(leaf, c, along = 3)

a <- nc_open("data/si-x_average_leaf_prism_2011.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_leaf_prism')))

leaf <- abind(leaf, c, along = 3)

a <- nc_open("data/si-x_average_leaf_prism_2012.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_leaf_prism')))

leaf <- abind(leaf, c, along = 3)

a <- nc_open("data/si-x_average_leaf_prism_2013.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_leaf_prism')))

leaf <- abind(leaf, c, along = 3)

a <- nc_open("data/si-x_average_leaf_prism_2014.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_leaf_prism')))

leaf <- abind(leaf, c, along = 3)

a <- nc_open("data/si-x_average_leaf_prism_2015.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_leaf_prism')))

leaf <- abind(leaf, c, along = 3)

a <- nc_open("data/si-x_average_leaf_prism_2016.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_leaf_prism')))

leaf <- abind(leaf, c, along = 3)

a <- nc_open("data/si-x_average_leaf_prism_2017.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_leaf_prism')))

leaf <- abind(leaf, c, along = 3)

a <- nc_open("data/si-x_average_leaf_prism_2018.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_leaf_prism')))

leaf <- abind(leaf, c, along = 3)

a <- nc_open("data/si-x_average_leaf_prism_2019.nc")
c <- as.matrix(ncvar_get(a, varid = c('six_average_leaf_prism')))

leaf <- abind(leaf, c, along = 3)

# read in monarch time series

library(Hmisc)
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
#install.packages("RColorBrewer")
library(RColorBrewer)
library(raster)

mexicoarea <- read.table('Butterflies1994-2019.txt',header = T)
#mexicoarea <- mexicoarea[1:19,]#2012
mexicoarea <- mexicoarea[1:26,]
mexicoarea[,1] <- 1994:2019
mexicoareadetrend <- matrix(NA,nrow = length(1994:2019),ncol = 2)
mexicoareadetrend[,2] <- lm(mexicoarea$MexicoArea~mexicoarea$Year)$residuals
mexicoareadetrend[,1] <- 1994:2019 #1994:2012
mexicoareadetrend <- data.frame(mexicoareadetrend)
colnames(mexicoareadetrend) <- c("Year", "MexicoArea")


# correlate monarch time series and leaf


mon1 <- aperm(leaf,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(26, length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
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

png("mexicomonarch_corr_leaf.png",13,8,
    units = "in",res = 600, pointsize=20, family= "helvetica")

#par(mfrow=c(4,3))#,mar = c(0, 0, 0, 0))

map("worldHires","Canada", xlim=c(-125,-65),ylim=c(25,50), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(25,50), fill=F, add = T,lwd = 1)  #plot the region of US
map("state", xlim=c(-125,-65),ylim=c(25,50), fill=F, add = T,lwd = 1)  #plot the region of US

map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(25,50), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, add = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8),legend = F)
text(-65,25, "Leaf",adj = c(1,0),cex = 1.5)

plot(rotate3, legend.only = T, col = rev(brewer.pal(8,"RdBu")), breaks = c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8),legend.width=1, legend.shrink=1,legend.args=list(text=c(expression(paste("r \np<0.1"))),side=3, line=.25, cex=1), axis.args = list(cex.axis = 1),horiz=F)
dev.off()

## quick plot of average onset
library(raster)
rho2 <- as.matrix(colMeans(mon1df, na.rm = T))
dim(rho2) <- c(length(latitude),length(longitude))
rho2 <- as.data.frame(rho2)
colnames(rho2) <- longitude
rownames(rho2) <- latitude
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,])
#rotate3 <- raster(t(flip(rho2, 1)))
#image(t(flip(x, 2)))

extent(rotate3) <- extent(c(min(longitude),max(longitude),min(latitude),max(latitude)))


png("mexicomonarch_mean_firstleaf.png",13,8,
    units = "in",res = 600, pointsize=20, family= "helvetica")

#par(mfrow=c(4,3))#,mar = c(0, 0, 0, 0))

map("worldHires","Canada", xlim=c(-125,-65),ylim=c(25,50), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(25,50), fill=F, add = T,lwd = 1)  #plot the region of US
#map("state", xlim=c(-125,-65),ylim=c(25,50), fill=F, add = T,lwd = 1)  #plot the region of US

map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(25,50), fill=F, add = T,lwd = 1)#plot the region of Mexico
plot(rotate3, add = T,legend = F)
text(-65,25, "Leaf",adj = c(1,0),cex = 1.5)
map("state", xlim=c(-125,-65),ylim=c(25,50), fill=F, add = T,lwd = 1)  #plot the region of US


plot(rotate3, legend.only = T,legend.width=1, legend.shrink=1,legend.args=list(text=c(expression(paste("DOY"))),side=3, line=.25, cex=1), axis.args = list(cex.axis = 1),horiz=F)
dev.off()
