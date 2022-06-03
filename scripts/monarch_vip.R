#correlate butterflies and VIP 


library(maps)
library(mapdata)
library(maptools)  #for shapefiles
library(scales)  #for transparency
library(ggplot2)
library(devtools)
library(dplyr)
library(stringr)
library(ggmap)
library(RColorBrewer)

library(ncdf4)
library(fields)
library(Hmisc)
library(mapdata)
#install.packages("R.matlab")
library(R.matlab)

#merge DOP SOS LOS EOS for regions over North America (split up for VIP-Jet analysis)
year <- 1981:2012 #change to 2014
lat <- seq(20,70,0.05)

a1 <- readMat("/Volumes/AOP-NEON1.4/VIP/JetVIP/EOS1_R3_20N70N_ON_7.mat") #98-66
#a2 <- readMat("DOP1_R3_20N70N_JA_5.mat") #104-58

lonmin1 <- -98
lonmax1 <- -66
lon <- seq(lonmin1,lonmax1,0.05)

eos <- a1$sos3
dim(eos) <- c(length(year), length(lat)*dim(a1$sos3)[3])

#a1 <- readMat("DOP1_R3_20N70N_JA_4.mat") #160-104
#a2 <- readMat("DOP1_R3_20N70N_JA_5.mat") #104-58

# lonmin1 <- -160
# lonmax1 <- -58
# lon <- seq(lonmin1,lonmax1,0.05)
# 
# a1$sos3[1,1,1121]
# a2$sos3[1,1,1]

dop <- a1$sos3
dim(dop) <- c(length(year), length(lat)*dim(a1$sos3)[3])

rho2 <- as.matrix(rho1)
dim(rho2) <- c(length(lat),length(lon))
rho2 <- as.data.frame(rho2)
rho2[rho2>0] <- 1
rho2[0>rho2] <- -1
colnames(rho2) <- lon
rownames(rho2) <- lat
rho2 <- as.matrix(rho2)
rotate3 <- raster(rho2[nrow(rho2):1,]) #need to flip rotate 3 on yaxis
extent(rotate3) <- extent(c(min(lon),max(lon),min(lat),max(lat)))

ja4_ns_sig <- rotate3


  
  rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(m1)))#NA[length(mon1)]
ptm <- proc.time()   
for (i in 1:length(m1)){
  y <- my.t.test.p.value(m1[,i],m2[,i],na.omit = T) 
  z <- my.t.test.statistic(m1[,i],m2[,i],na.omit = T)
  if(y <= 0.1 & is.na(y) == "FALSE"){
    rho1[i] <- z
  }
}
proc.time() - ptm


# a1 <- readMat("SOS1_R3_20N70N_AM_6.mat") #120-94
# a2 <- readMat("SOS1_R3_20N70N_AM_7.mat") #94-56
# 
# sos <- 
#   
# a1 <- readMat("LOS1_R3_20N70N_AM_6.mat") #120-94
# a2 <- readMat("LOS1_R3_20N70N_AM_7.mat") #94-56
# 
# los <- 
# 
# a1 <- readMat("EOS1_R3_20N70N_ON_6.mat") #146-98
# a2 <- readMat("EOS1_R3_20N70N_ON_7.mat") #98-66
# 
# eos <- 
  
# # playing w vip
# # install rhdf5
# # Install rhdf5 package (only need to run if not already installed)
# install.packages("BiocManager")
# BiocManager::install("rhdf5")
# 
# # Call the R HDF5 Library
# library("rhdf5")
# 
# #
# if (!requireNamespace("rhdf5", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("rhdf5", version = "3.8")
# 
# # load packages
# library(sf)
# install.packages("hdf5r")
# library("hdf5r")
# 
# library(raster)
# library(rasterVis)
# 
# vip1 <- Read10X_h5("~/Downloads/VIPPHEN_NDVI.A1994.004.2018171121505.hdf")


