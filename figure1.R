## This code produces Figure 1 and 2 of the white paper for Monarch Madness: Do changes in the polar jet stream correlate with monarch overwintering numbers? Amy Hudson and Kathleen Prudic.

# Figure 1. Climate differences between high and low monarch overwintering acreage numbers. The colored regions are the temperature values for significant correlations between monarch overwintering acreage and August (top panels) and September (bottom) average temperatures for 1994 to 2017 that precede a peak in monarch acreage (2003; right) and a trough in monarch acreage (2002; left). The wind vector field at 300 mb overlays the temperature field to emphasize the atmospheric influence over surface temperatures. 

#Figure 2. Same as Figure 1, but temperature anomalies from the 1994-2017 time period are shown as the base map. The peak in 2003 seems to be due to a wavier jet stream in August and September, with a high pressure system showing warmer temperature anomalies in August (red pixels in top right panel) and a low pressure system and cool temperature anomalies in September (blue pixels in bottom right panel).

# I accessed meridional wind: vwnd.mon.mean.nc and zonal wind: uwnd.mon.mean.nc from the NCEP/NCAR Reanalysis project spanning 1948-present https://www.esrl.noaa.gov/psd/data/gridded/data.ncep.reanalysis.derived.surface.html and mean temperature: cru_ts4.01.1901.2016.tmp.dat.nc from CRU TS4.01: Climatic Research Unit (CRU) Time-Series (TS) version 4.01 of high-resolution gridded data of month-by-month variation in climate (Jan. 1901- Dec. 2016) http://data.ceda.ac.uk/badc/cru/data/cru_ts/cru_ts_4.01/data/tmp/ 

#This data can be accessed on my google drive, along with the countries shape file: https://drive.google.com/drive/folders/10kMMlz3pLzPt50Ti6uJNs2gqcZl-vGyy?usp=sharing

## The code below is replicated for August 2003 and 2002 and September 2003 and 2002 mean temperatures and anomalies- I manually change the years subsetted and the months subsetted to produce the 4 pannels in figure 1 and 2. 

# ... sometimes the rasterVis library needs to be removed and re-installed. 
####################################################################################

# MERIDIONAL 
setwd("/Volumes/AOP-NEON1.4/monarch_transport")

library(ncdf4)
library(fields)
library(Hmisc)
library(mapdata)

#data <- na.omit(ELPC) # a data frame with a column cales "year"
data_time <- 1994:2017
#1901:2016 monthly
year <- matrix(rep(1948:2018,12),nrow = length(1948:2018),ncol = 12)
year <- as.vector(t(year))
month <- rep(1:12,71)#repeat 1:12 116 times
#repeat 1901:2016 12 times and make an array for the year vector
#crop to 1994 (jan or feb)
time <- cbind(year,month)
time <- time[1:840,]
itime <- subset(time,time[,1]>=1994)

#don't know how to subset the time variable to 1994
#1117

a <- nc_open("vwnd.mon.mean.nc")

wdim <- round(a$dim[[1]]$vals, digits = 5)# level
xdim <- round(a$dim[[2]]$vals, digits = 5)# lat
ydim <- round(a$dim[[3]]$vals, digits = 5) # lon
zdim <- a$dim[[4]]$vals # time

ws <- which(wdim == 300)
xs <- which(xdim == 62.5)
ys <-  which(ydim == 242.5) 
zs <- which(zdim == 1700568) # #know this from looking up the time variable 1117

vwnd <- a$var[[1]]
#uwnd[lon,lat,level,time]     

ptm <- proc.time()   
Clim_var.nc<- ncvar_get(a, vwnd,start = c(ys,xs,ws,zs), count = c(24,20, 1,288)) 
proc.time() - ptm
# dim(Clim_var.nc)[1]
# dim(Clim_var.nc)[2]
# dim(Clim_var.nc)[3]


latitude <- xdim[c(xs:(xs+19))]
longitude<- ydim[c(ys:(ys+23))]
## Good up to here 6/29; need to find out how to subset the time component to true false or 0 1
#mon <- Clim_var.nc[,,itime[,2]==2] # February
mon <- Clim_var.nc[,,itime[,2]==8] # August
#mon <- Clim_var.nc[,,itime[,2]==9] # Septemper

mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
m2003 <- mon1[10,,] #10 is 2003, 2009 is 16
#m2003 <- mon1[9,,] #2002

#m2003 <- mon1[16,,] #10 is 2003, 2009 is 16


dim(mon1)<- c(length(data_time), length(latitude)*length(longitude))

m2003 <- as.matrix(m2003)
dim(m2003) <- c(length(latitude),length(longitude))
m2003 <- as.data.frame(m2003)
m2003 <- as.matrix(m2003)
rotate3 <- raster(m2003)
extent(rotate3) <- extent(c(min(longitude)-360,max(longitude)-360,min(latitude),max(latitude)))
v <- rotate3
########################################################################################## ZONAL

a <- nc_open("uwnd.mon.mean.nc")

wdim <- round(a$dim[[1]]$vals, digits = 5)# level
xdim <- round(a$dim[[2]]$vals, digits = 5)# lat
ydim <- round(a$dim[[3]]$vals, digits = 5) # lon
zdim <- a$dim[[4]]$vals # time

ws <- which(wdim == 300)
xs <- which(xdim == 62.5)
ys <-  which(ydim == 242.5) 
zs <- which(zdim == 1700568) # #know this from looking up the time variable 1117

uwnd <- a$var[[1]]
#uwnd[lon,lat,level,time]     

ptm <- proc.time()   
Clim_var.nc<- ncvar_get(a, uwnd,start = c(ys,xs,ws,zs), count = c(24,20, 1,288)) 
proc.time() - ptm
dim(Clim_var.nc)[1]
dim(Clim_var.nc)[2]
dim(Clim_var.nc)[3]

latitude <- xdim[c(xs:(xs+19))]
longitude<- ydim[c(ys:(ys+23))]
## Good up to here 6/29; need to find out how to subset the time component to true false or 0 1
#mon <- Clim_var.nc[,,itime[,2]==2] # February
mon <- Clim_var.nc[,,itime[,2]==8] # August
#mon <- Clim_var.nc[,,itime[,2]==9] # September

mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon

m2003 <- mon1[10,,]
#m2003 <- mon1[9,,]
#m2003 <- mon1[16,,]


m2003 <- as.matrix(m2003)
dim(m2003) <- c(length(latitude),length(longitude))
m2003 <- as.data.frame(m2003)
m2003 <- as.matrix(m2003)
rotate3 <- raster(m2003)
extent(rotate3) <- extent(c(min(longitude)-360,max(longitude)-360,min(latitude),max(latitude)))
u <- rotate3


########################################################################################
#Temperature

#data <- na.omit(ELPC) # a data frame with a column cales "year"
data_time <- 1994:2016
#1901:2016 monthly
year <- matrix(rep(1901:2016,12),nrow = length(1901:2016),ncol = 12)
year <- as.vector(t(year))
month <- rep(1:12,116)#repeat 1:12 116 times
#repeat 1901:2016 12 times and make an array for the year vector
#crop to 1994 (jan or feb)
time <- cbind(year,month)
time <- subset(time,year >=1994)

a <- nc_open("cru_ts4.01.1901.2016.tmp.dat.nc")

xdim <- round(a$dim[[1]]$vals, digits = 5)# lon
ydim <- round(a$dim[[2]]$vals, digits = 5) # lat
zdim <- a$dim[[3]]$vals # time

xs <- which(xdim == -117.25)
ys <-  which(ydim == 14.75) #allows us to crop to the overwintering acreage 
zs <- which(zdim == 34348) # #know this from looking up the time variable 1117

tmp <- a$var[[1]]

ptm <- proc.time()   
Clim_var.nc<- ncvar_get(a, tmp,start = c(xs,ys,zs), count = c(114, 92,276))    
proc.time() - ptm
dim(Clim_var.nc)[1]
dim(Clim_var.nc)[2]
dim(Clim_var.nc)[3]


longitude <- xdim[c(xs:(xs+113))]
latitude <- ydim[c(ys:(ys+91))]
## Good up to here 6/29; need to find out how to subset the time component to true false or 0 1
#mon <- Clim_var.nc[,,time[,2]==2] # February
mon <- Clim_var.nc[,,time[,2]==8] # August
#mon <- Clim_var.nc[,,time[,2]==9] # September
########################################################################################

mon1 <- aperm(mon,c(3,2,1)) #reorder with time variable in front, lat, lon
dim(mon1)<- c(length(data_time), length(latitude)*length(longitude)) #reshape 2D w time, lat*lon
mon1df <- as.data.frame(mon1)

require(pracma)


mon1detrend<- detrend(as.matrix(mon1df))
mon1detrend<-as.data.frame(mon1detrend)

#require(dplR)
#plot(detrend.series(mon1df[,41],make.plot = T, method = c("ModNegExp")),type="l")
#lines(scale(mon1df[,41]), col = "red")


## Make Correlations w Monarchs

# #read in the monarch dataset
mexicoarea <- read.table('Butterflies.txt',header = T)
mexicoarea[,1] <- 1994:2017 #1994:2017
mexicoarea <- subset(mexicoarea,Year<=2016) #[1:19,]
mexicoareadetrend <- detrend(as.matrix(mexicoarea))
mexicoareadetrend[,1] <- mexicoarea$Year #1994:2012
mexicoareadetrend <- as.data.frame(mexicoareadetrend)

#make a rho and pval to find significant correlations.
rho <- NA[length(mon1)]
pval <- NA[length(mon1)]
# 
for (i in 1:length(mon1detrend)){
  rho[i]<- rcorr(mon1detrend[,i],mexicoareadetrend[,2],type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon1detrend[,i],mexicoareadetrend[,2],type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = 10488))#NA[length(mon1)]
mon1df1 <- as.data.frame(matrix(NA,nrow = 23,ncol = 10488))

for (i in 1:length(mon1detrend)){
  if (is.na(pval[i]) | pval[i] < 0.05){
    rho1[i] <- rho[i] # significant correlation values
    mon1df1[,i] <- mon1df[,i] #climate time series of significant pixels
  }
}

cM <- colMeans(mon1df1)

#mon1df1 <- as.matrix(mon1df1[10,]-cM) #temperature anomalies 2003
#mon1df1 <- as.matrix(mon1df1[9,]-cM) #temperature anomalies 2002

# 
# ##################################
# 
#mon1df1 <- as.matrix(mon1df1[10,]) #subsets 2003; #(1994:2016)[10]
mon1df1 <- as.matrix(mon1df1[9,]) #subsets 2002
#16 subsets 2009; #(1994:2016)[16]

dim(mon1df1) <- c(length(latitude),length(longitude))
mon1df1 <- as.data.frame(mon1df1)
colnames(mon1df1) <- longitude
rownames(mon1df1) <- latitude

#plot mon1df for one year high monarch pop
#plot mon1df for one year low monarch pop


#Subset initial dataset by significant rho.


library(maps)
library(mapdata)
library(maptools)  #for shapefiles
library(scales)  #for transparency
library(ggplot2)
library(devtools)
library(dplyr)
library(stringr)
library(ggmap) #need to cite ggmap
library(RColorBrewer)
library(raster)

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

#em = merge(extent(m),extent(w))

#plot(em, type="n")
#plot(m,add=TRUE, legend=FALSE)
#vectorplot(, add=TRUE, legend=FALSE)

#######################
# Was trying to plot only significant winds... had to run meridional.R and zonal.R for significant u and v as raster objects, and then from mapwindarrows.R I developed the speed and direction from those significant wind speeds.
# dir1 <- as.matrix(dir1)
# dir1 <- raster(dir1)
# speed <- as.matrix(speed)
# speed <- raster(speed)
# 
# extent(dir1) <- extent(m)
# extent(speed) <- extent(m)
# 
# w <- brick(speed,dir1)

#######################

extent(u) <- extent(m)
extent(v) <- extent(m)

w <- brick(u, v)

#plot(w[[1]])
#plot(w[[2]])

# Downloaded Countries outline in the form of ne_50m_admin_0_countries.zip from: https://www.naturalearthdata.com/downloads/50m-cultural-vectors/50m-admin-0-countries-2/
#7/11/2018
require(utils)
require(RNetCDF)
#require(rasterVis)
library(rasterVis)
library(rgdal)
## The commented code below did not work... Don't know why.#########################
# cntry <- readOGR(dsn = ".", layer = "ne_50m_admin_0_countries", stringsAsFactors = TRUE)
# cntry <- readOGR(dsn = ".", layer = "admin0-countries-50m")
# list.files('.', pattern='\\.shp$')
# file.exists('./ne_50m_admin_0_countries.shp')
# readOGR(dsn=path.expand("."), layer="ne_50m_admin_0_countries")
# library(raster)
# s <- shapefile("./ne_50m_admin_0_countries.shp")

cntry <- readOGR('./ne_50m_admin_0_countries.shp',stringsAsFactors = TRUE)
projection(cntry)
getClass(cntry)


#map.world <- map_data("world")

require(RNetCDF)
require(OceanView) 
library(plot3D)

#vectorplot(w, isField = "dXY", par.settings=YlOrRdTheme ,region = m, margin = FALSE, narrows = 10000) + layer(sp.polygons(cntry)) #tried making w a speed then dir1 raster, with 0 is due north... 


#maybe bind my significant raster with the m region- same extent needed as well. 
#Or could bold?
#need to clean up m
#need to learn how to make the color bar the same across the figures.

#################################
## Trying to change the color bar to be the same across all my August September 2003 2009 figures. Doesn't work yet. 



#vectorplot(w,isField = "dXY",region = m, clim = c(20,30),par.settings=YlOrRdTheme , margin = FALSE, narrows = 10000 )
#+ layer(sp.polygons(cntry)) 
#  

library(lattice)
#this works!!
vectorplot(w,
           isField = "dXY",
           region = m,
           par.settings= BuRdTheme(region = rev(brewer.pal(10, 'RdBu')), lim = c(10,30), at = seq(10,30,length = 21)),
           at = seq(10,30,length = 21),
           #margin = FALSE,
           #narrows = 10000,
           colorkey= list(at = seq(10,30,length = 21), title = expression(degree*C))) + layer(sp.polygons(cntry))

# temperature anomalies 
vectorplot(w, 
           isField = "dXY", 
           region = m, 
           par.settings= BuRdTheme(),
           at = seq(-3,3,length = 7),
           #margin = FALSE,
           #narrows = 10000,
           colorkey= list(at = seq(-3,3,length = 7), title = expression(degree*C),title)) + layer(sp.polygons(cntry))
########################


# rasterTheme(region = matlab.like(n = 10)), 
# narrows = 10000)
# 
#vectorplot(Wind.x, Wind.y, x = WMI$time - WMI$time[1], colvar = WMI$altitude,
#            xlab = "time", ylab = "m/s", #clab = "height, km", 
#            main = "wind velocity from flight", colkey = FALSE)
# colkey (side = 3, length = 0.5, width = 0.5, dist = -0.71, shift = -0.2, 
#         clim = range(WMI$altitude), add = TRUE, cex.axis = 0.8, 
#         mgp = c(1, 0.5, 0), clab = "height, km")

################