#NPN Monarch and Nectar Connector 

# Nectar connector + Monarchs gets us 53 nectar species' phenophases + Monarch observations

# First we look at the Fall migration path (bounding box below) and how longer growing seasons- later last yes flowering phenophase- increased nectar access- leads to higher monarch return

# This does not get us too many observations (although there does seem to be a relationship with monarch return for what we have)

# Other phenophases (and plant species?) may help increase those observation numbers...

# Next steps include adding in direct data requests to NPN with the Rnpn package; and checking out those other phenophases/ species to up sample sizes.

#################################################################################

#read in the site_phenometrics_data.csv

#npn <- read.csv("~/Downloads/datasheet_1595346252951/site_phenometrics_data.csv") # or in repo site_phenometrics_data.csv
npn <- read.csv("site_phenometrics_data.csv") # or in repo site_phenometrics_data.csv

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

map("worldHires",c("USA"), xlim=c(-130,-60),ylim=c(25,50), fill=F, add = F,lwd = 1)  #plot the region of US
map.axes()
map('state', add =T)

unique(npn$Phenophase_ID)
unique(npn$Phenophase_Description)
mm<- npn[which(npn$Phenophase_Description == "Migrating adults (in uniform direction)" ),]
boxplot()


# remove butterfly to focus on plants
nectar <- npn[which(npn$Genus != "Danaus"),]
nectar <- npn[which((npn$Genus != "Danaus")& (npn$Phenophase_ID == 201 | npn$Phenophase_ID == 500 | npn$Phenophase_ID == 501)),]

points(nectar$Longitude,nectar$Latitude, pch = 16, cex = 0.5, col = "black")

nectar$Mean_Last_Yes_DOY <- as.numeric(nectar$Mean_Last_Yes_DOY)
nectar$Mean_First_Yes_DOY <- as.numeric(nectar$Mean_First_Yes_DOY)
#remove days less than September 244
nectar <- nectar[which((nectar$Latitude< 40 & nectar$Latitude> 20)&(nectar$Longitude< -90 & nectar$Longitude> -104)),]
points(nectar$Longitude,nectar$Latitude, pch = 16, cex = 0.5, col = "orange")
table(nectar$Mean_Last_Yes_Year)


#average by year nectar$Mean_Last_Yes_DOY
y<- as.data.frame(cbind(nectar$Mean_Last_Yes_Year,nectar$Mean_Last_Yes_DOY))
y[which(y[,1] == '-9999'),] <- NA
y <- as.data.frame(y[complete.cases(y),])
raster::aggregate(y$V1, fun=mean)
apply(y,FUN = mean)
t <- aggregate(V2 ~  V1, y, FUN = mean,na.rm = T)
#t <- data.frame(cbind(2010:2019,c(291,229,NA, 143.8,176,NA,247.1666667,173.4,280.4, 237.5)))

plot(t$V1,t$V2)
#plot(t$X1,t$X2, type = "p")

abline(h = mean(t$V2, na.rm = T))

mexicoarea <- read.table('Butterflies1994-2019.txt',header = T)
mexicoarea[,1] <- 1994:2019
mexicoarea <- mexicoarea[16:26,]#2009-2019 overlap with nectar
mexicoareadetrend <- matrix(NA,nrow = length(2009:2019),ncol = 2)
mexicoareadetrend[,2] <- lm(mexicoarea$MexicoArea~mexicoarea$Year)$residuals
mexicoareadetrend[,1] <- 2009:2019 #1994:2012
mexicoareadetrend <- data.frame(mexicoareadetrend)
colnames(mexicoareadetrend) <- c("Year", "MexicoArea")
#plot(t$V1,scale(t$`mean(V2, na.rm = T)`), type = "l", ylim = c(-3,3))
plot(t$V1,scale(t$V2), type = "p",ylim = c(-3,3))
abline(h = 0)
lines(mexicoareadetrend$Year,mexicoareadetrend$MexicoArea, col = "black")
legend("top", legend = c("Winter Acreage Index"), col = c("black"), lty = c(1), lwd = c(1),bg = "white",cex = 0.70, bty = "n")

library(Hmisc)
rcorr(mexicoareadetrend$MexicoArea,t$`mean(V2, na.rm = T)`)
rcorr(mexicoareadetrend$MexicoArea,t$X2)

unique(nectar$Phenophase_ID)
table(nectar$Phenophase_Description)
# Flowers or flower buds 733
# Open flowers 691
last_nectar <- nectar[which(nectar$Mean_Last_Yes_DOY>0),]
table(last_nectar$Mean_Last_Yes_Year)
table(last_nectar$Mean_Last_Yes_DOY)
# color map by Mean_Last_Yes_DOY
# split by spring fall? August 1 is 213 

last_nectar_fall <- nectar[which(nectar$Mean_Last_Yes_DOY>213 | nectar$Mean_First_Yes_DOY>213),]
last_nectar_fall$Mean_Last_Yes_DOY
histogram(last_nectar_fall$Mean_Last_Yes_DOY)
table(last_nectar_fall$Mean_Last_Yes_DOY)
table(last_nectar_fall$Mean_First_Yes_DOY)

table(last_nectar_fall$Mean_Last_Yes_Year)

# group by year aggregate 
# take the average of DOY for each year (crop to locations in Eastern US- 110)

x <- aggregate(last_nectar_fall$Mean_Last_Yes_DOY, list(last_nectar_fall$Mean_Last_Yes_Year), mean)

# plot fall progression

# above 30 and east of 100
last_nectar_fall <- last_nectar_fall[which(last_nectar_fall$Latitude > 30 & last_nectar_fall$Longitude > -100),]

table(last_nectar_fall$Mean_Last_Yes_DOY)

table(last_nectar_fall$Mean_Last_Yes_Year)


# color by doy
map("worldHires",c("USA"), xlim=c(-130,-60),ylim=c(25,50), fill=F, add = F,lwd = 1)  #plot the region of US
map.axes()
map('state', add =T)

for (i in 1:length(last_nectar_fall[,1])){
  points(last_nectar_fall$Longitude[i],last_nectar_fall$Latitude[i], pch = 16, cex = 0.5, col = "black")
}



# #############
# # Correlate last yes of southern phenology with september jet stream
# 
# #read in jet position 1948-2017 
# n <- read.table("NHJ_position_global_1948jan-2019sep_ncepncar.txt")
# #n <- as.numeric(n)
# colnames(n) <- seq(0,357.5,2.5)
# #jet1 <- n[2:853,]
# jet1 <- n[2:862,]
# yr <- 1948:2019
# 
# jet <- jet1[seq(9, 862, 12),which(seq(0,357.5,2.5)>=252.5 & seq(0,357.5,2.5)<=265)]
# 
# jet <- jet1[seq(9, 852, 12),which(seq(0,357.5,2.5)>=252.5 & seq(0,357.5,2.5)<=265)]
# jet <- jet[which(yr<=2019 & yr>=2010),] 
# jet <- rowMeans(as.numeric(unlist(jet)))
# 
# jet <- lapply(jet, as.character)
# jet <- lapply(jet, as.numeric)
# jet <- as.data.frame(jet)
# #jet <- as.data.frame(pracma::detrend(as.matrix(jet), tt = 'linear'))
# j <- c(45.41666667, 62.08333333, 63.75, 64.58333333, 59.58333333, 54.16666667, 49.16666667, 55.83333333, 50, NA)
# j <- rowMeans(jet)
# 
# 
# plot(t$V1,scale(t$V2), type = "p",ylim = c(-3,3))
# abline(h = 0)
# lines(mexicoareadetrend$Year,mexicoareadetrend$MexicoArea, col = "black")
# 
# par(new = T)
# plot(2010:2019,j, type = "l",ylim = rev(c(45,65)), col ="gray", axes = F)#ylim = c(-3,3))
# axis(4, col = "gray",ylim = rev(c(45,65)))
# 
# legend("top", legend = c("Winter Acreage Index","September Jet stream over Great Plains"), col = c("black","gray"), lty = c(1,1), lwd = c(1,1),bg = "white",cex = 0.70, bty = "n")
# rcorr(j,mexicoareadetrend$MexicoArea[1:10])#r = -0.73, p<0.05
# 
# 
# 
# 
