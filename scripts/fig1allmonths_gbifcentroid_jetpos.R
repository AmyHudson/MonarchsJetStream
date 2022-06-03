# read in GBIF for point centroid; read in jet, average (monthly)

##########################
#

monarch <- read.delim("data/raw/0076011-200613084148143.csv") #2019
monarch <- read.delim("data/raw/0292940-200613084148143.csv") #2020

colnames(monarch)
unique(monarch$year)
# subset for year 
monarch_dec <- monarch[which(monarch$year>=1993 &monarch$year<=2019),]
monarch_dec <- monarch_dec[which(monarch_dec$month == 12),]

monarch_nodec <- monarch[which(monarch$year>=1994 &monarch$year<=2020),]
# grab all observations that are geo referenced for DJF



##########################

n <- read.table("data/processed/NHJ_position_global_1948jan-2021apr_ncepncar.txt")
#n <- as.numeric(n)
colnames(n) <- seq(0,357.5,2.5)
jet1 <- n[2:877,] #grab complete 2020 year
yr <- 1948:2020


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
library(matrixStats)

lon <- as.integer(seq(235-360,295-360,2.5))
#orange_purple <- c('#b35806','#b35806','#e08214','#e08214','#fdb863','#fdb863','#fee0b6','#fee0b6','#f7f7f7','#f7f7f7','#f7f7f7','#d8daeb','#d8daeb','#b2abd2','#b2abd2','#8073ac','#8073ac','#542788','#542788')


png("figures/fig1allmonths_gbifcentroid_jetpos.png",12,9,
    units = "in",res = 600, pointsize=20, family= "helvetica")
#par(mfrow=c(1,2), tcl=-0.5, family="serif", mai=c(0,0,0,0),mar = c(0, 0, 0, 0))
par(mfrow=c(3,4),mar = c(0, 0, 0, 0))

#import NHJI for each month


# mon name

mon_name <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV")
##########################

jet <- as.data.frame(jet1[seq(12, 876, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)])
jet <- as.data.frame(jet[which(yr==1994):which(yr==2020),])
jet2 <- as.numeric(unlist(jet))
dim(jet2) <- dim(jet)
jet3 <- colMeans(jet2)

map("worldHires",c("Canada", "USA","Mexico"), xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
## add grey polygon
lon_left <- -125
lon_right <- -65

polygon.x <- c(seq(lon_left,lon_right,by = 2.5),rev(seq(lon_left,lon_right,by = 2.5)))
polygon.y <- c(colMins(jet2),
               rev(colMaxs(jet2)))
polygon(x=polygon.x, y=polygon.y, col=adjustcolor("grey", alpha.f=0.3), border=F) #red positive correlation 


## add points
monarch_dec <- monarch[which(monarch$year>=1993 & monarch$year<=2019),]
monarch_dec <- monarch_dec[which(monarch_dec$month == 12),]

points(monarch_dec$decimalLongitude,monarch_dec$decimalLatitude, cex = 0.3, pch = 16, col = adjustcolor("grey", alpha.f=0.1))

monarch_dec <- monarch_dec[which(monarch_dec$decimalLongitude >-105),]
monarch_dec <- monarch_dec[-which(monarch_dec$decimalLatitude <30 & monarch_dec$decimalLongitude >-85),]
points(monarch_dec$decimalLongitude,monarch_dec$decimalLatitude, cex = 0.3, pch = 16, col = adjustcolor("orange", alpha.f=0.2))
points(mean(monarch_dec$decimalLongitude,na.rm = T), mean(monarch_dec$decimalLatitude, na.rm = T), pch=20, col="black",cex=2) 

text(-65,15, "DEC",adj = c(1,0),cex = 1.5)  
# for(j in 1:25){
#   l <- loess(jet2[j,]~lon, family = "symmetric")
#   lines(lon,l$fitted, lwd = 1, col = adjustcolor("gray", alpha.f=0.2))
# }
l <- loess(as.numeric(jet3)~lon, family = "symmetric")
lines(lon,l$fitted, lwd = 3)



for (i in 1:11){
  
jet <- as.data.frame(jet1[seq(i, 876, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)])
jet <- as.data.frame(jet[which(yr==1994):which(yr==2020),])
jet2 <- as.numeric(unlist(jet))
dim(jet2) <- dim(jet)
jet3 <- colMeans(jet2)

map("worldHires",c("Canada", "USA","Mexico"), xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line
polygon.x <- c(seq(lon_left,lon_right,by = 2.5),rev(seq(lon_left,lon_right,by = 2.5)))
polygon.y <- c(colMins(jet2),
               rev(colMaxs(jet2)))
polygon(x=polygon.x, y=polygon.y, col=adjustcolor("grey", alpha.f=0.3), border=F) #red positive correlation 

monarch_sub <- monarch_nodec[which(monarch_nodec$month == i),]
points(monarch_sub$decimalLongitude,monarch_sub$decimalLatitude, cex = 0.3, pch = 16, col = adjustcolor("grey", alpha.f=0.1))

monarch_sub <- monarch_sub[which(monarch_sub$decimalLongitude >-105),]
monarch_sub <- monarch_sub[-which(monarch_sub$decimalLatitude <30 & monarch_sub$decimalLongitude >-85),]
points(monarch_sub$decimalLongitude,monarch_sub$decimalLatitude, cex = 0.3, pch = 16, col = adjustcolor("orange", alpha.f=0.2))
points(mean(monarch_sub$decimalLongitude,na.rm = T), mean(monarch_sub$decimalLatitude, na.rm = T), pch=20, col="black",cex=2) 

text(-65,15, mon_name[i],adj = c(1,0),cex = 1.5)  

# for(j in 1:25){
#   l <- loess(jet2[j,]~lon, family = "symmetric")
#   lines(lon,l$fitted, lwd = 1, col = adjustcolor("gray", alpha.f=0.2))
# }
l <- loess(as.numeric(jet3)~lon, family = "symmetric")
lines(lon,l$fitted, lwd = 3)

}

dev.off()

##########################
i = 3
monarch_sub <- monarch_nodec[which(monarch_nodec$month == i),]
monarch_sub <- monarch_sub[which(monarch_sub$decimalLongitude >-105),]

monarch_sub <- monarch_sub[-which(monarch_sub$decimalLatitude <30 & monarch_sub$decimalLongitude >-85),]

map("worldHires",c("Canada", "USA","Mexico"), xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

points(monarch_sub$decimalLongitude,monarch_sub$decimalLatitude, cex = 0.3, pch = 16, col = adjustcolor("orange", alpha.f=0.2))
points(mean(monarch_sub$decimalLongitude,na.rm = T), mean(monarch_sub$decimalLatitude, na.rm = T), pch=20, col="black",cex=2) 


table(monarch_sub$year)
as.numeric(table(monarch_sub$year))
june <- c(1,  24,   6, 166,  46 , 0,0, 1,0,   4  , 6  , 3   ,7  , 5  , 8 , 13,  45 , 14  ,35, 62,  78, 270 ,537 ,990)
feb <- c(11,  12 ,0, 41 ,0,0,0,  2 ,0,0 , 2 ,0,  1   ,1 ,  2   ,7   ,1 , 13 , 14, 113, 105, 235 ,460)

#how many unique observers per year?
num_uniquelocations <- NA 
for (i in 1996:2019){
  test <- monarch_sub[which(monarch_sub$year == i),]
  num_uniquelocations[i-1995] <- length(unique(test$decimalLatitude))  
}

num_uniquelocations <- num_uniquelocations[which(num_uniquelocations!=0)]
plot(table(monarch_sub$year)/num_uniquelocations, type = "l")
library(dplR)
junedetrend <- detrend.series(june)
junedetrend <- detrend.series(feb)

mexicoarea <- read.table('Butterflies1994-2019.txt',header = T)
mexicoarea[,1] <- 1994:2019
#mexicoarea <- mexicoarea[3:26,]
mexicoarea <- mexicoarea[2:25,] #correlate Feb w previous year

mexicoareadetrend <- matrix(NA,nrow = length(1996:2019),ncol = 2)

mexicoareadetrend[,2] <- lm(mexicoarea$MexicoArea~mexicoarea$Year)$residuals
mexicoareadetrend <- data.frame(mexicoareadetrend)
colnames(mexicoareadetrend) <- c("Year", "MexicoArea")
library(Hmisc)

rcorr(junedetrend$Friedman,mexicoareadetrend$MexicoArea) 

junedetrend <- detrend.series(june[7:24])
rcorr(junedetrend$Friedman,roost1) 
