# read in GBIF for point centroid; read in jet, average seasonally (monthly?)
# DJF
# MAM
# JJA
# SON
##########################
#

monarch <- read.delim("/Volumes/AOP-NEON1.4/Monarchs/MonarchsJetStream-master/data/0076011-200613084148143.csv")

colnames(monarch)
# subset for year 

monarch <- monarch[which(monarch$decimalLatitude>= 15 & monarch$decimalLatitude<= 70 & monarch$decimalLongitude>= -125 & monarch$decimalLongitude <= -65),]
monarch_dec <- monarch[which(monarch$year>=1993 &monarch$year<=2018),]
monarch_dec <- monarch_dec[which(monarch_dec$month == 12),]

monarch_nodec <- monarch[which(monarch$year>=1994 &monarch$year<=2019),]
# grab all observations that are geo referenced for DJF



##########################
n <- read.table("NHJ_position_global_1948jan-2019sep_ncepncar.txt")
#n <- as.numeric(n)
colnames(n) <- seq(0,357.5,2.5)
jet1 <- n[2:853,]
yr <- 1948:2018

# ##########
# jet <- jet1[seq(2, 852, 12),which(seq(0,357.5,2.5)>=(235) & seq(0,357.5,2.5)<=295)]
# jet <- jet[which(yr<=2019 & yr>=2000),] 
# 
# jet <- lapply(jet, as.character)
# jet <- lapply(jet, as.numeric)
# jet <- as.data.frame(jet)
# #jet <- as.data.frame(pracma::detrend(as.matrix(jet), tt = 'linear'))
# jetdetrend <- pracma::detrend(as.matrix(jet))

##########
# Fig 1: Jet stream and Monarchs- Each 25 years of 25 jet streams colored by ranked Monarch return
# I will take the (detrended) overwintering monarch numbers highest and lowest population sizes and plot the jet stream latitudes with a loess function for each month.
# the jet was not detrended originally- now I detrend.

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


lon <- as.integer(seq(235-360,295-360,2.5))
#orange_purple <- c('#b35806','#b35806','#e08214','#e08214','#fdb863','#fdb863','#fee0b6','#fee0b6','#f7f7f7','#f7f7f7','#f7f7f7','#d8daeb','#d8daeb','#b2abd2','#b2abd2','#8073ac','#8073ac','#542788','#542788')


png("fig1ABCD_seasonalmonarchjetoverlay.png",12,6,
    units = "in",res = 600, pointsize=20, family= "helvetica")
#par(mfrow=c(1,2), tcl=-0.5, family="serif", mai=c(0,0,0,0),mar = c(0, 0, 0, 0))
par(mfrow=c(1,4),mar = c(0, 0, 0, 0))

#import NHJI for each month

############################################################################

jet <- jet1[seq(12, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
jet <- jet[46:70,]#1993-2017
jet <- as.numeric(unlist(jet))

jetb <- jet1[seq(1, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
jetb <- jetb[47:71,]#1994-2018
jetb <- as.numeric(unlist(jetb))

jetc <- jet1[seq(2, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
jetc <- jetc[47:71,]#1994-2018
jetc <- as.numeric(unlist(jetc))

jet3 <- rowMeans(as.data.frame(cbind(jet, jetb, jetc)))
jet3 <- rowMeans(as.data.frame(cbind(jet, jetb)))
jet3 <- rowMeans(as.data.frame(cbind(jetb, jetc)))

dim(jet3) <- c(25,25)
jet3 <- as.data.frame(jet3)
jet3 <- as.numeric(colMeans(jet3))

map("worldHires",c("Canada", "USA","Mexico"), xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line
points(monarch_dec$decimalLongitude,monarch_dec$decimalLatitude, cex = 0.3, pch = 16, col = adjustcolor("grey", alpha.f=0.1))
monarch_sub <- monarch_dec[which(monarch_dec$decimalLongitude >-105),]
monarch_sub <- monarch_dec[-which(monarch_dec$decimalLatitude <30 & monarch_dec$decimalLongitude >-85),]
points(monarch_dec$decimalLongitude,monarch_dec$decimalLatitude, cex = 0.3, pch = 16, col = adjustcolor("orange", alpha.f=0.2))


monarch_sub <- monarch_nodec[which(monarch_nodec$month == 1 | monarch_nodec$month == 2 ),]
points(monarch_sub$decimalLongitude,monarch_sub$decimalLatitude, cex = 0.3, pch = 16, col = adjustcolor("grey", alpha.f=0.1))

monarch_sub <- monarch_sub[which(monarch_sub$decimalLongitude >-105),]
monarch_sub <- monarch_sub[-which(monarch_sub$decimalLatitude <30 & monarch_sub$decimalLongitude >-85),]
points(monarch_sub$decimalLongitude,monarch_sub$decimalLatitude, cex = 0.3, pch = 16, col = adjustcolor("orange", alpha.f=0.2))


points(mean(c(monarch_sub$decimalLongitude,monarch_dec$decimalLongitude), na.rm = T), mean(c(monarch_sub$decimalLatitude,monarch_dec$decimalLatitude), na.rm = T), pch=20, col="black",cex=2)
text(-65,15, "DJF",adj = c(1,0),cex = 1.5)  


#points(monarch_sub_noflwest$decimalLongitude,monarch_sub_noflwest$decimalLatitude)

#points(mean(c(monarch_sub$decimalLongitude,monarch_dec$decimalLongitude), mean(c(monarch_sub$decimalLatitude,monarch_dec$decimalLatitude), na.rm = T)), 23, pch=20, col="black",cex=2) 


  l <- loess(as.numeric(jet3)~lon, family = "symmetric")
  lines(lon,l$fitted, lwd = 3)


  jet <- jet1[seq(3, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
  jet <- jet[47:71,]#1993-2017
  jet <- as.numeric(unlist(jet))
  
  jetb <- jet1[seq(4, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
  jetb <- jetb[47:71,]#1994-2018
  jetb <- as.numeric(unlist(jetb))
  
  jetc <- jet1[seq(5, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
  jetc <- jetc[47:71,]#1994-2018
  jetc <- as.numeric(unlist(jetc))
  
  jet3 <- rowMeans(as.data.frame(cbind(jet, jetb, jetc)))
  jet3 <- rowMeans(as.data.frame(cbind(jet, jetb)))
  jet3 <- rowMeans(as.data.frame(cbind(jetb, jetc)))
  
  dim(jet3) <- c(25,25)
  jet3 <- as.data.frame(jet3)
  jet3 <- as.numeric(colMeans(jet3))
  
  map("worldHires",c("Canada", "USA","Mexico"), xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
  map.axes()
  #title("August") this is mid line
  monarch_sub <- monarch_nodec[which(monarch_nodec$month == 3 | monarch_nodec$month == 4 | monarch_nodec$month == 5 ),]
  points(monarch_sub$decimalLongitude,monarch_sub$decimalLatitude, cex = 0.3, pch = 16, col = adjustcolor("grey", alpha.f=0.1))
  
  monarch_sub <- monarch_sub[which(monarch_sub$decimalLongitude >-105),]
  monarch_sub <- monarch_sub[-which(monarch_sub$decimalLatitude <30 & monarch_sub$decimalLongitude >-85),]
  points(monarch_sub$decimalLongitude,monarch_sub$decimalLatitude, cex = 0.3, pch = 16, col = adjustcolor("orange", alpha.f=0.2))
  points(mean(monarch_sub$decimalLongitude,na.rm = T), mean(monarch_sub$decimalLatitude, na.rm = T), pch=20, col="black",cex=2) 
  
  
  text(-65,15, "MAM",adj = c(1,0),cex = 1.5)  
  
  #points(-92, 33, pch=20, col="black",cex=2)  
  
  l <- loess(as.numeric(jet3)~lon, family = "symmetric")
  lines(lon,l$fitted, lwd = 3)
  
  jet <- jet1[seq(6, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
  jet <- jet[47:71,]#1993-2017
  jet <- as.numeric(unlist(jet))
  
  jetb <- jet1[seq(7, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
  jetb <- jetb[47:71,]#1994-2018
  jetb <- as.numeric(unlist(jetb))
  
  jetc <- jet1[seq(8, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
  jetc <- jetc[47:71,]#1994-2018
  jetc <- as.numeric(unlist(jetc))
  

  rbindlist(list(jet, jetb, jetc))[,lapply(.SD,mean), list(Lon, Lat)]
  
  jet3 <- as.data.frame(cbind(jet, jetb, jetc))
  jet3 <- rowMeans(as.data.frame(cbind(jet, jetb)))
  jet3 <- rowMeans(as.data.frame(cbind(jetb, jetc)))
  
  dim(jet3) <- c(25,25)
  jet3 <- as.data.frame(jet3)
  jet3 <- as.numeric(colMeans(jet3))
  
  map("worldHires",c("Canada", "USA","Mexico"), xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
  map.axes()
  #title("August") this is mid line
  
 
  
  monarch_sub <- monarch_nodec[which(monarch_nodec$month == 6 | monarch_nodec$month == 7 | monarch_nodec$month == 8 ),]
  points(monarch_sub$decimalLongitude,monarch_sub$decimalLatitude, cex = 0.3, pch = 16, col = adjustcolor("grey", alpha.f=0.1))
  
  monarch_sub <- monarch_sub[which(monarch_sub$decimalLongitude >-105),]
  monarch_sub <- monarch_sub[-which(monarch_sub$decimalLatitude <30 & monarch_sub$decimalLongitude >-85),]
  points(monarch_sub$decimalLongitude,monarch_sub$decimalLatitude, cex = 0.3, pch = 16, col = adjustcolor("orange", alpha.f=0.2))
  points(mean(monarch_sub$decimalLongitude,na.rm = T), mean(monarch_sub$decimalLatitude, na.rm = T), pch=20, col="black",cex=2)  
  
  
  
  text(-65,15, "JJA",adj = c(1,0),cex = 1.5)  
  
  
  l <- loess(as.numeric(jet3)~lon, family = "symmetric")
  lines(lon,l$fitted, lwd = 3)
  
  jet <- jet1[seq(9, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
  jet <- jet[47:71,]
  jet <- as.numeric(unlist(jet))
  
  jetb <- jet1[seq(10, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
  jetb <- jetb[47:71,]#1994-2018
  jetb <- as.numeric(unlist(jetb))
  
  jetc <- jet1[seq(11, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
  jetc <- jetc[47:71,]#1994-2018
  jetc <- as.numeric(unlist(jetc))
  
  jet3 <- rowMeans(as.data.frame(cbind(jet, jetb, jetc)))
  jet3 <- rowMeans(as.data.frame(cbind(jet, jetb)))
  jet3 <- rowMeans(as.data.frame(cbind(jetb, jetc)))
  
  dim(jet3) <- c(25,25)
  jet3 <- as.data.frame(jet3)
  jet3 <- as.numeric(colMeans(jet3))
  
  map("worldHires",c("Canada", "USA","Mexico"), xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
  map.axes()
  #title("August") this is mid line
  monarch_sub <- monarch_nodec[which(monarch_nodec$month == 9 | monarch_nodec$month == 10 | monarch_nodec$month == 11 ),]
  points(monarch_sub$decimalLongitude,monarch_sub$decimalLatitude, cex = 0.3, pch = 16, col = adjustcolor("grey", alpha.f=0.1))
  
  monarch_sub <- monarch_sub[which(monarch_sub$decimalLongitude >-105),]
  monarch_sub <- monarch_sub[-which(monarch_sub$decimalLatitude <30 & monarch_sub$decimalLongitude >-85),]
  points(monarch_sub$decimalLongitude,monarch_sub$decimalLatitude, cex = 0.3, pch = 16, col = adjustcolor("orange", alpha.f=0.2))
  points(mean(monarch_sub$decimalLongitude,na.rm = T), mean(monarch_sub$decimalLatitude, na.rm = T), pch=20, col="black",cex=2)
  
  text(-65,15, "SON",adj = c(1,0),cex = 1.5)  
  
 
  
  l <- loess(as.numeric(jet3)~lon, family = "symmetric")
  lines(lon,l$fitted, lwd = 3)

  dev.off()