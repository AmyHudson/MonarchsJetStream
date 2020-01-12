################################################################################################################################################################################################################################################################################################################
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


# import and detrend butterflies.txt 
mexicoarea <- read.table('Butterflies.txt',header = T)
mexicoarea <- mexicoarea[1:25,]
#detrend(mexicoarea,make.plot = T) 
#mexicoareadetrend <- detrend(mexicoarea,make.plot = F, method = c("ModNegExp")) # detrended using modified neg exp
mexicoareadetrend <- matrix(NA,nrow = length(1994:2018),ncol = 2)
mexicoareadetrend[,2] <- lm(mexicoarea$MexicoArea~mexicoarea$Year)$residuals
mexicoareadetrend[,1] <- 1994:2018 #1994:2012
mexicoareadetrend <- data.frame(mexicoareadetrend)
colnames(mexicoareadetrend) <- c("Year", "MexicoArea")


low_to_high <- mexicoareadetrend$Year[order(mexicoareadetrend[,2])]
#Loess curve fitting

lon <- as.integer(seq(235-360,295-360,2.5))
#orange_purple <- c('#b35806','#b35806','#e08214','#e08214','#fdb863','#fdb863','#fee0b6','#fee0b6','#f7f7f7','#f7f7f7','#f7f7f7','#d8daeb','#d8daeb','#b2abd2','#b2abd2','#8073ac','#8073ac','#542788','#542788')

orange_purple <- c("#7F3B08","#7F3B08","#7F3B08", "#B35806","#B35806", "#E08214","#E08214", "#FDB863", "#FDB863","#FEE0B6","#FEE0B6", "#F7F7F7","#F7F7F7","#F7F7F7", "#D8DAEB","#D8DAEB","#B2ABD2","#B2ABD2", "#8073AC","#8073AC", "#542788","#542788", "#2D004B","#2D004B","#2D004B")

n <- read.table("NHJ_position_global_1948jan-2019sep_ncepncar.txt")
colnames(n) <- seq(0,357.5,2.5)
jet1 <- n[2:853,]

png("fig3_allyears_1994-2018.png",8,13,
    units = "in",res = 600, pointsize=20, family= "helvetica")
#par(mfrow=c(1,2), tcl=-0.5, family="serif", mai=c(0,0,0,0),mar = c(0, 0, 0, 0))
par(mfrow=c(4,3),mar = c(0, 0, 0, 0))
# I would like to make 12 maps of the jet stream position, so 4 rows and 3 columns 

#import NHJI for each month

############################################################################
## January
#jet <- read.table("NHJ_USGS/position_jan_20thcentury.txt") #rownames(jet) #1871-2012 
#jet <- jet[124:142,which(seq(0,358,2)>=235 & seq(0,358,2)<=295)] #subset 1994-2012, subset -125 to -65 or 235 to 295
jet <- jet1[seq(1, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
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
text(-65,15, "JAN",adj = c(1,0),cex = 1.5)  

points(-100.14, 19.36, pch=20, col="black",cex=2)  

for (i in 1:25){
  l <- loess(as.numeric(jet[i,])~lon, family = "symmetric")
  lines(lon,l$fitted,col= adjustcolor(orange_purple[i],alpha.f = 0.7), type = "l", lwd = 2)
}
plot(1:10)
axis(side=1,labels=F)

############################################################################
## Feb
#jet <- read.table("NHJ_USGS/position_feb_20thcentury.txt") #rownames(jet) #1871-2012 
#jet <- jet[124:142,which(seq(0,358,2)>=235 & seq(0,358,2)<=295)] #subset 1994-2012, subset -125 to -65 or 235 to 295
jet <- jet1[seq(2, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
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
text(-65,15, "FEB",adj = c(1,0),cex = 1.5)

points(-100.14, 19.36, pch=20, col="black",cex=2)  

for (i in 1:25){
  l <- loess(as.numeric(jet[i,])~lon, family = "symmetric")
  lines(lon,l$fitted,col= adjustcolor(orange_purple[i],alpha.f = 0.7), type = "l", lwd = 2)
}

############################################################################
## March
#jet <- read.table("NHJ_USGS/position_mar_20thcentury.txt") #rownames(jet) #1871-2012 
#jet <- jet[124:142,which(seq(0,358,2)>=235 & seq(0,358,2)<=295)] #subset 1994-2012, subset -125 to -65 or 235 to 295
jet <- jet1[seq(3, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
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
text(-65,15, "MAR",adj = c(1,0),cex = 1.5) 

points(-100.14, 19.36, pch=20, col="black",cex=2)  

for (i in 1:25){
  l <- loess(as.numeric(jet[i,])~lon, family = "symmetric")
  lines(lon,l$fitted,col= adjustcolor(orange_purple[i],alpha.f = 0.7), type = "l", lwd = 2)
}

############################################################################
## April
#jet <- read.table("NHJ_USGS/position_apr_20thcentury.txt") #rownames(jet) #1871-2012 
#jet <- jet[124:142,which(seq(0,358,2)>=235 & seq(0,358,2)<=295)] #subset 1994-2012, subset -125 to -65 or 235 to 295
jet <- jet1[seq(4, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
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
text(-65,15, "APR",adj = c(1,0),cex = 1.5)


points(-100.14, 19.36, pch=20, col="black",cex=2)  

for (i in 1:25){
  l <- loess(as.numeric(jet[i,])~lon, family = "symmetric")
  lines(lon,l$fitted,col= adjustcolor(orange_purple[i],alpha.f = 0.7), type = "l", lwd = 2)
}

############################################################################
## May

#jet <- read.table("NHJ_USGS/position_may_20thcentury.txt") #rownames(jet) #1871-2012 
#jet <- jet[124:142,which(seq(0,358,2)>=235 & seq(0,358,2)<=295)] #subset 1994-2012, subset -125 to -65 or 235 to 295
jet <- jet1[seq(5, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
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
text(-65,15, "MAY",adj = c(1,0),cex = 1.5)

points(-100.14, 19.36, pch=20, col="black",cex=2)  

for (i in 1:25){
  l <- loess(as.numeric(jet[i,])~lon, family = "symmetric")
  lines(lon,l$fitted,col= adjustcolor(orange_purple[i],alpha.f = 0.7), type = "l", lwd = 2)
}

############################################################################
## June
#jet <- read.table("NHJ_USGS/position_jun_20thcentury.txt") #rownames(jet) #1871-2012 
#jet <- jet[124:142,which(seq(0,358,2)>=235 & seq(0,358,2)<=295)] #subset 1994-2012, subset -125 to -65 or 235 to 295
jet <- jet1[seq(6, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
jet <- jet[47:71,]#1994-2018: 47:71
rownames(jet) <- 1994:2018
jet <- lapply(jet, as.character)
jet <- lapply(jet, as.numeric)
jet <- as.data.frame(jet)
jet <- jet[order(mexicoareadetrend[,2]),]

map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico  text(-65,15, "JAN",adj = c(1,0),cex = 1.5)  
text(-65,15, "JUN",adj = c(1,0),cex = 1.5)

points(-100.14, 19.36, pch=20, col="black",cex=2)  

for (i in 1:25){
  l <- loess(as.numeric(jet[i,])~lon, family = "symmetric")
  lines(lon,l$fitted,col= adjustcolor(orange_purple[i],alpha.f = 0.7), type = "l", lwd = 2)
}

############################################################################
## July
jet <- jet1[seq(7, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
jet <- jet[47:71,]#1994-2018: 47:71
rownames(jet) <- 1994:2018
jet <- lapply(jet, as.character)
jet <- lapply(jet, as.numeric)
jet <- as.data.frame(jet)
#jet <- read.table("NHJ_USGS/position_jul_20thcentury.txt") #rownames(jet) #1871-2012 
#jet <- jet[124:142,which(seq(0,358,2)>=235 & seq(0,358,2)<=295)] #subset 1994-2012, subset -125 to -65 or 235 to 295
jet <- jet[order(mexicoareadetrend[,2]),]

map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico  text(-65,15, "JAN",adj = c(1,0),cex = 1.5)  
text(-65,15, "JUL",adj = c(1,0),cex = 1.5)

points(-100.14, 19.36, pch=20, col="black",cex=2)  

for (i in 1:25){
  l <- loess(as.numeric(jet[i,])~lon, family = "symmetric")
  lines(lon,l$fitted,col= adjustcolor(orange_purple[i],alpha.f = 0.7), type = "l", lwd = 2)
}

############################################################################
## August
#jet <- read.table("NHJ_USGS/position_aug_20thcentury.txt") #rownames(jet) #1871-2012 
#jet <- jet[124:142,which(seq(0,358,2)>=235 & seq(0,358,2)<=295)] #subset 1994-2012, subset -125 to -65 or 235 to 295
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
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico  text(-65,15, "JAN",adj = c(1,0),cex = 1.5) 
text(-65,15, "AUG",adj = c(1,0),cex = 1.5)

points(-100.14, 19.36, pch=20, col="black",cex=2)  

for (i in 1:25){
  l <- loess(as.numeric(jet[i,])~lon, family = "symmetric")
  lines(lon,l$fitted,col= adjustcolor(orange_purple[i],alpha.f = 0.7), type = "l", lwd = 2)
}
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
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico  text(-65,15, "JAN",adj = c(1,0),cex = 1.5)  
text(-65,15, "SEP",adj = c(1,0),cex = 1.5)

points(-100.14, 19.36, pch=20, col="black",cex=2)  

for (i in 1:25){
  l <- loess(as.numeric(jet[i,])~lon, family = "symmetric")
  lines(lon,l$fitted,col= adjustcolor(orange_purple[i],alpha.f = 0.7), type = "l", lwd = 2)
}
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
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico  text(-65,15, "JAN",adj = c(1,0),cex = 1.5)  
text(-65,15, "OCT",adj = c(1,0),cex = 1.5)

points(-100.14, 19.36, pch=20, col="black",cex=2)  

for (i in 1:25){
  l <- loess(as.numeric(jet[i,])~lon, family = "symmetric")
  lines(lon,l$fitted,col= adjustcolor(orange_purple[i],alpha.f = 0.7), type = "l", lwd = 2)
}
############################################################################
## November
#jet <- read.table("NHJ_USGS/position_nov_20thcentury.txt") #rownames(jet) #1871-2012 
#jet <- jet[124:142,which(seq(0,358,2)>=235 & seq(0,358,2)<=295)] #subset 1994-2012, subset -125 to -65 or 235 to 295
jet <- jet1[seq(11, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
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
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico  text(-65,15, "JAN",adj = c(1,0),cex = 1.5)  
text(-65,15, "NOV",adj = c(1,0),cex = 1.5)

points(-100.14, 19.36, pch=20, col="black",cex=2)  

for (i in 1:25){
  l <- loess(as.numeric(jet[i,])~lon, family = "symmetric")
  lines(lon,l$fitted,col= adjustcolor(orange_purple[i],alpha.f = 0.7), type = "l", lwd = 2)
}
############################################################################
## December
#jet <- read.table("NHJ_USGS/position_dec_20thcentury.txt") #rownames(jet) #1871-2012 
#jet <- jet[124:142,which(seq(0,358,2)>=235 & seq(0,358,2)<=295)] #subset 1994-2012, subset -125 to -65 or 235 to 295
jet <- jet1[seq(12, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
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
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico  text(-65,15, "JAN",adj = c(1,0),cex = 1.5)  
text(-65,15, "DEC",adj = c(1,0),cex = 1.5)

points(-100.14, 19.36, pch=20, col="black",cex=2)  

for (i in 1:25){
  l <- loess(as.numeric(jet[i,])~lon, family = "symmetric")
  lines(lon,l$fitted,col= adjustcolor(orange_purple[i],alpha.f = 0.7), type = "l", lwd = 2)
}
dev.off()

# Extra plotting ideas:

#orange <- c('#fff5eb','#fff5eb','#fee6ce','#fee6ce','#fdd0a2','#fdd0a2','#fdae6b','#fdae6b','#fd8d3c','#fd8d3c','#fd8d3c','#f16913','#f16913','#d94801','#d94801','#a63603','#a63603','#7f2704','#7f2704')

#add state lines
#map(database = "state",  xlim=c(-125,-65),ylim=c(15,60),col = "black", fill = F, add = T,lwd = 1)
#add canadian provinces
#provinces <- c("British Columbia","Alberta","Saskatchewan","Manitoba","Ontario","Quebec")
#canada <- getData("GADM",country = "CAN", level = 1)
#ca_provinces <- canada[canada$NAME_1 %in% provinces,]
#plot(ca_provinces,  xlim=c(-125,-65),ylim=c(15,60),col = "black", fill = F, add = T,lwd = 2)

## ADD:

# Legend
# 3 letter symbol of month #title(sub="hallo", adj=1, line=3, font=2)
# Add region of correlation with temperature?
# Add region of correlation with precipitation? 

# Check significant correlations for each longitude- expand on valerie's worksheet.


################################################################################################################################################################################################################################################################################################################
## Fig 2 Jet and Temp
## Composites

################################################################################################################################################################################################################################################################################################################
## Fig 3 Monarch and Temp
## Avg temp (monthly) for High return years and Low return years (developed for fig 1 above) 
# Mask out region that is significantly correlated with monarch migration (by month) 
# How many years? to choose? 8? detrended?
# 
#mexicoareadetrend[order(mexicoareadetrend[,2]),]$Year[1:8] #low
#mexicoareadetrend[order(mexicoareadetrend[,2]),]$Year[12:19] #high

#getting color bar for rasters to be the same across raster... 