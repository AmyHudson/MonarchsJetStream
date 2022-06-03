
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


##############
#FIG1

monarch_ts <- read.csv("MonarchTimeSeries.csv")
monarch_ts <- monarch_ts[2:29,]

png("figures/fig1_timeseriesplots.png",3,5,
    units = "in",res = 600, pointsize=12, family= "helvetica")
par(mfrow=c(4,1), mar = c(2,3,0.5,1))

plot(monarch_ts$Year, monarch_ts$JN_ROOST_8, type = "l", xlab = "", ylab = "Index", lty = c(2),xaxs="i")
abline(lm(monarch_ts$JN_ROOST_8~monarch_ts$Year))
legend("top", legend = c("August Roost"), col = c("black"), lty = c(2), lwd = c(1),bg = "white",cex = 0.70, bty = "n")

plot(monarch_ts$Year, monarch_ts$JN_ROOST_9, type = "l", xlab = "", ylab = "Index",  lty = c(3),xaxs="i")
abline(lm(monarch_ts$JN_ROOST_9~monarch_ts$Year))
legend("top", legend = c("September Roost"), col = c("black"), lty = c(3), lwd = c(1),bg = "white",cex = 0.70, bty = "n")

plot(monarch_ts$Year, monarch_ts$Mexico, type = "l", xlab = "", ylab = "Acreage",xaxs="i")
abline(lm(monarch_ts$Mexico~monarch_ts$Year))
legend("top", legend = c("Total area occupied at overwintering sites in Mexico (ha)"), col = c("black"), lty = c(1), lwd = c(1),bg = "white",cex = 0.70, bty = "n")

#truncate to 2002-2020 
monarch_ts1 <- monarch_ts[which(monarch_ts$Year>=2002 & monarch_ts$Year<=2020),]
#detrend each index
monarch_ts_detrend <- data.frame(pracma::detrend(as.matrix(monarch_ts1)))
     
library(Hmisc)
rcorr(monarch_ts_detrend$Mexico,monarch_ts_detrend$JN_ROOST_8)
rcorr(monarch_ts_detrend$Mexico,monarch_ts_detrend$JN_ROOST_9)
rcorr(monarch_ts_detrend$JN_ROOST_8,monarch_ts_detrend$JN_ROOST_9)


plot(2002:2020, scale(monarch_ts_detrend$JN_ROOST_8), lty = 2, lwd = 1, col = "black", type = "l", ylim = c(-3, 3), xaxs="i",
     yaxs="i", xlab = "", ylab = "")
lines(2002:2020, scale(monarch_ts_detrend$JN_ROOST_9), lty = 3,lwd = 1, col = "black")
lines(2002:2020, scale(monarch_ts_detrend$Mexico), lwd = 1, col = "black")
abline(h = 0)
legend("top", legend = c("August Roost Index, r=0.49","September Roost Index, r=0.54","Winter Area Index"), col = c("black","black","black"), lty = c(2,3,1), lwd = c(1,1,1),bg = "white",cex = 0.70, bty = "n")



dev.off()

roost <- read.csv("data/raw/monarch_journeynorth_Fall_Roost.csv")
roost[which(roost$Longitude == -11051301.0),]$Longitude<- -110.5 #error at one observation in Journey North at Canelo, AZ, which is at -110.51 (I matched to tenths place like the other observations)

png("figures/fig1_maps_jn.png",2,7,
    units = "in",res = 600, pointsize=12, family= "helvetica")
par(mfrow=c(4,1), mar = c(0,0,0,0))

for (i in c(8,9,12)){
roost1 <- roost[which(roost$Month == i),]


map("worldHires",c("Canada", "USA","Mexico"), xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()

## add points
points(roost1$Longitude,roost1$Latitude, cex = 0.3, pch = 16, col = adjustcolor("grey", alpha.f=0.1))
points(roost1$Longitude,roost1$Latitude, cex = 0.3, pch = 16, col = adjustcolor("orange", alpha.f=0.2))
if(i == 8 | i == 9){
  points(mean(roost1$Longitude,na.rm = T), mean(roost1$Latitude, na.rm = T), pch=20, col="black",cex=2) 
}

if(i == 12){
  points(-100.296, 19.556, pch=20, col="black",cex=2) 
}
}
dev.off()



gbif <- read.delim("data/raw/0292940-200613084148143.csv") #1994-2020
gbif <- gbif[which(gbif$year>=2002),]

png("figures/fig1_maps_gbif_2002-2020.png",2,7,
    units = "in",res = 600, pointsize=12, family= "helvetica") #
par(mfrow=c(4,1), mar = c(0,0,0,0))

for (i in c(8,9,12)){

  gbif1 <- gbif[which(gbif$month == i),]

  
  map("worldHires",c("Canada", "USA","Mexico"), xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
  map.axes()
  
  ## add points
  points(gbif1$decimalLongitude,gbif1$decimalLatitude, cex = 0.3, pch = 16, col = adjustcolor("grey", alpha.f=0.1))
  
  sub <- gbif1[which(gbif1$decimalLongitude >-105),]
  sub <- sub[-which(sub$decimalLatitude <33 & sub$decimalLongitude >-85),]
  
  points(sub$decimalLongitude,sub$decimalLatitude, cex = 0.3, pch = 16, col = adjustcolor("orange", alpha.f=0.2))
  points(mean(sub$decimalLongitude,na.rm = T), mean(sub$decimalLatitude, na.rm = T), pch=20, col="black",cex=2) 
}
dev.off()


################
# playing w detrending the Durham Fall roost time series
monarch_ts2 <- monarch_ts[which(monarch_ts$Year>=1998 & monarch_ts$Year<=2020),]
#detrend each index
monarch_ts_detrend1 <- data.frame(pracma::detrend(as.matrix(monarch_ts2))) 
rcorr(monarch_ts_detrend1$Mexico,monarch_ts_detrend1$Xerces_CA_Nov_OverWintering)
plot(1998:2020,scale(monarch_ts_detrend1$Durham_CA_OctDec_FallRoost), ylim = c(-2,3), type = "h", lwd = "3")
lines(1998:2020,scale(monarch_ts_detrend1$Xerces_CA_Nov_OverWintering), lty = c(2))
lines(1998:2020,scale(monarch_ts_detrend1$Mexico))
abline(h =0)

legend("top", legend = c("Durham Fall Roost","Xerces Overwintering","Mexico Winter Acreage"), col = c("black","black","black"), lty = c(1,2,1), lwd = c(3,1,1),bg = "white",cex = 0.70, bty = "n")

# try and detrend Durham values
x<- lm(monarch_ts$Year~monarch_ts$Durham_CA_OctDec_FallRoost)
as.numeric(x$residuals)
y <- c(-15.8412578, -15.5984845, NA,  NA  ,-11.1285739,  -2.4226792 , -8.1040908,  NA,  NA,  NA, 4.3590306,  NA,   -4.0639715,  NA,  NA, -1.0639715,   0.4259987,  NA,  NA,  NA,  3.8469430,  NA,  NA,  NA,  8.5596269 ,  9.4705414,   9.6242293,  10.4906010,  11.4460583)
z<- lm(monarch_ts$Year~y)
# dplR doesn't let you have NAs in the middle of the series... I could play with the code to allow it...
#library(dplR)
#detrend.series(monarch_ts$Durham_CA_OctDec_FallRoost)
z <- c(1.2412578 , 1.9984845, NA,  NA  ,0.5285739,  -7.1773208 , -0.4959092,  NA,  NA,  NA, -8.9590306,  NA,   1.4639715,  NA,  NA, 1.4639715,   0.9740013,  NA,  NA,  NA,  1.5530570,  NA,  NA,  NA,   0.8403731 , 0.9294586,  1.7757707,  1.9093990 , 1.9539417)
rcorr(monarch_ts$Mexico,z)

##################
# playing w detrending the Durham Fall roost time series
monarch_ts2 <- monarch_ts[which(monarch_ts$Year>=2001 & monarch_ts$Year<=2017),]
#detrend each index
monarch_ts_detrend1 <- data.frame(pracma::detrend(as.matrix(monarch_ts2))) 
rcorr(monarch_ts_detrend1$Mexico,monarch_ts_detrend1$Xerces_CA_Nov_OverWintering)


plot(2002:2020,scale(monarch_ts_detrend1$Durham_CA_OctDec_FallRoost), ylim = c(-2,3), type = "h", lwd = "3")
lines(2002:2020,scale(monarch_ts_detrend1$Xerces_CA_Nov_OverWintering), lty = c(2))
lines(2002:2020,scale(monarch_ts_detrend1$Mexico))
abline(h = 0)


legend("top", legend = c("Durham Fall Roost","Xerces Overwintering","Mexico Winter Acreage"), col = c("black","black","black"), lty = c(1,2,1), lwd = c(3,1,1),bg = "white",cex = 0.70, bty = "n")


# try and detrend Durham values
x<- lm(monarch_ts2$Year~monarch_ts2$Durham_CA_OctDec_FallRoost)
as.numeric(x$residuals)
x$residuals
y <- c( 0.1064104,  NA,  -8.7426189,  NA,  NA, -5.7426189,   -4.2326218,  NA,  NA,  NA,  -0.8353456 ,  NA,  NA,  NA ,3.9064683,  4.8137416,  4.9328375,  5.7937474)
rcorr(monarch_ts_detrend1$Mexico,y)


z<- lm(monarch_ts2$Year~y)
# dplR doesn't let you have NAs in the middle of the series... I could play with the code to allow it...
#library(dplR)
#detrend.series(monarch_ts$Durham_CA_OctDec_FallRoost)
z <- c(1.2412578 , 1.9984845, NA,  NA  ,0.5285739,  -7.1773208 , -0.4959092,  NA,  NA,  NA, -8.9590306,  NA,   1.4639715,  NA,  NA, 1.4639715,   0.9740013,  NA,  NA,  NA,  1.5530570,  NA,  NA,  NA,   0.8403731 , 0.9294586,  1.7757707,  1.9093990 , 1.9539417)
rcorr(monarch_ts$Mexico,z)

########################################
# plot summer indices and summer jet stream, show correlation values
###############
# library(pracma)
# 
# peak <- read.csv("~/Desktop/monarch_journeynorth_PEAK.csv") #2000 has a lot of 0s
# # peak$Number estimates start in 2011; roost start in 2002 but patchy.
# 
# peak1 <- peak[which((peak$Month == 8 | peak$Month == 9) & (peak$Latitude>35) & peak$Longitude>-105 & -50>peak$Longitude),]
# as.numeric(table(peak1$Year))
# peak1 <- as.numeric(pracma::detrend(as.matrix(as.numeric(table(peak1$Year)))))
# JN_PEAK_89 <- peak1
# 
# 
# plot(2000:2020, scale(peak1), type = "l",  xaxs="i",
#      yaxs="i", xlab = "", ylab = "", ylim = c(-3,3), lty = 3)
# abline(h = 0)
# 
# peak1 <- peak[which((peak$Month == 8) & (peak$Latitude>35) & peak$Longitude>-105  & -50>peak$Longitude),]
# t <- as.numeric(table(peak1$Year)) # 2008 is missing, so I added it in as 0. 
# peak1 <- c(12, 23,  4,  6,  0, 23, 26, 39, 0, 12, 23,  8, 11,  3, 13, 11,  4, 16, 46, 21)
# peak1 <- as.numeric(pracma::detrend(as.matrix(peak1)))
# 
# JN_PEAK_8 <- peak1
# 
# lines(2000:2020, scale(peak1), lty = 1,col = "black")
# 
# peak1 <- peak[which((peak$Month == 8 | (peak$Month == 9 & peak$Day <=15))& (peak$Latitude>35) & peak$Longitude>-105  & -50>peak$Longitude),]
# peak1 <- as.numeric(pracma::detrend(as.matrix(as.numeric(table(peak1$Year)))))
# JN_PEAK_8.5 <- peak1
# lines(2000:2020, scale(peak1), lty = 2,col = "black")
# 
# 
# 
# roost <- read.csv("~/Desktop/monarch_journeynorth_Fall_Roost.csv")
# 
# #plot points in peak and roost?
# 
# 
# roost1 <- roost[which((roost$Month == 8 | roost$Month == 9)& (roost$Latitude>35) & roost$Longitude>-105 & -50>roost$Longitude),]
# table(roost1$Year)
# roost1 <- as.numeric(pracma::detrend(as.matrix(as.numeric(table(roost1$Year)))))
# JN_ROOST_89 <- roost1
# lines(2002:2020, scale(roost1), lty = 3, col = "red")
# 
# roost1 <- roost[which((roost$Month == 8)& (roost$Latitude>35) & roost$Longitude>-105 & -50>roost$Longitude),]
# table(roost1$Year) 
# roost1 <- as.numeric(pracma::detrend(as.matrix(as.numeric(table(roost1$Year)))))
# JN_ROOST_8 <- roost1
# lines(2002:2020, scale(roost1), lty = 1, col = "red")
# 
# 
# roost1 <- roost[which((roost$Month == 8 | (roost$Month == 9 & roost$Day <=15))& (roost$Latitude>35) & roost$Longitude>-105 & -50>roost$Longitude),]
# roost1 <- as.numeric(pracma::detrend(as.matrix(as.numeric(table(roost1$Year)))))
# JN_ROOST_8.5 <- roost1
# lines(2002:2020, scale(roost1), lty = 2, col = "red")
# 
# 
# ## add mexico detrended
# 
# mexicoarea <- read.table('Butterflies1994-2020.txt',header = T)
# mexicoarea[,1] <- 1994:2020
# mexicoareadetrend <- matrix(NA,nrow = length(1994:2020),ncol = 2)
# mexicoareadetrend[,2] <- lm(mexicoarea$MexicoArea~mexicoarea$Year)$residuals
# mexicoareadetrend[,1] <- 1994:2020 #1994:2012
# mexicoareadetrend <- data.frame(mexicoareadetrend)
# colnames(mexicoareadetrend) <- c("Year", "MexicoArea")
# 
# 
# lines(2000:2020, scale(mexicoareadetrend$MexicoArea[7:26]), lwd = 2, col = "orange")
# 
# legend("topleft", legend = c("PEAK Aug","PEAK Aug+.5*Sep","PEAK Aug+Sep","Winter Acreage" ), col = c("black","black", "black", "orange"), lty = c(1,2,3,1), lwd = c(1,1,1,2),bg = "white",cex = 0.75, bty = "n")
# legend("top", legend = c("Fall_Roost Aug", "Fall_Roost Aug+.5*Sep" ,"Fall_Roost Aug+Sep"), col = c("red","red", "red"), lty = c(1,2,3), bg = "white", cex = 0.75,bty = "n")
# 
# # make sure lat lon are in North America?
# min(peak1$Latitude)
# max(peak1$Latitude)
# min(peak1$Longitude)
# max(peak1$Longitude)
# 
# min(roost1$Latitude)
# max(roost1$Latitude)
# min(roost1$Longitude)
# max(roost1$Longitude)
# 
# #make sure lat lon in the bounds I want them : -105 35 (check)
# 
# # run correlations with summer jet and then monarchs
# 
# library(Hmisc)
# 
# rcorr(JN_PEAK_8,mexicoareadetrend$MexicoArea[7:26]) # r = 0.41 p = 0.075
# rcorr(JN_PEAK_8.5,mexicoareadetrend$MexicoArea[7:26]) # r = 0.40 p = 0.079
# rcorr(JN_PEAK_89,mexicoareadetrend$MexicoArea[7:26]) # r = 0.43 p = 0.056
# 
# rcorr(JN_ROOST_8,mexicoareadetrend$MexicoArea[9:26]) # r = 0.51 p = 0.031
# rcorr(JN_ROOST_8.5,mexicoareadetrend$MexicoArea[9:26]) # r = 0.54 p = 0.021
# rcorr(JN_ROOST_89,mexicoareadetrend$MexicoArea[9:26]) # r = 0.52 p = 0.026
# 
# rcorr(JN_PEAK_8[3:20],JN_ROOST_8) # r = 0.81 p = 0.0
# rcorr(JN_PEAK_8.5[3:20],JN_ROOST_8.5) # r = 0.87 p = 0.0
# rcorr(JN_PEAK_89[3:20],JN_ROOST_89) # r = 0.85 p = 0.0
# 
# # now detrended
# rcorr(JN_PEAK_8,mexicoareadetrend$MexicoArea[7:26]) # r = 0.37 p = 0.11
# rcorr(JN_PEAK_8.5,mexicoareadetrend$MexicoArea[7:26]) # r = 0.32 p = 0.17
# rcorr(JN_PEAK_89,mexicoareadetrend$MexicoArea[7:26]) # r = 0.38 p = 0.10
# 
# rcorr(JN_ROOST_8,mexicoareadetrend$MexicoArea[9:26]) # r = 0.48 p = 0.044
# rcorr(JN_ROOST_8.5,mexicoareadetrend$MexicoArea[9:26]) # r = 0.54 p = 0.022
# rcorr(JN_ROOST_89,mexicoareadetrend$MexicoArea[9:26]) # r = 0.53 p = 0.023
# 
# rcorr(JN_PEAK_8[3:20],JN_ROOST_8) # r = 0.82 p = 0.0
# rcorr(JN_PEAK_8.5[3:20],JN_ROOST_8.5) # r = 0.79 p = 0.0
# rcorr(JN_PEAK_89[3:20],JN_ROOST_89) # r = 0.7 p = 0.0
# 
# # Combine PEAK and ROOST
# PR_avg_8 <- as.numeric(scale(JN_PEAK_8))
# PR_avg_8[3:20] <- (scale(JN_PEAK_8[3:20])+scale(JN_ROOST_8))/2
# lines(2000:2020, PR_avg_8, lwd = 2, col = "blue")
# rcorr(PR_avg_8,mexicoareadetrend$MexicoArea[7:26]) # r = 0.43 p = 0.0576
# 
# PR_avg_8.5 <- as.numeric(scale(JN_PEAK_8.5))
# PR_avg_8.5[3:20] <- (scale(JN_PEAK_8.5[3:20])+scale(JN_ROOST_8.5))/2
# lines(2000:2020, PR_avg_8.5, lwd = 2, lty = 2, col = "blue")
# rcorr(PR_avg_8.5,mexicoareadetrend$MexicoArea[7:26]) # r = 0.43 p = 0.0603
# 
# #maybe Tukey's biweight robust mean in dplR is better? NOPE
# library(dplR)
# tbrm(x)
# PR_tbrm_8 <- as.data.frame(matrix(NA, nrow = 20,ncol = 2))
# PR_tbrm_8[,1] <- as.numeric(scale(JN_PEAK_8))
# PR_tbrm_8[3:20,2] <- as.numeric(scale(JN_ROOST_8))
# PR_tbrm_8 <- apply(PR_tbrm_8, 1, tbrm)
# lines(2000:2020, PR_tbrm_8, lwd = 2, col = "purple")
# 
# rcorr(PR_tbrm_8,mexicoareadetrend$MexicoArea[7:26]) # r = 0.43 p = 0.0596

# constrain lats lons!



###############

library(pracma)

peak <- read.csv("~/Desktop/monarch_journeynorth_PEAK.csv") #2000 has a lot of 0s
# peak$Number estimates start in 2011; roost start in 2002 but patchy.

peak1 <- peak[which((peak$Month == 8 | peak$Month == 9) & (peak$Latitude>35 & peak$Latitude<55) & peak$Longitude>-100 & -70>peak$Longitude),]
as.numeric(table(peak1$Year))
peak1 <- as.numeric(pracma::detrend(as.matrix(as.numeric(table(peak1$Year)))))
JN_PEAK_89 <- peak1


#plot(2000:2020, scale(peak1), type = "l",  xaxs="i",
#     yaxs="i", xlab = "", ylab = "", ylim = c(-3,3), lty = 3)
#abline(h = 0)

peak1 <- peak[which((peak$Month == 8) & (peak$Latitude>35 & peak$Latitude<55) & peak$Longitude>-100 & -70>peak$Longitude),]
t <- as.numeric(table(peak1$Year)) # 2008 is missing, so I added it in as 0. 
peak1 <- c(12, 23,  4,  6,  0, 23, 26, 39, 0, 12, 23,  8, 11,  3, 13, 11,  4, 16, 46, 21)
peak1 <- as.numeric(pracma::detrend(as.matrix(peak1)))

JN_PEAK_8 <- peak1
plot(2000:2020, scale(peak1), type = "l",  xaxs="i",
     yaxs="i", xlab = "", ylab = "", ylim = c(-3,3), lty = 1)
abline(h = 0)

#lines(2000:2020, scale(peak1), lty = 1,col = "black")

peak1 <- peak[which((peak$Month == 8 | (peak$Month == 9 & peak$Day <=15)) & (peak$Latitude>35 & peak$Latitude<55) & peak$Longitude>-100 & -70>peak$Longitude),]
peak1 <- as.numeric(pracma::detrend(as.matrix(as.numeric(table(peak1$Year)))))
JN_PEAK_8.5 <- peak1
#lines(2000:2020, scale(peak1), lty = 2,col = "black")


##############
roost <- read.csv("~/Desktop/monarch_journeynorth_Fall_Roost.csv")
library(dplR)
roost1 <- roost[which(roost$Month == 8),]
#roost1 <- roost[which(roost$Month == 9),]

detrend.series(as.matrix(as.numeric(table(roost1$Year))), method = "Friedman")
detrend.series(mexicoarea$MexicoArea, method = "Spline")

library(Hmisc)
rcorr(detrend.series(as.matrix(as.numeric(table(roost1$Year))), method = "Friedman"),detrend.series(mexicoarea$MexicoArea, method = "Spline")[9:26]) # r = 0.48 p = 0.044
# try cropping the MexicoArea to period of overlap before detrending 
mexicoarea <- read.table('Butterflies1994-2020.txt',header = T)
mexicoarea[,1] <- 1994:2020
mexicoarea <- mexicoarea[9:26,]
rcorr(detrend.series(as.matrix(as.numeric(table(roost1$Year))), method = "Friedman"),detrend.series(mexicoarea$MexicoArea, method = "Spline"))# 0.0422
# now with peak?


##########
roost <- read.csv("~/Desktop/monarch_journeynorth_Fall_Roost.csv")
roost1 <- roost[which(roost$Month == 8),]


plot(2002:2020, as.numeric(table(roost1$Year)), lty = 2, lwd = 2, col = "black", type = "l", xaxs="i",
     yaxs="i", xlab = "", ylab = "")

roost1 <- as.numeric(pracma::detrend(as.matrix(as.numeric(table(roost1$Year)))))

plot(2002:2020, scale(roost1), lty = 2, lwd = 2, col = "black", type = "l", ylim = c(-3, 3), xaxs="i",
     yaxs="i", xlab = "", ylab = "")
lines(2002:2020, scale(mexicoareadetrend$MexicoArea), lwd = 2, col = "black")

legend("top", legend = c("Winter Acreage Index","August Roost Index", "r = 0.48"), col = c("black","black", "white"), lty = c(1,2,1), lwd = c(2,2,1),bg = "white",cex = 0.70, bty = "n")

plot(mexicoarea[,1], mexicoarea[,2], lty = 1, lwd = 2, col = "black", type = "l", xaxs="i",
     yaxs="i", xlab = "", ylab = "")
lines(mexicoarea[,1],lm(mexicoarea[,2]~mexicoarea[,1]))
abline(lm(mexicoarea[,2]~mexicoarea[,1]))
legend("top", legend = c("Winter Acreage"), col = c("black"), lty = c(1), lwd = c(2),bg = "white",cex = 0.70, bty = "n")

#plot points in peak and roost?

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

map("worldHires", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
map.axes()
polygon(x = c(-100, -70, -70, -100),                           # X-Coordinates of polygon
        y = c(35, 35, 55, 55),                             # Y-Coordinates of polygon
        #col = adjustcolor("#ffffbf", alpha.f=0.4),
        border = "red")                                     # Color of polygon
points(roost1$Longitude,roost1$Latitude)
title("ROOST 2002-2020",cex = 1.5)
text(-110,50,"-100 to -70; 35 to 55",cex = .5, col = "red")

par(mfrow=c(5,4))
for (i in 1:18){
  map("worldHires", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
  map.axes()
  polygon(x = c(-100, -70, -70, -100),                           # X-Coordinates of polygon
          y = c(35, 35, 55, 55),                             # Y-Coordinates of polygon
          #col = adjustcolor("#ffffbf", alpha.f=0.4),
          border = "red")                                     # Color of polygon
  roost2 <- roost1[which(roost1$Year == 2001+i),]
  points(roost2$Longitude,roost2$Latitude)
  title("ROOST 2002-2020",cex = 1.5)
  text(-110,50,"-100 to -70; 35 to 55",cex = .5, col = "red")
  
}


roost1 <- roost[which((roost$Month == 8 | roost$Month == 9)& (roost$Latitude>35 & roost$Latitude<55) & roost$Longitude>-100 & -70>roost$Longitude),]
table(roost1$Year)
roost1 <- as.numeric(pracma::detrend(as.matrix(as.numeric(table(roost1$Year)))))
JN_ROOST_89 <- roost1
#lines(2002:2020, scale(roost1), lty = 3, col = "red")

roost1 <- roost[which((roost$Month == 8)& (roost$Latitude>35 & roost$Latitude<55) & roost$Longitude>-100 & -70>roost$Longitude),]


table(roost1$Year) 
roost1 <- as.numeric(pracma::detrend(as.matrix(as.numeric(table(roost1$Year)))))
JN_ROOST_8 <- roost1
lines(2002:2020, scale(roost1), lty = 2, col = "black")


roost1 <- roost[which((roost$Month == 8 | (roost$Month == 9 & roost$Day <=15))& (roost$Latitude>35 & roost$Latitude<55) & roost$Longitude>-100 & -70>roost$Longitude),]
roost1 <- as.numeric(pracma::detrend(as.matrix(as.numeric(table(roost1$Year)))))
JN_ROOST_8.5 <- roost1
#lines(2002:2020, scale(roost1), lty = 2, col = "red")


## add mexico detrended

mexicoarea <- read.table('Butterflies1994-2020.txt',header = T)
mexicoarea[,1] <- 1994:2020
mexicoareadetrend <- matrix(NA,nrow = length(1994:2020),ncol = 2)

mexicoareadetrend[,2] <- lm(mexicoarea$MexicoArea~mexicoarea$Year)$residuals
mexicoareadetrend[,1] <- 1994:2020 #1994:2012
mexicoareadetrend <- data.frame(mexicoareadetrend)
colnames(mexicoareadetrend) <- c("Year", "MexicoArea")


lines(2000:2020, scale(mexicoareadetrend$MexicoArea[7:26]), lwd = 2, col = "orange")

legend("topleft", legend = c("PEAK Aug","ROOST Aug"), col = c("black","black"), lty = c(1,2), lwd = c(1,1),bg = "white",cex = 0.70, bty = "n")
legend("top", legend = c("PEAK_ROOST_AVG Aug", "Winter Acreage"), col = c("red","orange"), lty = c(1,1),lwd = c(1,2), bg = "white", cex = 0.70,bty = "n")

# run correlations with summer jet and then monarchs

library(Hmisc)

rcorr(JN_PEAK_8,mexicoareadetrend$MexicoArea[7:26]) # r = 0.37 p = 0.1078
rcorr(JN_ROOST_8,mexicoareadetrend$MexicoArea[9:26]) # r = 0.48 p = 0.0438**


rcorr(JN_PEAK_8.5,mexicoareadetrend$MexicoArea[7:26]) # r = 0.32 p = 0.17
rcorr(JN_PEAK_89,mexicoareadetrend$MexicoArea[7:26]) # r = 0.37 p = 0.11

rcorr(JN_ROOST_8.5,mexicoareadetrend$MexicoArea[9:26]) # r = 0.52 p = 0.03
rcorr(JN_ROOST_89,mexicoareadetrend$MexicoArea[9:26]) # r = 0.52 p = 0.03

rcorr(JN_PEAK_8[3:20],JN_ROOST_8) # r = 0.84 p = 0.0
rcorr(JN_PEAK_8.5[3:20],JN_ROOST_8.5) # r = 0.81 p = 0.0
rcorr(JN_PEAK_89[3:20],JN_ROOST_89) # r = 0.76 p = 0.0

# Combine PEAK and ROOST
PR_avg_8 <- as.numeric(scale(JN_PEAK_8))
PR_avg_8[3:20] <- (scale(JN_PEAK_8[3:20])+scale(JN_ROOST_8))/2
lines(2000:2020, PR_avg_8, lwd = 1, col = "red")
rcorr(PR_avg_8,mexicoareadetrend$MexicoArea[7:26]) # r = 0.42 p = 0.06

PR_avg_8.5 <- as.numeric(scale(JN_PEAK_8.5))
PR_avg_8.5[3:20] <- (scale(JN_PEAK_8.5[3:20])+scale(JN_ROOST_8.5))/2
#lines(2000:2020, PR_avg_8.5, lwd = 2, lty = 2, col = "blue")
rcorr(PR_avg_8.5,mexicoareadetrend$MexicoArea[7:26]) # r = 0.45 p < 0.05

#maybe Tukey's biweight robust mean in dplR is better? NOPE
library(dplR)

PR_tbrm_8 <- as.data.frame(matrix(NA, nrow = 20,ncol = 2))
PR_tbrm_8[,1] <- as.numeric(scale(JN_PEAK_8))
PR_tbrm_8[3:20,2] <- as.numeric(scale(JN_ROOST_8))
PR_tbrm_8 <- apply(PR_tbrm_8, 1, tbrm)
#lines(2000:2020, PR_tbrm_8, lwd = 2, col = "purple")

rcorr(PR_tbrm_8,mexicoareadetrend$MexicoArea[7:26]) # r = 0.43 p = 0.0595


cbind(JN_PEAK_8,PR_avg_8)


###############

#read in jet position 1948-2017 
n <- read.table("NHJ_position_global_1948jan-2020sep_ncepncar.txt")
#n <- as.numeric(n)
colnames(n) <- seq(0,357.5,2.5)
jet1 <- n[2:853,]
yr <- 1948:2018

## PEAK AND ROOST FOR AUGUST JET

jet <- jet1[seq(8, 852, 12),which(seq(0,357.5,2.5)>=255 & seq(0,357.5,2.5)<=295)]
jet <- jet[which(yr<=2020 & yr>=2000),] 

jet <- lapply(jet, as.character)
jet <- lapply(jet, as.numeric)
jet <- as.data.frame(jet)
#jet <- as.data.frame(pracma::detrend(as.matrix(jet), tt = 'linear'))
jetdetrend <- pracma::detrend(as.matrix(jet))

cor_table_AugustJet <- matrix(NA,nrow = 8, ncol = length(jet))
rownames(cor_table_AugustJet) <- c('JN_PEAK_8','JN_PEAK_8.5','JN_PEAK_89','PR_AVG_8','PR_AVG_8.5','JN_ROOST_8','JN_ROOST_8.5','JN_ROOST_89')


peak <- as.data.frame(cbind(JN_PEAK_8,JN_PEAK_8.5,JN_PEAK_89,PR_avg_8,PR_avg_8.5))

for (j in 1:5){
  for(i in 1:length(jet)){
    if (rcorr(scale(peak[,j]),jetdetrend[,i], type = "spearman")$P[1,2]<0.1){
      cor_table_AugustJet[j,i] <- rcorr(peak[,j],jetdetrend[,i], type = "spearman")[[1]][1,2]
    }
  }
}

jet <- jet1[seq(8, 852, 12),which(seq(0,357.5,2.5)>=255 & seq(0,357.5,2.5)<=295)]
jet <- jet[which(yr<=2020 & yr>=2002),] 

jet <- lapply(jet, as.character)
jet <- lapply(jet, as.numeric)
jet <- as.data.frame(jet)
#jet <- as.data.frame(pracma::detrend(as.matrix(jet), tt = 'linear'))
jetdetrend <- pracma::detrend(as.matrix(jet))

roost <- as.data.frame(cbind(JN_ROOST_8,JN_ROOST_8.5,JN_ROOST_89))
for (j in 1:3){
  for(i in 1:length(jet)){
    if (rcorr(scale(roost[,j]),jetdetrend[,i], type = "spearman")$P[1,2]<0.1){
      cor_table_AugustJet[j+5,i] <- rcorr(roost[,j],jetdetrend[,i], type = "spearman")[[1]][1,2]
    }
  }
}

colnames(cor_table_AugustJet) <- colnames(jet)

