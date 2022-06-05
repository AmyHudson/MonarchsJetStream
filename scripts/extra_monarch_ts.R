# extra monarch time series and explorations with jet stream and correlation with winter acreage

################

library(pracma)

peak <- read.csv("~/Desktop/OLD/monarch_journeynorth_PEAK.csv") #2000 has a lot of 0s
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

##################################

##########################
monarch <- read.delim("data/raw/0292940-200613084148143.csv") #2020

colnames(monarch)
unique(monarch$year)
# subset for year 
monarch_dec <- monarch[which(monarch$year>=1993 &monarch$year<=2019),]
monarch_dec <- monarch_dec[which(monarch_dec$month == 12),]

monarch_nodec <- monarch[which(monarch$year>=1994 &monarch$year<=2020),]
# grab all observations that are geo referenced for DJF


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


##########################

#gbif 


gbif <- read.delim("data/raw/0292940-200613084148143.csv") #1994-2020
gbif <- gbif[which(gbif$year>=2002),]

png("figures/gbif_2002-2020.png",2,7,
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