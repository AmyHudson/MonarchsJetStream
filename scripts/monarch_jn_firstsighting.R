
# plot summer indices and summer jet stream, show correlation values
###############
library(pracma)

first <- read.csv("~/Desktop/monarch_journeynorth_FirstSighting.csv") # 2000-2020 first$Number estimates start in 2012/2011;


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
# polygon(x = c(-100, -70, -70, -100),                           # X-Coordinates of polygon
#         y = c(35, 35, 55, 55),                             # Y-Coordinates of polygon
#         #col = adjustcolor("#ffffbf", alpha.f=0.4),
#         border = "red")                                     # Color of polygon
first3 <- first[which(first$Month == 3),]
points(first3$Longitude,first3$Latitude)
first3 <- first[which(first$Month == 2),]
points(first3$Longitude,first3$Latitude, col = "blue")
first3 <- first[which(first$Month == 6),]
points(first3$Longitude,first3$Latitude, col = "red")

title("First Sighting 2000-2019\nFeb-blue, March-black, June-red",cex = 1.5)
#text(-110,50,"-100 to -70; 35 to 55",cex = .5, col = "red")


first1 <- first[which(first$Month == 2& (first$Latitude>15) & first$Longitude>-105 & -90>first$Longitude),]
table(first1$Year)
as.numeric(table(first1$Year))
#c(18, 15,  2, 0, 1,  4,  5,  2,  1, 10, 0,  2, 18,  1,  6,  1, 26,  2, 18, 37)
first1 <- as.numeric(pracma::detrend(as.matrix(c(19, 15,  2, 0, 1,  4 , 5  ,3 , 1, 10 , 0, 2 ,21 , 2,  6 , 2 ,28,  2 ,22 ,46))))
#first1 <- as.numeric(pracma::detrend(as.matrix(c(18, 15,  2, 0, 1,  4,  5,  2,  1, 10, 0,  2, 18,  1,  6,  1, 26,  2, 18, 37))))
JN_first_2 <- first1



first1 <- first[which(first$Month == 3 & (first$Longitude>-105 & -90>first$Longitude)),] # & 
as.numeric(table(first1$Year))
first1 <- as.numeric(pracma::detrend(as.matrix(as.numeric(table(first1$Year)))))
JN_first_3 <- first1

#now try for each year to grab the latitude of the year's centroid? 
cent <- first[which(first$Month == 3& (first$Longitude>-105 & -90>first$Longitude)),] #
#write.csv(cent,"cent3.csv")

maxlat <- NA
minlat <- NA
meanlat <- NA

for (i in 2000:2019){
  
  cent1 <- cent[which(cent$Year == i),]
  maxlat[i-1999]<- max(cent1$Latitude)
  minlat[i-1999]<- min(cent1$Latitude)
  meanlat[i-1999]<- mean(cent1$Latitude)
  
} 

first1 <- first[which(first$Month == 6& (first$Latitude>35) & first$Longitude>-110),]

first1 <- as.numeric(pracma::detrend(as.matrix(as.numeric(table(first1$Year)))))

#first1 <- as.numeric(pracma::detrend(as.matrix(as.numeric(table(first1$Year)))))
JN_first_6 <- first1

cent <- first[which(first$Month == 6 & (first$Latitude>35 & first$Longitude>-110)),] #
#write.csv(cent,"cent3.csv")
cent <- first[which(first$Month == 6 & (first$Longitude>-110)),] #


maxlat <- NA
minlat <- NA
meanlat <- NA

for (i in 2000:2019){
  
  cent1 <- cent[which(cent$Year == i),]
  maxlat[i-1999]<- max(cent1$Latitude)
  minlat[i-1999]<- min(cent1$Latitude)
  meanlat[i-1999]<- mean(cent1$Latitude)
  
} 
plot(2000:2019, scale(maxlat), type = "l")
lines(2000:2019, scale(mexicoarea[7:26,2]), col = "orange", lwd = 2)
lines(2000:2019, -scale(meanlat),lty = 2)

maxlat <- as.numeric(pracma::detrend(as.matrix(maxlat)))

mexicoarea <- read.table('Butterflies1994-2019.txt',header = T)
mexicoarea[,1] <- 1994:2019
mexicoareadetrend <- matrix(NA,nrow = length(1994:2019),ncol = 2)
mexicoareadetrend[,2] <- lm(mexicoarea$MexicoArea~mexicoarea$Year)$residuals
mexicoareadetrend[,1] <- 1994:2019 #1994:2012
mexicoareadetrend <- data.frame(mexicoareadetrend)
colnames(mexicoareadetrend) <- c("Year", "MexicoArea")

library(Hmisc)
rcorr(JN_first_6,mexicoareadetrend$MexicoArea[7:26]) 
rcorr(maxlat,mexicoareadetrend$MexicoArea[7:26]) 
rcorr(minlat,mexicoareadetrend$MexicoArea[7:26]) 
rcorr(meanlat,mexicoareadetrend$MexicoArea[7:26]) 


rcorr(JN_first_2,mexicoareadetrend$MexicoArea[7:26])
rcorr(JN_first_2,mexicoareadetrend$MexicoArea[6:25]) 

rcorr(JN_first_3,mexicoareadetrend$MexicoArea[7:26]) 
rcorr(JN_first_3,mexicoareadetrend$MexicoArea[6:25]) 
rcorr(JN_first_2,JN_first_3) 


rcorr(meanlat,mexicoareadetrend$MexicoArea[7:26])

rcorr(JN_first_2[3:20],JN_ROOST_8)#r = 0.32 p = 0.19
rcorr(JN_first_2,meanlat)
rcorr(JN_first_6[3:20],JN_ROOST_8)

rcorr(JN_first_3[3:20],JN_ROOST_8)#r = 0.58 p = 0.01
rcorr(JN_first_3,meanlat)#r = 0.49 p = 0.03
rcorr(meanlat[3:20],JN_ROOST_8)#r = 0.32 p = 0.20


rcorr(JN_first_6,mexicoareadetrend$MexicoArea[7:26])

summary(lm(mexicoareadetrend$MexicoArea[9:26] ~ JN_first_3[3:20]+ meanlat[3:20] ))
summary(lm(JN_ROOST_8 ~ JN_first_3[3:20]+ meanlat[3:20] ))
summary(lm(JN_ROOST_8 ~ JN_first_3[3:20]))
summary(lm(JN_ROOST_8 ~ JN_first_6[3:20]))


plot(2000:2019, scale(JN_first_2), type = "l",  xaxs="i",
     yaxs="i", xlab = "", ylab = "", ylim = c(-3,3), lty = 1)
abline(h = 0)
lines(2000:2019, scale(JN_first_3), col = "black", lty = 2)
lines(2000:2019,-scale(meanlat), col = "blue", lty = 2)

lines(2000:2019, scale(JN_first_6), col = "black", lty = 3)

lines(2000:2019, scale(mexicoareadetrend$MexicoArea[7:26]), lwd = 2, col = "orange")

legend("topleft", legend = c("first Feb","first Mar","mean Lat Mar (inverse)"), col = c("black","black","blue"), lty = c(1,2,2), lwd = c(1,1,1),bg = "white",cex = 0.70, bty = "n")
legend("top", legend = c("first Jun", "Winter Acreage"), col = c("black","orange"), lty = c(3,1),lwd = c(1,2), bg = "white", cex = 0.70,bty = "n")






###############

#read in jet position 1948-2017 
n <- read.table("NHJ_position_global_1948jan-2019sep_ncepncar.txt")
#n <- as.numeric(n)
colnames(n) <- seq(0,357.5,2.5)
jet1 <- n[2:853,]
yr <- 1948:2019


jet <- jet1[seq(3, 852, 12),which(seq(0,357.5,2.5)>=255 & seq(0,357.5,2.5)<=295)]
jet <- jet[which(yr<=2019 & yr>=2000),] 

jet <- lapply(jet, as.character)
jet <- lapply(jet, as.numeric)
jet <- as.data.frame(jet)
#jet <- as.data.frame(pracma::detrend(as.matrix(jet), tt = 'linear'))
jetdetrend <- pracma::detrend(as.matrix(jet))

cor_table_AugustJet <- as.data.frame(matrix(NA,nrow = 1, ncol = length(jet)))
rownames(cor_table_AugustJet) <- c('JN_first_3')


first <- as.data.frame(cbind(JN_first_3))

  for(i in 1:length(jet)){
    if (rcorr(scale(first),jetdetrend[,i], type = "spearman")$P[1,2]<0.1){
      cor_table_AugustJet[,i] <- rcorr(first,jetdetrend[,i], type = "spearman")[[1]][1,2]
    }
  }

jet <- jet1[seq(8, 852, 12),which(seq(0,357.5,2.5)>=255 & seq(0,357.5,2.5)<=295)]
jet <- jet[which(yr<=2019 & yr>=2002),] 

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

################
# for March
cent <- first[which(first$Month == 3& (first$Longitude>-105 & -93>first$Longitude)),] #
#write.csv(cent,"cent3.csv")

maxlat <- NA
minlat <- NA
meanlat <- NA

for (i in 2000:2019){
  
  cent1 <- cent[which(cent$Year == i),]
  maxlat[i-1999]<- max(cent1$Latitude)
  minlat[i-1999]<- min(cent1$Latitude)
  meanlat[i-1999]<- mean(cent1$Latitude)
  
} 
#read in jet position 1948-2017 
n <- read.table("NHJ_position_global_1948jan-2019sep_ncepncar.txt")
#n <- as.numeric(n)
colnames(n) <- seq(0,357.5,2.5)
jet1 <- n[2:853,]
yr <- 1948:2019


jet <- jet1[seq(3, 852, 12),which(seq(0,357.5,2.5)>=(235) & seq(0,357.5,2.5)<=295)]
jet <- jet[which(yr<=2019 & yr>=2000),] 

jet <- lapply(jet, as.character)
jet <- lapply(jet, as.numeric)
jet <- as.data.frame(jet)
#jet <- as.data.frame(pracma::detrend(as.matrix(jet), tt = 'linear'))
jetdetrend <- pracma::detrend(as.matrix(jet))

cor_table<- as.data.frame(matrix(NA,nrow = 1, ncol = length(jet)))
#rownames(cor_table) <- c('JN_first_3')

for(i in 1:length(jet)){
  if (rcorr(meanlat,jetdetrend[,i], type = "spearman")$P[1,2]<0.1){
    cor_table[,i] <- rcorr(meanlat,jetdetrend[,i], type = "spearman")[[1]][1,2]
  }
}
cor_table

cor_table<- as.data.frame(matrix(NA,nrow = 1, ncol = length(jet)))
#rownames(cor_table) <- c('JN_first_3')

for(i in 1:length(jet)){
  if (rcorr(maxlat,jetdetrend[,i], type = "spearman")$P[1,2]<0.1){
    cor_table[,i] <- rcorr(maxlat,jetdetrend[,i], type = "spearman")[[1]][1,2]
  }
}
cor_table

################
#For Feb
cent <- first[which(first$Month == 2& (first$Longitude>-105 & -93>first$Longitude)),] #
#write.csv(cent,"cent3.csv")

maxlat <- NA
minlat <- NA
meanlat <- NA

for (i in 2000:2019){
  
  cent1 <- cent[which(cent$Year == i),]
  maxlat[i-1999]<- max(cent1$Latitude)
  minlat[i-1999]<- min(cent1$Latitude)
  meanlat[i-1999]<- mean(cent1$Latitude)
  
} 
#read in jet position 1948-2017 
n <- read.table("NHJ_position_global_1948jan-2019sep_ncepncar.txt")
#n <- as.numeric(n)
colnames(n) <- seq(0,357.5,2.5)
jet1 <- n[2:853,]
yr <- 1948:2019


jet <- jet1[seq(2, 852, 12),which(seq(0,357.5,2.5)>=(235) & seq(0,357.5,2.5)<=295)]
jet <- jet[which(yr<=2019 & yr>=2000),] 

jet <- lapply(jet, as.character)
jet <- lapply(jet, as.numeric)
jet <- as.data.frame(jet)
#jet <- as.data.frame(pracma::detrend(as.matrix(jet), tt = 'linear'))
jetdetrend <- pracma::detrend(as.matrix(jet))

cor_table<- as.data.frame(matrix(NA,nrow = 1, ncol = length(jet)))
#rownames(cor_table) <- c('JN_first_3')

for(i in 1:length(jet)){
  if (rcorr(meanlat,jetdetrend[,i], type = "spearman")$P[1,2]<0.1){
    cor_table[,i] <- rcorr(meanlat,jetdetrend[,i], type = "spearman")[[1]][1,2]
  }
}
cor_table

cor_table<- as.data.frame(matrix(NA,nrow = 1, ncol = length(jet)))
#rownames(cor_table) <- c('JN_first_3')

for(i in 1:length(jet)){
  if (rcorr(maxlat,jetdetrend[,i], type = "spearman")$P[1,2]<0.1){
    cor_table[,i] <- rcorr(maxlat,jetdetrend[,i], type = "spearman")[[1]][1,2]
  }
}
cor_table
