# Fig 1: Jet stream and Monarchs
#no major difference between median/ mean visualization


# I will take the (detrended) overwintering monarch numbers highest and lowest population sizes and plot the jet stream latitudes with a loess function for each month.
library(matrixStats)
library(maps)
library(mapdata)
library(maptools)  #for shapefiles
library(scales)  #for transparency
library(ggplot2)
library(devtools)
library(dplyr)
library(stringr)
library(ggmap) #need to cite ggmap
library(matrixStats) #colMedians
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


high_to_low <- rev(mexicoareadetrend$Year[order(mexicoareadetrend[,2])])
#Loess curve fitting

lon <- as.integer(seq(235-360,295-360,2.5))
orange_purple <- c('#b35806','#b35806','#e08214','#e08214','#fdb863','#fdb863','#fee0b6','#fee0b6','#f7f7f7','#f7f7f7','#f7f7f7','#d8daeb','#d8daeb','#b2abd2','#b2abd2','#8073ac','#8073ac','#542788','#542788')


#import NHJI for each month
n <- read.table("NHJ_position_global_1948jan-2019sep_ncepncar.txt")
colnames(n) <- seq(0,357.5,2.5)
jet1 <- n[2:853,]

png("fig1_median_4n_longregions.png",8,13,
    units = "in",res = 600, pointsize=20, family= "helvetica")
#par(mfrow=c(1,2), tcl=-0.5, family="serif", mai=c(0,0,0,0),mar = c(0, 0, 0, 0))
par(mfrow=c(4,3),mar = c(0, 0, 0, 0))
# I would like to make 12 maps of the jet stream position, so 4 rows and 3 columns 


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

#is.num <- lapply(jet, as.numeric)
#jet[is.num] <- lapply(jet[is.num], round, 1)

axis(side=1,labels=F)
map("worldHires","Canada", xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F,xaxt="n",xlab="")  #plot the region of Canada
axis(side=1,labels=F)

map.axes()
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico

text(-65,15, "JAN",adj = c(1,0),cex = 1.5)
# Insert longitude of significance


#box(lwd=5, col="orange")
#rect(xleft=par("usr")[1]*1, ybottom=par("usr")[3]*1.2, 
#     xright=par("usr")[2]*1.2,ytop=par("usr")[4]*1.2, 
#     lwd=5, border="orange", xpd=TRUE)

# I should probably take the confidence interval on the non loess filtered data
# 
# l1 <- loess(as.numeric(jet[1,])~lon)#, family = "symmetric")$fitted #because now 0.5, need to rework
# l2 <- loess(as.numeric(jet[2,])~lon)#, family = "symmetric")$fitted
# l3 <- loess(as.numeric(jet[3,])~lon)#, family = "symmetric")$fitted
# l4 <- loess(as.numeric(jet[4,])~lon)#, family = "symmetric")$fitted
# #l5 <- loess(as.numeric(jet[5,])~lon)#, family = "symmetric")$fitted
# 
# #c_hi <- matrix(NA,nrow = 1,ncol = 25)
# #c_lo <- matrix(NA,nrow = 1,ncol = 25)
# #
# # for (i in 1:25){
# #   a <- mean(c(l1[i],l2[i],l3[i],l4[i]))
# #   s <- sd(c(l1[i],l2[i],l3[i],l4[i]))
# #   n <- 4
# #   error <- qnorm(0.975)*s/sqrt(n)
# #   c_hi[,i] <- a+error
# #   c_lo[,i] <- a-error
# # }
# # 
# # polygon.x <- c(seq(lon[1],lon[25], by = 2.5),rev(seq(lon[1],lon[25], by = 2.5)))
# # #polygon.y <- c(data$PROXY_LOWER[complete.cases(data$PROXY_LOWER)],rev(data$PROXY_UPPER[complete.cases(data$PROXY_UPPER)]))
# # polygon.y <- c(c_lo,rev(c_hi))
# # 
# # polygon(x=polygon.x, y=polygon.y, col=adjustcolor("#fdb863", alpha.f=0.4), border=F)
# 
# l1 <- loess(as.numeric(jet[22,])~lon)#, family = "symmetric")$fitted
# l2 <- loess(as.numeric(jet[23,])~lon)#, family = "symmetric")$fitted
# l3 <- loess(as.numeric(jet[24,])~lon)#, family = "symmetric")$fitted
# l4 <- loess(as.numeric(jet[25,])~lon)#, family = "symmetric")$fitted
#l5 <- loess(as.numeric(jet[19,])~lon)#, family = "symmetric")$fitted
# c_hi <- matrix(NA,nrow = 1,ncol = 25)
# c_lo <- matrix(NA,nrow = 1,ncol = 25)
# 
# for (i in 1:25){
#   a <- mean(c(l1[i],l2[i],l3[i],l4[i]))
#   s <- sd(c(l1[i],l2[i],l3[i],l4[i]))
#   n <- 4
#   error <- qnorm(0.975)*s/sqrt(n)
#   c_hi[,i] <- a+error
#   c_lo[,i] <- a-error
# }
# 
# polygon.x <- c(seq(lon[1],lon[25], by = 2.5),rev(seq(lon[1],lon[25], by = 2.5)))
# #polygon.y <- c(data$PROXY_LOWER[complete.cases(data$PROXY_LOWER)],rev(data$PROXY_UPPER[complete.cases(data$PROXY_UPPER)]))
# polygon.y <- c(c_lo,rev(c_hi))
# 
# polygon(x=polygon.x, y=polygon.y, col=adjustcolor("#b2abd2", alpha.f=0.4), border=F)
# #colMedians
# #bootstrap? 

# #plot low monarch years
# for (i in 1:4){
#   l <- loess(as.numeric(jet[i,])~lon)#, family = "symmetric")
#   lines(lon,l$fitted,col= adjustcolor("#e08214", alpha.f=0.4), type = "l", lwd = 1)
# }
# #plot high monarch years
# for (i in 16:19){
#   l <- loess(as.numeric(jet[i,])~lon)#, family = "symmetric")
#   lines(lon,l$fitted,col= "#8073ac", type = "l", lwd = 1)
# }
#make above transparent? 
avg_lo <- loess(colMedians(as.matrix(jet[1:4,]))~lon)#, family = "symmetric")
lines(lon,avg_lo$fitted,col= "#b35806", type = "l", lwd = 5)

avg_hi <- loess(colMedians(as.matrix(jet[22:25,]))~lon)#, family = "symmetric")
lines(lon,avg_hi$fitted,col= "#542788", type = "l", lwd = 5)

points(-100.14, 19.36, pch=20, col="black",cex=2)  

# add error bars around each *significant* longitude?


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
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
text(-65,15, "FEB",adj = c(1,0),cex = 1.5)
# Insert polygons of significant longitudes colored by sign of correlation

#Feb: -105, -102.5, -100
lon_left <- -125
lon_right <- -97.5
  
polygon.x <- c(seq(lon_left,lon_right,by = 2.5),rev(seq(lon_left,lon_right,by = 2.5)))
polygon.y <- c(colMins(as.matrix(jet[which(seq(-125,-65,2.5)<=lon_right & seq(-125,-65,2.5)>= lon_left)])),
               rev(colMaxs(as.matrix(jet[which(seq(-125,-65,2.5)<=lon_right & seq(-125,-65,2.5)>= lon_left)]))))

polygon(x=polygon.x, y=polygon.y, col=adjustcolor("#B2182B", alpha.f=0.4), border=F) #red positive correlation 

#correlation strength? 
#col <- brewer.pal(11,"RdBu")
#col
# 
l1 <- loess(as.numeric(jet[1,])~lon)#, family = "symmetric")$fitted
l2 <- loess(as.numeric(jet[2,])~lon)#, family = "symmetric")$fitted
l3 <- loess(as.numeric(jet[3,])~lon)#, family = "symmetric")$fitted
l4 <- loess(as.numeric(jet[4,])~lon)#, family = "symmetric")$fitted
#l5 <- loess(as.numeric(jet[5,])~lon)#, family = "symmetric")$fitted
# c_hi <- matrix(NA,nrow = 1,ncol = 25)
# c_lo <- matrix(NA,nrow = 1,ncol = 25)
# 
# for (i in 1:25){
#   a <- mean(c(l1[i],l2[i],l3[i],l4[i]))
#   s <- sd(c(l1[i],l2[i],l3[i],l4[i]))
#   n <- 4
#   error <- qnorm(0.975)*s/sqrt(n)
#   c_hi[,i] <- a+error
#   c_lo[,i] <- a-error
# }
# 
# polygon.x <- c(seq(lon[1],lon[25], by = 2.5),rev(seq(lon[1],lon[25], by = 2.5)))
# #polygon.y <- c(data$PROXY_LOWER[complete.cases(data$PROXY_LOWER)],rev(data$PROXY_UPPER[complete.cases(data$PROXY_UPPER)]))
# polygon.y <- c(c_lo,rev(c_hi))
# 
# polygon(x=polygon.x, y=polygon.y, col=adjustcolor("#fdb863", alpha.f=0.4), border=F)

l1 <- loess(as.numeric(jet[22,])~lon)#)#, family = "symmetric")$fitted
l2 <- loess(as.numeric(jet[23,])~lon)#)#, family = "symmetric")$fitted
l3 <- loess(as.numeric(jet[24,])~lon)#)#, family = "symmetric")$fitted
l4 <- loess(as.numeric(jet[25,])~lon)#)#, family = "symmetric")$fitted
#l5 <- loess(as.numeric(jet[19,])~lon)#)#, family = "symmetric")$fitted
# c_hi <- matrix(NA,nrow = 1,ncol = 25)
# c_lo <- matrix(NA,nrow = 1,ncol = 25)
# 
# for (i in 1:25){
#   a <- mean(c(l1[i],l2[i],l3[i],l4[i]))
#   s <- sd(c(l1[i],l2[i],l3[i],l4[i]))
#   n <- 4
#   error <- qnorm(0.975)*s/sqrt(n)
#   c_hi[,i] <- a+error
#   c_lo[,i] <- a-error
# }
# 
# polygon.x <- c(seq(lon[1],lon[25], by = 2.5),rev(seq(lon[1],lon[25], by = 2.5)))
# #polygon.y <- c(data$PROXY_LOWER[complete.cases(data$PROXY_LOWER)],rev(data$PROXY_UPPER[complete.cases(data$PROXY_UPPER)]))
# polygon.y <- c(c_lo,rev(c_hi))
# 
# polygon(x=polygon.x, y=polygon.y, col=adjustcolor("#b2abd2", alpha.f=0.4), border=F)
#colMedians
#bootstrap? 

# #plot low monarch years
# for (i in 1:4){
#   l <- loess(as.numeric(jet[i,])~lon)#, family = "symmetric")
#   lines(lon,l$fitted,col= adjustcolor("#e08214", alpha.f=0.4), type = "l", lwd = 1)
# }
# #plot high monarch years
# for (i in 16:19){
#   l <- loess(as.numeric(jet[i,])~lon)#, family = "symmetric")
#   lines(lon,l$fitted,col= "#8073ac", type = "l", lwd = 1)
# }
#make above transparent? 
avg_lo <- loess(colMedians(as.matrix(jet[1:4,]))~lon)#)#, family = "symmetric")
lines(lon,avg_lo$fitted,col= "#b35806", type = "l", lwd = 5)

avg_hi <- loess(colMedians(as.matrix(jet[22:25,]))~lon)#)#, family = "symmetric")
lines(lon,avg_hi$fitted,col= "#542788", type = "l", lwd = 5)

points(-103, 26, pch=20, col="black",cex=2)  

# plot_colors <- c("#542788","#b35806")
# text <- c("High","Low")
# #plot.new()
# par(xpd=TRUE)
# legend("top",legend = text, text.width = max(sapply(text, strwidth)),
#        col=plot_colors, lwd=5, cex=1, horiz = T)
# par(xpd=FALSE)
# # add error bars around each *significant* longitude?

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
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico

text(-65,15, "MAR",adj = c(1,0),cex = 1.5)
# Insert polygons of significant longitudes colored by sign of correlation

#Mar: -110, -107.5, -105 negative correlation 
lon_left <- -120
lon_right <- -105

polygon.x <- c(seq(lon_left,lon_right,by = 2.5),rev(seq(lon_left,lon_right,by = 2.5)))
polygon.y <- c(colMins(as.matrix(jet[which(seq(-125,-65,2.5)<=lon_right & seq(-125,-65,2.5)>= lon_left)])),
               rev(colMaxs(as.matrix(jet[which(seq(-125,-65,2.5)<=lon_right & seq(-125,-65,2.5)>= lon_left)]))))
polygon(x=polygon.x, y=polygon.y, col=adjustcolor("#2166AC", alpha.f=0.4), border=F) #red positive correlation 

#correlation strength? 

# I should probably take the confidence interval on the non loess filtered data
# 
l1 <- loess(as.numeric(jet[1,])~lon)#)#, family = "symmetric")$fitted
l2 <- loess(as.numeric(jet[2,])~lon)#)#, family = "symmetric")$fitted
l3 <- loess(as.numeric(jet[3,])~lon)#)#, family = "symmetric")$fitted
l4 <- loess(as.numeric(jet[4,])~lon)#)#, family = "symmetric")$fitted
#l5 <- loess(as.numeric(jet[5,])~lon)#, family = "symmetric")$fitted
# c_hi <- matrix(NA,nrow = 1,ncol = 25)
# c_lo <- matrix(NA,nrow = 1,ncol = 25)
# 
# for (i in 1:25){
#   a <- mean(c(l1[i],l2[i],l3[i],l4[i]))
#   s <- sd(c(l1[i],l2[i],l3[i],l4[i]))
#   n <- 4
#   error <- qnorm(0.975)*s/sqrt(n)
#   c_hi[,i] <- a+error
#   c_lo[,i] <- a-error
# }
# 
# polygon.x <- c(seq(lon[1],lon[25], by = 2.5),rev(seq(lon[1],lon[25], by = 2.5)))
# #polygon.y <- c(data$PROXY_LOWER[complete.cases(data$PROXY_LOWER)],rev(data$PROXY_UPPER[complete.cases(data$PROXY_UPPER)]))
# polygon.y <- c(c_lo,rev(c_hi))
# 
# polygon(x=polygon.x, y=polygon.y, col=adjustcolor("#fdb863", alpha.f=0.4), border=F)

l1 <- loess(as.numeric(jet[22,])~lon)#, family = "symmetric")$fitted
l2 <- loess(as.numeric(jet[23,])~lon)#, family = "symmetric")$fitted
l3 <- loess(as.numeric(jet[24,])~lon)#, family = "symmetric")$fitted
l4 <- loess(as.numeric(jet[25,])~lon)#, family = "symmetric")$fitted

# c_hi <- matrix(NA,nrow = 1,ncol = 25)
# c_lo <- matrix(NA,nrow = 1,ncol = 25)
# 
# for (i in 1:25){
#   a <- mean(c(l1[i],l2[i],l3[i],l4[i]))
#   s <- sd(c(l1[i],l2[i],l3[i],l4[i]))
#   n <- 4
#   error <- qnorm(0.975)*s/sqrt(n)
#   c_hi[,i] <- a+error
#   c_lo[,i] <- a-error
# }
# 
# polygon.x <- c(seq(lon[1],lon[25], by = 2.5),rev(seq(lon[1],lon[25], by = 2.5)))
# #polygon.y <- c(data$PROXY_LOWER[complete.cases(data$PROXY_LOWER)],rev(data$PROXY_UPPER[complete.cases(data$PROXY_UPPER)]))
# polygon.y <- c(c_lo,rev(c_hi))
# 
# polygon(x=polygon.x, y=polygon.y, col=adjustcolor("#b2abd2", alpha.f=0.4), border=F)
#colMedians
#bootstrap? 

# #plot low monarch years
# for (i in 1:4){
#   l <- loess(as.numeric(jet[i,])~lon)#, family = "symmetric")
#   lines(lon,l$fitted,col= adjustcolor("#e08214", alpha.f=0.4), type = "l", lwd = 1)
# }
# #plot high monarch years
# for (i in 16:19){
#   l <- loess(as.numeric(jet[i,])~lon)#, family = "symmetric")
#   lines(lon,l$fitted,col= "#8073ac", type = "l", lwd = 1)
# }
#make above transparent? 
avg_lo <- loess(colMedians(as.matrix(jet[1:4,]))~lon)#, family = "symmetric")
lines(lon,avg_lo$fitted,col= "#b35806", type = "l", lwd = 5)

avg_hi <- loess(colMedians(as.matrix(jet[22:25,]))~lon)#, family = "symmetric")
lines(lon,avg_hi$fitted,col= "#542788", type = "l", lwd = 5)

points(-102, 31, pch=20, col="black",cex=2)  

# add error bars around each *significant* longitude?


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
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico

text(-65,15, "APR",adj = c(1,0),cex = 1.5)

lon_left <- -80
lon_right <- -77.5

polygon.x <- c(seq(lon_left,lon_right,by = 2.5),rev(seq(lon_left,lon_right,by = 2.5)))
polygon.y <- c(colMins(as.matrix(jet[which(seq(-125,-65,2.5)<=lon_right & seq(-125,-65,2.5)>= lon_left)])),
               rev(colMaxs(as.matrix(jet[which(seq(-125,-65,2.5)<=lon_right & seq(-125,-65,2.5)>= lon_left)]))))

polygon(x=polygon.x, y=polygon.y, col=adjustcolor("#B2182B", alpha.f=0.4), border=F) 

# I should probably take the confidence interval on the non loess filtered data
# 
# l1 <- loess(as.numeric(jet[1,])~lon)#, family = "symmetric")$fitted
# l2 <- loess(as.numeric(jet[2,])~lon)#, family = "symmetric")$fitted
# l3 <- loess(as.numeric(jet[3,])~lon)#, family = "symmetric")$fitted
# l4 <- loess(as.numeric(jet[4,])~lon)#, family = "symmetric")$fitted
# #l5 <- loess(as.numeric(jet[5,])~lon)#, family = "symmetric")$fitted
# # c_hi <- matrix(NA,nrow = 1,ncol = 25)
# # c_lo <- matrix(NA,nrow = 1,ncol = 25)
# # 
# # for (i in 1:25){
# #   a <- mean(c(l1[i],l2[i],l3[i],l4[i]))
# #   s <- sd(c(l1[i],l2[i],l3[i],l4[i]))
# #   n <- 4
# #   error <- qnorm(0.975)*s/sqrt(n)
# #   c_hi[,i] <- a+error
# #   c_lo[,i] <- a-error
# # }
# # 
# # polygon.x <- c(seq(lon[1],lon[25], by = 2.5),rev(seq(lon[1],lon[25], by = 2.5)))
# # #polygon.y <- c(data$PROXY_LOWER[complete.cases(data$PROXY_LOWER)],rev(data$PROXY_UPPER[complete.cases(data$PROXY_UPPER)]))
# # polygon.y <- c(c_lo,rev(c_hi))
# # 
# # polygon(x=polygon.x, y=polygon.y, col=adjustcolor("#fdb863", alpha.f=0.4), border=F)
# 
# l1 <- loess(as.numeric(jet[22,])~lon)#, family = "symmetric")$fitted
# l2 <- loess(as.numeric(jet[23,])~lon)#, family = "symmetric")$fitted
# l3 <- loess(as.numeric(jet[24,])~lon)#, family = "symmetric")$fitted
# l4 <- loess(as.numeric(jet[25,])~lon)#, family = "symmetric")$fitted
# # c_hi <- matrix(NA,nrow = 1,ncol = 25)
# # c_lo <- matrix(NA,nrow = 1,ncol = 25)
# # 
# # for (i in 1:25){
# #   a <- mean(c(l1[i],l2[i],l3[i],l4[i]))
# #   s <- sd(c(l1[i],l2[i],l3[i],l4[i]))
# #   n <- 4
# #   error <- qnorm(0.975)*s/sqrt(n)
# #   c_hi[,i] <- a+error
# #   c_lo[,i] <- a-error
# # }
# # 
# # polygon.x <- c(seq(lon[1],lon[25], by = 2.5),rev(seq(lon[1],lon[25], by = 2.5)))
# # #polygon.y <- c(data$PROXY_LOWER[complete.cases(data$PROXY_LOWER)],rev(data$PROXY_UPPER[complete.cases(data$PROXY_UPPER)]))
# # polygon.y <- c(c_lo,rev(c_hi))
# # 
# # polygon(x=polygon.x, y=polygon.y, col=adjustcolor("#b2abd2", alpha.f=0.4), border=F)
# #colMedians
# #bootstrap? 
# 
# # #plot low monarch years
# # for (i in 1:4){
# #   l <- loess(as.numeric(jet[i,])~lon)#, family = "symmetric")
# #   lines(lon,l$fitted,col= adjustcolor("#e08214", alpha.f=0.4), type = "l", lwd = 1)
# # }
# # #plot high monarch years
# # for (i in 16:19){
# #   l <- loess(as.numeric(jet[i,])~lon)#, family = "symmetric")
# #   lines(lon,l$fitted,col= "#8073ac", type = "l", lwd = 1)
# # }
# #make above transparent? 
avg_lo <- loess(colMedians(as.matrix(jet[1:4,]))~lon)#, family = "symmetric")
lines(lon,avg_lo$fitted,col= "#b35806", type = "l", lwd = 5)

avg_hi <- loess(colMedians(as.matrix(jet[22:25,]))~lon)#, family = "symmetric")
lines(lon,avg_hi$fitted,col= "#542788", type = "l", lwd = 5)

points(-95, 35, pch=20, col="black",cex=2) 
# add error bars around each *significant* longitude?

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
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
text(-65,15, "MAY",adj = c(1,0),cex = 1.5)

# I should probably take the confidence interval on the non loess filtered data
# 
# l1 <- loess(as.numeric(jet[1,])~lon)#, family = "symmetric")$fitted
# l2 <- loess(as.numeric(jet[2,])~lon)#, family = "symmetric")$fitted
# l3 <- loess(as.numeric(jet[3,])~lon)#, family = "symmetric")$fitted
# l4 <- loess(as.numeric(jet[4,])~lon)#, family = "symmetric")$fitted
# #l5 <- loess(as.numeric(jet[5,])~lon)#, family = "symmetric")$fitted
# # c_hi <- matrix(NA,nrow = 1,ncol = 25)
# # c_lo <- matrix(NA,nrow = 1,ncol = 25)
# # 
# # for (i in 1:25){
# #   a <- mean(c(l1[i],l2[i],l3[i],l4[i]))
# #   s <- sd(c(l1[i],l2[i],l3[i],l4[i]))
# #   n <- 4
# #   error <- qnorm(0.975)*s/sqrt(n)
# #   c_hi[,i] <- a+error
# #   c_lo[,i] <- a-error
# # }
# # 
# # polygon.x <- c(seq(lon[1],lon[25], by = 2.5),rev(seq(lon[1],lon[25], by = 2.5)))
# # #polygon.y <- c(data$PROXY_LOWER[complete.cases(data$PROXY_LOWER)],rev(data$PROXY_UPPER[complete.cases(data$PROXY_UPPER)]))
# # polygon.y <- c(c_lo,rev(c_hi))
# # 
# # polygon(x=polygon.x, y=polygon.y, col=adjustcolor("#fdb863", alpha.f=0.4), border=F)
# 
# l1 <- loess(as.numeric(jet[22,])~lon)#, family = "symmetric")$fitted
# l2 <- loess(as.numeric(jet[23,])~lon)#, family = "symmetric")$fitted
# l3 <- loess(as.numeric(jet[24,])~lon)#, family = "symmetric")$fitted
# l4 <- loess(as.numeric(jet[25,])~lon)#, family = "symmetric")$fitted
# # c_hi <- matrix(NA,nrow = 1,ncol = 25)
# # c_lo <- matrix(NA,nrow = 1,ncol = 25)
# # 
# # for (i in 1:25){
# #   a <- mean(c(l1[i],l2[i],l3[i],l4[i]))
# #   s <- sd(c(l1[i],l2[i],l3[i],l4[i]))
# #   n <- 4
# #   error <- qnorm(0.975)*s/sqrt(n)
# #   c_hi[,i] <- a+error
# #   c_lo[,i] <- a-error
# # }
# # 
# # polygon.x <- c(seq(lon[1],lon[25], by = 2.5),rev(seq(lon[1],lon[25], by = 2.5)))
# # #polygon.y <- c(data$PROXY_LOWER[complete.cases(data$PROXY_LOWER)],rev(data$PROXY_UPPER[complete.cases(data$PROXY_UPPER)]))
# # polygon.y <- c(c_lo,rev(c_hi))
# # 
# # polygon(x=polygon.x, y=polygon.y, col=adjustcolor("#b2abd2", alpha.f=0.4), border=F)
# #colMedians
# #bootstrap? 
# 
# # #plot low monarch years
# # for (i in 1:4){
# #   l <- loess(as.numeric(jet[i,])~lon)#, family = "symmetric")
# #   lines(lon,l$fitted,col= adjustcolor("#e08214", alpha.f=0.4), type = "l", lwd = 1)
# # }
# # #plot high monarch years
# # for (i in 16:19){
# #   l <- loess(as.numeric(jet[i,])~lon)#, family = "symmetric")
# #   lines(lon,l$fitted,col= "#8073ac", type = "l", lwd = 1)
# # }
# #make above transparent? 
avg_lo <- loess(colMedians(as.matrix(jet[1:4,]))~lon)#, family = "symmetric")
lines(lon,avg_lo$fitted,col= "#b35806", type = "l", lwd = 5)

avg_hi <- loess(colMedians(as.matrix(jet[22:25,]))~lon)#, family = "symmetric")
lines(lon,avg_hi$fitted,col= "#542788", type = "l", lwd = 5)


points(-90, 40, pch=20, col="black",cex=2)  
# add error bars around each *significant* longitude?

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
#title("August") this is mid line

map("worldHires","USA", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)  #plot the region of US
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
text(-65,15, "JUN",adj = c(1,0),cex = 1.5)
# Insert polygons of significant longitudes colored by sign of correlation

#Jun: -97.5, -95, -92.5 positive correlation 
lon_left <- -97.5
lon_right <- -75

polygon.x <- c(seq(lon_left,lon_right,by = 2.5),rev(seq(lon_left,lon_right,by = 2.5)))
polygon.y <- c(colMins(as.matrix(jet[which(seq(-125,-65,2.5)<=lon_right & seq(-125,-65,2.5)>= lon_left)])),
               rev(colMaxs(as.matrix(jet[which(seq(-125,-65,2.5)<=lon_right & seq(-125,-65,2.5)>= lon_left)]))))
polygon(x=polygon.x, y=polygon.y, col=adjustcolor("#B2182B", alpha.f=0.4), border=F)  

#correlation strength? 
col <- brewer.pal(11,"RdBu")
col # red "#B2182B" blue "#2166AC"

# I should probably take the confidence interval on the non loess filtered data
# 
# l1 <- loess(as.numeric(jet[1,])~lon)#, family = "symmetric")$fitted
# l2 <- loess(as.numeric(jet[2,])~lon)#, family = "symmetric")$fitted
# l3 <- loess(as.numeric(jet[3,])~lon)#, family = "symmetric")$fitted
# l4 <- loess(as.numeric(jet[4,])~lon)#, family = "symmetric")$fitted
#l5 <- loess(as.numeric(jet[5,])~lon)#, family = "symmetric")$fitted
# c_hi <- matrix(NA,nrow = 1,ncol = 25)
# c_lo <- matrix(NA,nrow = 1,ncol = 25)
# 
# for (i in 1:25){
#   a <- mean(c(l1[i],l2[i],l3[i],l4[i]))
#   s <- sd(c(l1[i],l2[i],l3[i],l4[i]))
#   n <- 4
#   error <- qnorm(0.975)*s/sqrt(n)
#   c_hi[,i] <- a+error
#   c_lo[,i] <- a-error
# }
# 
# polygon.x <- c(seq(lon[1],lon[25], by = 2.5),rev(seq(lon[1],lon[25], by = 2.5)))
# #polygon.y <- c(data$PROXY_LOWER[complete.cases(data$PROXY_LOWER)],rev(data$PROXY_UPPER[complete.cases(data$PROXY_UPPER)]))
# polygon.y <- c(c_lo,rev(c_hi))
# 
# polygon(x=polygon.x, y=polygon.y, col=adjustcolor("#fdb863", alpha.f=0.4), border=F)

# l1 <- loess(as.numeric(jet[22,])~lon)#, family = "symmetric")$fitted
# l2 <- loess(as.numeric(jet[23,])~lon)#, family = "symmetric")$fitted
# l3 <- loess(as.numeric(jet[24,])~lon)#, family = "symmetric")$fitted
# l4 <- loess(as.numeric(jet[25,])~lon)#, family = "symmetric")$fitted
# c_hi <- matrix(NA,nrow = 1,ncol = 25)
# c_lo <- matrix(NA,nrow = 1,ncol = 25)
# 
# for (i in 1:25){
#   a <- mean(c(l1[i],l2[i],l3[i],l4[i]))
#   s <- sd(c(l1[i],l2[i],l3[i],l4[i]))
#   n <- 4
#   error <- qnorm(0.975)*s/sqrt(n)
#   c_hi[,i] <- a+error
#   c_lo[,i] <- a-error
# }
# 
# polygon.x <- c(seq(lon[1],lon[25], by = 2.5),rev(seq(lon[1],lon[25], by = 2.5)))
# #polygon.y <- c(data$PROXY_LOWER[complete.cases(data$PROXY_LOWER)],rev(data$PROXY_UPPER[complete.cases(data$PROXY_UPPER)]))
# polygon.y <- c(c_lo,rev(c_hi))
# 
# polygon(x=polygon.x, y=polygon.y, col=adjustcolor("#b2abd2", alpha.f=0.4), border=F)
#colMedians
#bootstrap? 

# #plot low monarch years
# for (i in 1:4){
#   l <- loess(as.numeric(jet[i,])~lon)#, family = "symmetric")
#   lines(lon,l$fitted,col= adjustcolor("#e08214", alpha.f=0.4), type = "l", lwd = 1)
# }
# #plot high monarch years
# for (i in 16:19){
#   l <- loess(as.numeric(jet[i,])~lon)#, family = "symmetric")
#   lines(lon,l$fitted,col= "#8073ac", type = "l", lwd = 1)
# }
#make above transparent? 
avg_lo <- loess(colMedians(as.matrix(jet[1:4,]))~lon)#, family = "symmetric")
lines(lon,avg_lo$fitted,col= "#b35806", type = "l", lwd = 5)

avg_hi <- loess(colMedians(as.matrix(jet[22:25,]))~lon)#, family = "symmetric")
lines(lon,avg_hi$fitted,col= "#542788", type = "l", lwd = 5)


points(-90, 50, pch=20, col="black",cex=2)  

# add error bars around each *significant* longitude?

############################################################################
## July
#jet <- read.table("NHJ_USGS/position_jul_20thcentury.txt") #rownames(jet) #1871-2012 
#jet <- jet[124:142,which(seq(0,358,2)>=235 & seq(0,358,2)<=295)] #subset 1994-2012, subset -125 to -65 or 235 to 295
jet <- jet1[seq(7, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
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
text(-65,15, "JUL",adj = c(1,0),cex = 1.5)

lon_left <- -125
lon_right <- -120

polygon.x <- c(seq(lon_left,lon_right,by = 2.5),rev(seq(lon_left,lon_right,by = 2.5)))
polygon.y <- c(colMins(as.matrix(jet[which(seq(-125,-65,2.5)<=lon_right & seq(-125,-65,2.5)>= lon_left)])),
               rev(colMaxs(as.matrix(jet[which(seq(-125,-65,2.5)<=lon_right & seq(-125,-65,2.5)>= lon_left)]))))

polygon(x=polygon.x, y=polygon.y, col=adjustcolor("#2166AC", alpha.f=0.4), border=F) 

#Jul: -122.5 neg correlation 
#lon_left <- -122.5
#lon_right <- -120
#
#polygon.x <- c(seq(lon_left,lon_right,by = 2.5),rev(seq(lon_left,lon_right,by = 2.5)))
#polygon.y <- c(colMins(as.matrix(jet[which(seq(-125,-65,2.5)<=lon_right & seq(-125,-65,2.5)>= lon_left)])),
#               rev(colMaxs(as.matrix(jet[which(seq(-125,-65,2.5)<=lon_right & seq(-125,-65,2.5)>= lon_left)]))))
#
#polygon(x=polygon.x, y=polygon.y, col=adjustcolor("#2166AC", alpha.f=0.4), border=F) 

#correlation strength? 
#col <- brewer.pal(11,"RdBu")
#col # red "#B2182B" blue "#2166AC"

# I should probably take the confidence interval on the non loess filtered data
# 
# l1 <- loess(as.numeric(jet[1,])~lon)#, family = "symmetric")$fitted
# l2 <- loess(as.numeric(jet[2,])~lon)#, family = "symmetric")$fitted
# l3 <- loess(as.numeric(jet[3,])~lon)#, family = "symmetric")$fitted
# l4 <- loess(as.numeric(jet[4,])~lon)#, family = "symmetric")$fitted
# #l5 <- loess(as.numeric(jet[5,])~lon)#, family = "symmetric")$fitted
# c_hi <- matrix(NA,nrow = 1,ncol = 25)
# c_lo <- matrix(NA,nrow = 1,ncol = 25)
# 
# for (i in 1:25){
#   a <- mean(c(l1[i],l2[i],l3[i],l4[i]))
#   s <- sd(c(l1[i],l2[i],l3[i],l4[i]))
#   n <- 4
#   error <- qnorm(0.975)*s/sqrt(n)
#   c_hi[,i] <- a+error
#   c_lo[,i] <- a-error
# }
# 
# polygon.x <- c(seq(lon[1],lon[25], by = 2.5),rev(seq(lon[1],lon[25], by = 2.5)))
# #polygon.y <- c(data$PROXY_LOWER[complete.cases(data$PROXY_LOWER)],rev(data$PROXY_UPPER[complete.cases(data$PROXY_UPPER)]))
# polygon.y <- c(c_lo,rev(c_hi))
# 
# polygon(x=polygon.x, y=polygon.y, col=adjustcolor("#fdb863", alpha.f=0.4), border=F)
# 
# l1 <- loess(as.numeric(jet[22,])~lon)#, family = "symmetric")$fitted
# l2 <- loess(as.numeric(jet[23,])~lon)#, family = "symmetric")$fitted
# l3 <- loess(as.numeric(jet[24,])~lon)#, family = "symmetric")$fitted
# l4 <- loess(as.numeric(jet[25,])~lon)#, family = "symmetric")$fitted
# c_hi <- matrix(NA,nrow = 1,ncol = 25)
# c_lo <- matrix(NA,nrow = 1,ncol = 25)
# 
# for (i in 1:25){
#   a <- mean(c(l1[i],l2[i],l3[i],l4[i]))
#   s <- sd(c(l1[i],l2[i],l3[i],l4[i]))
#   n <- 4
#   error <- qnorm(0.975)*s/sqrt(n)
#   c_hi[,i] <- a+error
#   c_lo[,i] <- a-error
# }
# 
# polygon.x <- c(seq(lon[1],lon[25], by = 2.5),rev(seq(lon[1],lon[25], by = 2.5)))
# #polygon.y <- c(data$PROXY_LOWER[complete.cases(data$PROXY_LOWER)],rev(data$PROXY_UPPER[complete.cases(data$PROXY_UPPER)]))
# polygon.y <- c(c_lo,rev(c_hi))
# 
# polygon(x=polygon.x, y=polygon.y, col=adjustcolor("#b2abd2", alpha.f=0.4), border=F)
# #colMedians
# #bootstrap? 
# 
# # #plot low monarch years
# # for (i in 1:4){
# #   l <- loess(as.numeric(jet[i,])~lon)#, family = "symmetric")
# #   lines(lon,l$fitted,col= adjustcolor("#e08214", alpha.f=0.4), type = "l", lwd = 1)
# # }
# # #plot high monarch years
# # for (i in 16:19){
# #   l <- loess(as.numeric(jet[i,])~lon)#, family = "symmetric")
# #   lines(lon,l$fitted,col= "#8073ac", type = "l", lwd = 1)
# # }
# #make above transparent? 
avg_lo <- loess(colMedians(as.matrix(jet[1:4,]))~lon)#, family = "symmetric")
lines(lon,avg_lo$fitted,col= "#b35806", type = "l", lwd = 5)

avg_hi <- loess(colMedians(as.matrix(jet[22:25,]))~lon)#, family = "symmetric")
lines(lon,avg_hi$fitted,col= "#542788", type = "l", lwd = 5)


points(-92, 45, pch=20, col="black",cex=2)  

# add error bars around each *significant* longitude?

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
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
text(-65,15, "AUG",adj = c(1,0),cex = 1.5)

#Aug: -107.5, -87.5 -85, -75, -72.5 positive correlation 
lon_left <- -105
lon_right <- -70

polygon.x <- c(seq(lon_left,lon_right,by = 2.5),rev(seq(lon_left,lon_right,by = 2.5)))
polygon.y <- c(colMins(as.matrix(jet[which(seq(-125,-65,2.5)<=lon_right & seq(-125,-65,2.5)>= lon_left)])),
               rev(colMaxs(as.matrix(jet[which(seq(-125,-65,2.5)<=lon_right & seq(-125,-65,2.5)>= lon_left)]))))

polygon(x=polygon.x, y=polygon.y, col=adjustcolor("#B2182B", alpha.f=0.4), border=F) 

#correlation strength? 
col <- brewer.pal(11,"RdBu")
col # red "#B2182B" blue "#2166AC"

# I should probably take the confidence interval on the non loess filtered data
# 
# l1 <- loess(as.numeric(jet[1,])~lon)#, family = "symmetric")$fitted
# l2 <- loess(as.numeric(jet[2,])~lon)#, family = "symmetric")$fitted
# l3 <- loess(as.numeric(jet[3,])~lon)#, family = "symmetric")$fitted
# l4 <- loess(as.numeric(jet[4,])~lon)#, family = "symmetric")$fitted
# #l5 <- loess(as.numeric(jet[5,])~lon)#, family = "symmetric")$fitted
# c_hi <- matrix(NA,nrow = 1,ncol = 25)
# c_lo <- matrix(NA,nrow = 1,ncol = 25)
# 
# for (i in 1:25){
#   a <- mean(c(l1[i],l2[i],l3[i],l4[i]))
#   s <- sd(c(l1[i],l2[i],l3[i],l4[i]))
#   n <- 4
#   error <- qnorm(0.975)*s/sqrt(n)
#   c_hi[,i] <- a+error
#   c_lo[,i] <- a-error
# }
# 
# polygon.x <- c(seq(lon[1],lon[25], by = 2.5),rev(seq(lon[1],lon[25], by = 2.5)))
# #polygon.y <- c(data$PROXY_LOWER[complete.cases(data$PROXY_LOWER)],rev(data$PROXY_UPPER[complete.cases(data$PROXY_UPPER)]))
# polygon.y <- c(c_lo,rev(c_hi))
# 
# polygon(x=polygon.x, y=polygon.y, col=adjustcolor("#fdb863", alpha.f=0.4), border=F)
# 
# l1 <- loess(as.numeric(jet[22,])~lon)#, family = "symmetric")$fitted
# l2 <- loess(as.numeric(jet[23,])~lon)#, family = "symmetric")$fitted
# l3 <- loess(as.numeric(jet[24,])~lon)#, family = "symmetric")$fitted
# l4 <- loess(as.numeric(jet[25,])~lon)#, family = "symmetric")$fitted
# c_hi <- matrix(NA,nrow = 1,ncol = 25)
# c_lo <- matrix(NA,nrow = 1,ncol = 25)
# 
# for (i in 1:25){
#   a <- mean(c(l1[i],l2[i],l3[i],l4[i]))
#   s <- sd(c(l1[i],l2[i],l3[i],l4[i]))
#   n <- 4
#   error <- qnorm(0.975)*s/sqrt(n)
#   c_hi[,i] <- a+error
#   c_lo[,i] <- a-error
# }
# 
# polygon.x <- c(seq(lon[1],lon[25], by = 2.5),rev(seq(lon[1],lon[25], by = 2.5)))
# #polygon.y <- c(data$PROXY_LOWER[complete.cases(data$PROXY_LOWER)],rev(data$PROXY_UPPER[complete.cases(data$PROXY_UPPER)]))
# polygon.y <- c(c_lo,rev(c_hi))
# 
# polygon(x=polygon.x, y=polygon.y, col=adjustcolor("#b2abd2", alpha.f=0.4), border=F)
# #colMedians
# #bootstrap? 
# 
# # #plot low monarch years
# # for (i in 1:4){
# #   l <- loess(as.numeric(jet[i,])~lon)#, family = "symmetric")
# #   lines(lon,l$fitted,col= adjustcolor("#e08214", alpha.f=0.4), type = "l", lwd = 1)
# # }
# # #plot high monarch years
# # for (i in 16:19){
# #   l <- loess(as.numeric(jet[i,])~lon)#, family = "symmetric")
# #   lines(lon,l$fitted,col= "#8073ac", type = "l", lwd = 1)
# # }
# #make above transparent? 
avg_lo <- loess(colMedians(as.matrix(jet[1:4,]))~lon)#, family = "symmetric")
lines(lon,avg_lo$fitted,col= "#b35806", type = "l", lwd = 5)

avg_hi <- loess(colMedians(as.matrix(jet[22:25,]))~lon)#, family = "symmetric")
lines(lon,avg_hi$fitted,col= "#542788", type = "l", lwd = 5)


points(-80, 45, pch=20, col="black",cex=2)  

# add error bars around each *significant* longitude?

############################################################################
## September

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
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
text(-65,15, "SEP",adj = c(1,0),cex = 1.5)
#text(-65,15, "SEP_C",adj = c(1,0),cex = 1.5)

#Sep: neg correlation

lon_left <- -107.5
lon_right <- -95

polygon.x <- c(seq(lon_left,lon_right,by = 2.5),rev(seq(lon_left,lon_right,by = 2.5)))
polygon.y <- c(colMins(as.matrix(jet[which(seq(-125,-65,2.5)<=lon_right & seq(-125,-65,2.5)>= lon_left)])),
               rev(colMaxs(as.matrix(jet[which(seq(-125,-65,2.5)<=lon_right & seq(-125,-65,2.5)>= lon_left)]))))

polygon(x=polygon.x, y=polygon.y, col=adjustcolor("#2166AC", alpha.f=0.4), border=F) 

lon_left <- -120
lon_right <- -115

polygon.x <- c(seq(lon_left,lon_right,by = 2.5),rev(seq(lon_left,lon_right,by = 2.5)))
polygon.y <- c(colMins(as.matrix(jet[which(seq(-125,-65,2.5)<=lon_right & seq(-125,-65,2.5)>= lon_left)])),
               rev(colMaxs(as.matrix(jet[which(seq(-125,-65,2.5)<=lon_right & seq(-125,-65,2.5)>= lon_left)]))))

polygon(x=polygon.x, y=polygon.y, col=adjustcolor("#2166AC", alpha.f=0.4), border=F) 
#correlation strength? 
col <- brewer.pal(11,"RdBu")
col # red "#B2182B" blue "#2166AC"

# I should probably take the confidence interval on the non loess filtered data
# 
# l1 <- loess(as.numeric(jet[1,])~lon)#, family = "symmetric")$fitted
# l2 <- loess(as.numeric(jet[2,])~lon)#, family = "symmetric")$fitted
# l3 <- loess(as.numeric(jet[3,])~lon)#, family = "symmetric")$fitted
# l4 <- loess(as.numeric(jet[4,])~lon)#, family = "symmetric")$fitted
# #l5 <- loess(as.numeric(jet[5,])~lon)#, family = "symmetric")$fitted
# c_hi <- matrix(NA,nrow = 1,ncol = 25)
# c_lo <- matrix(NA,nrow = 1,ncol = 25)
# 
# for (i in 1:25){
#   a <- mean(c(l1[i],l2[i],l3[i],l4[i]))
#   s <- sd(c(l1[i],l2[i],l3[i],l4[i]))
#   n <- 4
#   error <- qnorm(0.975)*s/sqrt(n)
#   c_hi[,i] <- a+error
#   c_lo[,i] <- a-error
# }
# 
# polygon.x <- c(seq(lon[1],lon[25], by = 2.5),rev(seq(lon[1],lon[25], by = 2.5)))
# #polygon.y <- c(data$PROXY_LOWER[complete.cases(data$PROXY_LOWER)],rev(data$PROXY_UPPER[complete.cases(data$PROXY_UPPER)]))
# polygon.y <- c(c_lo,rev(c_hi))
# 
# polygon(x=polygon.x, y=polygon.y, col=adjustcolor("#fdb863", alpha.f=0.4), border=F)
# 
# l1 <- loess(as.numeric(jet[22,])~lon)#, family = "symmetric")$fitted
# l2 <- loess(as.numeric(jet[23,])~lon)#, family = "symmetric")$fitted
# l3 <- loess(as.numeric(jet[24,])~lon)#, family = "symmetric")$fitted
# l4 <- loess(as.numeric(jet[25,])~lon)#, family = "symmetric")$fitted
# c_hi <- matrix(NA,nrow = 1,ncol = 25)
# c_lo <- matrix(NA,nrow = 1,ncol = 25)
# 
# for (i in 1:25){
#   a <- mean(c(l1[i],l2[i],l3[i],l4[i]))
#   s <- sd(c(l1[i],l2[i],l3[i],l4[i]))
#   n <- 4
#   error <- qnorm(0.975)*s/sqrt(n)
#   c_hi[,i] <- a+error
#   c_lo[,i] <- a-error
# }
# 
# polygon.x <- c(seq(lon[1],lon[25], by = 2.5),rev(seq(lon[1],lon[25], by = 2.5)))
# #polygon.y <- c(data$PROXY_LOWER[complete.cases(data$PROXY_LOWER)],rev(data$PROXY_UPPER[complete.cases(data$PROXY_UPPER)]))
# polygon.y <- c(c_lo,rev(c_hi))
# 
# polygon(x=polygon.x, y=polygon.y, col=adjustcolor("#b2abd2", alpha.f=0.4), border=F)
# #colMedians
# #bootstrap? 
# 
# # #plot low monarch years
# # for (i in 1:4){
# #   l <- loess(as.numeric(jet[i,])~lon)#, family = "symmetric")
# #   lines(lon,l$fitted,col= adjustcolor("#e08214", alpha.f=0.4), type = "l", lwd = 1)
# # }
# # #plot high monarch years
# # for (i in 16:19){
# #   l <- loess(as.numeric(jet[i,])~lon)#, family = "symmetric")
# #   lines(lon,l$fitted,col= "#8073ac", type = "l", lwd = 1)
# # }
# #make above transparent? 
avg_lo <- loess(colMedians(as.matrix(jet[1:4,]))~lon)#, family = "symmetric")
lines(lon,avg_lo$fitted,col= "#b35806", type = "l", lwd = 5)

avg_hi <- loess(colMedians(as.matrix(jet[22:25,]))~lon)#, family = "symmetric")
lines(lon,avg_hi$fitted,col= "#542788", type = "l", lwd = 5)

points(-96, 39, pch=20, col="black",cex=2)
# add error bars around each *significant* longitude?
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
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
text(-65,15, "OCT",adj = c(1,0),cex = 1.5)

#Oct: -122.5 pos correlation -115 -112.5 pos
lon_left <- -122.5
lon_right <- -112.5

polygon.x <- c(seq(lon_left,lon_right,by = 2.5),rev(seq(lon_left,lon_right,by = 2.5)))
polygon.y <- c(colMins(as.matrix(jet[which(seq(-125,-65,2.5)<=lon_right & seq(-125,-65,2.5)>= lon_left)])),
               rev(colMaxs(as.matrix(jet[which(seq(-125,-65,2.5)<=lon_right & seq(-125,-65,2.5)>= lon_left)]))))

polygon(x=polygon.x, y=polygon.y, col=adjustcolor("#B2182B", alpha.f=0.4), border=F) 

lon_left <- -67.5
lon_right <- -65

polygon.x <- c(seq(lon_left,lon_right,by = 2.5),rev(seq(lon_left,lon_right,by = 2.5)))
polygon.y <- c(colMins(as.matrix(jet[which(seq(-125,-65,2.5)<=lon_right & seq(-125,-65,2.5)>= lon_left)])),
               rev(colMaxs(as.matrix(jet[which(seq(-125,-65,2.5)<=lon_right & seq(-125,-65,2.5)>= lon_left)]))))

polygon(x=polygon.x, y=polygon.y, col=adjustcolor("#B2182B", alpha.f=0.4), border=F) 

# I should probably take the confidence interval on the non loess filtered data
# # 
# l1 <- loess(as.numeric(jet[1,])~lon)#, family = "symmetric")$fitted
# l2 <- loess(as.numeric(jet[2,])~lon)#, family = "symmetric")$fitted
# l3 <- loess(as.numeric(jet[3,])~lon)#, family = "symmetric")$fitted
# l4 <- loess(as.numeric(jet[4,])~lon)#, family = "symmetric")$fitted
# #l5 <- loess(as.numeric(jet[5,])~lon)#, family = "symmetric")$fitted
# c_hi <- matrix(NA,nrow = 1,ncol = 25)
# c_lo <- matrix(NA,nrow = 1,ncol = 25)
# 
# for (i in 1:25){
#   a <- mean(c(l1[i],l2[i],l3[i],l4[i]))
#   s <- sd(c(l1[i],l2[i],l3[i],l4[i]))
#   n <- 4
#   error <- qnorm(0.975)*s/sqrt(n)
#   c_hi[,i] <- a+error
#   c_lo[,i] <- a-error
# }
# 
# polygon.x <- c(seq(lon[1],lon[25], by = 2.5),rev(seq(lon[1],lon[25], by = 2.5)))
# #polygon.y <- c(data$PROXY_LOWER[complete.cases(data$PROXY_LOWER)],rev(data$PROXY_UPPER[complete.cases(data$PROXY_UPPER)]))
# polygon.y <- c(c_lo,rev(c_hi))
# 
# polygon(x=polygon.x, y=polygon.y, col=adjustcolor("#fdb863", alpha.f=0.4), border=F)
# 
# l1 <- loess(as.numeric(jet[22,])~lon)#, family = "symmetric")$fitted
# l2 <- loess(as.numeric(jet[23,])~lon)#, family = "symmetric")$fitted
# l3 <- loess(as.numeric(jet[24,])~lon)#, family = "symmetric")$fitted
# l4 <- loess(as.numeric(jet[25,])~lon)#, family = "symmetric")$fitted
# c_hi <- matrix(NA,nrow = 1,ncol = 25)
# c_lo <- matrix(NA,nrow = 1,ncol = 25)
# 
# for (i in 1:25){
#   a <- mean(c(l1[i],l2[i],l3[i],l4[i]))
#   s <- sd(c(l1[i],l2[i],l3[i],l4[i]))
#   n <- 4
#   error <- qnorm(0.975)*s/sqrt(n)
#   c_hi[,i] <- a+error
#   c_lo[,i] <- a-error
# }
# 
# polygon.x <- c(seq(lon[1],lon[25], by = 2.5),rev(seq(lon[1],lon[25], by = 2.5)))
# #polygon.y <- c(data$PROXY_LOWER[complete.cases(data$PROXY_LOWER)],rev(data$PROXY_UPPER[complete.cases(data$PROXY_UPPER)]))
# polygon.y <- c(c_lo,rev(c_hi))
# 
# polygon(x=polygon.x, y=polygon.y, col=adjustcolor("#b2abd2", alpha.f=0.4), border=F)
# #colMedians
# #bootstrap? 
# 
# # #plot low monarch years
# # for (i in 1:4){
# #   l <- loess(as.numeric(jet[i,])~lon)#, family = "symmetric")
# #   lines(lon,l$fitted,col= adjustcolor("#e08214", alpha.f=0.4), type = "l", lwd = 1)
# # }
# # #plot high monarch years
# # for (i in 16:19){
# #   l <- loess(as.numeric(jet[i,])~lon)#, family = "symmetric")
# #   lines(lon,l$fitted,col= "#8073ac", type = "l", lwd = 1)
# # }
# #make above transparent? 
avg_lo <- loess(colMedians(as.matrix(jet[1:4,]))~lon)#, family = "symmetric")
lines(lon,avg_lo$fitted,col= "#b35806", type = "l", lwd = 5)

avg_hi <- loess(colMedians(as.matrix(jet[22:25,]))~lon)#, family = "symmetric")
lines(lon,avg_hi$fitted,col= "#542788", type = "l", lwd = 5)

points(-102, 31, pch=20, col="black",cex=2)  

# add error bars around each *significant* longitude?

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
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
text(-65,15, "NOV",adj = c(1,0),cex = 1.5)

# I should probably take the confidence interval on the non loess filtered data
# 
# l1 <- loess(as.numeric(jet[1,])~lon)#, family = "symmetric")$fitted
# l2 <- loess(as.numeric(jet[2,])~lon)#, family = "symmetric")$fitted
# l3 <- loess(as.numeric(jet[3,])~lon)#, family = "symmetric")$fitted
# l4 <- loess(as.numeric(jet[4,])~lon)#, family = "symmetric")$fitted
# #l5 <- loess(as.numeric(jet[5,])~lon)#, family = "symmetric")$fitted
# c_hi <- matrix(NA,nrow = 1,ncol = 25)
# c_lo <- matrix(NA,nrow = 1,ncol = 25)
# 
# for (i in 1:25){
#   a <- mean(c(l1[i],l2[i],l3[i],l4[i]))
#   s <- sd(c(l1[i],l2[i],l3[i],l4[i]))
#   n <- 4
#   error <- qnorm(0.975)*s/sqrt(n)
#   c_hi[,i] <- a+error
#   c_lo[,i] <- a-error
# }
# 
# polygon.x <- c(seq(lon[1],lon[25], by = 2.5),rev(seq(lon[1],lon[25], by = 2.5)))
# #polygon.y <- c(data$PROXY_LOWER[complete.cases(data$PROXY_LOWER)],rev(data$PROXY_UPPER[complete.cases(data$PROXY_UPPER)]))
# polygon.y <- c(c_lo,rev(c_hi))
# 
# polygon(x=polygon.x, y=polygon.y, col=adjustcolor("#fdb863", alpha.f=0.4), border=F)
# 
# l1 <- loess(as.numeric(jet[22,])~lon)#, family = "symmetric")$fitted
# l2 <- loess(as.numeric(jet[23,])~lon)#, family = "symmetric")$fitted
# l3 <- loess(as.numeric(jet[24,])~lon)#, family = "symmetric")$fitted
# l4 <- loess(as.numeric(jet[25,])~lon)#, family = "symmetric")$fitted
# c_hi <- matrix(NA,nrow = 1,ncol = 25)
# c_lo <- matrix(NA,nrow = 1,ncol = 25)
# 
# for (i in 1:25){
#   a <- mean(c(l1[i],l2[i],l3[i],l4[i]))
#   s <- sd(c(l1[i],l2[i],l3[i],l4[i]))
#   n <- 4
#   error <- qnorm(0.975)*s/sqrt(n)
#   c_hi[,i] <- a+error
#   c_lo[,i] <- a-error
# }
# 
# polygon.x <- c(seq(lon[1],lon[25], by = 2.5),rev(seq(lon[1],lon[25], by = 2.5)))
# #polygon.y <- c(data$PROXY_LOWER[complete.cases(data$PROXY_LOWER)],rev(data$PROXY_UPPER[complete.cases(data$PROXY_UPPER)]))
# polygon.y <- c(c_lo,rev(c_hi))
# 
# polygon(x=polygon.x, y=polygon.y, col=adjustcolor("#b2abd2", alpha.f=0.4), border=F)
# #colMedians
# #bootstrap? 
# 
# # #plot low monarch years
# # for (i in 1:4){
# #   l <- loess(as.numeric(jet[i,])~lon)#, family = "symmetric")
# #   lines(lon,l$fitted,col= adjustcolor("#e08214", alpha.f=0.4), type = "l", lwd = 1)
# # }
# # #plot high monarch years
# # for (i in 16:19){
# #   l <- loess(as.numeric(jet[i,])~lon)#, family = "symmetric")
# #   lines(lon,l$fitted,col= "#8073ac", type = "l", lwd = 1)
# # }
# #make above transparent? 
avg_lo <- loess(colMedians(as.matrix(jet[1:4,]))~lon)#, family = "symmetric")
lines(lon,avg_lo$fitted,col= "#b35806", type = "l", lwd = 5)

avg_hi <- loess(colMedians(as.matrix(jet[22:25,]))~lon)#, family = "symmetric")
lines(lon,avg_hi$fitted,col= "#542788", type = "l", lwd = 5)

points(-103, 26, pch=20, col="black",cex=2)  

# add error bars around each *significant* longitude?

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
map("worldHires","Mexico", xlim=c(-125,-65),ylim=c(15,70), fill=F, add = T,lwd = 1)#plot the region of Mexico
text(-65,15, "DEC",adj = c(1,0),cex = 1.5)

# I should probably take the confidence interval on the non loess filtered data
# 
# l1 <- loess(as.numeric(jet[1,])~lon)#, family = "symmetric")$fitted
# l2 <- loess(as.numeric(jet[2,])~lon)#, family = "symmetric")$fitted
# l3 <- loess(as.numeric(jet[3,])~lon)#, family = "symmetric")$fitted
# l4 <- loess(as.numeric(jet[4,])~lon)#, family = "symmetric")$fitted
# #l5 <- loess(as.numeric(jet[5,])~lon)#, family = "symmetric")$fitted
# c_hi <- matrix(NA,nrow = 1,ncol = 25)
# c_lo <- matrix(NA,nrow = 1,ncol = 25)
# 
# for (i in 1:25){
#   a <- mean(c(l1[i],l2[i],l3[i],l4[i]))
#   s <- sd(c(l1[i],l2[i],l3[i],l4[i]))
#   n <- 4
#   error <- qnorm(0.975)*s/sqrt(n)
#   c_hi[,i] <- a+error
#   c_lo[,i] <- a-error
# }
# 
# polygon.x <- c(seq(lon[1],lon[25], by = 2.5),rev(seq(lon[1],lon[25], by = 2.5)))
# #polygon.y <- c(data$PROXY_LOWER[complete.cases(data$PROXY_LOWER)],rev(data$PROXY_UPPER[complete.cases(data$PROXY_UPPER)]))
# polygon.y <- c(c_lo,rev(c_hi))
# 
# polygon(x=polygon.x, y=polygon.y, col=adjustcolor("#fdb863", alpha.f=0.4), border=F)
# 
# l1 <- loess(as.numeric(jet[22,])~lon)#, family = "symmetric")$fitted
# l2 <- loess(as.numeric(jet[23,])~lon)#, family = "symmetric")$fitted
# l3 <- loess(as.numeric(jet[24,])~lon)#, family = "symmetric")$fitted
# l4 <- loess(as.numeric(jet[25,])~lon)#, family = "symmetric")$fitted
# c_hi <- matrix(NA,nrow = 1,ncol = 25)
# c_lo <- matrix(NA,nrow = 1,ncol = 25)
# 
# for (i in 1:25){
#   a <- mean(c(l1[i],l2[i],l3[i],l4[i]))
#   s <- sd(c(l1[i],l2[i],l3[i],l4[i]))
#   n <- 4
#   error <- qnorm(0.975)*s/sqrt(n)
#   c_hi[,i] <- a+error
#   c_lo[,i] <- a-error
# }
# 
# polygon.x <- c(seq(lon[1],lon[25], by = 2.5),rev(seq(lon[1],lon[25], by = 2.5)))
# #polygon.y <- c(data$PROXY_LOWER[complete.cases(data$PROXY_LOWER)],rev(data$PROXY_UPPER[complete.cases(data$PROXY_UPPER)]))
# polygon.y <- c(c_lo,rev(c_hi))
# 
# polygon(x=polygon.x, y=polygon.y, col=adjustcolor("#b2abd2", alpha.f=0.4), border=F)
# #colMedians
# #bootstrap? 
# 
# # #plot low monarch years
# # for (i in 1:4){
# #   l <- loess(as.numeric(jet[i,])~lon)#, family = "symmetric")
# #   lines(lon,l$fitted,col= adjustcolor("#e08214", alpha.f=0.4), type = "l", lwd = 1)
# # }
# # #plot high monarch years
# # for (i in 16:19){
# #   l <- loess(as.numeric(jet[i,])~lon)#, family = "symmetric")
# #   lines(lon,l$fitted,col= "#8073ac", type = "l", lwd = 1)
# # }
# #make above transparent? 
avg_lo <- loess(colMedians(as.matrix(jet[1:4,]))~lon)#, family = "symmetric")
lines(lon,avg_lo$fitted,col= "#b35806", type = "l", lwd = 5)

avg_hi <- loess(colMedians(as.matrix(jet[22:25,]))~lon)#, family = "symmetric")
lines(lon,avg_hi$fitted,col= "#542788", type = "l", lwd = 5)

points(-100.14, 19.36, pch=20, col="black",cex=2)  

# add error bars around each *significant* longitude?




dev.off()
