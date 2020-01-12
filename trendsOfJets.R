## Trend analysis of Jet Stream longitudes over the 1994-2018 period

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
pracma::detrend(pracma::detrend(mexicoareadetrend$MexicoArea))
summary(lm(mexicoareadetrend$MexicoArea ~ mexicoareadetrend$Year))

for (i in 1:dim(jet)[2]){
  if (summary(lm(jet[,i] ~ year))$coefficients[2,4]   < 0.05){ # test for significant linear trend
    print(colnames(jet)[i])
  }
}


high_to_low <- rev(mexicoareadetrend$Year[order(mexicoareadetrend[,2])])
#Loess curve fitting

lon <- as.integer(seq(235-360,295-360,2.5))
orange_purple <- c('#b35806','#b35806','#e08214','#e08214','#fdb863','#fdb863','#fee0b6','#fee0b6','#f7f7f7','#f7f7f7','#f7f7f7','#d8daeb','#d8daeb','#b2abd2','#b2abd2','#8073ac','#8073ac','#542788','#542788')


#png("fig1_median_4n_longregions.png",8,13,
#    units = "in",res = 600, pointsize=20, family= "helvetica")
#par(mfrow=c(1,2), tcl=-0.5, family="serif", mai=c(0,0,0,0),mar = c(0, 0, 0, 0))
par(mfrow=c(6,2),mar = c(0, 0, 0, 0))
# I would like to make 12 maps of the jet stream position, so 4 rows and 3 columns 

#import NHJI for each month
n <- read.table("NHJ_position_global_1948jan-2019sep_ncepncar.txt")
colnames(n) <- seq(0,357.5,2.5)
jet1 <- n[2:853,]
year <- 1994:2018
############################################################################
## January

jet <- jet1[seq(2, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
jet <- jet[47:71,]#1994-2018: 47:71
rownames(jet) <- 1994:2018
jet <- lapply(jet, as.character)
jet <- lapply(jet, as.numeric)
jet <- as.data.frame(jet)
jet <- jet[order(mexicoareadetrend[,2]),]

for (i in 1:dim(jet)[2]){
  if (summary(lm(jet[,i] ~ year))$coefficients[2,4]   < 0.05){ # test for significant linear trend
    print(colnames(jet)[i])
  }
}

#no spring or fall indices show a trend

rbPal <- colorRampPalette(c('gold', 'dark green'))(25)
bluePal <- colorRampPalette(c(rgb(0,0,1,1), rgb(0,0,1,0)), alpha = TRUE)(25)

plot(1994:2018,jet[,1], type = "l", col = rbPal[1], ylim = c(20,70), xlab = NULL, ylab = c("latitude"))
for(i in 2:length(jet)){
  lines(1994:2018,jet[,i], col = rbPal[i])
}
# define normality in jet indices for 1981:2012

for (i in 1:25){
  if (shapiro.test(jet[,i])$p.value < 0.05){ #test for normality
    print(colnames(jet)[i])
  }
}



rho <- NA[length(mon2detrend)]
pval <- NA[length(mon2detrend)]
# 
for (i in 1:length(mon2detrend)){
  rho[i]<- rcorr(mon2detrend[,i],jet[,which(jet_long==-75)],type = c("spearman"))$r[1,2]
  pval[i]<- rcorr(mon2detrend[,i],jet[,which(jet_long==-75)],type = c("spearman"))$P[1,2]
}

rho1 <- as.data.frame(matrix(NA,nrow = 1,ncol = length(rho)))#NA[length(mon1)]
mon1df1 <- as.data.frame(matrix(NA,nrow = 25,ncol = length(rho)))

for (i in 1:length(mon2detrend)){
  if (is.na(pval[i])==F & pval[i] < 0.1){
    rho1[i] <- 1 #rho[i] # significant correlation values
    mon1df1[,i] <- mon2detrend[,i] #climate time series of significant pixels
  }
}



#Feb
jet <- jet1[seq(2, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
jet <- jet[47:71,]#1994-2018: 47:71
rownames(jet) <- 1994:2018
jet <- lapply(jet, as.character)
jet <- lapply(jet, as.numeric)
jet <- as.data.frame(jet)
jet <- jet[order(mexicoareadetrend[,2]),]
for (i in 1:dim(jet)[2]){
  if (summary(lm(jet[,i] ~ year))$coefficients[2,4]   < 0.05){ # test for significant linear trend
    print(colnames(jet)[i])
  }
}


rbPal <- colorRampPalette(c('gold', 'dark green'))(25)
bluePal <- colorRampPalette(c(rgb(0,0,1,1), rgb(0,0,1,0)), alpha = TRUE)(25)

plot(1994:2018,jet[,1], type = "l", col = rbPal[1], ylim = c(20,70), xlab = NULL, ylab = c("latitude"))
for(i in 2:length(jet)){
  lines(1994:2018,jet[,i], col = rbPal[i])
}
# define normality in jet indices for 1981:2012

for (i in 1:25){
  if (shapiro.test(jet[,i])$p.value < 0.05){ #test for normality
    print(colnames(jet)[i])
  }
}