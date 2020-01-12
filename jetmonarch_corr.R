#correlations between jet at each longitude and monarch overwintering numbers
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

mexicoarea <- read.table('Butterflies.txt',header = T)
#mexicoarea <- mexicoarea[1:19,]#2012
mexicoarea <- mexicoarea[1:25,]

#detrend(mexicoarea,make.plot = T) 
#mexicoareadetrend <- detrend(mexicoarea,make.plot = T, method = c("ModNegExp")) # detrended using modified neg exp
#mexicoareadetrend <- detrend(mexicoarea,make.plot = T, method = c("Spline")) # detrended using modified neg exp
mexicoareadetrend <- matrix(NA,nrow = length(1994:2018),ncol = 2)
mexicoareadetrend[,2] <- lm(mexicoarea$MexicoArea~mexicoarea$Year)$residuals
mexicoareadetrend[,1] <- 1994:2018 #1994:2012
mexicoareadetrend <- data.frame(mexicoareadetrend)
colnames(mexicoareadetrend) <- c("Year", "MexicoArea")
# are there trends in mean or variance for the jet longitudes?

#read in jet position 1948-2018 
n <- read.table("NHJ_position_global_1948jan-2019sep_ncepncar.txt")
#n <- as.numeric(n)
colnames(n) <- seq(0,357.5,2.5)
jet1 <- n[2:853,]


#yr <- 1948:2018
#length(1948:2018)
#length(sort(rep(1948:2019,12))[1:852])


#jet <- read.table("NHJ_USGS/position_jan_20thcentury.txt") 
#jet <- jet[124:142,which(seq(0,358,2)>=235 & seq(0,358,2)<=295)] #subset 1994-2012, subset -125 to -65 or 235 to 295
jet <- jet1[seq(1, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
jet <- jet[47:71,]#1994: 47:71

jet <- lapply(jet, as.character)
jet <- lapply(jet, as.numeric)
jet <- as.data.frame(jet)
jet <- as.data.frame(pracma::detrend(as.matrix(jet), tt = 'linear'))

library(Hmisc)
cor_table <- matrix(NA,nrow = 12, ncol = length(jet))
rownames(cor_table) <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')


for(i in 1:length(jet)){
  if (rcorr(mexicoareadetrend$MexicoArea,jet[,i], type = "spearman")$P[1,2]<0.1){
    cor_table[1,i] <- rcorr(mexicoareadetrend$MexicoArea,jet[,i], type = "spearman")[[1]][1,2]
  }
}

#jet <- read.table("NHJ_USGS/position_feb_20thcentury.txt") #rownames(jet) #1871-2012 
#jet <- jet[124:142,which(seq(0,358,2)>=235 & seq(0,358,2)<=295)] #subset 1994-2012, subset -125 to -65 or 235 to 295
jet <- jet1[seq(2, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
jet <- jet[47:71,]#1994-2018: 47:71
rownames(jet) <- 1994:2018
jet <- lapply(jet, as.character)
jet <- lapply(jet, as.numeric)
jet <- as.data.frame(jet)
jet <- as.data.frame(pracma::detrend(as.matrix(jet), tt = 'linear'))


for(i in 1:length(jet)){
  if (rcorr(mexicoareadetrend$MexicoArea,jet[,i], type = "spearman")$P[1,2]<0.1){
    cor_table[2,i] <- rcorr(mexicoareadetrend$MexicoArea,jet[,i], type = "spearman")[[1]][1,2]
  }
}

#jet <- read.table("NHJ_USGS/position_mar_20thcentury.txt") #rownames(jet) #1871-2012 
#jet <- jet[124:142,which(seq(0,358,2)>=235 & seq(0,358,2)<=295)] #subset 1994-2012, subset -125 to -65 or 235 to 295
jet <- jet1[seq(3, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
jet <- jet[47:71,]#1994-2018: 47:71
rownames(jet) <- 1994:2018
jet <- lapply(jet, as.character)
jet <- lapply(jet, as.numeric)
jet <- as.data.frame(jet)
jet <- as.data.frame(pracma::detrend(as.matrix(jet), tt = 'linear'))

for(i in 1:length(jet)){
  if (rcorr(mexicoareadetrend$MexicoArea,jet[,i], type = "spearman")$P[1,2]<0.1){
    cor_table[3,i] <- rcorr(mexicoareadetrend$MexicoArea,jet[,i], type = "spearman")[[1]][1,2]
  }
}

#jet <- read.table("NHJ_USGS/position_apr_20thcentury.txt") #rownames(jet) #1871-2012 
#jet <- jet[124:142,which(seq(0,358,2)>=235 & seq(0,358,2)<=295)] #subset 1994-2012, subset -125 to -65 or 235 to 295
jet <- jet1[seq(4, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
jet <- jet[47:71,]#1994-2018: 47:71
rownames(jet) <- 1994:2018
jet <- lapply(jet, as.character)
jet <- lapply(jet, as.numeric)
jet <- as.data.frame(jet)
jet <- as.data.frame(pracma::detrend(as.matrix(jet), tt = 'linear'))


for(i in 1:length(jet)){
  if (rcorr(mexicoareadetrend$MexicoArea,jet[,i], type = "spearman")$P[1,2]<0.1){
    cor_table[4,i] <- rcorr(mexicoareadetrend$MexicoArea,jet[,i], type = "spearman")[[1]][1,2]
  }
}

#jet <- read.table("NHJ_USGS/position_may_20thcentury.txt") #rownames(jet) #1871-2012 
#jet <- jet[124:142,which(seq(0,358,2)>=235 & seq(0,358,2)<=295)] #subset 1994-2012, subset -125 to -65 or 235 to 295
jet <- jet1[seq(5, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
jet <- jet[47:71,]#1994-2018: 47:71
rownames(jet) <- 1994:2018
jet <- lapply(jet, as.character)
jet <- lapply(jet, as.numeric)
jet <- as.data.frame(jet)
jet <- as.data.frame(pracma::detrend(as.matrix(jet), tt = 'linear'))

for(i in 1:length(jet)){
  if (rcorr(mexicoareadetrend$MexicoArea,jet[,i], type = "spearman")$P[1,2]<0.1){
    cor_table[5,i] <- rcorr(mexicoareadetrend$MexicoArea,jet[,i], type = "spearman")[[1]][1,2]
  }
}

#jet <- read.table("NHJ_USGS/position_jun_20thcentury.txt") #rownames(jet) #1871-2012 
#jet <- jet[124:142,which(seq(0,358,2)>=235 & seq(0,358,2)<=295)] #subset 1994-2012, subset -125 to -65 or 235 to 295
jet <- jet1[seq(6, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
jet <- jet[47:71,]#1994-2018: 47:71
rownames(jet) <- 1994:2018
jet <- lapply(jet, as.character)
jet <- lapply(jet, as.numeric)
jet <- as.data.frame(jet)
jet <- as.data.frame(pracma::detrend(as.matrix(jet), tt = 'linear'))

for(i in 1:length(jet)){
  if (rcorr(mexicoareadetrend$MexicoArea,jet[,i], type = "spearman")$P[1,2]<0.1){
    cor_table[6,i] <- rcorr(mexicoareadetrend$MexicoArea,jet[,i], type = "spearman")[[1]][1,2]
  }
}

#jet <- read.table("NHJ_USGS/position_jul_20thcentury.txt") #rownames(jet) #1871-2012 
#jet <- jet[124:142,which(seq(0,358,2)>=235 & seq(0,358,2)<=295)] #subset 1994-2012, subset -125 to -65 or 235 to 295
jet <- jet1[seq(7, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
jet <- jet[47:71,]#1994-2018: 47:71
rownames(jet) <- 1994:2018
jet <- lapply(jet, as.character)
jet <- lapply(jet, as.numeric)
jet <- as.data.frame(jet)
jet <- as.data.frame(pracma::detrend(as.matrix(jet), tt = 'linear'))

for(i in 1:length(jet)){
  if (rcorr(mexicoareadetrend$MexicoArea,jet[,i], type = "spearman")$P[1,2]<0.1){
    cor_table[7,i] <- rcorr(mexicoareadetrend$MexicoArea,jet[,i], type = "spearman")[[1]][1,2]
  }
}

#jet <- read.table("NHJ_USGS/position_aug_20thcentury.txt") #rownames(jet) #1871-2012 
#jet <- jet[124:142,which(seq(0,358,2)>=235 & seq(0,358,2)<=295)] #subset 1994-2012, subset -125 to -65 or 235 to 295
jet <- jet1[seq(8, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
jet <- jet[47:71,]#1994-2018: 47:71
rownames(jet) <- 1994:2018
jet <- lapply(jet, as.character)
jet <- lapply(jet, as.numeric)
jet <- as.data.frame(jet)
jet <- as.data.frame(pracma::detrend(as.matrix(jet), tt = 'linear'))

for(i in 1:length(jet)){
  if (rcorr(mexicoareadetrend$MexicoArea,jet[,i], type = "spearman")$P[1,2]<0.1){
    cor_table[8,i] <- rcorr(mexicoareadetrend$MexicoArea,jet[,i], type = "spearman")[[1]][1,2]
  }
}

#jet <- read.table("NHJ_USGS/position_sep_20thcentury.txt") #rownames(jet) #1871-2012 
#jet <- jet[124:142,which(seq(0,358,2)>=235 & seq(0,358,2)<=295)] #subset 1994-2012, subset -125 to -65 or 235 to 295
jet <- jet1[seq(9, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
jet <- jet[47:71,]#1994-2018: 47:71
rownames(jet) <- 1994:2018
jet <- lapply(jet, as.character)
jet <- lapply(jet, as.numeric)
jet <- as.data.frame(jet)
jet <- as.data.frame(pracma::detrend(as.matrix(jet), tt = 'linear'))

for(i in 1:length(jet)){
  if (rcorr(mexicoareadetrend$MexicoArea,jet[,i], type = "spearman")$P[1,2]<0.1){
    cor_table[9,i] <- rcorr(mexicoareadetrend$MexicoArea,jet[,i], type = "spearman")[[1]][1,2]
  }
}

#jet <- read.table("NHJ_USGS/position_oct_20thcentury.txt") #rownames(jet) #1871-2012 
#jet <- jet[124:142,which(seq(0,358,2)>=235 & seq(0,358,2)<=295)] #subset 1994-2012, subset -125 to -65 or 235 to 295
jet <- jet1[seq(10, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
jet <- jet[47:71,]#1994-2018: 47:71
rownames(jet) <- 1994:2018
jet <- lapply(jet, as.character)
jet <- lapply(jet, as.numeric)
jet <- as.data.frame(jet)
jet <- as.data.frame(pracma::detrend(as.matrix(jet), tt = 'linear'))

for(i in 1:length(jet)){
  if (rcorr(mexicoareadetrend$MexicoArea,jet[,i], type = "spearman")$P[1,2]<0.1){
    cor_table[10,i] <- rcorr(mexicoareadetrend$MexicoArea,jet[,i], type = "spearman")[[1]][1,2]
  }
}

#jet <- read.table("NHJ_USGS/position_nov_20thcentury.txt") #rownames(jet) #1871-2012 
#jet <- jet[124:142,which(seq(0,358,2)>=235 & seq(0,358,2)<=295)] #subset 1994-2012, subset -125 to -65 or 235 to 295
jet <- jet1[seq(11, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
jet <- jet[47:71,]#1994-2018: 47:71
rownames(jet) <- 1994:2018
jet <- lapply(jet, as.character)
jet <- lapply(jet, as.numeric)
jet <- as.data.frame(jet)
jet <- as.data.frame(pracma::detrend(as.matrix(jet), tt = 'linear'))

for(i in 1:length(jet)){
  if (rcorr(mexicoareadetrend$MexicoArea,jet[,i], type = "spearman")$P[1,2]<0.1){
    cor_table[11,i] <- rcorr(mexicoareadetrend$MexicoArea,jet[,i], type = "spearman")[[1]][1,2]
  }
}

#jet <- read.table("NHJ_USGS/position_dec_20thcentury.txt") #rownames(jet) #1871-2012 
#jet <- jet[124:142,which(seq(0,358,2)>=235 & seq(0,358,2)<=295)] #subset 1994-2012, subset -125 to -65 or 235 to 295
jet <- jet1[seq(12, 852, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
jet <- jet[47:71,]#1994-2018: 47:71
rownames(jet) <- 1994:2018
jet <- lapply(jet, as.character)
jet <- lapply(jet, as.numeric)
jet <- as.data.frame(jet)
jet <- as.data.frame(pracma::detrend(as.matrix(jet), tt = 'linear'))

for(i in 1:length(jet)){
  if (rcorr(mexicoareadetrend$MexicoArea,jet[,i], type = "spearman")$P[1,2]<0.1){
    cor_table[12,i] <- rcorr(mexicoareadetrend$MexicoArea,jet[,i], type = "spearman")[[1]][1,2]
  }
}

colnames(cor_table) <- colnames(jet)

# This produces a table... I could make a figure for every month showing the longitudes of correlation and the distribution of jet streams for that latitude as a boxplot filled in by the strength of correlation?

# Or I could pair it up with the Figure looking at the loess filters of all the years and use as stippling?

#Now I should do this as a composite analysis... 

com_table <- matrix(NA,nrow = 12, ncol = length(jet))
rownames(com_table) <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

jet <- read.table("NHJ_USGS/position_jan_20thcentury.txt") #rownames(jet) #1871-2012 
jet <- jet[124:142,which(seq(0,358,2)>=235 & seq(0,358,2)<=295)] #subset 1994-2012, subset -125 to -65 or 235 to 295
jet <- jet[order(mexicoareadetrend[,2]),]

colMeans(jet[1:5,])
#########################################
### not done below  #10/1
# for(i in 1:length(jet)){
#   test <- wilcox.test(as.numeric(jet[1:5,1]),as.numeric(jet[15:19,1]))
#   if(test$p.value<0.05){
#     #com_table[1,i] <- 
#   }
# }
# for(i in 1:length(jet)){
#   if (rcorr(mexicoareadetrend$MexicoArea,jet[,i], type = "spearman")$P[1,2]<0.05){
#     cor_table[1,i] <- rcorr(mexicoareadetrend$MexicoArea,jet[,i], type = "spearman")[[1]][1,2]
#   }
# }