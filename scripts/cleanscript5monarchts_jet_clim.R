# read in 5 monarch time series; plot stacked plot of all ts; plot detrended ts all in 1 figure (maybe just 4 if CA doesn't pan out)

#read in jet and correlate with CA ts

# correlate subset monthly jet streams w climate 
# correlate ts w climate (tmin tmax tavg precip spei?)

#read in CA ts
CA <- read_excel("data/MonarchDesert-Data.xlsx", 
                                      sheet = "AnalysisData")
CA <- CA[which(CA$year>=1998 & CA$year<= 2020 ),]

unique(CA$latitude) #5 sites: 3 unique locations, 2 unknown locations
#points(CA$longitude,CA$latitude, cex = 0.75, pch = 16)
# aggregate by year
CA_ts <- aggregate(CA$observations, list(CA$year), FUN = sum)
#plot(CA_ts)
# read in mexico winter acreage
mexicoarea <- read.table('Butterflies1994-2020.txt',header = T)

#par(new = T)
#plot(mexicoarea[which(mexicoarea$Year>=1998),], axes = F, type = "l", col = "grey")
#axis(4, col = "gray",ylim = c(0,19))
mexicoareadetrend <- matrix(NA,nrow = length(1994:2020),ncol = 2)

mexicoareadetrend[,2] <- lm(mexicoarea$MexicoArea~mexicoarea$Year)$residuals
mexicoareadetrend[,1] <- 1994:2020 #1994:2012
mexicoareadetrend <- data.frame(mexicoareadetrend)
colnames(mexicoareadetrend) <- c("Year", "MexicoArea")

mexicoareadetrend_roost <- matrix(NA,nrow = length(2002:2020),ncol = 2)
mexicoarea <- mexicoarea[which(mexicoarea$Year>=2002),]
mexicoareadetrend_roost[,2] <- lm(mexicoarea$MexicoArea~mexicoarea$Year)$residuals
mexicoareadetrend_roost[,1] <- 2002:2020 #1994:2012
mexicoareadetrend_roost <- data.frame(mexicoareadetrend_roost)
colnames(mexicoareadetrend_roost) <- c("Year", "MexicoArea")

roost <- read.csv("data/raw/monarch_journeynorth_Fall_Roost.csv")
roost1 <- roost[which(roost$Month == 8),]

table(roost1$Year) 
roost1 <- as.numeric(pracma::detrend(as.matrix(as.numeric(table(roost1$Year)))))
JN_ROOST_8 <- roost1

roost1 <- roost[which(roost$Month == 9),]

table(roost1$Year) 
roost1 <- as.numeric(pracma::detrend(as.matrix(as.numeric(table(roost1$Year)))))
JN_ROOST_9 <- roost1

roost1 <- roost[which(roost$Month == 10),]

table(roost1$Year) 
roost1 <- as.numeric(pracma::detrend(as.matrix(as.numeric(table(roost1$Year)))))
JN_ROOST_10 <- roost1

library(Hmisc)
rcorr(JN_ROOST_8,mexicoareadetrend_roost$MexicoArea) #r = 0.49 p = 0.04 *
rcorr(JN_ROOST_9,mexicoareadetrend_roost$MexicoArea) #r = 0.54 p = 0.02 *
rcorr(JN_ROOST_10,mexicoareadetrend_roost$MexicoArea) #r = 0.3 p = 0.23
rcorr(JN_ROOST_8,JN_ROOST_9) # r = 0.89 p = 0 *
rcorr(JN_ROOST_8,JN_ROOST_10) # r = 0.18 p = 0.47
rcorr(JN_ROOST_10,JN_ROOST_9) # r = 0.06 p = 0.80

n <- read.table("NHJ_position_global_1948jan-2021apr_ncepncar.txt")
#n <- as.numeric(n)
colnames(n) <- seq(0,357.5,2.5)
jet1 <- n[2:877,]
yr <- 1948:2020

library(Hmisc)
jet <- jet1[,which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
cor_table <- matrix(NA,nrow = 12, ncol = length(jet))
rownames(cor_table) <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

for(j in 1:9){
  jet <- jet1[seq(j, 876, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
  jet <- jet[which(yr<=2020 & yr>=2002),] 
  
  jet <- lapply(jet, as.character)
  jet <- lapply(jet, as.numeric)
  jet <- as.data.frame(jet)
  #jet <- as.data.frame(pracma::detrend(as.matrix(jet), tt = 'linear'))
  jetdetrend <- pracma::detrend(as.matrix(jet))
  
  for(i in 1:length(jet)){
    if (rcorr(mexicoareadetrend_roost$MexicoArea,jetdetrend[,i], type = "spearman")$P[1,2]<0.1){
      cor_table[j,i] <- rcorr(mexicoareadetrend_roost$MexicoArea,jetdetrend[,i], type = "spearman")[[1]][1,2]
    }
  }
  
}
colnames(cor_table) <- colnames(jet)

############
cor_table <- matrix(NA,nrow = 12, ncol = length(jet))
rownames(cor_table) <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

for(j in 1:9){
  jet <- jet1[seq(j, 876, 12),which(seq(0,357.5,2.5)>=235 & seq(0,357.5,2.5)<=295)]
  jet <- jet[which(yr<=2020 & yr>=1994),] 
  
  jet <- lapply(jet, as.character)
  jet <- lapply(jet, as.numeric)
  jet <- as.data.frame(jet)
  #jet <- as.data.frame(pracma::detrend(as.matrix(jet), tt = 'linear'))
  jetdetrend <- pracma::detrend(as.matrix(jet))
  
  for(i in 1:length(jet)){
    if (rcorr(mexicoareadetrend$MexicoArea,jetdetrend[,i], type = "spearman")$P[1,2]<0.1){
      cor_table[j,i] <- rcorr(mexicoareadetrend$MexicoArea,jetdetrend[,i], type = "spearman")[[1]][1,2]
    }
  }
  
}
colnames(cor_table) <- colnames(jet)
