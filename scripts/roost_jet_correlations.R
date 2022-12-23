# generate long table correlating monarch roosting time series with jet stream position by longitude
# columns: period; longitude; index(August; September; Mexico); month; r; p
# then this feeds in to correlating jet stream at certain longitudes with climate over those regions
library(tidyr)

yrmin <- 2004
yrmax <- 2018

alpha <- 0.1

# read in monarch roosts
monarch_ts <- read.csv("data/processed/MonarchTimeSeries.csv")
monarch_ts1 <- monarch_ts[which(monarch_ts$Year>=yrmin & monarch_ts$Year<=yrmax),]
#detrend each index
monarch_ts_detrend <- data.frame(pracma::detrend(as.matrix(monarch_ts1)))
#monarch_ts_detrend <- detrend(data.frame(monarch_ts1), method = "Spline")

JN_ROOST_8 <- monarch_ts_detrend$JN_ROOST_8

JN_ROOST_9 <- monarch_ts_detrend$JN_ROOST_9
mexicoarea <- monarch_ts_detrend$Mexico

rcorr(mexicoarea,JN_ROOST_8, type = "spearman")
rcorr(mexicoarea,JN_ROOST_9, type = "spearman")
rcorr(JN_ROOST_9,JN_ROOST_8, type = "spearman")

rcorr(mexicoarea,JN_ROOST_8, type = "pearson")
rcorr(mexicoarea,JN_ROOST_9, type = "pearson")
rcorr(JN_ROOST_9,JN_ROOST_8, type = "pearson")

# read in jet
n <- read.table("data/processed/NHJ_position_global_1948-2021_ncepncar.txt")
#n <- as.numeric(n)
colnames(n) <- seq(0,357.5,2.5)
jet1 <- n[2:889,]
jetyr <- 1948:2021

library(Hmisc)
jet <- jet1[,which(seq(0,357.5,2.5)>=250 & seq(0,357.5,2.5)<=300)]
cor_table <- matrix(NA,nrow = 11, ncol = length(jet))
colnames(cor_table) <- seq(250-360,300-360,2.5)

rownames(cor_table) <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov')

for(j in 1:11){
  jet <- jet1[seq(j, dim(jet1)[1], 12),which(seq(0,357.5,2.5)>=250 & seq(0,357.5,2.5)<=300)]
  jet <- jet[which(jetyr<=yrmax & jetyr>=yrmin),] 
  
  jet <- lapply(jet, as.character)
  jet <- lapply(jet, as.numeric)
  jet <- as.data.frame(jet)
  #jet <- as.data.frame(pracma::detrend(as.matrix(jet), tt = 'linear'))
  jetdetrend <- pracma::detrend(as.matrix(jet))
  
  #jetdetrend <- as.data.frame(jetdetrend)[c(1,2,3,4,8,9,12,14,15,16,17,19),]
  #mexicoarea <- mexicoarea[c(1,2,3,4,8,9,12,14,15,16,17,19)]
  
  for(i in 1:length(jet)){
    if (rcorr(mexicoarea,jetdetrend[,i],type = "spearman")$P[1,2]<alpha){
      cor_table[j,i] <- rcorr(mexicoarea,jetdetrend[,i],type = "spearman")[[1]][1,2]
    }
  }
  
}
cor_table <- cbind(month.abb[1:11],data.frame(cor_table))
#names(cor_table) <- sub("^X.", "-", names(cor_table))

cor_table <- pivot_longer(data.frame(cor_table),cols = 2:22)
cor_table$name <- as.numeric(gsub('X.', '-', cor_table$name))
cor_table$index <- c("mexicoarea")
cor_table$yrmin <- yrmin
cor_table$yrmax <- yrmax
colnames(cor_table) <- c("month","longitude","r,p<0.1","index","yrmin","yrmax")

cor_table8 <- matrix(NA,nrow = 8, ncol = length(jet))
colnames(cor_table8) <- seq(250-360,300-360,2.5)

rownames(cor_table8) <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug')

for(j in 1:8){
  jet <- jet1[seq(j, dim(jet1)[1], 12),which(seq(0,357.5,2.5)>=250 & seq(0,357.5,2.5)<=300)]
  jet <- jet[which(jetyr<=yrmax & jetyr>=yrmin),] 
  
  jet <- lapply(jet, as.character)
  jet <- lapply(jet, as.numeric)
  jet <- as.data.frame(jet)
  #jet <- as.data.frame(pracma::detrend(as.matrix(jet), tt = 'linear'))
  jetdetrend <- pracma::detrend(as.matrix(jet))
  
  for(i in 1:length(jet)){
    if (rcorr(JN_ROOST_8,jetdetrend[,i],type = "spearman")$P[1,2]<alpha){
      cor_table8[j,i] <- rcorr(JN_ROOST_8,jetdetrend[,i],type = "spearman")[[1]][1,2]
    }
  }
  
}
cor_table8 <- cbind(month.abb[1:8],data.frame(cor_table8))
#names(cor_table8) <- sub("^X.", "-", names(cor_table8))

cor_table8 <- pivot_longer(data.frame(cor_table8),cols = 2:22)
cor_table8$name <- as.numeric(gsub('X.', '-', cor_table8$name))
cor_table8$index <- c("augroost")
cor_table8$yrmin <- yrmin
cor_table8$yrmax <- yrmax
colnames(cor_table8) <- c("month","longitude","r,p<0.1","index","yrmin","yrmax")


cor_table9 <- matrix(NA,nrow = 9, ncol = length(jet))
colnames(cor_table9) <- seq(250-360,300-360,2.5)

rownames(cor_table9) <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug', 'Sep')

for(j in 1:9){
  jet <- jet1[seq(j, dim(jet1)[1], 12),which(seq(0,357.5,2.5)>=250 & seq(0,357.5,2.5)<=300)]
  jet <- jet[which(jetyr<=yrmax & jetyr>=yrmin),] 
  
  jet <- lapply(jet, as.character)
  jet <- lapply(jet, as.numeric)
  jet <- as.data.frame(jet)
  #jet <- as.data.frame(pracma::detrend(as.matrix(jet), tt = 'linear'))
  jetdetrend <- pracma::detrend(as.matrix(jet))
  
  for(i in 1:length(jet)){
    if (rcorr(JN_ROOST_9,jetdetrend[,i], type = "spearman")$P[1,2]<alpha){
      cor_table9[j,i] <- rcorr(JN_ROOST_9,jetdetrend[,i], type = "spearman")[[1]][1,2]
    }
  }
  
}
cor_table9 <- cbind(month.abb[1:9],data.frame(cor_table9))
#names(cor_table9) <- sub("^X.", "-", names(cor_table9))

cor_table9 <- pivot_longer(data.frame(cor_table9),cols = 2:22)
cor_table9$name <- as.numeric(gsub('X.', '-', cor_table9$name))
cor_table9$index <- c("seproost")
cor_table9$yrmin <- yrmin
cor_table9$yrmax <- yrmax
colnames(cor_table9) <- c("month","longitude","r,p<0.1","index","yrmin","yrmax")

cor_table_all <- rbind(data.frame(cor_table),data.frame(cor_table8),data.frame(cor_table9))
colnames(cor_table_all) <- c("month","longitude","r,p<0.1","index","yrmin","yrmax")


write.csv(cor_table_all,"data/processed/roost_jet_corr.csv",row.names = F)


######################
# make a long form table of correlations 
# period, roostIndex, month, jetlatitude, rho, pval
# only want certain months for each
