# generate long table correlating monarch roosting time series with jet stream position by longitude
# columns: period; longitude; index(August; September; Mexico); month; r; p
# then this feeds in to correlating jet stream at certain longitudes with climate over those regions


yrmin <- 2002
yrmax <- 2019
alpha <- 0.1

# read in monarch roosts
monarch_ts <- read.csv("data/processed/MonarchTimeSeries.csv")
monarch_ts1 <- monarch_ts[which(monarch_ts$Year>=yrmin & monarch_ts$Year<=yrmax),]
#detrend each index
monarch_ts_detrend <- data.frame(pracma::detrend(as.matrix(monarch_ts1)))
#monarch_ts_detrend <- detrend(data.frame(monarch_ts1), method = "Spline")

JN_ROOST_8 <- scale(monarch_ts_detrend$JN_ROOST_8)

JN_ROOST_9 <- scale(monarch_ts_detrend$JN_ROOST_9)
mexicoarea <- scale(monarch_ts_detrend$Mexico)

rcorr(mexicoarea,JN_ROOST_8, type = "spearman")
rcorr(mexicoarea,JN_ROOST_9, type = "spearman")
rcorr(JN_ROOST_9,JN_ROOST_8, type = "spearman")


# read in jet
n <- read.table("data/processed/NHJ_position_global_1948-2021_ncepncar.txt")
#n <- as.numeric(n)
colnames(n) <- seq(0,357.5,2.5)
jet1 <- n[2:889,]
jetyr <- 1948:2021

library(Hmisc)
jet <- jet1[,which(seq(0,357.5,2.5)>=250 & seq(0,357.5,2.5)<=290)]
cor_table <- matrix(NA,nrow = 11, ncol = length(jet))
colnames(cor_table) <- seq(250-360,290-360,2.5)

rownames(cor_table) <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov')

for(j in 1:11){
  jet <- jet1[seq(j, dim(jet1)[1], 12),which(seq(0,357.5,2.5)>=250 & seq(0,357.5,2.5)<=290)]
  jet <- jet[which(jetyr<=yrmax & jetyr>=yrmin),] 
  
  jet <- lapply(jet, as.character)
  jet <- lapply(jet, as.numeric)
  jet <- as.data.frame(jet)
  #jet <- as.data.frame(pracma::detrend(as.matrix(jet), tt = 'linear'))
  jetdetrend <- pracma::detrend(as.matrix(jet))
  
  #jetdetrend <- as.data.frame(jetdetrend)[c(1,2,3,4,8,9,12,14,15,16,17,19),]
  #mexicoarea <- mexicoarea[c(1,2,3,4,8,9,12,14,15,16,17,19)]
  
  for(i in 1:length(jet)){
    if (rcorr(mexicoarea,scale(jetdetrend[,i]),type = "spearman")$P[1,2]<alpha){
      cor_table[j,i] <- rcorr(mexicoarea,scale(jetdetrend[,i]),type = "spearman")[[1]][1,2]
    }
  }
  
}

cor_table <- matrix(NA,nrow = 8, ncol = length(jet))
colnames(cor_table) <- seq(250-360,290-360,2.5)

rownames(cor_table) <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug')

for(j in 1:8){
  jet <- jet1[seq(j, dim(jet1)[1], 12),which(seq(0,357.5,2.5)>=250 & seq(0,357.5,2.5)<=290)]
  jet <- jet[which(jetyr<=yrmax & jetyr>=yrmin),] 
  
  jet <- lapply(jet, as.character)
  jet <- lapply(jet, as.numeric)
  jet <- as.data.frame(jet)
  #jet <- as.data.frame(pracma::detrend(as.matrix(jet), tt = 'linear'))
  jetdetrend <- pracma::detrend(as.matrix(jet))
  
  for(i in 1:length(jet)){
    if (rcorr(JN_ROOST_8,scale(jetdetrend[,i]))$P[1,2]<alpha){
      cor_table[j,i] <- rcorr(JN_ROOST_8,scale(jetdetrend[,i]))[[1]][1,2]
    }
  }
  
}

cor_table <- matrix(NA,nrow = 9, ncol = length(jet))
colnames(cor_table) <- seq(250-360,290-360,2.5)

rownames(cor_table) <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug', 'Sep')

for(j in 1:9){
  jet <- jet1[seq(j, dim(jet1)[1], 12),which(seq(0,357.5,2.5)>=250 & seq(0,357.5,2.5)<=290)]
  jet <- jet[which(jetyr<=yrmax & jetyr>=yrmin),] 
  
  jet <- lapply(jet, as.character)
  jet <- lapply(jet, as.numeric)
  jet <- as.data.frame(jet)
  #jet <- as.data.frame(pracma::detrend(as.matrix(jet), tt = 'linear'))
  jetdetrend <- pracma::detrend(as.matrix(jet))
  
  for(i in 1:length(jet)){
    if (rcorr(JN_ROOST_9,jetdetrend[,i], type = "spearman")$P[1,2]<alpha){
      cor_table[j,i] <- rcorr(JN_ROOST_9,jetdetrend[,i], type = "spearman")[[1]][1,2]
    }
  }
  
}

######################
# make a long form table of correlations 
# period, roostIndex, month, jetlatitude, rho, pval
# only want certain months for each
