

yrmin <- 2004
yrmax <- 2018
mo <- c(5,8,9)
month.abb[mo]
climvar <- c("tmn","tmx","pre","uwnd","vwnd","ndvi")
index <- c("JN_ROOST8","JN_ROOST9","mexicoarea","jet5","jet8","jet9")

# produces monarch and jet stream indices 

monarch_ts <- read.csv("data/processed/MonarchTimeSeries.csv")
monarch_ts1 <- monarch_ts[which(monarch_ts$Year>=yrmin & monarch_ts$Year<=yrmax),]
#detrend each index
monarch_ts_detrend <- data.frame(pracma::detrend(as.matrix(monarch_ts1)))

JN_ROOST_8 <- monarch_ts_detrend$JN_ROOST_8

JN_ROOST_9 <- monarch_ts_detrend$JN_ROOST_9
mexicoarea <- monarch_ts_detrend$Mexico


# read in jet and create indices

n <- read.table("data/processed/NHJ_position_global_1948-2021_ncepncar.txt")
#n <- as.numeric(n)
colnames(n) <- seq(0,357.5,2.5)
jet1 <- n[2:889,]
jetyr <- 1948:2021
jet <- jet1[,which(seq(0,357.5,2.5)>=250 & seq(0,357.5,2.5)<=300)]


j <- 5
jet <- jet1[seq(j, dim(jet1)[1], 12),which(seq(0,357.5,2.5)>=250 & seq(0,357.5,2.5)<=300)]
jet <- jet[which(jetyr<=yrmax & jetyr>=yrmin),] 

jet <- lapply(jet, as.character)
jet <- lapply(jet, as.numeric)
jet <- as.data.frame(jet)
#jet <- as.data.frame(pracma::detrend(as.matrix(jet), tt = 'linear'))
jetdetrend <- pracma::detrend(as.matrix(jet))

jet5 <- rowMeans(jetdetrend[,4:7])
rcorr(jet5,JN_ROOST_8, type = "spearman")
rcorr(jet5,JN_ROOST_9, type = "spearman")
rcorr(jet5,mexicoarea, type = "spearman")
# 
# j <- 6
# jet <- jet1[seq(j, dim(jet1)[1], 12),which(seq(0,357.5,2.5)>=250 & seq(0,357.5,2.5)<=300)]
# jet <- jet[which(jetyr<=yrmax & jetyr>=yrmin),] 
# 
# jet <- lapply(jet, as.character)
# jet <- lapply(jet, as.numeric)
# jet <- as.data.frame(jet)
# #jet <- as.data.frame(pracma::detrend(as.matrix(jet), tt = 'linear'))
# jetdetrend <- pracma::detrend(as.matrix(jet))
# 
# jet6 <- rowMeans(jetdetrend[,4:7])
# rcorr(jet6,JN_ROOST_8, type = "spearman")
# rcorr(jet6,JN_ROOST_9, type = "spearman")
# rcorr(jet6,mexicoarea, type = "spearman")
# 
# jet6w <- rowMeans(jetdetrend[,13:14])
# rcorr(jet6w,JN_ROOST_8, type = "spearman")
# rcorr(jet6w,JN_ROOST_9, type = "spearman")
# rcorr(jet6w,mexicoarea, type = "spearman")

j <- 8
jet <- jet1[seq(j, dim(jet1)[1], 12),which(seq(0,357.5,2.5)>=250 & seq(0,357.5,2.5)<=300)]
jet <- jet[which(jetyr<=yrmax & jetyr>=yrmin),] 

jet <- lapply(jet, as.character)
jet <- lapply(jet, as.numeric)
jet <- as.data.frame(jet)
#jet <- as.data.frame(pracma::detrend(as.matrix(jet), tt = 'linear'))
jetdetrend <- pracma::detrend(as.matrix(jet))

jet8 <- rowMeans(jetdetrend[,14:18])
rcorr(jet8,JN_ROOST_8, type = "spearman")
rcorr(jet8,JN_ROOST_9, type = "spearman")
rcorr(jet8,mexicoarea, type = "spearman")

j <- 9
jet <- jet1[seq(j, dim(jet1)[1], 12),which(seq(0,357.5,2.5)>=250 & seq(0,357.5,2.5)<=300)]
jet <- jet[which(jetyr<=yrmax & jetyr>=yrmin),] 

jet <- lapply(jet, as.character)
jet <- lapply(jet, as.numeric)
jet <- as.data.frame(jet)
#jet <- as.data.frame(pracma::detrend(as.matrix(jet), tt = 'linear'))
jetdetrend <- pracma::detrend(as.matrix(jet))

jet9 <- rowMeans(jetdetrend[,1:5])
rcorr(jet9,JN_ROOST_9, type = "spearman")
rcorr(jet9,mexicoarea, type = "spearman")

#jet9w <- rowMeans(jetdetrend[,7:9])
#rcorr(jet9w,JN_ROOST_9, type = "spearman")
#rcorr(jet9w,mexicoarea, type = "spearman")
