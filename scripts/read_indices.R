

yrmin <- 2004
yrmax <- 2018

mo <- c(8,9)
month.abb[mo]
climvar <- c("tmn","tmx","pre","uwnd","vwnd","ndvi")
index <- c("JN_ROOST8","JN_ROOST9","mexicoarea","jet8as","jet9ms")

# produces monarch and jet stream indices 

monarch_ts <- read.csv("data/processed/MonarchTimeSeries.csv")
monarch_ts1 <- monarch_ts[which(monarch_ts$Year>=yrmin & monarch_ts$Year<=yrmax),]
#detrend each index
monarch_ts_detrend <- data.frame(pracma::detrend(as.matrix(monarch_ts1)))

JN_ROOST_8 <- monarch_ts_detrend$JN_ROOST_8

JN_ROOST_9 <- monarch_ts_detrend$JN_ROOST_9
mexicoarea <- monarch_ts_detrend$Mexico

###############################
# read in jet and create indices

jet <- read.table("data/processed/NHJ_position_global_1948-2021_ncepncar.txt")
#n <- as.numeric(n)
colnames(jet) <- seq(0,357.5,2.5)
jet <- jet[2:889,]
jetyr <- 1948:2021

y <- rep(jetyr,each = 12)

library(Hmisc)
jet <- jet[which(y <= yrmax &
                   y >= yrmin),
           which(seq(0,357.5,2.5)>=250 & 
                   seq(0,357.5,2.5)<=300)]
colnames(jet) <- seq(250-360,300-360,2.5)

lons <- seq(250-360,300-360,2.5)
m <- rep(1:12,length(yrmin:yrmax))


jet8as <- pracma::detrend(rowMeans(sapply(jet[which(m == 8),
                              which(lons >= -80 &
                                      lons <= -67.5)], as.numeric)))
rcorr(jet8as,JN_ROOST_8, type = "pearson")
rcorr(jet8as,JN_ROOST_9, type = "pearson")
rcorr(jet8as,mexicoarea, type = "pearson")

shapiro.test(jet8as)
shapiro.test(JN_ROOST_8) # not normal
shapiro.test(JN_ROOST_9)
shapiro.test(mexicoarea)


#Pearson: linear trend, Spearman: monotonic trend. 
#Higher Pearson correlation means there are observations in the tails of the distribution that have large influence relative to their ranked values

plot(jet8as,JN_ROOST_8)

jet9ms <- pracma::detrend(rowMeans(sapply(jet[which(m == 9),
                              which(lons >= -110 &
                                      lons <= -100)], as.numeric)))
rcorr(jet9ms,JN_ROOST_9, type = "pearson")
rcorr(jet9ms,mexicoarea, type = "pearson")
shapiro.test(jet9ms)


