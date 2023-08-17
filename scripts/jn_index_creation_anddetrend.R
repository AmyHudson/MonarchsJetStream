
# create Journey north roost index for August and September
roost <- read.csv("data/raw/monarch_journeynorth_Fall_Roost.csv", fileEncoding="UTF-8-BOM")
roost[which(roost$Longitude == -11051301.0),]$Longitude<- -110.5 #error at one observation in Journey North at Canelo, AZ, which is at -110.51 (I matched to tenths place like the other observations)

roost1 <- roost[which(roost$Month == 8),]
JN_ROOST_8 <- as.numeric(table(roost1$Year))

roost1 <- roost[which(roost$Month == 9),]
JN_ROOST_9 <- as.numeric(table(roost1$Year))

#detrend roost data
library(pracma)

roost1 <- roost[which(roost$Month == 8),]
JN_ROOST_8_detrend <- as.numeric(pracma::detrend(as.matrix(as.numeric(table(roost1$Year)))))

roost1 <- roost[which(roost$Month == 9),]
JN_ROOST_9_detrend <- as.numeric(pracma::detrend(as.matrix(as.numeric(table(roost1$Year)))))

