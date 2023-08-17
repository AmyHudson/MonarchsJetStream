# determining whether can create model of monarch success w jet and roosting

yrmin <- 2004
yrmax <- 2018

alpha <- 0.1


# read in jet 
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


jet3as <- rowMeans(sapply(jet[which(m == 3),
                              which(lons >= -72.5 &
                                      lons <= -67.5)], as.numeric))
jet4s <- rowMeans(sapply(jet[which(m == 4),
                              which(lons >= -87.5 &
                                      lons <= -60)], as.numeric))
jet5as <- rowMeans(sapply(jet[which(m == 5),
                             which(lons >= -102.5 &
                                     lons <= -95)], as.numeric))
jet5m <- rowMeans(sapply(jet[which(m == 5),
                              which(lons >= -72.5 &
                                      lons <= -60)], as.numeric))

jet6mas <- rowMeans(sapply(jet[which(m == 6),
                               which(lons >= -107.5 &
                                       lons <= -102.5)], as.numeric))
jet6m <- rowMeans(sapply(jet[which(m == 6),
                               which(lons >= -102.5 &
                                       lons <= -77.5)], as.numeric))

jet8m <- rowMeans(sapply(jet[which(m == 8),
                             which(lons >= -97.5 &
                                     lons <= -95.0)], as.numeric))
jet8as <- rowMeans(sapply(jet[which(m == 8),
                             which(lons >= -80 &
                                     lons <= -67.5)], as.numeric))
jet9ms <- rowMeans(sapply(jet[which(m == 9),
                             which(lons >= -110 &
                                     lons <= -100)], as.numeric))
jet9m <- rowMeans(sapply(jet[which(m == 9),
                             which(lons >= -92.5 &
                                     lons <= -85.0)], as.numeric))
jet10m <- rowMeans(sapply(jet[which(m == 10),
                              which(lons >= -107.5 &
                                      lons <= -100)], as.numeric))
jet11m <- rowMeans(sapply(jet[which(m == 10),
                              which(lons >= -110 &
                                      lons <= -102.5)], as.numeric))
#make a dataframe with butterfly

# read in butterflies and jet correlations to determine strongest jet relationship

library(dplyr)

cor_table_all <- read.csv("data/processed/roost_jet_corr.csv")
cor_table_all[complete.cases(cor_table_all),]

cor_table_all %>%
  filter(month == "Aug",
         r.p.0.1 > 0) #%>%
  # group_by(index) %>%
  # mutate(min(longitude),
  #        max(longitude))
cor_table_all %>%
  filter(month == "Sep",
         r.p.0.1 < 0)

# read in monarch roosts
monarch_ts <- read.csv("data/processed/MonarchTimeSeries.csv")
monarch_ts <- monarch_ts[which(monarch_ts$Year>=yrmin & monarch_ts$Year<=yrmax),]

all <- cbind(monarch_ts,jet3as,jet4s,jet5as,jet5m,jet6m,jet6mas,jet8as,jet8m,jet9m,jet9ms,jet10m,jet11m)
write.csv(all, "data/processed/MonarchJet20042018.csv", row.names = F)

library(corrplot)
# mat : is a matrix of data
# ... : further arguments to pass 
# to the native R cor.test function
cor.mtest <- function(mat, ...)
{
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) 
  {
    for (j in (i + 1):n)
    {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

#detrend series
library(Hmisc)
all <- read.csv("data/processed/MonarchJet20042018.csv")
all <- pracma::detrend(as.matrix(all))

# matrix of the p-value of the correlation
p.mat <- cor.mtest(all[,2:16])

M <- cor(all[,2:16])
png("figures/corrplot_jetroosts_detrend.png",6,6,
    units = "in",res = 600, pointsize=20, family= "helvetica")
corrplot(M,type ='upper',p.mat = p.mat, sig.level = 0.1,insig = "blank")
# this needs to be detrended to make sense- jet9ms isn't correlated to september roost because of sept pos trend
dev.off()


# ######################
# GAM Model building

library(mgcv)
gam(Mexico ~ s(JN_ROOST_8) +
      s(JN_ROOST_9) +
      s(jet8as) +
      s(jet9ms) +
      s(jet10m) +
      s(jet11m), data = all)

modMex <- gam(Mexico ~
                s(Year) +
                #s(jet5m,k = 5) +
                #s(jet6m,k = 5) +
                #s(jet6mas,k = 5) +
                #s(jet8as,k = 5) +
                s(jet9ms,k = 5), #+
                #s(jet9m,k = 5) +
                #s(jet10m,k = 5) +
                #s(jet11m,k = 5),
              data = all, method = "REML")
summary(modMex)

