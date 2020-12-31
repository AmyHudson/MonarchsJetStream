# pulling NPN data for Amy H's monarch-jet stream analysis
# TCrimmins 12-22-20
# Bounding box: 20N to 40N; -104E to -90E

library(rnpn)

# download individual-level phenometrics
# all flowering data
# subset to bounding box + 2010-2020

NPNdata <- npn_download_individual_phenometrics(request_source = "TCrimmins", years = c(1993:2019),
                                     coords = c(20, -104, 40, -90))
                                                                   
# 1858
# subset to flowering
unique(NPNdata$phenophase_description)

NNflowering <- subset(NPNdata, phenophase_description=="Open flowers (1 location)" | phenophase_description=="Open flowers" |
                        phenophase_description=="Open flowers (lilac)")


#write out file so I can plot in excel (sorry!!)
#write.csv(NNflowering,"c:/NPN/Manuscripts/Working/Hudson_etal_monarch_stuff/NNflowering.csv")

x <- boxplot(last_yes_doy ~ last_yes_year,data = NNflowering, main="Last Yes of Open Flowers in bounding box", xlab="Year", ylab="DOY")

# first checked out mean

mean_doy <- aggregate(last_yes_doy ~ last_yes_year,data = NNflowering, FUN = mean, na.rm = T)
plot(mean_doy, type = "l")
library(pracma)
plot(mean_doy$last_yes_year,scale(pracma::detrend(mean_doy$last_yes_doy)),type = "l")
lines(mexicoareadetrend, col = "gray")
library(Hmisc)
rcorr(mexicoareadetrend$MexicoArea,pracma::detrend(mean_doy$last_yes_doy))

#mean_doy <- aggregate(last_yes_doy ~ last_yes_year,data = NNflowering, FUN = median, na.rm = T)
#plot(mean_doy, type = "l")


#box plot stats to try = 1) extreme of the lower whisker, 2) the lower hinge, 3) the median, 4) the upper hinge and 5) the extreme of the upper whisker
# interquartile range ()

x$stats[3,]
plot(mean_doy$last_yes_year,scale(pracma::detrend(x$stats[3,])),type = "l")
rcorr(mexicoareadetrend$MexicoArea,pracma::detrend(x$stats[3,]))
rcorr(mexicoareadetrend$MexicoArea,x$stats[3,])

x$stats[4,]
plot(mean_doy$last_yes_year,x$stats[4,],type = "l")
plot(mean_doy$last_yes_year,scale(pracma::detrend(x$stats[4,])),type = "l")
rcorr(mexicoareadetrend$MexicoArea,pracma::detrend(x$stats[4,]))
rcorr(mexicoareadetrend$MexicoArea,x$stats[4,])

range_doy <- x$stats[4,]-x$stats[2,]
plot(mean_doy$last_yes_year,range_doy,type = "l")
plot(mean_doy$last_yes_year,scale(pracma::detrend(range_doy)),type = "l")
rcorr(mexicoareadetrend$MexicoArea,pracma::detrend(range_doy))
rcorr(mexicoareadetrend$MexicoArea,range_doy)

