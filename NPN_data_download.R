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

summary(NNflowering)
#write out file so I can plot in excel (sorry!!)
#write.csv(NNflowering,"c:/NPN/Manuscripts/Working/Hudson_etal_monarch_stuff/NNflowering.csv")
png("npn_flowering.png",6,6, type = "cairo",
    #png("output/SPEI-3m_site.png",4,6, type = "cairo",
    #png("output/SurfacePrecip_20thCent_1.5sd.png",4,6,
    units = "in",res = 600, pointsize=10, family= "helvetica")
#par(tck = .02)
par(mfrow=c(6,1),mar = c(2, 4.1, 1, 0.5)) #c(5.1, 4.1, 4.1, 2.1) c(bottom, left, top, right))
# read in monarch overwintering acreage data:
mexicoarea <- read.table('Butterflies1994-2019.txt',header = T)
mexicoarea[,1] <- 1994:2019
mexicoarea <- mexicoarea[16:26,]#2009-2019 overlap with nectar
mexicoareadetrend <- matrix(NA,nrow = length(2009:2019),ncol = 2)
mexicoareadetrend[,2] <- lm(mexicoarea$MexicoArea~mexicoarea$Year)$residuals
mexicoareadetrend[,1] <- 2009:2019 #1994:2012
mexicoareadetrend <- data.frame(mexicoareadetrend)
colnames(mexicoareadetrend) <- c("Year", "MexicoArea")

plot(2009:2019,scale(mexicoarea$MexicoArea), type = "l", xlab = NULL, ylab = c("Mexico (scaled)"))
lines(mexicoareadetrend, col = "gray")
#abline(lm(scale(mexicoarea$MexicoArea) ~ mexicoareadetrend$Year), col = "red")

x <- boxplot(last_yes_doy ~ last_yes_year,data = NNflowering, main="Last Yes of Open Flowers in bounding box", xlab="Year", ylab="DOY")

#box_plot <- ggplot(NNflowering, aes(x = last_yes_year, group = last_yes_year, y = last_yes_doy))
# Add the geometric object box plot
# box_plot +
#   geom_boxplot() +
#   geom_jitter(shape = 15,
#               color = "steelblue",
#               position = position_jitter(width = 0.21)) +
#   geom_smooth(method = "lm", se = FALSE) +
#   scale_x_discrete(limits = 2009:2019) +
#   theme_classic()


# first checked out mean

mean_doy <- aggregate(last_yes_doy ~ last_yes_year,data = NNflowering, FUN = mean, na.rm = T)
plot(mean_doy, type = "l", xlab = NULL, ylab = c("Mean DOY"))
#abline(lm(mean_doy$last_yes_doy ~ mean_doy$last_yes_year), col = "red")

library(pracma)


#plot(mean_doy$last_yes_year,scale(pracma::detrend(mean_doy$last_yes_doy)),type = "l", ylim = c(-2,2.5))



library(Hmisc)
rcorr(mexicoareadetrend$MexicoArea,pracma::detrend(mean_doy$last_yes_doy))

#mean_doy <- aggregate(last_yes_doy ~ last_yes_year,data = NNflowering, FUN = median, na.rm = T)
#plot(mean_doy, type = "l")


#box plot stats to try = 1) extreme of the lower whisker, 2) the lower hinge, 3) the median, 4) the upper hinge and 5) the extreme of the upper whisker
# interquartile range ()

x$stats[3,]
plot(mean_doy$last_yes_year,x$stats[3,],type = "l", ylab = c("Median DOY"), xlab = NULL)
rcorr(mexicoareadetrend$MexicoArea,pracma::detrend(x$stats[3,]))
rcorr(mexicoareadetrend$MexicoArea,x$stats[3,])

x$stats[4,]
plot(mean_doy$last_yes_year,x$stats[4,],type = "l", ylab = c("Upper Hinge"), xlab = NULL)
#plot(mean_doy$last_yes_year,scale(pracma::detrend(x$stats[4,])),type = "l")
rcorr(mexicoareadetrend$MexicoArea,pracma::detrend(x$stats[4,]))
rcorr(mexicoareadetrend$MexicoArea,x$stats[4,])

range_doy <- x$stats[4,]-x$stats[2,]
plot(mean_doy$last_yes_year,range_doy,type = "l", ylab = c("Range DOY"), xlab = NULL)
#plot(mean_doy$last_yes_year,scale(pracma::detrend(range_doy)),type = "l")
#lines(mexicoareadetrend, col = "gray")
rcorr(mexicoareadetrend$MexicoArea,pracma::detrend(range_doy))
rcorr(mexicoareadetrend$MexicoArea,range_doy)


