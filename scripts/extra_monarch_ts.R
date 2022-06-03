# extra monarch time series and explorations with jet stream and correlation with winter acreage

################
# playing w detrending the Durham Fall roost time series
monarch_ts2 <- monarch_ts[which(monarch_ts$Year>=1998 & monarch_ts$Year<=2020),]
#detrend each index
monarch_ts_detrend1 <- data.frame(pracma::detrend(as.matrix(monarch_ts2))) 
rcorr(monarch_ts_detrend1$Mexico,monarch_ts_detrend1$Xerces_CA_Nov_OverWintering)
plot(1998:2020,scale(monarch_ts_detrend1$Durham_CA_OctDec_FallRoost), ylim = c(-2,3), type = "h", lwd = "3")
lines(1998:2020,scale(monarch_ts_detrend1$Xerces_CA_Nov_OverWintering), lty = c(2))
lines(1998:2020,scale(monarch_ts_detrend1$Mexico))
abline(h =0)

legend("top", legend = c("Durham Fall Roost","Xerces Overwintering","Mexico Winter Acreage"), col = c("black","black","black"), lty = c(1,2,1), lwd = c(3,1,1),bg = "white",cex = 0.70, bty = "n")

# try and detrend Durham values
x<- lm(monarch_ts$Year~monarch_ts$Durham_CA_OctDec_FallRoost)
as.numeric(x$residuals)
y <- c(-15.8412578, -15.5984845, NA,  NA  ,-11.1285739,  -2.4226792 , -8.1040908,  NA,  NA,  NA, 4.3590306,  NA,   -4.0639715,  NA,  NA, -1.0639715,   0.4259987,  NA,  NA,  NA,  3.8469430,  NA,  NA,  NA,  8.5596269 ,  9.4705414,   9.6242293,  10.4906010,  11.4460583)
z<- lm(monarch_ts$Year~y)
# dplR doesn't let you have NAs in the middle of the series... I could play with the code to allow it...
#library(dplR)
#detrend.series(monarch_ts$Durham_CA_OctDec_FallRoost)
z <- c(1.2412578 , 1.9984845, NA,  NA  ,0.5285739,  -7.1773208 , -0.4959092,  NA,  NA,  NA, -8.9590306,  NA,   1.4639715,  NA,  NA, 1.4639715,   0.9740013,  NA,  NA,  NA,  1.5530570,  NA,  NA,  NA,   0.8403731 , 0.9294586,  1.7757707,  1.9093990 , 1.9539417)
rcorr(monarch_ts$Mexico,z)

##################
# playing w detrending the Durham Fall roost time series
monarch_ts2 <- monarch_ts[which(monarch_ts$Year>=2001 & monarch_ts$Year<=2017),]
#detrend each index
monarch_ts_detrend1 <- data.frame(pracma::detrend(as.matrix(monarch_ts2))) 
rcorr(monarch_ts_detrend1$Mexico,monarch_ts_detrend1$Xerces_CA_Nov_OverWintering)


plot(2002:2020,scale(monarch_ts_detrend1$Durham_CA_OctDec_FallRoost), ylim = c(-2,3), type = "h", lwd = "3")
lines(2002:2020,scale(monarch_ts_detrend1$Xerces_CA_Nov_OverWintering), lty = c(2))
lines(2002:2020,scale(monarch_ts_detrend1$Mexico))
abline(h = 0)


legend("top", legend = c("Durham Fall Roost","Xerces Overwintering","Mexico Winter Acreage"), col = c("black","black","black"), lty = c(1,2,1), lwd = c(3,1,1),bg = "white",cex = 0.70, bty = "n")


# try and detrend Durham values
x<- lm(monarch_ts2$Year~monarch_ts2$Durham_CA_OctDec_FallRoost)
as.numeric(x$residuals)
x$residuals
y <- c( 0.1064104,  NA,  -8.7426189,  NA,  NA, -5.7426189,   -4.2326218,  NA,  NA,  NA,  -0.8353456 ,  NA,  NA,  NA ,3.9064683,  4.8137416,  4.9328375,  5.7937474)
rcorr(monarch_ts_detrend1$Mexico,y)


z<- lm(monarch_ts2$Year~y)
# dplR doesn't let you have NAs in the middle of the series... I could play with the code to allow it...
#library(dplR)
#detrend.series(monarch_ts$Durham_CA_OctDec_FallRoost)
z <- c(1.2412578 , 1.9984845, NA,  NA  ,0.5285739,  -7.1773208 , -0.4959092,  NA,  NA,  NA, -8.9590306,  NA,   1.4639715,  NA,  NA, 1.4639715,   0.9740013,  NA,  NA,  NA,  1.5530570,  NA,  NA,  NA,   0.8403731 , 0.9294586,  1.7757707,  1.9093990 , 1.9539417)
rcorr(monarch_ts$Mexico,z)
