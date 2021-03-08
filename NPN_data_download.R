# pulling NPN data for Amy H's monarch-jet stream analysis
# TCrimmins 12-22-20
# Bounding box: 20N to 40N; -104E to -90E

library(rnpn)

# download individual-level phenometrics
# all flowering data
# subset to bounding box + 2010-2020

#NPNdata <- npn_download_individual_phenometrics(request_source = "TCrimmins", years = c(1993:2019),
#                                     coords = c(20, -104, 40, -90))
NPNdata <- npn_download_status_data(request_source = "TCrimmins", years = c(1993:2019),
                                                coords = c(20, -104, 40, -90))
# subset to flowering
unique(NPNdata$phenophase_description)

NNflowering <- subset(NPNdata, phenophase_description=="Open flowers (1 location)" | phenophase_description=="Open flowers" |
                        phenophase_description=="Open flowers (lilac)")

# all flowering within the September through November window
NNflowering <- NNflowering[which(NNflowering$day_of_year<=334 & NNflowering$day_of_year>=244),]

summary(NNflowering)

# group by individual_id
length(unique(NNflowering$individual_id))
length(unique(NNflowering$site_id))
#########################################
#fraction of yes to no per individual (by site?)- this doesn't work because 
# there are different periods of observation for each individual and site, so 
# ratio will be different
datetxt <- as.Date(NNflowering$observation_date)


# intensity
df <- as.data.frame(cbind(NNflowering$site_id,NNflowering$individual_id,NNflowering$phenophase_status,NNflowering$intensity_value,as.numeric(format(datetxt, format = "%Y"))))
colnames(df) <- c("site_id","individual_id","phenophase_status","intensity_value","year")
df
# intensity of individuals flowering
df1 <- df[which(df$intensity_value != -9999),]
unique(df1$intensity_value)
df1[which(df1$intensity_value==c("Less than 5%")),6] <- 0.01
df1[which(df1$intensity_value==c("5-24%")),6] <- 0.05
df1[which(df1$intensity_value==c("25-49%")),6] <- 0.25
df1[which(df1$intensity_value==c("50-74%")),6] <- 0.50
df1[which(df1$intensity_value==c("75-94%")),6] <- 0.75
df1[which(df1$intensity_value==c("95% or more")),6] <- 1



###############
df <- as.data.frame(cbind(NNflowering$site_id,NNflowering$individual_id,NNflowering$phenophase_status,as.numeric(format(datetxt, format = "%Y"))))
colnames(df) <- c("site_id","individual_id","phenophase_status","year")
df
# removed phenophase status that were (-1)- which designate uncertainty
df <- df[which(df$phenophase_status != -1),]
# number of individuals flowering by year in migration pathway (at least 1 day)
df1 <- df[which(df$phenophase_status == 1),]
install.packages("dplyr")
library(dplyr)
x <- aggregate(df1,
               by = list(df1$year),
               FUN = n_distinct,
               na.rm = T)
plot(x$Group.1,x$individual_id, type = "b", 
     xlab = "", 
     ylab = "# of Individuals Flowering", 
     main = c("Individual in flower at least 1 day "))
# number of sites with individuals flowering by year
plot(x$Group.1,x$site_id, type = "b", 
     xlab = "", 
     ylab = "# of Sites w Individuals Flowering", 
     main = c("Sites w Individual in flower at least 1 day "))

# number of days per year where at least 1 individual in flower
x <- aggregate(df1,
               by = list(df1$year),
               FUN = sum,
               na.rm=T)
plot(x$Group.1,x$phenophase_status, type = "b", 
     xlab = "", 
     ylab = "Number of Days", 
     main = c("At least 1 individual in flower"))


#what proportion of individuals at a site were in flower


########################################
#write out file so I can plot in excel (sorry!!)
#write.csv(NNflowering,"c:/NPN/Manuscripts/Working/Hudson_etal_monarch_stuff/NNflowering.csv")

# png("npn_flowering.png",6,6, type = "cairo",
#     #png("output/SPEI-3m_site.png",4,6, type = "cairo",
#     #png("output/SurfacePrecip_20thCent_1.5sd.png",4,6,
#     units = "in",res = 600, pointsize=10, family= "helvetica")
# par(mfrow=c(3,1),mar = c(2, 4.1, 1, 0.5)) #c(5.1, 4.1, 4.1, 2.1) c(bottom, left, top, right))

NNflowering1 <- NNflowering

datetxt <- as.Date(NNflowering1$observation_date)
df <- data.frame(date = datetxt,
                 year = as.numeric(format(datetxt, format = "%Y")),
                 month = as.numeric(format(datetxt, format = "%m")),
                 day = as.numeric(format(datetxt, format = "%d")))
df1 <- as.data.frame(cbind(df$year,NNflowering1$day_of_year,NNflowering1$phenophase_status))
colnames(df1) <- c("Year","DOY","Status")
df1$Status <- as.character(df1$Status)
#x <- boxplot(day_of_year ~ df$year,data = NNflowering, main="Open Flowers in bounding box", xlab="Year", ylab="DOY")

library(ggplot2)
#box_plot <- 
#Add the geometric object box plot
# ggplot(df1, aes(x = Year, group = Year, y = DOY)) +
#   xlab("Year") +
#   ylab("DOY") +
#   geom_boxplot(aes(Status == "1")) +
#   #geom_jitter(shape = 15,
#   #            color = "light grey",
#   #            size = 0.5,
#   #            position = position_jitter(width = 0.21)) +
#   geom_jitter(aes(Year, DOY, colour = Status), 
#               position = position_jitter(width = .21, height=-0.7),
#               size=2) +
#   scale_color_manual(name="Legend", 
#                      values=rev(c("green2", "light grey", "black"))) +
#   scale_x_discrete(limits = 2009:2019) +
#   scale_y_continuous(limits=c(244, 334), 
#                      breaks = seq(244, 334, 10)) +
#   theme_classic()
#   
#   
  
  
###########################
# color by -1,0, and 1

NNflowering1 <- NNflowering[which(NNflowering$phenophase_status == 1),]

datetxt <- as.Date(NNflowering1$observation_date)
df <- data.frame(date = datetxt,
                 year = as.numeric(format(datetxt, format = "%Y")),
                 month = as.numeric(format(datetxt, format = "%m")),
                 day = as.numeric(format(datetxt, format = "%d")))
df1 <- as.data.frame(cbind(df$year,NNflowering1$day_of_year,NNflowering1$phenophase_status))
colnames(df1) <- c("Year","DOY","Status")
df2 <- df1

#x <-hist(NNflowering$phenophase_status)
#x$counts


ggplot(df1, aes(x = Year, group = Year, y = DOY)) +
  xlab("Year") +
  ylab("DOY") +
  geom_boxplot(aes(Year, DOY)) +
  geom_jitter(shape = 15,
              color = "red",
              size = 0.5,
              position = position_jitter(width = 0.21)) +
  scale_x_discrete(limits = 2009:2019) +
  scale_y_continuous(limits=c(244, 334), 
                     breaks = seq(240, 340, 20)) +
  theme_classic()

NNflowering1 <- NNflowering[which(NNflowering$phenophase_status == 0),]

datetxt <- as.Date(NNflowering1$observation_date)
df <- data.frame(date = datetxt,
                 year = as.numeric(format(datetxt, format = "%Y")),
                 month = as.numeric(format(datetxt, format = "%m")),
                 day = as.numeric(format(datetxt, format = "%d")))
df1 <- as.data.frame(cbind(df$year,NNflowering1$day_of_year,NNflowering1$phenophase_status))
colnames(df1) <- c("Year","DOY","Status")


#x <-hist(NNflowering$phenophase_status)
#x$counts


ggplot(df1, aes(x = Year, group = Year, y = DOY)) +
  xlab("Year") +
  ylab("DOY") +
  geom_boxplot(aes(Year, DOY)) +
  geom_jitter(shape = 15,
              color = "grey",
              size = 0.5,
              position = position_jitter(width = 0.21)) +
  scale_x_discrete(limits = 2009:2019) +
  scale_y_continuous(limits=c(244, 334), 
                     breaks = seq(240, 340, 20)) +
  theme_classic()

###############
# divide the number of 1s per region by the number of 0s per region for each year

# df2 = 1s
# df1 = 0s
library(dplyr)
df2 <- group_by(df2,Year) %>% summarise(Status = sum(Status))
df1$Status <- rep(1,length(df1[,1]))
df1 <- group_by(df1,Year) %>% summarise(Status = sum(Status))
df2$Status/df1$Status
plot(2009:2019,df2$Status/df1$Status, type = "b", xlab = "", ylab = "1s/0s")
df2$Status/(df1$Status+df2$Status)
plot(2009:2019,df2$Status/(df1$Status+df2$Status), type = "b", xlab = "", ylab = "1s/(1s+0s)", main = "#flowers observed/\n#observations attempted")


###############

# NNflowering1 <- NNflowering[which(NNflowering$phenophase_status == -1),]
# 
# datetxt <- as.Date(NNflowering1$observation_date)
# df <- data.frame(date = datetxt,
#                  year = as.numeric(format(datetxt, format = "%Y")),
#                  month = as.numeric(format(datetxt, format = "%m")),
#                  day = as.numeric(format(datetxt, format = "%d")))
# df1 <- as.data.frame(cbind(df$year,NNflowering1$day_of_year,NNflowering1$phenophase_status))
# colnames(df1) <- c("Year","DOY","Status")
# 

#x <-hist(NNflowering$phenophase_status)
#x$counts


# ggplot(df1, aes(x = Year, group = Year, y = DOY)) +
#   xlab("Year") +
#   ylab("DOY") +
#   geom_boxplot(aes(Year, DOY)) +
#   geom_jitter(shape = 15,
#               color = "black",
#               size = 0.5,
#               position = position_jitter(width = 0.21)) +
#   scale_x_discrete(limits = 2009:2019) +
#   scale_y_continuous(limits=c(244, 334), 
#                      breaks = seq(240, 340, 20)) +
#   theme_classic()

