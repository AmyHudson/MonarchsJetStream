# 

p2 <- p %>% 
  as.vector() %>% 
  matrix(length(longitude),length(latitude), 
         byrow = TRUE) %>% 
  unlist()

rotate3 <- SpatialPixelsDataFrame(points = expand.grid(longitude,latitude), 
                                  data = as.data.frame(p2), 
                                  proj4string = CRS(proj4string(dset))) %>% 
  raster()

plot(rotate3)


allclim_anomalies <- precip
colnames(allclim_anomalies) <- c("x","y","state","Months","Precip")

allclim_anomalies[,6] <- tmin[,5]
allclim_anomalies[,7] <- tmax[,5]
#allclim_anomalies[,7] <- spei[,4]


allclim_anomalies[,4] <- factor(precip$Months, levels = c("September", "October", "November",  "December",  "January",   "February",  "March",     "April", "May", "June" ))
#colnames(allclim_anomalies) <- c("longitude","latitude", "Months", "Precip","Tmin","Tmax","SPEI")
colnames(allclim_anomalies) <- c("longitude","latitude", "state","Months", "Precip","Tmin","Tmax")

allclim_anomalies <- allclim_anomalies %>% 
  pivot_longer(names_to = "clim_var", values_to = "pval", Precip:Tmax) 
allclim_anomalies2 <- spei
allclim_anomalies2[,5] <- rep("SPEI",dim(spei)[1])
allclim_anomalies2[,6] <- spei$SPEI
colnames(allclim_anomalies2) <- colnames(allclim_anomalies)

allclim_anomalies3 <- rbind(allclim_anomalies,allclim_anomalies2)
allclim_anomalies4<- allclim_anomalies3[complete.cases(allclim_anomalies3),]

#write.csv(allclim_anomalies4,"data/processed/allclim_pvals_NJ_entry_years_no2014.csv", row.names = F)