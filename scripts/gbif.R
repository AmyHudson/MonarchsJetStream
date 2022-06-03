#gbif 


gbif <- read.delim("data/raw/0292940-200613084148143.csv") #1994-2020
gbif <- gbif[which(gbif$year>=2002),]

png("figures/gbif_2002-2020.png",2,7,
    units = "in",res = 600, pointsize=12, family= "helvetica") #
par(mfrow=c(4,1), mar = c(0,0,0,0))

for (i in c(8,9,12)){
  
  gbif1 <- gbif[which(gbif$month == i),]
  
  
  map("worldHires",c("Canada", "USA","Mexico"), xlim=c(-125,-65),ylim=c(15,70), fill=F,lwd = 1, add = F)  #plot the region of Canada
  map.axes()
  
  ## add points
  points(gbif1$decimalLongitude,gbif1$decimalLatitude, cex = 0.3, pch = 16, col = adjustcolor("grey", alpha.f=0.1))
  
  sub <- gbif1[which(gbif1$decimalLongitude >-105),]
  sub <- sub[-which(sub$decimalLatitude <33 & sub$decimalLongitude >-85),]
  
  points(sub$decimalLongitude,sub$decimalLatitude, cex = 0.3, pch = 16, col = adjustcolor("orange", alpha.f=0.2))
  points(mean(sub$decimalLongitude,na.rm = T), mean(sub$decimalLatitude, na.rm = T), pch=20, col="black",cex=2) 
}
dev.off()
