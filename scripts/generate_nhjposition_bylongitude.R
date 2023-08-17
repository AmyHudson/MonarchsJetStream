# modified from Flurin's script: position_jetstream_global_new.R
# downloaded U-Wind Monthly Mean Pressure: uwnd.mon.mean.nc from https://psl.noaa.gov/data/gridded/data.ncep.reanalysis.pressure.html , covering jan1948-apr2022, and renamed

library(ncdf4)


file.ncep <- nc_open("data/raw/uwnd.mon.mean_NCEPNCAR_pressurelevels_1948jan2022apr.nc") # uwnd.mon.mean_NCEPNCAR_pressurelevels.nc
data.ncep <- ncvar_get(file.ncep)

lat.ncep <- file.ncep$dim[[2]]$vals; 
lon.ncep <- file.ncep$dim[[3]]$vals
lev.ncep <- file.ncep$dim[[1]]$vals

data.ncep.nh <- data.ncep[,which(lat.ncep >= 0),which(lev.ncep == 300),]

maxpos.ncep <- apply(data.ncep.nh, c(1,3), which.max) 
#maxspeed.ncep <- apply(data.ncep.nh, c(1,3), max, na.rm = T)

maxlocation.ncep <- maxpos.ncep

for(x in 1:dim(maxpos.ncep)[1]){for(y in 1:dim(maxpos.ncep)[2]){maxlocation.ncep[x,y] <- lat.ncep[which(lat.ncep >= 0)][maxpos.ncep[x,y]]}}

#maxspeed.ncep.ts <- ts(t(maxspeed.ncep), start = 1948, frequency = 12)
maxlocation.ncep.ts <- ts(t(maxlocation.ncep), start = 1948, frequency = 12)
maxlocation.ncep.ts <- maxlocation.ncep.ts[1:888,]

write.table(maxlocation.ncep.ts, file = "data/processed/NHJ_position_global_1948-2021_ncepncar.txt", sep = "\t", col.names = paste(as.character(lon.ncep),"_degE",sep=""), row.names = F, quote = F)

maxlocation.ncep.ts <- maxlocation.ncep.ts[1:876,]

write.table(maxlocation.ncep.ts, file = "data/processed/NHJ_position_global_1948-2020_ncepncar.txt", sep = "\t", col.names = paste(as.character(lon.ncep),"_degE",sep=""), row.names = F, quote = F)

