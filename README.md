# MonarchsJetStream
This repository is for identifying monarch and jet stream relationships.



## Data, sources and citations:

### Monarch Data
MonarchTimeSeries.csv summarizes all of the monarch time series in one file.

Butterflies1994-2020.txt and 2021 from recent [world wildlife fund- mexico annual report](https://files.worldwildlife.org/wwfcmsprod/files/Publication/file/7907txsoa8_Monarch_Butterfly_Survey_2021_2022_May24_2022_.pdf) 

monarch_journeynorth_Fall_Roost.csv shows Month,Day,Year,Latitude,Longitude,Number for roosting events observed by citizen scientists from 2002 to 2020. Number is not always recorded. This was manually downloaded for each year and month from the [Journey North map interface](https://maps.journeynorth.org/map/?map=monarch-roost-fall&year=2021) by clicking on the View Data tab. journeynorth.R creates figures to visualize where and the interannual occurrence of roosting events.



### Climate Data

#### Surface temperature, precipitation, spei, 

#### u and v wind velocities at Surface and Pressure levels

Kalnay et al.,The NCEP/NCAR 40-year reanalysis project, Bull. Amer. Meteor. Soc., 77, 437-470, 1996.

Please note: If you acquire NCEP Reanalysis Derived data products from PSL, we ask that you acknowledge us in your use of the data. This may be done by including text such as NCEP Reanalysis Derived data provided by the NOAA/OAR/ESRL PSL, Boulder, Colorado, USA, from their Web site at / in any documents or publications using these data. We would also appreciate receiving a copy of the relevant publications.

Download from https://www.psl.noaa.gov/data/gridded/data.ncep.reanalysis.derived.surface.html on 5/25/2022

I also downloaded u and v wind at different atmospheric pressures from https://psl.noaa.gov/data/gridded/data.ncep.reanalysis.pressure.html on 5/25/2022

To generate the latitudinal position of the maximum zonal wind speeds at 300hPa (data/processed/NHJ_position_global_1948jan-2022apr_ncepncar.txt), scripts/generate_nhjposition_bylongitude.R used
data/raw/uwnd.mon.mean_NCEPNCAR_pressurelevels_1948jan2022apr.nc 


