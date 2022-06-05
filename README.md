# MonarchsJetStream
Here we identify when monarch migration is influenced by the jet stream and potential mechanisms for that influence. 

## Data, sources and citations:

### Monarch Data
We create 3 indices of monarch population at multiple points along the migration path: roosting indices in August and September were developed from Journey North database of citizen scientist observations; these roosting indices supplement the winter mexico area index, a population estimate conducted by WWF every winter (usually December) by measuring the amount of forest area the roosts are present at.

MonarchTimeSeries.csv summarizes all of the monarch time series in one file.

Mexico acreage in MonarchTimeSeries.csv and Butterflies1994-2020.txt and 2021 from recent [world wildlife fund- mexico annual report](https://files.worldwildlife.org/wwfcmsprod/files/Publication/file/7907txsoa8_Monarch_Butterfly_Survey_2021_2022_May24_2022_.pdf) 

monarch_journeynorth_Fall_Roost.csv shows Month,Day,Year,Latitude,Longitude,Number for roosting events observed by citizen scientists from 2002 to 2020. Number is not always recorded. This was manually downloaded for each year and month from the [Journey North map interface](https://maps.journeynorth.org/map/?map=monarch-roost-fall&year=2021) by clicking on the View Data tab. journeynorth.R creates figures to visualize where and the interannual occurrence of roosting events.


### Climate Data <!-- <h1 align="center">Climate Data</h1>  -->

#### Surface temperature (max,min), precipitation 
CRU TS v. 4.06 [https://crudata.uea.ac.uk/cru/data/hrg/](https://crudata.uea.ac.uk/cru/data/hrg/) [(Harris et al. 2020)](https://doi.org/10.1038/s41597-020-0453-3)

#### u and v wind velocities at the surface and multiple pressure levels
NCEP/NCAR reanalysis project (2.5 degree, monthly aggregates)
[Surface](https://www.psl.noaa.gov/data/gridded/data.ncep.reanalysis.derived.surface.html)
[Pressure Levels](https://psl.noaa.gov/data/gridded/data.ncep.reanalysis.pressure.html)

To generate the latitudinal position of the maximum zonal wind speeds at 300hPa (data/processed/NHJ_position_global_1948jan-2022apr_ncepncar.txt), scripts/generate_nhjposition_bylongitude.R used
data/raw/uwnd.mon.mean_NCEPNCAR_pressurelevels_1948jan2022apr.nc 

<!-- Kalnay et al.,The NCEP/NCAR 40-year reanalysis project, Bull. Amer. Meteor. Soc., 77, 437-470, 1996.
Please note: If you acquire NCEP Reanalysis Derived data products from PSL, we ask that you acknowledge us in your use of the data. This may be done by including text such as NCEP Reanalysis Derived data provided by the NOAA/OAR/ESRL PSL, Boulder, Colorado, USA, from their Web site at / in any documents or publications using these data. We would also appreciate receiving a copy of the relevant publications. -->

### Figures

jnroost89_mx_mapsanddetrendedtimeseries.png was created in powerpoint by merging timeseriesplots and maps_jn.png
