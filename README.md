# MonarchsJetStream
Here we identify when monarch migration is influenced by the jet stream and potential mechanisms for that influence. 

![Monthly monarch and jet stream movement](/figures/allmonths_gbifcentroid_jetpos.png)
Adult monarch observations from GBIF.com (orange points) show the monthly movement of the eastern migration- overlapping with the position of the monthly jet stream.
<!-- need to add legend to this -->

## Data, sources and citations:

### Monarch Data
Monarch's roost during their fall migration and overwintering, potentially to protect against frost and predation. Because roosts represent multiple individuals, sometimes in the thousands, we leveraged roost observations over individual monarch observations. In August, roosting events are clustered in the Midwest and Northeast (A). In September, roosting observations shift to along the migration corridor (B). By December, monarchs have clustered in the high-elevation roosts in and around the Mexico Butterfly Biosphere Reserve in south central Mexico (C).

We created [3 indices of monarch population](/data/processed/MonarchTimeSeries.csv) at multiple points along the fall migration path. [Monarch roosting observations recorded by Journey North citizen scientists](/data/raw/monarch_journeynorth_Fall_Roost.csv) were downloaded for each year (2002-2021) from the [Journey North map interface](https://maps.journeynorth.org/map/?map=monarch-roost-fall&year=2021) by clicking on the View Data tab. These observations consist of the Month, Day, Year, Latitude, Longitude, and Number for each roosting event; number of monarchs in roost is not always recorded. August (A) and September (B) Roost indices were created by summing the number of roosting observations by month for each year, and removing the linear trend to focus on interannual variability. These indices supplement the winter Mexico area (C), a population estimate conducted by WWF every winter (usually December) by measuring the amount of forest area the roosts are present, found in the most recent [world wildlife fund- mexico annual report](https://files.worldwildlife.org/wwfcmsprod/files/Publication/file/7907txsoa8_Monarch_Butterfly_Survey_2021_2022_May24_2022_.pdf) and also detrended. 

![3 monarch roosting time series](/figures/jnroost89_mx_mapsanddetrendedtimeseries2.png)

August and September roosts are strongly correlated, while September roosts are more strongly correlated with Mexico area in December.

### Climate Data <!-- <h1 align="center">Climate Data</h1>  -->

#### Surface temperature (max,min), precipitation 
CRU TS v. 4.06 [https://crudata.uea.ac.uk/cru/data/hrg/](https://crudata.uea.ac.uk/cru/data/hrg/) [(Harris et al. 2020)](https://doi.org/10.1038/s41597-020-0453-3)

#### u and v wind velocities at the surface and multiple pressure levels
NCEP/NCAR reanalysis project (2.5 degree, monthly aggregates)
[Surface](https://www.psl.noaa.gov/data/gridded/data.ncep.reanalysis.derived.surface.html)
[Pressure Levels](https://psl.noaa.gov/data/gridded/data.ncep.reanalysis.pressure.html)

To generate the [latitudinal position of the maximum zonal wind speeds at 300hPa](data/processed/NHJ_position_global_1948jan-2022apr_ncepncar.txt), this [script](/scripts/generate_nhjposition_bylongitude.R) used [raw values from the NCEP NCAR group](/data/raw/uwnd.mon.mean_NCEPNCAR_pressurelevels_1948jan2022apr.nc).

<!-- Kalnay et al.,The NCEP/NCAR 40-year reanalysis project, Bull. Amer. Meteor. Soc., 77, 437-470, 1996.
Please note: If you acquire NCEP Reanalysis Derived data products from PSL, we ask that you acknowledge us in your use of the data. This may be done by including text such as NCEP Reanalysis Derived data provided by the NOAA/OAR/ESRL PSL, Boulder, Colorado, USA, from their Web site at / in any documents or publications using these data. We would also appreciate receiving a copy of the relevant publications. -->
