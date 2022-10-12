# Monarchs and the Jet Stream:

The seasonal progression of the eastern monarch migration matches the seasonal climatology of the northern hemisphere jet stream (NHJ). We know from previous studies that changes in the NHJ position can influence weather events and vegetation phenology at the surface. In this study we examine when the eastern monarch migration is influenced most by the jet stream and potential mechanisms for that influence. We define the jet stream as the [latitude of the maximum zonal wind speeds at 300hPa](data/processed/NHJ_position_global_1948jan-2022apr_ncepncar.txt) for each longitude band, generated by this [script](/scripts/generate_nhjposition_bylongitude.R) using [raw values from NCEP/NCAR](/data/raw/uwnd.mon.mean_NCEPNCAR_pressurelevels_1948jan2022apr.nc).

![Monthly monarch and jet stream movement](/figures/allmonths_gbifcentroid_jetpos.png)
Fig 1. Adult monarch observations (orange points) from GBIF 1994-2020 show the monthly movement of the eastern migration. The mean latitudinal position of the jet stream (black line) from 1994-2020 is smoothed with a loess filter across longitudes, with the grey polygon designating the minimum and maximum latitude over that period.
<!-- need to add legend to this; convert to ggplot -->

Monarchs roost during fall migration and overwintering, potentially to protect against frost and predation. We leveraged roost observations over individual monarch observations because roosts represent multiple individuals, sometimes in the thousands, and may better represent greater population values. We created [3 indices of monarch population](/data/processed/MonarchTimeSeries.csv) at multiple points along the fall migration. [Monarch roosting observations recorded by Journey North citizen scientists](/data/raw/monarch_journeynorth_Fall_Roost.csv) were downloaded for each available year (2002-2021) from the [Journey North map interface](https://maps.journeynorth.org/map/?map=monarch-roost-fall&year=2021) by clicking on the View Data tab. These observations consist of the Month, Day, Year, Latitude, Longitude, and Number for each roosting event; number of monarchs in roost is not always recorded so we only included the location of the roost as a single event and ignored the number observed in the roost. August and September Roost indices were created by summing the number of roosting observation locations by month for each year from Journey North observations, and removing the linear trend to focus on interannual variability. These indices supplement the Mexico winter roosts, a monarch population estimate conducted by the World Wildlife Fund (WWF) every winter (usually in December) by measuring the amount of forest area the monarch roosts are present at known habitats in and around the Mexico Butterfly Biosphere Reserve in south central Mexico's high elevation forests, found in the most recent [WWF- mexico annual report](https://files.worldwildlife.org/wwfcmsprod/files/Publication/file/7907txsoa8_Monarch_Butterfly_Survey_2021_2022_May24_2022_.pdf) and also linearly detrended. 

![3 monarch roosting time series](/figures/jnroost89_mx_mapsanddetrendedtimeseries2.png)
Fig 2. Locations and time series of all August (A), September (B), and December (C) roosting events. In August, roosting events are clustered in the Midwest and Northeast. In September, roosting observations shift to along the migration corridor. By December, monarchs have clustered in the high-elevation roosts in and around the Mexico Butterfly Biosphere Reserve in south central Mexico. (D) August and September roosts are strongly correlated after being detrended, while September roosts are more strongly correlated with estimates of roosting area in Mexico in December. Roost values for October and November showed less significant relationships with either August, September, or December, and were not included.
 
To determine potential mechanisms for how the jet stream could influence these roosting events, we explored relationships between the monarch roost numbers and monthly climate (min and max temperatures, precipitation, drought from [CRU TS v. 4.06](https://crudata.uea.ac.uk/cru/data/hrg/) [(Harris et al. 2020)](https://doi.org/10.1038/s41597-020-0453-3) and surface wind velocities from [NCEP/NCAR reanalysis](https://www.psl.noaa.gov/data/gridded/data.ncep.reanalysis.derived.surface.html)) and vegetation greenness (NDVI from [NOAA's CDR AVHRR product](https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.ncdc:C01558)). We also correlated these variables with the jet stream longitudinal bands significantly related to the monarch roosts.

<!-- Need to point to data files in Repo

## Data, sources and citations:
### Monarch Data
### Climate Data <!-- <h1 align="center">Climate Data</h1>  -->
#### Surface temperature (max,min), precipitation, drought
#### u and v wind velocities at the surface and multiple pressure levels
NCEP/NCAR reanalysis project (2.5 degree, monthly aggregates)
[Surface](https://www.psl.noaa.gov/data/gridded/data.ncep.reanalysis.derived.surface.html)
[Pressure Levels](https://psl.noaa.gov/data/gridded/data.ncep.reanalysis.pressure.html)

 -->

<!-- Kalnay et al.,The NCEP/NCAR 40-year reanalysis project, Bull. Amer. Meteor. Soc., 77, 437-470, 1996.
Please note: If you acquire NCEP Reanalysis Derived data products from PSL, we ask that you acknowledge us in your use of the data. This may be done by including text such as NCEP Reanalysis Derived data provided by the NOAA/OAR/ESRL PSL, Boulder, Colorado, USA, from their Web site at / in any documents or publications using these data. We would also appreciate receiving a copy of the relevant publications. -->
