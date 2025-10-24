## R script to get data for city shapefiles from eBird
# Will not run for anyone except Corey T. Callaghan, 
# however data outputs from this script are in the Intermediate Data folder

# WILL NOT RUN #

# get data from eBird
## packages
library(readr)
library(bigrquery)
library(dbplyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(sf)

# create connection with online database
con <- DBI::dbConnect(bigrquery::bigquery(),
                      dataset= "ebird",
                      project="ebird-database",
                      billing="ebird-database")

# create ebird table
ebird <- tbl(con, 'ebird_qa_june_2024')


## extract data
checklists_locations <- ebird %>%
  dplyr::filter(OBSERVATION_DATE >= "2019-01-01") %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER,
                LOCALITY_ID, LATITUDE, LONGITUDE, OBSERVATION_DATE, BCR_CODE,
                TIME_OBSERVATIONS_STARTED, OBSERVER_ID, PROTOCOL_TYPE, DURATION_MINUTES, 
                EFFORT_DISTANCE_KM, EFFORT_AREA_HA, NUMBER_OBSERVERS, GROUP_IDENTIFIER,
                COUNTRY, STATE_CODE) %>%
  dplyr::filter(PROTOCOL_TYPE=="Stationary") %>%
  dplyr::filter(NUMBER_OBSERVERS==1) %>%
  dplyr::filter(DURATION_MINUTES<=240) %>%
  dplyr::filter(DURATION_MINUTES>=5) %>%
  distinct() %>%
  collect(n=Inf)

# convert checklists to sf
checklists_sf <- checklists_locations %>%
  st_as_sf(crs=4326, coords=c("LONGITUDE", "LATITUDE"))

# write out shapefile
st_write(checklists_sf, "Shapefiles/ebird_checklists_sample_shp/ebird_checklists_sample_shp.shp")

# now get only the locations
locations <- checklists_locations %>%
  dplyr::select(LOCALITY_ID, LONGITUDE, LATITUDE) %>%
  distinct()

# convert locations to sf
locations_sf <- locations %>%
  st_as_sf(crs=4326, coords=c("LONGITUDE", "LATITUDE"))

# write out shapefile
st_write(locations_sf, "Shapefiles/ebird_locations_sample_shp/ebird_locations_sample_shp.shp")


