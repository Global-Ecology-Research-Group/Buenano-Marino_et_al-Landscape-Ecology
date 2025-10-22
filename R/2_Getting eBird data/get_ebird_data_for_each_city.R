# an R script to get eBird data for each city for analysis

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

# read in locations
locations <- readRDS("Intermediate data/locations_in_cities.RDS")

# read in cities for inclusion
cities_for_inclusion <- read_csv("Intermediate data/cities_for_analysis.csv")

# for now we'll just try to get 25 cities at a time and try to minimize the number
# of queries we have to do to save money...

# first 25 cities
cities_chunk_1 <- locations %>%
  dplyr::filter(NAME20 %in% cities_for_inclusion$NAME20) %>%
  group_by(NAME20) %>%
  summarize(N=n()) %>%
  arrange(N)

localities_chunk_1 <- locations %>%
  dplyr::filter(NAME20 %in% c(cities_chunk_1 %>%
                  slice(1:25) %>%
                  .$NAME20))

## extract data
chunk_1_data <- ebird %>%
  dplyr::filter(COUNTRY=="United States") %>%
  dplyr::filter(OBSERVATION_DATE >= "2019-01-01") %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, COMMON_NAME, SCIENTIFIC_NAME, OBSERVATION_COUNT, CATEGORY,
                LOCALITY_ID, LATITUDE, LONGITUDE, OBSERVATION_DATE, BCR_CODE,
                TIME_OBSERVATIONS_STARTED, OBSERVER_ID, PROTOCOL_TYPE, DURATION_MINUTES, 
                EFFORT_DISTANCE_KM, EFFORT_AREA_HA, NUMBER_OBSERVERS, GROUP_IDENTIFIER,
                COUNTRY, STATE_CODE) %>%
  dplyr::filter(PROTOCOL_TYPE=="Stationary") %>%
  dplyr::filter(NUMBER_OBSERVERS==1) %>%
  dplyr::filter(DURATION_MINUTES<=240) %>%
  dplyr::filter(DURATION_MINUTES>=5) %>%
  dplyr::filter(LOCALITY_ID %in% local(localities_chunk_1$LOCALITY_ID)) %>%
  collect(n=Inf)

# now break up this data to each individual city and export into a folder
break_up_data <- function(city_name){
  
  city_dat_loc <- locations %>%
    dplyr::filter(NAME20 == city_name)
  
  city_dat <- chunk_1_data %>%
    dplyr::filter(LOCALITY_ID %in% city_dat_loc$LOCALITY_ID)
  
  saveRDS(city_dat, paste0("Intermediate data/city_level_ebird_data/", city_name, ".RDS"))
  
}

cities_chunk_1_list <- locations %>%
  dplyr::filter(NAME20 %in% cities_for_inclusion$NAME20) %>%
  group_by(NAME20) %>%
  summarize(N=n()) %>%
  arrange(N) %>%
  slice(1:25)

lapply(unique(cities_chunk_1_list$NAME20), break_up_data)

################ next 25 cities!
cities_chunk_2 <- locations %>%
  dplyr::filter(NAME20 %in% cities_for_inclusion$NAME20) %>%
  group_by(NAME20) %>%
  summarize(N=n()) %>%
  arrange(N)

localities_chunk_2 <- locations %>%
  dplyr::filter(NAME20 %in% c(cities_chunk_2 %>%
                                slice(26:50) %>%
                                .$NAME20))

## extract data
chunk_2_data <- ebird %>%
  dplyr::filter(COUNTRY=="United States") %>%
  dplyr::filter(OBSERVATION_DATE >= "2019-01-01") %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, COMMON_NAME, SCIENTIFIC_NAME, OBSERVATION_COUNT, CATEGORY,
                LOCALITY_ID, LATITUDE, LONGITUDE, OBSERVATION_DATE, BCR_CODE,
                TIME_OBSERVATIONS_STARTED, OBSERVER_ID, PROTOCOL_TYPE, DURATION_MINUTES, 
                EFFORT_DISTANCE_KM, EFFORT_AREA_HA, NUMBER_OBSERVERS, GROUP_IDENTIFIER,
                COUNTRY, STATE_CODE) %>%
  dplyr::filter(PROTOCOL_TYPE=="Stationary") %>%
  dplyr::filter(NUMBER_OBSERVERS==1) %>%
  dplyr::filter(DURATION_MINUTES<=240) %>%
  dplyr::filter(DURATION_MINUTES>=5) %>%
  dplyr::filter(LOCALITY_ID %in% local(localities_chunk_2$LOCALITY_ID)) %>%
  collect(n=Inf)

# now break up this data to each individual city and export into a folder
break_up_data <- function(city_name){
  
  city_dat_loc <- locations %>%
    dplyr::filter(NAME20 == city_name)
  
  city_dat <- chunk_2_data %>%
    dplyr::filter(LOCALITY_ID %in% city_dat_loc$LOCALITY_ID)
  
  saveRDS(city_dat, paste0("Intermediate data/city_level_ebird_data/", city_name, ".RDS"))
  
}

cities_chunk_2_list <- locations %>%
  dplyr::filter(NAME20 %in% cities_for_inclusion$NAME20) %>%
  group_by(NAME20) %>%
  summarize(N=n()) %>%
  arrange(N) %>%
  slice(26:50)

lapply(unique(cities_chunk_2_list$NAME20), break_up_data)

################ next 25 cities!
cities_chunk_3 <- locations %>%
  dplyr::filter(NAME20 %in% cities_for_inclusion$NAME20) %>%
  group_by(NAME20) %>%
  summarize(N=n()) %>%
  arrange(N)

localities_chunk_3 <- locations %>%
  dplyr::filter(NAME20 %in% c(cities_chunk_3 %>%
                                slice(51:75) %>%
                                .$NAME20))

## extract data
chunk_3_data <- ebird %>%
  dplyr::filter(COUNTRY=="United States") %>%
  dplyr::filter(OBSERVATION_DATE >= "2019-01-01") %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, COMMON_NAME, SCIENTIFIC_NAME, OBSERVATION_COUNT, CATEGORY,
                LOCALITY_ID, LATITUDE, LONGITUDE, OBSERVATION_DATE, BCR_CODE,
                TIME_OBSERVATIONS_STARTED, OBSERVER_ID, PROTOCOL_TYPE, DURATION_MINUTES, 
                EFFORT_DISTANCE_KM, EFFORT_AREA_HA, NUMBER_OBSERVERS, GROUP_IDENTIFIER,
                COUNTRY, STATE_CODE) %>%
  dplyr::filter(PROTOCOL_TYPE=="Stationary") %>%
  dplyr::filter(NUMBER_OBSERVERS==1) %>%
  dplyr::filter(DURATION_MINUTES<=240) %>%
  dplyr::filter(DURATION_MINUTES>=5) %>%
  dplyr::filter(LOCALITY_ID %in% local(localities_chunk_3$LOCALITY_ID)) %>%
  collect(n=Inf)

# now break up this data to each individual city and export into a folder
break_up_data <- function(city_name){
  
  city_dat_loc <- locations %>%
    dplyr::filter(NAME20 == city_name)
  
  city_dat <- chunk_3_data %>%
    dplyr::filter(LOCALITY_ID %in% city_dat_loc$LOCALITY_ID)
  
  saveRDS(city_dat, paste0("Intermediate data/city_level_ebird_data/", city_name, ".RDS"))
  
}

cities_chunk_3_list <- locations %>%
  dplyr::filter(NAME20 %in% cities_for_inclusion$NAME20) %>%
  group_by(NAME20) %>%
  summarize(N=n()) %>%
  arrange(N) %>%
  slice(51:75)

lapply(unique(cities_chunk_3_list$NAME20), break_up_data)

################ next 25 cities!
cities_chunk_4 <- locations %>%
  dplyr::filter(NAME20 %in% cities_for_inclusion$NAME20) %>%
  group_by(NAME20) %>%
  summarize(N=n()) %>%
  arrange(N)

localities_chunk_4 <- locations %>%
  dplyr::filter(NAME20 %in% c(cities_chunk_4 %>%
                                slice(76:100) %>%
                                .$NAME20))

## extract data
chunk_4_data <- ebird %>%
  dplyr::filter(COUNTRY=="United States") %>%
  dplyr::filter(OBSERVATION_DATE >= "2019-01-01") %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, COMMON_NAME, SCIENTIFIC_NAME, OBSERVATION_COUNT, CATEGORY,
                LOCALITY_ID, LATITUDE, LONGITUDE, OBSERVATION_DATE, BCR_CODE,
                TIME_OBSERVATIONS_STARTED, OBSERVER_ID, PROTOCOL_TYPE, DURATION_MINUTES, 
                EFFORT_DISTANCE_KM, EFFORT_AREA_HA, NUMBER_OBSERVERS, GROUP_IDENTIFIER,
                COUNTRY, STATE_CODE) %>%
  dplyr::filter(PROTOCOL_TYPE=="Stationary") %>%
  dplyr::filter(NUMBER_OBSERVERS==1) %>%
  dplyr::filter(DURATION_MINUTES<=240) %>%
  dplyr::filter(DURATION_MINUTES>=5) %>%
  dplyr::filter(LOCALITY_ID %in% local(localities_chunk_4$LOCALITY_ID)) %>%
  collect(n=Inf)

# now break up this data to each individual city and export into a folder
break_up_data <- function(city_name){
  
  city_dat_loc <- locations %>%
    dplyr::filter(NAME20 == city_name)
  
  city_dat <- chunk_4_data %>%
    dplyr::filter(LOCALITY_ID %in% city_dat_loc$LOCALITY_ID)
  
  saveRDS(city_dat, paste0("Intermediate data/city_level_ebird_data/", city_name, ".RDS"))
  
}

cities_chunk_4_list <- locations %>%
  dplyr::filter(NAME20 %in% cities_for_inclusion$NAME20) %>%
  group_by(NAME20) %>%
  summarize(N=n()) %>%
  arrange(N) %>%
  slice(76:100)

lapply(unique(cities_chunk_4_list$NAME20), break_up_data)

################ next 25 cities!
cities_chunk_5 <- locations %>%
  dplyr::filter(NAME20 %in% cities_for_inclusion$NAME20) %>%
  group_by(NAME20) %>%
  summarize(N=n()) %>%
  arrange(N)

localities_chunk_5 <- locations %>%
  dplyr::filter(NAME20 %in% c(cities_chunk_5 %>%
                                slice(101:125) %>%
                                .$NAME20))

## extract data
chunk_5_data <- ebird %>%
  dplyr::filter(COUNTRY=="United States") %>%
  dplyr::filter(OBSERVATION_DATE >= "2019-01-01") %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, COMMON_NAME, SCIENTIFIC_NAME, OBSERVATION_COUNT, CATEGORY,
                LOCALITY_ID, LATITUDE, LONGITUDE, OBSERVATION_DATE, BCR_CODE,
                TIME_OBSERVATIONS_STARTED, OBSERVER_ID, PROTOCOL_TYPE, DURATION_MINUTES, 
                EFFORT_DISTANCE_KM, EFFORT_AREA_HA, NUMBER_OBSERVERS, GROUP_IDENTIFIER,
                COUNTRY, STATE_CODE) %>%
  dplyr::filter(PROTOCOL_TYPE=="Stationary") %>%
  dplyr::filter(NUMBER_OBSERVERS==1) %>%
  dplyr::filter(DURATION_MINUTES<=240) %>%
  dplyr::filter(DURATION_MINUTES>=5) %>%
  dplyr::filter(LOCALITY_ID %in% local(localities_chunk_5$LOCALITY_ID)) %>%
  collect(n=Inf)

# now break up this data to each individual city and export into a folder
break_up_data <- function(city_name){
  
  city_dat_loc <- locations %>%
    dplyr::filter(NAME20 == city_name)
  
  city_dat <- chunk_5_data %>%
    dplyr::filter(LOCALITY_ID %in% city_dat_loc$LOCALITY_ID)
  
  saveRDS(city_dat, paste0("Intermediate data/city_level_ebird_data/", city_name, ".RDS"))
  
}

cities_chunk_5_list <- locations %>%
  dplyr::filter(NAME20 %in% cities_for_inclusion$NAME20) %>%
  group_by(NAME20) %>%
  summarize(N=n()) %>%
  arrange(N) %>%
  slice(101:125)

lapply(unique(cities_chunk_5_list$NAME20), break_up_data)


################ next 25 cities!
cities_chunk_6 <- locations %>%
  dplyr::filter(NAME20 %in% cities_for_inclusion$NAME20) %>%
  group_by(NAME20) %>%
  summarize(N=n()) %>%
  arrange(N)

localities_chunk_6 <- locations %>%
  dplyr::filter(NAME20 %in% c(cities_chunk_6 %>%
                                slice(126:150) %>%
                                .$NAME20))

## extract data
chunk_6_data <- ebird %>%
  dplyr::filter(COUNTRY=="United States") %>%
  dplyr::filter(OBSERVATION_DATE >= "2019-01-01") %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, COMMON_NAME, SCIENTIFIC_NAME, OBSERVATION_COUNT, CATEGORY,
                LOCALITY_ID, LATITUDE, LONGITUDE, OBSERVATION_DATE, BCR_CODE,
                TIME_OBSERVATIONS_STARTED, OBSERVER_ID, PROTOCOL_TYPE, DURATION_MINUTES, 
                EFFORT_DISTANCE_KM, EFFORT_AREA_HA, NUMBER_OBSERVERS, GROUP_IDENTIFIER,
                COUNTRY, STATE_CODE) %>%
  dplyr::filter(PROTOCOL_TYPE=="Stationary") %>%
  dplyr::filter(NUMBER_OBSERVERS==1) %>%
  dplyr::filter(DURATION_MINUTES<=240) %>%
  dplyr::filter(DURATION_MINUTES>=5) %>%
  dplyr::filter(LOCALITY_ID %in% local(localities_chunk_6$LOCALITY_ID)) %>%
  collect(n=Inf)

# now break up this data to each individual city and export into a folder
break_up_data <- function(city_name){
  
  city_dat_loc <- locations %>%
    dplyr::filter(NAME20 == city_name)
  
  city_dat <- chunk_6_data %>%
    dplyr::filter(LOCALITY_ID %in% city_dat_loc$LOCALITY_ID)
  
  saveRDS(city_dat, paste0("Intermediate data/city_level_ebird_data/", city_name, ".RDS"))
  
}

cities_chunk_6_list <- locations %>%
  dplyr::filter(NAME20 %in% cities_for_inclusion$NAME20) %>%
  group_by(NAME20) %>%
  summarize(N=n()) %>%
  arrange(N) %>%
  slice(126:150)

lapply(unique(cities_chunk_6_list$NAME20), break_up_data)


################ next 50 cities!
cities_chunk_7 <- locations %>%
  dplyr::filter(NAME20 %in% cities_for_inclusion$NAME20) %>%
  group_by(NAME20) %>%
  summarize(N=n()) %>%
  arrange(N)

localities_chunk_7 <- locations %>%
  dplyr::filter(NAME20 %in% c(cities_chunk_7 %>%
                                slice(151:200) %>%
                                .$NAME20))

## extract data
chunk_7_data <- ebird %>%
  dplyr::filter(COUNTRY=="United States") %>%
  dplyr::filter(OBSERVATION_DATE >= "2019-01-01") %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, COMMON_NAME, SCIENTIFIC_NAME, OBSERVATION_COUNT, CATEGORY,
                LOCALITY_ID, LATITUDE, LONGITUDE, OBSERVATION_DATE, BCR_CODE,
                TIME_OBSERVATIONS_STARTED, OBSERVER_ID, PROTOCOL_TYPE, DURATION_MINUTES, 
                EFFORT_DISTANCE_KM, EFFORT_AREA_HA, NUMBER_OBSERVERS, GROUP_IDENTIFIER,
                COUNTRY, STATE_CODE) %>%
  dplyr::filter(PROTOCOL_TYPE=="Stationary") %>%
  dplyr::filter(NUMBER_OBSERVERS==1) %>%
  dplyr::filter(DURATION_MINUTES<=240) %>%
  dplyr::filter(DURATION_MINUTES>=5) %>%
  dplyr::filter(LOCALITY_ID %in% local(localities_chunk_7$LOCALITY_ID)) %>%
  collect(n=Inf)

# now break up this data to each individual city and export into a folder
break_up_data <- function(city_name){
  
  city_dat_loc <- locations %>%
    dplyr::filter(NAME20 == city_name)
  
  city_dat <- chunk_7_data %>%
    dplyr::filter(LOCALITY_ID %in% city_dat_loc$LOCALITY_ID)
  
  saveRDS(city_dat, paste0("Intermediate data/city_level_ebird_data/", city_name, ".RDS"))
  
}

cities_chunk_7_list <- locations %>%
  dplyr::filter(NAME20 %in% cities_for_inclusion$NAME20) %>%
  group_by(NAME20) %>%
  summarize(N=n()) %>%
  arrange(N) %>%
  slice(151:200)

lapply(unique(cities_chunk_7_list$NAME20), break_up_data)


################ next 50 cities!
cities_chunk_8 <- locations %>%
  dplyr::filter(NAME20 %in% cities_for_inclusion$NAME20) %>%
  group_by(NAME20) %>%
  summarize(N=n()) %>%
  arrange(N)

localities_chunk_8 <- locations %>%
  dplyr::filter(NAME20 %in% c(cities_chunk_8 %>%
                                slice(201:250) %>%
                                .$NAME20))

## extract data
chunk_8_data <- ebird %>%
  dplyr::filter(COUNTRY=="United States") %>%
  dplyr::filter(OBSERVATION_DATE >= "2019-01-01") %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, COMMON_NAME, SCIENTIFIC_NAME, OBSERVATION_COUNT, CATEGORY,
                LOCALITY_ID, LATITUDE, LONGITUDE, OBSERVATION_DATE, BCR_CODE,
                TIME_OBSERVATIONS_STARTED, OBSERVER_ID, PROTOCOL_TYPE, DURATION_MINUTES, 
                EFFORT_DISTANCE_KM, EFFORT_AREA_HA, NUMBER_OBSERVERS, GROUP_IDENTIFIER,
                COUNTRY, STATE_CODE) %>%
  dplyr::filter(PROTOCOL_TYPE=="Stationary") %>%
  dplyr::filter(NUMBER_OBSERVERS==1) %>%
  dplyr::filter(DURATION_MINUTES<=240) %>%
  dplyr::filter(DURATION_MINUTES>=5) %>%
  dplyr::filter(LOCALITY_ID %in% local(localities_chunk_8$LOCALITY_ID)) %>%
  collect(n=Inf)

# now break up this data to each individual city and export into a folder
break_up_data <- function(city_name){
  
  city_dat_loc <- locations %>%
    dplyr::filter(NAME20 == city_name)
  
  city_dat <- chunk_8_data %>%
    dplyr::filter(LOCALITY_ID %in% city_dat_loc$LOCALITY_ID)
  
  saveRDS(city_dat, paste0("Intermediate data/city_level_ebird_data/", city_name, ".RDS"))
  
}

cities_chunk_8_list <- locations %>%
  dplyr::filter(NAME20 %in% cities_for_inclusion$NAME20) %>%
  group_by(NAME20) %>%
  summarize(N=n()) %>%
  arrange(N) %>%
  slice(201:250)

lapply(unique(cities_chunk_8_list$NAME20), break_up_data)


################ next 50 cities!
cities_chunk_9 <- locations %>%
  dplyr::filter(NAME20 %in% cities_for_inclusion$NAME20) %>%
  group_by(NAME20) %>%
  summarize(N=n()) %>%
  arrange(N)

localities_chunk_9 <- locations %>%
  dplyr::filter(NAME20 %in% c(cities_chunk_9 %>%
                                slice(251:300) %>%
                                .$NAME20))

## extract data
chunk_9_data <- ebird %>%
  dplyr::filter(COUNTRY=="United States") %>%
  dplyr::filter(OBSERVATION_DATE >= "2019-01-01") %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, COMMON_NAME, SCIENTIFIC_NAME, OBSERVATION_COUNT, CATEGORY,
                LOCALITY_ID, LATITUDE, LONGITUDE, OBSERVATION_DATE, BCR_CODE,
                TIME_OBSERVATIONS_STARTED, OBSERVER_ID, PROTOCOL_TYPE, DURATION_MINUTES, 
                EFFORT_DISTANCE_KM, EFFORT_AREA_HA, NUMBER_OBSERVERS, GROUP_IDENTIFIER,
                COUNTRY, STATE_CODE) %>%
  dplyr::filter(PROTOCOL_TYPE=="Stationary") %>%
  dplyr::filter(NUMBER_OBSERVERS==1) %>%
  dplyr::filter(DURATION_MINUTES<=240) %>%
  dplyr::filter(DURATION_MINUTES>=5) %>%
  dplyr::filter(LOCALITY_ID %in% local(localities_chunk_9$LOCALITY_ID)) %>%
  collect(n=Inf)

# now break up this data to each individual city and export into a folder
break_up_data <- function(city_name){
  
  city_dat_loc <- locations %>%
    dplyr::filter(NAME20 == city_name)
  
  city_dat <- chunk_9_data %>%
    dplyr::filter(LOCALITY_ID %in% city_dat_loc$LOCALITY_ID)
  
  saveRDS(city_dat, paste0("Intermediate data/city_level_ebird_data/", city_name, ".RDS"))
  
}

cities_chunk_9_list <- locations %>%
  dplyr::filter(NAME20 %in% cities_for_inclusion$NAME20) %>%
  group_by(NAME20) %>%
  summarize(N=n()) %>%
  arrange(N) %>%
  slice(251:300)

lapply(unique(cities_chunk_9_list$NAME20), break_up_data)
