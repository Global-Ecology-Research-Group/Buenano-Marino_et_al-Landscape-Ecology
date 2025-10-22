library(dplyr)
library(readr)
library(tidyverse)
library(geojsonR)
library(sf)
library(mapview)
library(vegan)


##### loop to calculate building height at each locality_id within each city ####

city_ebird <- list.files(path="Intermediate data/city_level_ebird_data/")

bird <- lapply(city_ebird, readRDS)


for(i in 1:length(bird)) {
  
  bird_richness_by_checklist <- bird[[i]] %>%
    group_by(SAMPLING_EVENT_IDENTIFIER) %>%
    summarise(total_richness = length(unique(COMMON_NAME)),
              total_N = sum(as.numeric(OBSERVATION_COUNT)),
              shan_div = diversity(as.numeric(OBSERVATION_COUNT)),
              duration = unique(DURATION_MINUTES),
              LOCALIT = unique(LOCALITY_ID),  
              LAT = unique(LATITUDE),
              LONG = unique(LONGITUDE))
  
  # Use city_ebird[i] for the filename
  city_name <- city_ebird[i]  # Assuming city_ebird is a list or vector with names
  
  # Create a filename using the city name
  filename <- paste0("Intermediate data/bird_richness_by_city/", city_name, "")
  
  # Save the data frame as an RDS file
  saveRDS(bird_richness_by_checklist, file = filename)
}

