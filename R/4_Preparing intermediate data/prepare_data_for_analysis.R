# this script pulls everything in for a given city and aggregates data and exports data fot that city
# output files are located in city_level_ebird_data folder
## WILL NOT RUN because shapefiles must be on local drive (to big for GitHub push)

# packages
library(tidyverse)
library(sf)
library(ggplot2)
library(vegan)
library(mapview)
library(purrr)



## read in GEE data
env <- readRDS("Intermediate_data/land_cover_GEE/gee_land_cover_combined.RDS")

# function to read in a city and aggregate data
prepare_data <- function(city_name){
  
  message(paste0("Aggregating data for ", city_name))
  
 # read in eBird data
  ebird_city <- readRDS(paste0("Intermediate_data/city_level_ebird_data/", city_name, ".RDS"))
  
  ebird_city$OBSERVATION_COUNT <- as.numeric(ebird_city$OBSERVATION_COUNT)
  ebird_city$COMMON_NAME <- as.factor(ebird_city$COMMON_NAME)
  ebird_city$SAMPLING_EVENT_IDENTIFIER <- as.factor(ebird_city$SAMPLING_EVENT_IDENTIFIER)
  
 # make sf dataframe
  ebird_locations_sf <- ebird_city %>%
    dplyr::select(LOCALITY_ID, LONGITUDE, LATITUDE) %>%
    distinct() %>%
    st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
  
  
  # the building height data
  b_h_sf <- read_sf(paste0("Intermediate_data/city_level_geojson_building_data/", city_name, "_buildings.geojson"))
  
  # explicity setting the projection
  st_crs(ebird_locations_sf) <- 4326
  st_crs(b_h_sf) <- 4326
  
  # creating a new column with the unique building id
  b_h_sf <- b_h_sf %>%
    mutate(id = 1:length(b_h_sf$height))
  
  # creating the 250m buffer df for each of the localities in the city
  sf_use_s2(TRUE)
  
  localities_250m <- ebird_locations_sf %>%
    st_buffer(dist = 250)# remove slice once it works.
  
  sf_use_s2(FALSE)
  # Now we have to do the intersection, and find out which buildings intersect with each polygon
  # likely need to make this a loop, and then a function later
  buildings <- localities_250m %>%
    mutate(n_overlaps = lengths(st_intersects(geometry, b_h_sf)))
  
  building_locality_id <- data.frame()
  
  for (i in 1:length(localities_250m$LOCALITY_ID)) {
    this_ebird_hotspot <- localities_250m[i, ]
    
    test_build <- st_intersects(this_ebird_hotspot$geometry, b_h_sf)[[1]]
    
    filt_build <- as.data.frame(b_h_sf) %>%
      select(id, height) %>%
      filter(id %in% test_build) %>%
      summarise(id = this_ebird_hotspot$LOCALITY_ID,
                mean_h = mean(height, na.rm =T),
                min_h = min(height, na.rm =T),
                max_h = max(height, na.rm =T),
                sd_h = sd(height, na.rm =T)) %>%
      mutate(n_overlaps = length(test_build))
    
    building_locality_id <- rbind(building_locality_id, filt_build)
  }
  
  ## renaming to LOCALITY_ID
  building_locality_id <- building_locality_id %>%
    rename(LOCALITY_ID = id)
  
  # now get eBird data for a city
  ebird_city <- ebird_city %>%
    filter(!(SAMPLING_EVENT_IDENTIFIER %in% SAMPLING_EVENT_IDENTIFIER[is.na(OBSERVATION_COUNT)]))
  # making the variables numeric/factor
  
  bird_richness_by_checklist <- ebird_city %>%
    group_by(SAMPLING_EVENT_IDENTIFIER) %>%
    summarise(total_richness = length(unique(COMMON_NAME)),
              total_N = sum(OBSERVATION_COUNT),
              shan_div = diversity(OBSERVATION_COUNT),
              duration = unique(DURATION_MINUTES),
              LOCALITY_ID = unique(LOCALITY_ID), 
              LAT = unique(LATITUDE),
              LONG = unique(LONGITUDE))
  
  GEE_locality_city <- ebird_city %>%
    left_join(., env, by="LOCALITY_ID") %>%
    dplyr::select(LOCALITY_ID, 21:30) %>%
    distinct()
    
  
# COMBINE INTO ONE FINAL DATAFRAME FOR ANALYSIS
  analysis_dat <- bird_richness_by_checklist %>%
    left_join(., GEE_locality_city, by = "LOCALITY_ID") %>%
    left_join(., building_locality_id, by = "LOCALITY_ID")
  
  
  saveRDS(analysis_dat, paste0("Intermediate_data/compiled_data/", city_name, ".RDS"))
  
  
  
}

city_files <- list.files(path="Intermediate_data/city_level_ebird_data/",
                         pattern = "\\.RDS")

city_names <- tools::file_path_sans_ext(basename(city_files))

# have to do this since we only have 212 cities with data and 299 with eBird data
building_files <- list.files(path="Intermediate_data/city_level_geojson_building_data/",
                             pattern = "\\.geojson")

modified_files <- sub("_buildings\\.geojson$", "", building_files)


modified_files <- trimws(modified_files)

city_names <- trimws(city_names)

# have to use this list for the time being
filtered_list <- modified_files[modified_files %in% city_names]

purrr::map(filtered_list, prepare_data)



