# Convert building height into tables 

library(dplyr)
library(readr)
library(tidyverse)
library(geojsonR)
library(sf)
library(mapview)

# localities in cities

cities <- readRDS("Intermediate data/cities_in_cont_us.RDS")

city_analysis <- read.csv("Intermediate data/cities_for_analysis.csv") 

## filtering the locality_ids in cities that we want for analysis

cities_final <- cities %>%
  filter(NAME20 %in% city_analysis$NAME20)


######### Just the code for 1 city ##########

## now the geometry of all the ebird locations
ebird_locations <- read_sf(dsn = "Shapefiles/ebird_locality_id_in_cities_building_shp/", 
                           layer = "ebird_locations_sample_shp")

ebird_city <- readRDS("Intermediate data/city_level_ebird_data/Alton, IL.RDS") 

ebird_city <- rename(ebird_city, LOCALIT = LOCALITY_ID)

## read in GEE data

env <- readRDS("Intermediate data/land_cover_GEE/gee_land_cover_combined.RDS")

env <- rename(env, LOCALIT = LOCALITY_ID)

# filtering to only pig pine key
env <- env %>%
  filter(LOCALIT %in% building$LOCALIT)

# again need to filter this to only the localities in city we want to analyze

localities <- ebird_locations %>%
  filter(LOCALIT %in% ebird_city$LOCALIT)


# the building height data

b_h_sf <- read_sf("Intermediate data/city_level_geojson_building_data/Alton, IL_buildings.geojson")

# creating a new column with the unique building id
b_h_sf <- b_h_sf %>%
  mutate(id = 1:length(b_h_sf$height))

# creating the 250m buffer df for each of the localities in Big Pine Key, FL

localities_250m <- st_buffer(localities, 250)

sf_use_s2(FALSE)

# take a look at a map to make sure we did it right, looks good
# mapview(localities) + mapview(localities_250m)



# Now we have to do the intersection, and find out which buildings intersect with each polygon
# likely need to make this a loop, and then a function later

buildings <- localities_250m %>%
  mutate(n_overlaps = lengths(st_intersects(geometry, b_h_sf)))


building_localit <- data.frame()

for (i in 1:length(localities_250m$LOCALIT)) {
  this_ebird_hotspot <- localities_250m[i, ]
  
  test_build <- st_intersects(this_ebird_hotspot$geometry, b_h_sf)[[1]]
  
  filt_build <- as.data.frame(b_h_sf) %>%
    select(id, height) %>%
    filter(id %in% test_build) %>%
    summarise(id = this_ebird_hotspot$LOCALIT,
              mean_h = mean(height, na.rm =T),
              min_h = min(height, na.rm =T),
              max_h = max(height, na.rm =T),
              sd_h = sd(height, na.rm =T)) %>%
    mutate(n_overlaps = length(test_build))
  
  building_localit <- rbind(building_localit, filt_build)
}

building_localit <- rename(building_localit, LOCALIT = id)


# saving an RDS of the new combined file
saveRDS(building_localit, file = "Data/building_height_big_pine_key.RDS")
