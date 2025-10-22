# R script to get the checklists assigned to each city
# filter out non-city checklists

# packages
library(sf)
library(tidyverse)


# read in cities
cities <- st_read("Shapefile_urban_areas/Urban_areas.shp")

cities_west <- st_read("Shapefile_urban_areas/Urban_areas_W.shp")

# now combine these two into one cities dataframe

cities_combine <- cities %>%
  rbind(cities_west)

# read in eBird
ebird_locations <- st_read("Shapefiles/ebird_locality_id_in_cities_building_shp/ebird_locations_sample_shp.shp")

# get locations that are within cities
locations_in_cities <- ebird_locations %>%
  st_within(cities_combine) %>%
  as.data.frame() %>%
  left_join(., ebird_locations %>%
              st_set_geometry(NULL) %>%
              mutate(row.id=1:nrow(.))) %>%
  left_join(., cities_combine %>%
              st_set_geometry(NULL) %>%
              mutate(col.id=1:nrow(.)))

locations_cities <- locations_in_cities %>%
  dplyr::select(LOCALIT, NAME20) %>%
  rename(LOCALITY_ID=LOCALIT)

saveRDS(locations_cities, "Intermediate data/locations_in_cities.RDS")

# quick summary
locations_cities %>%
  group_by(NAME20) %>%
  summarize(N=n()) %>%
  arrange(desc(N)) %>%
  head()

