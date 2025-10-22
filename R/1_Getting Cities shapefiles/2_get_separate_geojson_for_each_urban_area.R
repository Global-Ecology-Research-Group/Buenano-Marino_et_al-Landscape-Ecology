# R script to get a separate geojson with building height data for each city/urban area

library(dplyr)
library(sf)
library(ggplot2)
library(readr)

# Read in building height shapefiles
list.buildings <- list.files("Shapefiles/building_height_shapefiles", pattern=".geojson", full.names = TRUE)

# reading in city data
urban_areas_E <- st_read("Shapefile_urban_areas/Urban_areas.shp")
urban_areas_W <- st_read("Shapefile_urban_areas/Urban_areas_W.shp")
urban_areas <- rbind(urban_areas_E, urban_areas_W) %>%
  mutate(NAME20=gsub("Louisville/Jefferson County, KY--IN", "Louisville--Jefferson County, KY--IN", NAME20))

# filter city data based on filtered list
city_filter <- readr::read_csv("Intermediate data/building_height/cities_for_analysis.csv") %>%
  mutate(NAME20=gsub("Louisville/Jefferson County, KY--IN", "Louisville--Jefferson County, KY--IN", NAME20))

# now for each city I want to loop through all the buildings and assign any that are 
# within it, append them and then write that out as a separate geojson for that city
get_building_data_function <- function(city_name){
  
  message(paste0("Aggregating building height data for: ", city_name))
  
  # Extract the geometry of the current city
  city_geom <- urban_areas %>% 
    filter(NAME20 == city_name) %>% 
    st_geometry() %>%
    st_make_valid()
  
  # Create an empty list to store buildings for this city
  city_buildings <- list()
  
  # Loop through building files and process each one
  for (building_file in list.buildings) {
    
    # Read the building data
    buildings <- st_read(building_file, quiet = TRUE) %>%
      st_make_valid()
    
    # Filter buildings within the city's geometry using spatial intersection
    buildings_in_city <- st_intersection(buildings, city_geom)
    
    # If there are buildings in this city, append to the list
    if (nrow(buildings_in_city) > 0) {
      city_buildings[[length(city_buildings) + 1]] <- buildings_in_city
    }
  }
  
  # Combine all the buildings for this city
  city_buildings_combined <- do.call(rbind, city_buildings)
  
  # Write the combined buildings to a GeoJSON file for the city
  st_write(city_buildings_combined, paste0("Intermediate data/city_level_geojson_building_data/", city_name, "_buildings.geojson"), delete_dsn = TRUE)

  
}

city_exists_list <- data.frame(c(NAME20=list.files("Intermediate data/city_level_geojson_building_data/"))) %>%
  rename(NAME20=1) %>%
  mutate(NAME20=gsub("_buildings.geojson", "", NAME20)) %>%
  mutate(exists="yes")

city_to_analyze <- city_filter %>%
  left_join(., city_exists_list, by="NAME20") %>%
  dplyr::filter(is.na(exists))

lapply(unique(city_to_analyze$NAME20), get_building_data_function)

