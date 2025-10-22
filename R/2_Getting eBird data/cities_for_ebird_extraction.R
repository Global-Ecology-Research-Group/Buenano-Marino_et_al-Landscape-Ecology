library(sf)
library(dplyr)

# read in list of cities that you want urban areas for
# city_list <- read_csv("")

# reading in city data
urban_areas_E <- st_read("Shapefile_urban_areas/Urban_areas.shp")
urban_areas_W <- st_read("Shapefile_urban_areas/Urban_areas_W.shp")
urban_areas <- rbind(urban_areas_E, urban_areas_W)

urban_areas_test <- urban_areas %>%
  filter(NAME20 %in% unique(city_list$NAME20))

st_write(urban_areas_test, "Shapefiles/urban_areas_filtered.shp")