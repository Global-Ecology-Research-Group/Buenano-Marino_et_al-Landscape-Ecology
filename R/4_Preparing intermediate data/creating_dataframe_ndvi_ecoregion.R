# Creating ecoregion and NDVI dataframe

library(readr)
library(dplyr)
library(sf)

#NDVI data
ndvi_est <- read_csv("Intermediate data/NDVI/250m_buffer_NDVI_scaled.csv")

ndvi_est <- ndvi_est[, 2:3] 

# all the ebird localities
ebird <- read_sf("Shapefiles/ebird_locality_id_in_cities_building_shp/ebird_entire_batch.shp")

# the EPA Ecoregion data
ecoregion <- read_sf("Intermediate data/epa_ecoregion/epa_level2_eco/NA_CEC_Eco_Level2.shp") %>%
  st_as_sf(crs = st_crs(ebird))

# setting the crs
ecoregion <- st_transform(ecoregion, crs = st_crs(ebird))

ecoregion <- st_make_valid(ecoregion)

ebird <- st_make_valid(ebird)

# Now run an intersection

ebird_eco <- st_within(ebird, ecoregion)

# Add ecoregion info directly (assuming one-to-one or dominant ecoregion match)
eco_joined <- ebird %>%
  mutate(eco_index = lengths(ebird_eco) > 0) %>%
  mutate(eco_name = sapply(ebird_eco, function(x) {
    if (length(x) > 0) return(ecoregion$NA_L2NAME[x[1]]) else return(NA)
  }))

eco_ebird_ndvi <- eco_joined %>%
  left_join(ndvi_est, by = "LOCALIT")

# saveRDS(eco_ebird_ndvi, file = "Intermediate data/ecoregion_ebird_ndvi.RDS")
