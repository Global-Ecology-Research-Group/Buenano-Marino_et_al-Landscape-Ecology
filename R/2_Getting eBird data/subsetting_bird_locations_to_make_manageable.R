library(sf)
library(readr)
library(dplyr)


cities <- read_rds("Intermediate Data/locations_in_cities.RDS")

cities <- rename(cities, "LOCALIT" = "LOCALITY_ID")

localities <- st_read("Shapefiles/ebird_locality_id_in_cities_building_shp/ebird_locations_sample_shp.shp")


## Need to find a way to filter out points that are not in the continental United States

cities_filtered <- cities[- grep("AS", cities$NAME20), ]
cities_filtered_2 <- cities_filtered[-grep("GU", cities_filtered$NAME20), ]
cities_filtered_3 <- cities_filtered_2[!grepl("HI", cities_filtered_2$NAME20), ]
cities_filtered_4 <- cities_filtered_3[!grepl("AK", cities_filtered_3$NAME20), ]
cities_filtered_5 <- cities_filtered_4[!grepl("VI", cities_filtered_4$NAME20), ]
cities_filtered_6 <- cities_filtered_5[!grepl("MP", cities_filtered_5$NAME20), ]
cities_filtered_7 <- cities_filtered_6[!grepl("PR", cities_filtered_6$NAME20), ]

# save RDS of just the cities that we are actually going to analyze
# cities_cont_us <- saveRDS(cities_filtered_7, file = "Intermediate data/cities_in_cont_us.RDS")

# Now we filter all the localities that occur in cities_filtered_7
localities <- localities %>%
  filter(LOCALIT %in% cities_filtered_7$LOCALIT)

st_write(localities, "Shapefiles/ebird_locality_id_in_cities_building_shp/ebird_entire_batch.shp")

#Checking to see if it worked
cities_lost <- cities %>%
  filter(!NAME20 %in% cities_filtered_7$NAME20)

# trying to get only the cities I haven't done yet in the west
env <- read.csv("Intermediate data/land_cover_GEE/localities_needed.csv")

localities <- rename(localities, "LOCALITY_ID" = "LOCALIT")

localities_west <- localities %>%
  filter(LOCALITY_ID %in% env$LOCALITY_ID)
 
#### First batch of 10,000 LOCALITY, GEE can't process/export more than 100,000
#### vertices of geometry in a single table
#### Make sure to change resolution in GEE to 25m 

localities_1st_batch <- localities_shp[1:25000, ]

st_write(localities_1st_batch, "Shapefiles/ebird_locality_id_in_cities_building_shp/ebird_1st_batch.shp")

# 2nd batch
localities_2nd_batch <- localities_shp[25001:41028, ]

st_write(localities_2nd_batch, "Shapefiles/ebird_locality_id_in_cities_building_shp/ebird_2nd_batch.shp")

# 3rd batch
localities_3rd_batch <- localities[100001:150001, ]

st_write(localities_3rd_batch, "Shapefiles/ebird_locality_id_in_cities_building_shp/ebird_3rd_batch.shp")

# 4th batch
localities_4th_batch <- localities[150001:200000, ]

st_write(localities_4th_batch, "Shapefiles/ebird_locality_id_in_cities_building_shp/ebird_4th_batch.shp")

# 5th batch
localities_5th_batch <- localities[200001:250000, ]

st_write(localities_5th_batch, "Shapefiles/ebird_locality_id_in_cities_building_shp/ebird_5th_batch.shp")

#6th batch
localities_6th_batch <- localities[250001:300000, ]

st_write(localities_6th_batch, "Shapefiles/ebird_locality_id_in_cities_building_shp/ebird_6th_batch.shp")

#7th batch
localities_7th_batch <- localities[300001:350000, ]

st_write(localities_7th_batch, "Shapefiles/ebird_locality_id_in_cities_building_shp/ebird_7th_batch.shp")

#8th batch
localities_8th_batch <- localities[350001:400000, ]

st_write(localities_8th_batch, "Shapefiles/ebird_locality_id_in_cities_building_shp/ebird_8th_batch.shp")

#9th batch
localities_9th_batch <- localities[400001:450000, ]

st_write(localities_9th_batch, "Shapefiles/ebird_locality_id_in_cities_building_shp/ebird_9th_batch.shp")

#10th batch
localities_10th_batch <- localities[450001:500000, ]

st_write(localities_10th_batch, "Shapefiles/ebird_locality_id_in_cities_building_shp/ebird_10th_batch.shp")

#11th batch
localities_11th_batch <- localities[500001:563589, ]

st_write(localities_11th_batch, "Shapefiles/ebird_locality_id_in_cities_building_shp/ebird_11th_batch.shp")

###### Breaking this up into digestable pieces for GEE


st_write(localities, "Shapefiles/ebird_locality_id_in_cities_building_shp/ebird_shp.shp")


### Checking the data is alll in the conterminous united states
ggplot() +
  geom_sf(data = localities, size = 4, shape = 23, fill = "darkred")


########## FIXING data for LANDSCAPE ECOLOGY (JUNE 5, 2025)
cities_analysis <- read.csv("Intermediate data/cities_for_analysis.csv")%>%
  rename("city" = "NAME20")

locations_in_cities <- readRDS("Intermediate data/locations_in_cities.RDS")

#### I need to filter down to only the localities that we end up analyzing in the data
#### So I don't have to read in a TON of localitie sinto GEE

locations_in_cities <- rename(locations_in_cities, "city" = "NAME20")

# Now find localities that are only in cities_analysis
localities_analysis <- locations_in_cities %>%
  filter(city %in% cities_analysis$city) %>%
  rename("LOCALIT" = "LOCALITY_ID")

## Now filter this on the shapefile

localities_shp <- localities %>%
  filter(LOCALIT %in% localities_analysis$LOCALIT)

##### Now exporting these as shape files that are manageable for GEE

localities_1st_batch <- localities_shp[1:25000, ]

st_write(localities_1st_batch, "Shapefiles/ebird_locality_id_in_cities_building_shp/ebird_1st_batch.shp", append = FALSE)

# 2nd batch
localities_2nd_batch <- localities_shp[50001:100000, ]

st_write(localities_2nd_batch, "Shapefiles/ebird_locality_id_in_cities_building_shp/ebird_2nd_batch.shp")

# 3rd batch
localities_3rd_batch <- localities_shp[100001:150000, ]

st_write(localities_3rd_batch, "Shapefiles/ebird_locality_id_in_cities_building_shp/ebird_3rd_batch.shp")

# 4th batch
localities_4th_batch <- localities_shp[150001:200000, ]

st_write(localities_4th_batch, "Shapefiles/ebird_locality_id_in_cities_building_shp/ebird_4th_batch.shp")

# 5th batch
localities_5th_batch <- localities_shp[200001:250000, ]

st_write(localities_5th_batch, "Shapefiles/ebird_locality_id_in_cities_building_shp/ebird_5th_batch.shp")

#6th batch
localities_6th_batch <- localities_shp[250001:300000, ]

st_write(localities_6th_batch, "Shapefiles/ebird_locality_id_in_cities_building_shp/ebird_6th_batch.shp")

#7th batch
localities_7th_batch <- localities_shp[300001:350000, ]

st_write(localities_7th_batch, "Shapefiles/ebird_locality_id_in_cities_building_shp/ebird_7th_batch.shp")

#8th batch
localities_8th_batch <- localities_shp[350001:400000, ]

st_write(localities_8th_batch, "Shapefiles/ebird_locality_id_in_cities_building_shp/ebird_8th_batch.shp")

#9th batch
localities_9th_batch <- localities_shp[400001:450000, ]

st_write(localities_9th_batch, "Shapefiles/ebird_locality_id_in_cities_building_shp/ebird_9th_batch.shp")

#10th batch
localities_10th_batch <- localities_shp[450001:490261, ]

st_write(localities_10th_batch, "Shapefiles/ebird_locality_id_in_cities_building_shp/ebird_10th_batch.shp")


### SUPPLEMENTARY just grabbing 20 cities
### To compare the 30m resolution to the 10m resolution

cities_20 <- cities_analysis[1:20, ]

# Now find localities that are only in cities_analysis
localities_analysis_20 <- locations_in_cities %>%
  filter(city %in% cities_20$city) %>%
  rename("LOCALIT" = "LOCALITY_ID")

## Now filter this on the shapefile

localities_shp_20 <- localities %>%
  filter(LOCALIT %in% localities_analysis_20$LOCALIT)

#checking to see if this worked
plot(localities_shp_20)

st_write(localities_shp_20, "Shapefiles/ebird_locality_id_in_cities_building_shp/ebird_20_cities_supp.shp")
