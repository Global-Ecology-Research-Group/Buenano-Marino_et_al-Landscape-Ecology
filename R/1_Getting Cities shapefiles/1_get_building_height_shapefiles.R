# Code to get building height data from URL
# 20 Aug 2024
# Brittany Mason

# load packages
library(sf)
library(dplyr)
library(tidyr)
library(geojsonsf)
library(stringr)

# import csv file that contains URL links to building height shapefiles
building_height <- read.csv("Shapefiles/dataset-links-to-building-height-shapefile.csv")

# just get US
us <- building_height %>%
  filter(Location=="UnitedStates")

# create function to get building heights
get_shapefiles <- function(link){
  url <- link
  download_path <- tempfile(fileext = ".csv.gz")
  download.file(url, download_path, mode = "wb")
  
  # Read the content of the file
  lines <- readLines(gzfile(download_path))
  
  # Parse each line as GeoJSON
  geojson_features <- lapply(lines, geojson_sf)
  
  # Combine all the features into one sf object
  sf_data <- bind_rows(geojson_features)
  
  # Extract the quadkey using a regular expression
  quadkey <- str_extract(url, "quadkey%3D[0-9]+")
  
  # Remove the 'quadkey%3D' part
  quadkey <- sub("quadkey%3D", "", quadkey)
  
  # write data
  st_write(sf_data, paste("Shapefiles/building_height_shapefiles/", quadkey, ".geojson", sep=""))
}

# use function to get data - here I am just doing the first 5 rows of data
shapefiles <- lapply(us$Url[1:5], get_shapefiles)

