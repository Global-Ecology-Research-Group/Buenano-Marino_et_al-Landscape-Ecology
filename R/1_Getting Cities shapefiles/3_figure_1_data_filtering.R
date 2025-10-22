# filtering the urban areas shapefile to make figure 1

# Load necessary libraries
library(sf)
library(dplyr)
library(stringr)

# Set the path to the folder containing RDS files
rds_folder_path <- "Intermediate_data/compiled_data"

# Get list of RDS files
rds_files <- list.files(path = rds_folder_path, pattern = "\\.RDS$", full.names = TRUE)

# Extract city names from file names
city_names <- basename(rds_files) %>%
  str_remove("\\.RDS$")

# Read the shapefile
shapefile_path <- "Shapefiles/urban_areas/Urban_areas.shp"
urban_areas <- st_read(shapefile_path)

# Filter the shapefile to include only matching cities
filtered_urban_areas <- urban_areas %>%
  filter(NAME20 %in% city_names)

# Save the new filtered shapefile
st_write(filtered_urban_areas, "Shapefiles/urban_areas/Filtered_Urban_areas.shp")

print(paste("New shapefile created at:", output_path))
