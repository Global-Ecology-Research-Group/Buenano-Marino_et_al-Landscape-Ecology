# R script to get potential list of cities for inclusion in modelling
# it follows from Cristina's script which assessed 'completeness' of buildings
# for each city
# and now we want to initially only consider cities that have at least 100 unique locality_ids

# packages
library(tidyverse)
library(sf)

filtered_cities <- read_csv("Shapefiles/urban_areas_filtered_80.csv")

ebird_samples_per_city <- readRDS("Intermediate data/locations_in_cities.RDS") %>%
  group_by(NAME20) %>%
  summarize(number_localities=n())

# now filter to cities that have at least 100 unique localities in a city
potential_city_inclusion_list <- filtered_cities %>%
  dplyr::filter(NAME20 %in% c(ebird_samples_per_city %>%
                  dplyr::filter(number_localities >=100) %>%
                  .$NAME20))

write_csv(potential_city_inclusion_list, "Intermediate data/cities_for_analysis.csv")

