#### Data wrangling for building height bird biodiversity paper

library(readr)
library(dplyr)
library(tidyverse)
library(vegan)
library(mgcv)
library(corrplot)

# First let's read in the eBird data

bird <- read_rds("Data/example_data.RDS")

# Read in the location in cities RDS

cities <- read_rds("Intermediate Data/locations_in_cities.RDS")

min(bird$OBSERVATION_DATE)
max(bird$OBSERVATION_DATE)

# Now we need to append the city names NAME20 to the eBird dataset
# Then we can calculate richness looped through cities


bird_city <- left_join(bird, cities, by = "LOCALITY_ID")

# filtering out NAs

bird_city <- bird_city %>%
  filter(OBSERVATION_COUNT > 0)
# making the variables numeric/factor

bird_city$OBSERVATION_COUNT <- as.numeric(bird_city$OBSERVATION_COUNT)
bird_city$COMMON_NAME <- as.factor(bird_city$COMMON_NAME)
bird_city$SAMPLING_EVENT_IDENTIFIER <- as.factor(bird_city$SAMPLING_EVENT_IDENTIFIER)

# Now we can calculate richness for each city based on all the LOCALITY_IDs (uneven sampling)

bird_richness_by_checklist <- bird_city %>%
  group_by(SAMPLING_EVENT_IDENTIFIER) %>%
  summarise(total_richness = length(unique(COMMON_NAME)), 
            total_observers = sum(NUMBER_OBSERVERS),
            total_N = sum(OBSERVATION_COUNT),
            shan_div = diversity(OBSERVATION_COUNT),
            city = unique(NAME20),
            duration = unique(DURATION_MINUTES),
            LOCALIT = unique(LOCALITY_ID), 
            LAT = unique(LATITUDE),
            LONG = unique(LONGITUDE))


############ TESTING MODEL for Big Pine Key
# for now just do Big Pine Key, FL
big_pine <- bird_richness_by_checklist %>%
  filter(city == "Big Pine Key, FL")


#### Now we are ready for some ENV variables (including building height), 
#### that we can append by the "city" column
env <- readRDS("Data/land_cover_GEE/gee_land_cover_combined.RDS")

env <- rename(env, LOCALIT = LOCALITY_ID)

# filtering to only pig pine key
env <- env %>%
  filter(LOCALIT %in% building$LOCALIT)

## Here's the building height data

building <- readRDS("Data/building_height_big_pine_key.RDS")

# combining the env and building predictors with the checklist level data

df <- big_pine %>%
  inner_join(env, by = "LOCALIT") %>%
  inner_join(building, by = "LOCALIT")


df_all_values <- df %>%
  select(where(is.numeric))

# quick correlation plot to look a colinearity
corrplot(cor(df_all_values), method = 'number')


####### Now a quick and dirty model for big pine key
mod <- mgcv::gam(log(total_richness) ~ s(duration) + s(LAT, LONG) +
                   scale(water) + scale(trees) + scale(built) + scale(flooded_vegetation) +
                   scale(mean_h), data=df)
summary(mod)

plot(df$mean_h, df$built)

