# Exploring the relationship between predictor variables and species richness
# June 11, 2025
# Brittany Mason

library(dplyr)
library(ggplot2)
library(sf)
library(tidyr)

# get list of all cities
folder_path <- "Intermediate_data/compiled_data"
city_files <- list.files(folder_path, pattern = "\\.RDS$", full.names = TRUE)

# Read all files, add the "name" column, and bind them together
all_city_data <- city_files %>%
  lapply(function(file) {
    data <- readRDS(file)  
    data$name <- gsub("\\.RDS$", "", basename(file))  # Remove ".RDS" from file name
    return(data)
  }) %>%
  bind_rows()  # Combine all data frames into one

# join with data frame containing information on ecoregion
er_data <- readRDS("Intermediate_data/ecoregion_ebird/ecoregion_ebird_ndvi.RDS")

er_data <- er_data %>%
  st_drop_geometry() %>%
  dplyr::select(LOCALIT, eco_name, mean) %>%
  rename(NDVI=mean)

all_city_data <- left_join(all_city_data, er_data, by=c("LOCALITY_ID" = "LOCALIT"))

# because we have a large dataset and it is hard to view trends if all points are plotted, 
# let's randomly select 500 data points to visualize

# 1. Sample and select relevant columns (you already did this)
plot_data <- all_city_data %>%
  slice_sample(n = 1000) %>%
  select(total_richness, bare, built, grass,
         shrub_and_scrub, trees, water, NDVI, mean_h) %>%
  rename(Bare = bare, Impervious = built, 
         Grass = grass, "Shrub and Scrub" = shrub_and_scrub, 
         Trees = trees, Water = water, "Building Height" = mean_h)

# 2. Reshape the data to long format
plot_data_long <- plot_data %>%
  pivot_longer(-total_richness, names_to = "predictor", values_to = "value")

# 3. Plot using facet_wrap
ggplot(plot_data_long, aes(x = value, y = total_richness)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  facet_wrap(~ predictor, scales = "free_x") +
  labs(x = "Predictor Value", y = "Species Richness") +
  theme_bw()

ggsave("Figures_and_Tables/Figure_A4.jpeg", height=6, width=7, units="in")


