# effect of land use/land cover values computed at different spatial resolutions

library(readr)
library(ggplot2)
library(dplyr)
library(corrplot)
library(patchwork)
library(ggpubr)

# reading in the supplement fo just 20 cities

# The 10m resolution data
supp_10m <- read.csv("Intermediate data/land_cover_GEE/250m_buffer_landcover_est_supp.csv") %>%
  rename("bare_10" = "bare",
         "flooded_vegetation_10" = "flooded_vegetation",
         "built_10" = "built",
         "crops_10" = "crops",
         "grass_10" = "grass",
         "trees_10" = "trees",
         "water_10" = "water",
         "shrub_and_scrub_10" = "shrub_and_scrub")

supp_10m <- supp_10m[, c(2:7, 9, 11:12)]

#The 30m resolution data
supp_30m <- read.csv("Intermediate data/land_cover_GEE/250m_buffer_landcover_est_supp_30.csv") %>%
  rename("bare_30" = "bare",
         "flooded_vegetation_30" = "flooded_vegetation",
         "built_30" = "built",
         "crops_30" = "crops",
         "grass_30" = "grass",
         "trees_30" = "trees",
         "water_30" = "water",
         "shrub_and_scrub_30" = "shrub_and_scrub")

supp_30m <- supp_30m[, c(2:7, 9, 11:12)]


## Adding our 25m data
gee_25m <- readRDS("Intermediate data/land_cover_GEE/gee_land_cover_combined.RDS") %>%
  rename("bare_25" = "bare",
         "flooded_vegetation_25" = "flooded_vegetation",
         "built_25" = "built",
         "crops_25" = "crops",
         "grass_25" = "grass",
         "trees_25" = "trees",
         "water_25" = "water",
         "shrub_and_scrub_25" = "shrub_and_scrub")

supp_25m <- gee_25m[, c(1:6, 8, 10:11)] %>%
  rename("LOCALIT" = "LOCALITY_ID") %>%
  filter(LOCALIT %in% supp_10m$LOCALIT)

# Filtering down to the same number of localities
supp_10m <- supp_10m %>%
  filter(LOCALIT %in% supp_25m$LOCALIT)

supp_30m <- supp_30m %>%
  filter(LOCALIT %in% supp_25m$LOCALIT)

supp_25m <- supp_25m[order(supp_25m$LOCALIT), ]
supp_10m <- supp_10m[order(supp_10m$LOCALIT), ]
supp_30m <- supp_30m[order(supp_30m$LOCALIT), ]

# All in a single dataframe
supp_data <- supp_10m %>%
  left_join(supp_30m, by = "LOCALIT") %>%
  left_join(supp_25m, by = "LOCALIT")


# Running the correlation analysis

######## 10m to 25m #########


cor_sup2 <- cor(supp_10m[, 2:9], supp_25m[, c(2:9)])

# bare
p1 <- ggplot(data = supp_data, aes(x = bare_10, y = bare_25)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  stat_cor(method = "pearson", label.x = min(supp_data$bare_10, na.rm = TRUE), 
           label.y = max(supp_data$bare_25, na.rm = TRUE)) +
  xlab("Bare 10m") +
  ylab("Bare 25m") +
  theme_bw()


# water
p2 <- ggplot(data = supp_data, aes(x = water_10, y = water_25)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  stat_cor(method = "pearson", label.x = min(supp_data$water_10, na.rm = TRUE), 
           label.y = max(supp_data$water_25, na.rm = TRUE)) +
  xlab("Water 10m") +
  ylab("Water 25m") +
  theme_bw()

# built
p3 <- ggplot(data = supp_data, aes(x = built_10, y = built_25)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  stat_cor(method = "pearson", label.x = min(supp_data$built_10, na.rm = TRUE), 
           label.y = max(supp_data$built_25, na.rm = TRUE)) +
  xlab("Built 10m") +
  ylab("Built 25m") +
  theme_bw()

# crops
p4 <- ggplot(data = supp_data, aes(x = crops_10, y = crops_25)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  stat_cor(method = "pearson", label.x = min(supp_data$crops_10, na.rm = TRUE), 
           label.y = max(supp_data$crops_25, na.rm = TRUE)) +
  xlab("Crops 10m") +
  ylab("Crops 25m") +
  theme_bw()

# flooded veg
p5 <- ggplot(data = supp_data, aes(x = flooded_vegetation_10, y = flooded_vegetation_25)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  stat_cor(method = "pearson", label.x = min(supp_data$flooded_vegetation_10, na.rm = TRUE), 
           label.y = max(supp_data$flooded_vegetation_25, na.rm = TRUE)) +
  xlab("Flooded Vegetation 10m") +
  ylab("Flooded Vegetation 25m") +
  theme_bw()

# grass
p6 <- ggplot(data = supp_data, aes(x = grass_10, y = grass_25)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  stat_cor(method = "pearson", label.x = min(supp_data$grass_10, na.rm = TRUE), 
           label.y = max(supp_data$grass_25, na.rm = TRUE)) +
  xlab("Grass 10m") +
  ylab("Grass 25m") +
  theme_bw()


# shrub and scrub
p7 <- ggplot(data = supp_data, aes(x = shrub_and_scrub_10, y = shrub_and_scrub_25)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  stat_cor(method = "pearson", label.x = min(supp_data$shrub_and_scrub_10, na.rm = TRUE), 
           label.y = max(supp_data$shrub_and_scrub_25, na.rm = TRUE)) +
  xlab("Shrub and Scrub 10m") +
  ylab("Shrub and Scrub 25m") +
  theme_bw()

# trees
p8 <- ggplot(data = supp_data, aes(x = trees_10, y = trees_25)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  stat_cor(method = "pearson", label.x = min(supp_data$trees_10, na.rm = TRUE), 
           label.y = max(supp_data$trees_25, na.rm = TRUE)) +
  xlab("Trees 10m") +
  ylab("Trees 25m") +
  theme_bw()



p1 + p2 + p3 +
  p4 + p5 + p6 +
  p7 + p8


######## 25m to 30m ###########

# bare
p10 <- ggplot(data = supp_data, aes(x = bare_30, y = bare_25)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  stat_cor(method = "pearson", label.x = min(supp_data$bare_30, na.rm = TRUE), 
           label.y = max(supp_data$bare_25, na.rm = TRUE)) +
  xlab("Bare 30m") +
  ylab("Bare 25m") +
  theme_bw()


# water
p11 <- ggplot(data = supp_data, aes(x = water_30, y = water_25)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  stat_cor(method = "pearson", label.x = min(supp_data$water_30, na.rm = TRUE), 
           label.y = max(supp_data$water_25, na.rm = TRUE)) +
  xlab("Water 30m") +
  ylab("Water 25m") +
  theme_bw()

# built
p12 <- ggplot(data = supp_data, aes(x = built_30, y = built_25)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  stat_cor(method = "pearson", label.x = min(supp_data$built_30, na.rm = TRUE), 
           label.y = max(supp_data$built_25, na.rm = TRUE)) +
  xlab("Built 30m") +
  ylab("Built 25m") +
  theme_bw()

# crops
p13 <- ggplot(data = supp_data, aes(x = crops_30, y = crops_25)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  stat_cor(method = "pearson", label.x = min(supp_data$crops_30, na.rm = TRUE), 
           label.y = max(supp_data$crops_25, na.rm = TRUE)) +
  xlab("Crops 30m") +
  ylab("Crops 25m") +
  theme_bw()

# flooded veg
p14 <- ggplot(data = supp_data, aes(x = flooded_vegetation_30, y = flooded_vegetation_25)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  stat_cor(method = "pearson", label.x = min(supp_data$flooded_vegetation_30, na.rm = TRUE), 
           label.y = max(supp_data$flooded_vegetation_25, na.rm = TRUE)) +
  xlab("Flooded Vegetation 30m") +
  ylab("Flooded Vegetation 25m") +
  theme_bw()

# grass
p15 <- ggplot(data = supp_data, aes(x = grass_30, y = grass_25)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  stat_cor(method = "pearson", label.x = min(supp_data$grass_30, na.rm = TRUE), 
           label.y = max(supp_data$grass_25, na.rm = TRUE)) +
  xlab("Grass 30m") +
  ylab("Grass 25m") +
  theme_bw()


# shrub and scrub
p16 <- ggplot(data = supp_data, aes(x = shrub_and_scrub_30, y = shrub_and_scrub_25)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  stat_cor(method = "pearson", label.x = min(supp_data$shrub_and_scrub_30, na.rm = TRUE), 
           label.y = max(supp_data$shrub_and_scrub_25, na.rm = TRUE)) +
  xlab("Shrub and Scrub 30m") +
  ylab("Shrub and Scrub 25m") +
  theme_bw()

# trees
p17 <- ggplot(data = supp_data, aes(x = trees_30, y = trees_25)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  stat_cor(method = "pearson", label.x = min(supp_data$trees_30, na.rm = TRUE), 
           label.y = max(supp_data$trees_25, na.rm = TRUE)) +
  xlab("Trees 30m") +
  ylab("Trees 25m") +
  theme_bw()



p10 + p11 + p12 +
  p13 + p14 + p15 +
  p16 + p17


# BARE AREAS 

# bare to built comparison

ggplot(data = supp_data, aes(x = bare_25, y = built_25)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  stat_cor(method = "pearson", label.x = quantile(supp_data$bare_25, 0.9, na.rm = TRUE), 
           label.y = max(supp_data$built_25, na.rm = TRUE)) +
  xlab("Bare 25m") +
  ylab("Built 25m") +
  ylim(0, NA) +
  theme_bw()

median(supp_data$bare_25)
min(supp_data$bare_25)
max(supp_data$bare_25)
