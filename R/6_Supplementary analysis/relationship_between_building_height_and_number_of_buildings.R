# Relationship between building height and number of buildings
# 13 June 2025
# Brittany Mason

library(dplyr)
library(ggplot2)
library(readr)

# read in the data
building_height <- read_csv("Intermediate_data/building_height/summary_building_height_city_updated.csv")

# filter the data so we are only look at cities with 80% building height data
building_height_filtered <- building_height %>%
  filter(percent_buildings_with_height >= 0.7)

# check the distribution of the data
hist(building_height_filtered$quantile_0.99)
hist(building_height_filtered$total_buildings)

# plot the relationship between the 99% quantile and number of buildings
ggplot(building_height_filtered, aes(x = total_buildings, y = quantile_0.99)) +
  geom_point(alpha = 0.7, color = "#1f78b4") +
  geom_smooth(method = "lm", se = TRUE, color = "gray30", linetype = "dashed") +
  scale_x_log10() +  # Log-transform the x-axis
  theme_classic() +
  labs(
    x = "Total Number of Buildings (log scale)",
    y = "99th Percentile of Building Height"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

ggplot(building_height_filtered, aes(x = total_buildings, y = max_height)) +
  geom_point(alpha = 0.7, color = "#1f78b4") +
  geom_smooth(method = "lm", se = TRUE, color = "gray30", linetype = "dashed") +
  scale_x_log10() +  # Log-transform the x-axis
  theme_classic() +
  labs(
    x = "Total Number of Buildings (log scale)",
    y = "99th Percentile of Building Height"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# save the plot 
ggsave("Results/Figure_A1.jpeg", height = 5, width=5, units="in")

# log transform building height since it is positvely skewed and then model the data
building_height_filtered$log_buildings <- log(building_height_filtered$total_buildings)

model <- lm(quantile_0.99 ~ log_buildings, data = building_height_filtered)
summary(model)

# check the residuals
plot(model, which = 1)

