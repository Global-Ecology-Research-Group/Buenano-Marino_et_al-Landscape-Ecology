# Install necessary packages if not already installed
if (!require("mgcv")) install.packages("mgcv")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("gridExtra")) install.packages("gridExtra")
if (!require("stringr")) install.packages("stringr")
if (!require("car")) install.packages("car")
if (!require("ggpubr")) install.packages("ggpubr")
if (!require("tibble")) install.packages("tibble")
if (!require("FSA")) install.packages("FSA")
if (!require("sf")) install.packages("sf")

# Load required libraries
library(mgcv)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(stringr)
library(car)
library(ggpubr)
library(tibble)
library(MuMIn)
library(FSA)
library(sf)

# GAM Model Checking ------------------------------------------------------

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
  select(LOCALIT, eco_name, mean) %>%
  rename(NDVI=mean)

all_city_data <- left_join(all_city_data, er_data, by=c("LOCALITY_ID" = "LOCALIT"))

# now get a table with just name and eco_name
name_eco <- all_city_data %>%
  group_by(name) %>%
  summarise(eco_name=first(eco_name))

# histogram of species richness across all cities
hist(all_city_data$total_richness)

# check for multicolinearity
city_rel <- all_city_data %>%
  filter(complete.cases(mean_h),
         complete.cases(NDVI)) %>%
  select(total_richness, duration, mean_h, built, NDVI, LAT, LONG, name)

cor(city_rel[,3:5], method="pearson")
cor.test(city_rel$mean_h, city_rel$built, method = "pearson")
# interestingly, this indicates that there is a weak negative correlation between
# mean building height and built area... as built area increases, mean building height 
# tends to decrease slightly

# plot the data
ggplot(data = city_rel, aes(x = mean_h, y = built)) +
  geom_point(alpha = 0.2, color = "blue") +  # Add scatter points with some transparency
  geom_smooth(method = "lm", color = "red", fill = "lightpink", se = TRUE) +  # Add linear model line with SE
  labs(
    title = "Scatterplot of Mean Height vs. Built",
    x = "Mean Height (mean_h)",
    y = "Built (%)"
  ) +
  theme_classic()

# does order of building height and built area matter?
model_test1 <- gam(total_richness ~ mean_h + built +
                         s(duration,  k=15, bs="cr") + 
                         s(LAT, LONG,  k=30),
                       family = Gamma(link = "log"),
                       data = city_rel, method = "REML")

model_test2 <- gam(total_richness ~ built + mean_h + 
                     s(duration,  k=15, bs="cr") + 
                     s(LAT, LONG,  k=30),
                   family = Gamma(link = "log"),
                   data = city_rel, method = "REML")

summary(model_test1)
summary(model_test2)

AIC(model_test1, model_test2)
# the models are the same

# let's look at the model prediction for the relationship between building height and species richness

# model testing
gam.check(model_test1)

# try different k-values
city_test <- city_rel %>% filter(name=="Alton, IL")

model_test1 <- gam(total_richness ~ mean_h + built + NDVI +
                     s(duration, k=15, bs="cr") + 
                     s(LAT, LONG, k=30, bs="tp"),
                   family = Gamma(link = "log"),
                   data = city_test, method = "REML")

gam.check(model_test1)

# examine cities that failed with k=15 and k=30
problem_cities <- c("Newark, OH", "Port Huron, MI", "Waterbury, CT", "Youngstown, OH")

city_test <- city_rel %>% filter(name=="Port Huron, MI")

city_rel %>% 
  filter(name %in% problem_cities) %>%
  group_by(name) %>%
  summarise(count=n())

model_test1 <- gam(total_richness ~ mean_h + built + NDVI +
                     s(duration, k=15, bs="cr") + 
                     s(LAT, LONG, k=5, bs="tp"),
                   family = Gamma(link = "log"),
                   data = city_test, method = "REML")

gam.check(model_test1)

# let's determine how many checklists we have for each city
checklist_per_city <- city_rel %>% 
  group_by(name) %>%
  summarise(count=n())

hist(checklist_per_city$count)
summary(checklist_per_city$count)

# how about unique geographical coordinates
geo_per_city <- city_rel %>% 
  mutate(coords=paste(LAT, LONG, sep=" ")) %>%
  group_by(name) %>%
  summarise(count=n_distinct(coords))

hist(geo_per_city$count)
summary(geo_per_city$count)

# Run GAM Model for all Cities ------------------------------------------------------

# Set k_val for smooth terms
k_val_dur <- 15 
k_val_geo <- 30

# Function to read and analyze a city
analyze_city <- function(file_path) {
  tryCatch({
    # Read city data
    city_data <- readRDS(file_path)
    city_name <- tools::file_path_sans_ext(basename(file_path))

    city_data <- city_data %>%
      filter(complete.cases(mean_h))
    
    city_data <- left_join(city_data, er_data, by=c("LOCALITY_ID" = "LOCALIT"))
    
    # Models for species richness
    # Model 1: only 'built'
    model1_richness <- gam(total_richness ~ built + 
                             s(duration, k=k_val_dur, bs="cr") + 
                             s(LAT, LONG,  k=k_val_geo, bs="tp"),
                           family = Gamma(link = "log"),
                           data = city_data, method = "REML")
    
    # Model 2: 'built' and 'mean_h'
    model2_richness <- gam(total_richness ~ built + mean_h + 
                             s(duration,  k=k_val_dur, bs="cr") + 
                             s(LAT, LONG,  k=k_val_geo, bs="tp"),
                           family = Gamma(link = "log"),
                           data = city_data, method = "REML")
    
    # Model 3: only 'mean_h'
    model3_richness <- gam(total_richness ~ mean_h + 
                             s(duration,  k=k_val_dur, bs="cr") + 
                             s(LAT, LONG,  k=k_val_geo, bs="tp"),
                           family = Gamma(link = "log"),
                           data = city_data, method = "REML")
    
    # Model 4: only NDVI
    model4_richness <- gam(total_richness ~ NDVI + 
                             s(duration,  k=k_val_dur, bs="cr") + 
                             s(LAT, LONG,  k=k_val_geo, bs="tp"),
                           family = Gamma(link = "log"),
                           data = city_data, method = "REML")
    
    # Model 5: NDVI and building height
    model5_richness <- gam(total_richness ~ NDVI + mean_h +
                             s(duration,  k=k_val_dur, bs="cr") + 
                             s(LAT, LONG,  k=k_val_geo, bs="tp"),
                           family = Gamma(link = "log"),
                           data = city_data, method = "REML")
    
    # Model 6: NDVI and built
    model6_richness <- gam(total_richness ~ NDVI + built +
                             s(duration,  k=k_val_dur, bs="cr") + 
                             s(LAT, LONG,  k=k_val_geo, bs="tp"),
                           family = Gamma(link = "log"),
                           data = city_data, method = "REML")
    
    # Model 7: NDVI, built, and building height
    model7_richness <- gam(total_richness ~ NDVI + built + mean_h +
                             s(duration,  k=k_val_dur, bs="cr") + 
                             s(LAT, LONG,  k=k_val_geo, bs="tp"),
                           family = Gamma(link = "log"),
                           data = city_data, method = "REML")
    
    # Extract relevant information for built and mean_h
    extract_info <- function(model) {
      summary_model <- summary(model)
      
      # Extract estimates and p-values for built and mean_h
      estimates <- summary_model$p.table[ , "Estimate"]
      std_errors <- summary_model$p.table[ , "Std. Error"]
      p_values <- summary_model$p.table[ , "Pr(>|t|)"]
      
      list(
        aic = AIC(model),
        r2 = summary_model$r.sq,
        dev_expl = summary_model$dev.expl,
        estimates = estimates,
        std_errors = std_errors,
        p_values = p_values
      )
    }
    
    # Extract information from models
    info_model1 <- extract_info(model1_richness)
    info_model2 <- extract_info(model2_richness)
    info_model3 <- extract_info(model3_richness)
    info_model4 <- extract_info(model4_richness)
    info_model5 <- extract_info(model5_richness)
    info_model6 <- extract_info(model6_richness)
    info_model7 <- extract_info(model7_richness)
    
    # Create a data frame with results
    results <- data.frame(
      city = city_name,
      model = c("Model1", "Model2", "Model3", "Model4", "Model5", "Model6", "Model7"),
      aic = c(info_model1$aic, info_model2$aic, info_model3$aic, info_model4$aic, info_model5$aic, info_model6$aic, info_model7$aic),
      r2 = c(info_model1$r2, info_model2$r2, info_model3$r2, info_model4$r2, info_model5$r2, info_model6$r2, info_model7$r2),
      dev_expl = c(info_model1$dev_expl, info_model2$dev_expl, info_model3$dev_expl, info_model4$dev_expl, info_model5$dev_expl, info_model6$dev_expl, info_model7$dev_expl),
      built_estimate = c(info_model1$estimates["built"], info_model2$estimates["built"], NA, NA, NA, info_model6$estimates["built"], info_model7$estimates["built"]),
      built_std_error = c(info_model1$std_errors["built"], info_model2$std_errors["built"], NA, NA, NA, info_model6$std_errors["built"], info_model7$std_errors["built"]),
      built_p_value = c(info_model1$p_values["built"], info_model2$p_values["built"], NA, NA, NA, info_model6$p_values["built"], info_model7$p_values["built"]),
      mean_h_estimate = c(NA, info_model2$estimates["mean_h"], info_model3$estimates["mean_h"], NA, info_model5$estimates["mean_h"], NA, info_model7$estimates["mean_h"]),
      mean_h_std_error = c(NA, info_model2$std_errors["mean_h"], info_model3$std_errors["mean_h"], NA, info_model5$std_errors["mean_h"], NA, info_model7$std_errors["mean_h"]),
      mean_h_p_value = c(NA, info_model2$p_values["mean_h"], info_model3$p_values["mean_h"], NA, info_model5$p_values["mean_h"], NA, info_model7$p_values["mean_h"]),
      ndvi_estimate = c(NA, NA, NA, info_model4$estimates["NDVI"], info_model5$estimates["NDVI"], info_model6$estimates["NDVI"], info_model7$estimates["NDVI"]),
      ndvi_std_error = c(NA, NA, NA, info_model4$std_errors["NDVI"], info_model5$std_errors["NDVI"], info_model6$std_errors["NDVI"], info_model7$std_errors["NDVI"]),
      ndvi_p_value = c(NA, NA, NA, info_model4$p_values["NDVI"], info_model5$p_values["NDVI"], info_model6$p_values["NDVI"], info_model7$p_values["NDVI"])
    )
    
    return(results)
  }, error = function(e) {
    message(sprintf("Error processing city: %s\nError: %s", file_path, e$message))
    return(NULL)  # Return NULL if an error occurs
  })
}

# Read and analyze all cities
folder_path <- "Intermediate_data/compiled_data"
city_files <- list.files(folder_path, pattern = "\\.RDS$", full.names = TRUE)

results_list <- lapply(seq_along(city_files), function(i) {
  city <- city_files[[i]]
  message("Processing city ", i, " of ", length(city_files), ": ", city)
  analyze_city(city)
})
results <- do.call(rbind, results_list)

# Calculate model weights based on AIC values for each city
results_with_weights <- results %>%
  group_by(city) %>%
  mutate(
    min_aic = min(aic),
    weight = exp(-0.5 * (aic - min_aic)),
    weight_normalized = weight / sum(weight)
  ) %>%
  ungroup() %>%
  select(-min_aic)

# Save results to a CSV file
write.csv(results_with_weights, "Intermediate_data/model_results/city_analysis_results_with_weights.csv", row.names = FALSE)
results_with_weights <- read.csv("Intermediate_data/model_results/city_analysis_results_with_weights.csv")


# Print and save summary of best models for each city based on AIC
best_models <- results_with_weights %>%
  group_by(city) %>%
  slice_min(aic) %>%
  select(city, model, aic, r2, dev_expl,
         built_estimate, built_std_error, built_p_value,
         mean_h_estimate, mean_h_std_error, mean_h_p_value,
         weight_normalized) %>%
  arrange(city)

print(best_models)
write.csv(best_models, "Intermediate_data/model_results/best_models_summary_with_weights.csv", row.names = FALSE)

# summarise this result
best_models %>%
  mutate(model = case_when(
           model == "Model1" ~ "Impervious Cover",
           model == "Model2" ~ "Building Height and Impervious Cover",
           model == "Model3" ~ "Building Height",
           model == "Model4" ~ "NDVI",
           model == "Model5" ~ "Building Height and NDVI",
           model == "Model6" ~ "Impervious Cover and NDVI",
           model == "Model7" ~ "Building Height, Impervious Cover, and NDVI",
           TRUE ~ model
         )) %>%
  group_by(model) %>%
  summarise(count=n()/nrow(best_models)*100)

summary(best_models$dev_expl)

# now let's do this by ecoregion
best_models <- left_join(best_models, name_eco, by=c("city" = "name"))

View(best_models %>%
  mutate(model = case_when(
    model == "Model1" ~ "Impervious Cover",
    model == "Model2" ~ "Building Height and Impervious Cover",
    model == "Model3" ~ "Building Height",
    model == "Model4" ~ "NDVI",
    model == "Model5" ~ "Building Height and NDVI",
    model == "Model6" ~ "Impervious Cover and NDVI",
    model == "Model7" ~ "Building Height, Impervious Cover, and NDVI",
    TRUE ~ model
  )) %>%
  group_by(model, eco_name) %>%
  summarise(count=n()/nrow(best_models)*100))

# Graphs##################################################################

# Modify the city names to wrap after a specific number of characters
best_models$city <- str_wrap(best_models$city, width = 35)

best_models_filtered <- best_models %>%
  filter(!is.na(r2)) %>%  # Remove rows where 'r2' is NA
  mutate(city = factor(city, levels = best_models %>%
                         filter(!is.na(r2)) %>%
                         arrange(r2) %>%
                         pull(city)),
         model = dplyr::recode(model,
                               "Model1" = "Impervious Cover",
                               "Model2" = "Building Height and Impervious Cover",
                               "Model3" = "Building Height",
                               "Model4" = "NDVI",
                               "Model5" = "Building Height and NDVI",
                               "Model6" = "Impervious Cover and NDVI",
                               "Model7" = "Building Height, Impervious Cover, and NDVI"))  

# Plotting: Create separate graphs for each model where only the cities for that model are shown
ggplot(best_models_filtered, aes(x = r2, y = city, fill = model)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(
    title = "Best Models for Each City",
    x = "R-squared",
    y = "City",
    fill = "Model"
  ) +
  theme(strip.text = element_text(size = 16), 
         axis.text.y = element_text(size = 8),
        legend.position = "none",
        plot.title = element_text(size = 18))  + 
  facet_wrap(~ model, scales = "free_y")  # Create separate graphs for each model


###############################################################################graph2
# Prepare data for plotting
plot_data <- results_with_weights %>%
  select(city, model, weight_normalized, r2) %>%
  mutate(city = str_wrap(city, width = 35),
         model = case_when(
           model == "Model1" ~ "Impervious Cover",
           model == "Model2" ~ "Building Height and Impervious Cover",
           model == "Model3" ~ "Building Height",
           model == "Model4" ~ "NDVI",
           model == "Model5" ~ "Building Height and NDVI",
           model == "Model6" ~ "Impervious Cover and NDVI",
           model == "Model7" ~ "Building Height, Impervious Cover, and NDVI",
           TRUE ~ model
         ))

# make the model into a factor so we can control the order in which each model is plotted
plot_data$model <- factor(plot_data$model, levels=c("Building Height, Impervious Cover, and NDVI",
                                                   "Impervious Cover and NDVI",
                                                   "Building Height and NDVI",
                                                   "Building Height and Impervious Cover",
                                                   "NDVI",
                                                   "Impervious Cover",
                                                   "Building Height"))

# model weight
ggplot(plot_data, aes(x = model, y = weight_normalized, fill = model)) +
  geom_boxplot(outlier.size = 2, alpha = 0.7, width = 0.6) +  # Use boxplot for grouping by model
  scale_fill_brewer(palette = "Set2") + 
  theme_classic() +
  coord_flip() +
  labs(
    x = "",
    y = "Model Weight",
    color = "Model"
  ) +
  theme(
    axis.text.x = element_text(size = 18),  # Adjust the x-axis text size
    axis.text.y = element_text(size = 18),  # Adjust the y-axis text size
    axis.title.x = element_text(size = 22),
    legend.position = "none",
    panel.grid.major.x = element_line(color = "gray90"),
    panel.grid.minor.x = element_line(color = "gray95")
  ) 

ggsave("Figures_and_Tables/Figure_3.png", width = 12, height = 7, units = "in")

ggplot(plot_data, aes(x = model, y = weight_normalized, fill = model)) +
  geom_violin(alpha = 0.7, width = 0.8, trim = TRUE) + 
  scale_fill_brewer(palette = "Set2") + 
  theme_classic() +
  labs(
    x = "",
    y = "Model Weight",
    color = "Model"
  ) +
  theme(
    axis.text.x = element_text(size = 18),  # Adjust the x-axis text size
    axis.text.y = element_text(size = 18),  # Adjust the y-axis text size
    axis.title.y = element_text(size = 22),
    legend.position = "none",
    panel.grid.major.x = element_line(color = "gray90"),
    panel.grid.minor.x = element_line(color = "gray95")
  ) +
  coord_flip()

# R2
ggplot(plot_data, aes(x = model, y = r2, fill = model)) +
  geom_violin(alpha = 0.7, width = 0.8, trim = TRUE) + 
  scale_fill_brewer(palette = "Set2") + 
  theme_minimal() +
  labs(
    x = "",
    y = "R2",
    color = "Model"
  ) +
  theme(
    axis.text.x = element_text(size = 8),  # Adjust the x-axis text size
    axis.text.y = element_text(size = 8),  # Adjust the y-axis text size
    legend.position = "none",
    plot.title = element_text(size = 14),
    panel.grid.major.x = element_line(color = "gray90"),
    panel.grid.minor.x = element_line(color = "gray95")
  ) 

# R2
ggplot(plot_data, aes(x = model, y = r2, fill = model)) +
  geom_boxplot(outlier.size = 2, alpha = 0.7, width = 0.6) +  # Use boxplot for grouping by model
  scale_fill_brewer(palette = "Set2") + 
  theme_minimal() +
  labs(
    x = "",
    y = "R2",
    color = "Model"
  ) +
  theme(
    axis.text.x = element_text(size = 8),  # Adjust the x-axis text size
    axis.text.y = element_text(size = 8),  # Adjust the y-axis text size
    legend.position = "none",
    plot.title = element_text(size = 14),
    panel.grid.major.x = element_line(color = "gray90"),
    panel.grid.minor.x = element_line(color = "gray95")
  ) 



########################################################################

# let's determine if the difference in model weight are statistically significant
# Check normality of weight_normalized
plot_data$model <- factor(plot_data$model, levels=c("Building Height",
                                                    "Impervious Cover",
                                                    "NDVI",
                                                    "Building Height and Impervious Cover",
                                                    "Building Height and NDVI",
                                                    "Impervious Cover and NDVI",
                                                    "Building Height, Impervious Cover, and NDVI"
                                                    ))

plot_data$model_wrapped <- str_wrap(plot_data$model, width = 20)

ggplot(plot_data, aes(x = weight_normalized, fill = model)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~model_wrapped, scales="free_y") +
  labs(fill = "Model", x="Model Weight", y="Density") +
  theme_classic() +
  theme(strip.text = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12), 
        axis.title = element_text(size = 12),
        axis.text= element_text(size=10),
        legend.position = "none") +
  scale_x_continuous(labels = function(x) ifelse(x %in% c(0, 1), as.character(x), sprintf("%.2f", x))) 

ggsave("Figures_and_Tables/Figure_A9.png", width = 8, height = 8, units = "in")

# built is not normally distributed and the variances are not nearly equal, so we will use the Kruskal-Wallis test
kruskal_result <- kruskal.test(weight_normalized ~ model, data = plot_data)
kruskal_result

# Prepare data for plotting
plot_data <- results_with_weights %>%
  select(city, model, weight_normalized, r2) %>%
  mutate(city = str_wrap(city, width = 35),
         model = case_when(
           model == "Model1" ~ "Impervious Cover",
           model == "Model2" ~ "Impervious Cover and Building Height",
           model == "Model3" ~ "Building Height",
           model == "Model4" ~ "NDVI",
           model == "Model5" ~ "NDVI and Building\nHeight",
           model == "Model6" ~ "NDVI and Impervious\nCover",
           model == "Model7" ~ "NDVI, Impervious Cover,\nand Building Height",
           TRUE ~ model
         ))

# let's use Wilcoxon test for pairwise comparision
wilcox_results <- pairwise.wilcox.test(
  plot_data$weight_normalized,
  plot_data$model,
  p.adjust.method = "fdr"
)

# Format the results into a tibble for ggpubr
wilcox_df <- as.data.frame(wilcox_results$p.value) %>%
  rownames_to_column("group1") %>%
  pivot_longer(-group1, names_to = "group2", values_to = "p") %>%
  filter(!is.na(p)) %>%
  mutate(p = ifelse(p < 0.001, "< 0.001", sprintf("%.3f", p))) %>%
  rename(model1=group1, model2=group2)

# Step 2: Summarize Data (Means and SEs)
summary_stats <- plot_data %>%
  group_by(model) %>%
  summarize(
    mean = mean(weight_normalized),
    se = sd(weight_normalized) / sqrt(n()),
    .groups = "drop"
  )

wilcox_df
summary_stats

#combine tables
# First, make sure the column names in summary_stats match for joining
summary_stats_renamed1 <- summary_stats %>%
  rename(model1 = model, model1_mean = mean, model1_se = se)

summary_stats_renamed2 <- summary_stats %>%
  rename(model2 = model, model2_mean = mean, model2_se = se)

# Now join model1 and model2 stats to wilcox_df
combined_df <- wilcox_df %>%
  left_join(summary_stats_renamed1, by = "model1") %>%
  left_join(summary_stats_renamed2, by = "model2")  %>%
  select(model1, model1_mean, model1_se,
         model2, model2_mean, model2_se,
         p)
combined_df

write_csv(combined_df, "Figures_and_Tables/Table_A1.csv")

# create a heatmap of results
# Convert p-values to numeric
wilcox_df <- wilcox_df %>%
  mutate(p_numeric = as.numeric(str_replace(p, "< ", "")))

# Create full matrix of p-values
p_matrix <- wilcox_df %>%
  select(model1, model2, p_numeric) %>%
  pivot_wider(names_from = model2, values_from = p_numeric) %>%
  column_to_rownames("model1")

# Re-expand the p-value grid to include symmetric entries
heatmap_data <- wilcox_df %>%
  mutate(p = as.numeric(str_replace(p, "< ", ""))) %>%
  bind_rows(
    wilcox_df %>%
      rename(model1 = model2, model2 = model1) %>%
      mutate(p = as.numeric(str_replace(p, "< ", "")))
  )

# Create a filled matrix plot
ggplot(heatmap_data, aes(x = model1, y = model2, fill = p)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.3f", p)), size = 3) +
  scale_fill_gradient(low = "white", high = "red", name = "p-value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = NULL, y = NULL, title = "Pairwise p-values from Wilcoxon tests")

# Model comparision -------------------------------------------------------

# Is there a significant difference in deviance explained between models? 

# visualize the data
ggplot(results_with_weights, aes(x = model, y = dev_expl)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Model", y = "Deviance Explained")

# compare the range of deviance explained for each city
dev_diff <- results_with_weights %>%
  group_by(city) %>%
  summarise(min_dev=min(dev_expl),
            max_dev=max(dev_expl)) %>%
  mutate(diff_deviance=max_dev-min_dev)

hist(dev_diff$diff_deviance)

summary(dev_diff$diff_deviance)

# let's also make a histogram of deviance explained from the best models
hist(best_models$dev_expl)

# make into a nice plot for export
jpeg("Figures_and_Tables/Figure_A7.jpeg", width = 1600, height = 800, res = 150)

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))  # 1 row, 2 columns

# Plot (a)
hist(best_models$dev_expl,
     main = "(a) Deviance Explained from Best Models",
     xlab = "Deviance Explained",
     col = "lightblue")


# Plot (a)
hist(dev_diff$diff_deviance,
     main = "(b) Difference in Deviance Explained Between the\nBest Performing and Worst Performing Models",
     xlab = "Difference",
     col = "lightgreen")

dev.off()



#########################################################################

# let's plot by eco-region
results_with_weights <- left_join(results_with_weights, name_eco, by=c("city"="name"))

plot_data_eco <- results_with_weights %>%
  select(city, model, weight_normalized, r2, eco_name) %>%
  mutate(city = str_wrap(city, width = 35),
         model = case_when(
           model == "Model1" ~ "Impervious Cover",
           model == "Model2" ~ "Building Height\nand Impervious Cover",
           model == "Model3" ~ "Building Height",
           model == "Model4" ~ "NDVI",
           model == "Model5" ~ "Building Height\n and NDVI",
           model == "Model6" ~ "Impervious Cover\nand NDVI",
           model == "Model7" ~ "Building Height, Impervious\nCover, and NDVI",
           TRUE ~ model),
         model = factor(model, levels=c("Building Height, Impervious\nCover, and NDVI",
                                        "Impervious Cover\nand NDVI",
                                        "Building Height\n and NDVI",
                                        "Building Height\nand Impervious Cover",
                                        "NDVI",
                                        "Impervious Cover",
                                        "Building Height"
                                        ))) %>%
  filter(complete.cases(eco_name)) %>%
  group_by(eco_name, model) %>%
  filter(n() > 1) %>%   # Keep only city-model groups with more than 1 observation
  ungroup()

# model weight

# Count the number of unique cities per eco_name
eco_counts <- plot_data_eco %>%
  distinct(city, eco_name) %>%
  count(eco_name, name = "n_cities")

# Add "(N = n)" to eco_name
plot_data_eco <- plot_data_eco %>%
  left_join(eco_counts, by = "eco_name") %>%
  mutate(eco_name = paste(eco_name, " (N=", n_cities, ")", sep=""))

# Wrap legend text before plotting
plot_data_eco <- plot_data_eco %>%
  mutate(eco_name = str_wrap(eco_name, width = 20),
         eco_name = case_when(
           eco_name == "OZARK/OUACHITA-APPALACHIAN\nFORESTS (N=16)" ~ "OZARK/OUACHITA-\nAPPALACHIAN\nFORESTS (N=16)",
           TRUE ~ eco_name
         ))

ggplot(plot_data_eco, aes(x = model, y = weight_normalized, fill = model)) +
  geom_boxplot(outlier.size = 2, alpha = 0.7, width = 0.6) +
  scale_fill_brewer(palette = "Set2", name = "Model") +
  theme_classic() +
  coord_flip() +
  labs(
    x = "Model Type",
    y = "Model Weight"
  ) +
  facet_wrap(~eco_name) +
  scale_y_continuous(
    labels = function(x) {
      ifelse(x %in% c(0, 1), as.character(x), sprintf("%.2f", x))
    }
  ) +
  theme(
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 18),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 18),
    strip.text = element_text(size = 12),  # facet title text size
    legend.position = "none")


ggsave("Figures_and_Tables/Figure_A8.png", width = 13, height = 15, units = "in")


