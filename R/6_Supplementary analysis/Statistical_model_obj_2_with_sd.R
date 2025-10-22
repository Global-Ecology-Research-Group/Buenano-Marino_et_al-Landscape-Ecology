# Install required packages if not already installed
if (!require("mgcv")) install.packages("mgcv")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("gridExtra")) install.packages("gridExtra")
if (!require("scales")) install.packages("scales")
if(!require("vcd")) install.packages("vcd")
if(!require("corrplot")) install.packages("corrplot")
if(!require("lmtest")) install.packages("lmtest")
if(!require("grid")) install.packages("grid")
if(!require("stringr")) install.packages("stringr")

# Load required libraries
library(mgcv)
library(dplyr)
library(ggplot2)
library(gridExtra)  # For arranging multiple plots
library(scales)
library(sf)
library(vcd)
library(lmtest)
library(grid)
library(corrplot)
library(stringr)
library(readr)

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

# add NDVI and ecoregion data to all_city_data
er_data <- readRDS("Intermediate_data/ecoregion_ebird/ecoregion_ebird_ndvi.RDS")

er_data <- er_data %>%
  as.data.frame() %>%
  dplyr::select(LOCALIT, eco_name, mean) %>%
  rename(NDVI=mean)

all_city_data <- left_join(all_city_data, er_data, by=c("LOCALITY_ID"="LOCALIT"))

# check the distribution of the response variable
hist(all_city_data$total_richness)
# it is positively skewed

# check for multicolinearity
city_rel <- all_city_data %>%
  filter(complete.cases(mean_h),
         complete.cases(NDVI),
         complete.cases(sd_h)) %>%
  select(total_richness, duration, mean_h, sd_h, built,
         bare, grass, shrub_and_scrub, trees, water, NDVI, LAT, LONG, name)

summary(city_rel)

mean(city_rel$mean_h)
sd(city_rel$sd_h)

# let's make the column names prettier
city_rel_corr <- city_rel %>%
  rename("Total Richness" = total_richness, "Duration" = duration, "Mean Height" = mean_h,
         "Standard Deviation of Height" = sd_h,
         "Impervious Cover" = built, "Bare" = bare, 
         "Grass" = grass, "Shrub and Scrub" = shrub_and_scrub, 
         "Trees" = trees, "Water" = water)

# let's check the correlation metrix of the variables
corr_mat <- cor(city_rel_corr[,-c(1, 12:14)], method="pearson")
corr_mat

# correlation between mean height and sd
cor(city_rel_corr %>% select(`Mean Height`, `Standard Deviation of Height`))

# visualize the correlation
jpeg("Results/predictor_correlation_sd.jpeg", width = 7, height = 7, units = "in", res = 300)

corrplot(corr_mat, method = "color", 
         type = "lower",      # only lower triangle
         tl.col = "black",    # text label color
         addCoef.col = "black", # add correlation coefficients
         number.cex = 0.7,
         diag = FALSE)

# Close the device
dev.off()

city_test <- city_rel %>%
  filter(name=="Exeter, NH")

model_test <- gam(total_richness ~ built + sd_h +
      bare + grass + shrub_and_scrub + water + trees + NDVI +
      s(duration, k=20, bs="cr") + 
      s(LAT, LONG, k=50, bs="tp"),
    family = Gamma(link = "log"),
    data = city_test, method = "REML")

summary(model_test)

gam.check(model_test)

# run a model with all data and visualize the relationship between species richness and building height
model_test <- gam(total_richness ~ built + sd_h +
                    bare + grass + shrub_and_scrub + water + trees + NDVI +
                    s(duration, k=20, bs="cr") + 
                    s(LAT, LONG, k=50, bs="tp"),
                  family = Gamma(link = "log"),
                  data = city_rel, method = "REML")

summary(model_test)

gam.check(model_test)

plot(model_test, select = 1, shade = TRUE)


# Model the Data ----------------------------------------------------------

# scale the variables
summary(city_rel)

city_data <- city_rel

city_data$sd_h <- scale(city_data$sd_h)
city_data$built <- scale(city_data$built)
city_data$bare <- scale(city_data$bare)
city_data$grass <- scale(city_data$grass)
city_data$shrub_and_scrub <- scale(city_data$shrub_and_scrub)
city_data$water <- scale(city_data$water)
city_data$trees <- scale(city_data$trees)
city_data$NDVI <- scale(city_data$NDVI)

hist(city_data$built)
hist(city_data$sd_h)
hist(city_data$trees)
hist(city_data$bare)
hist(city_data$grass)
hist(city_data$shrub_and_scrub)
hist(city_data$water)
hist(city_data$NDVI)

# Set k_val for smooth terms
k_val_dur <- 20 
k_val_geo <- 50

# Function to read and analyze a city
analyze_city <- function(file_path) {
  tryCatch({
    city_data <- readRDS(file_path)
    city_name <- tools::file_path_sans_ext(basename(file_path))
    
    # add NDVI
    city_data <- left_join(city_data, er_data, by=c("LOCALITY_ID"="LOCALIT"))
    
    # rescale the variables
    city_data$sd_h <- scale(city_data$sd_h)
    city_data$built <- scale(city_data$built)
    city_data$bare <- scale(city_data$bare)
    city_data$grass <- scale(city_data$grass)
    city_data$shrub_and_scrub <- scale(city_data$shrub_and_scrub)
    city_data$water <- scale(city_data$water)
    city_data$trees <- scale(city_data$trees)
    city_data$snow_and_ice <- scale(city_data$snow_and_ice)
    city_data$crops <- scale(city_data$crops)
    city_data$flooded_vegetation <- scale(city_data$flooded_vegetation)
    city_data$NDVI <- scale(city_data$NDVI)
    
    # New model
    new_model <- gam(total_richness ~ built + sd_h +
                       bare + grass + shrub_and_scrub + water + trees + NDVI +
                       s(duration, k=k_val_dur, bs="cr") + 
                       s(LAT, LONG, k=k_val_geo, bs="tp"),
                     family = Gamma(link = "log"),
                     data = city_data, method = "REML")
    
    # Extract coefficients
    coef_summary <- summary(new_model)$p.table
    coef_data <- data.frame(
      city = city_name,
      variable = rownames(coef_summary),
      estimate = coef_summary[, "Estimate"],
      std_error = coef_summary[, "Std. Error"],
      p_value = coef_summary[, "Pr(>|t|)"]
    )
    
    return(coef_data)
  }, error = function(e) {
    message(sprintf("Error processing city: %s\nError: %s", file_path, e$message))
    return(NULL)
  })
}

# Read and analyze all cities
city_files <- list.files(folder_path, pattern = "\\.RDS$", full.names = TRUE)

results_list <- lapply(seq_along(city_files), function(i) {
  message("Running loop ", i, ": ", city_files[[i]])
  analyze_city(city_files[[i]])
})
results <- do.call(rbind, results_list)

# save the results
saveRDS(results, "Intermediate_data/model_results/Obj2_Models_sd.RDS")

# Function to remove outliers based on IQR
#remove_outliers <- function(x, factor = 1.5) {
  #qnt <- quantile(x, probs=c(.25, .75), na.rm = TRUE)
  #H <- factor * IQR(x, na.rm = TRUE)
  #x[x < (qnt[1] - H) | x > (qnt[2] + H)] <- NA
  #return(x)
#}

# Apply the function to remove outliers
#results <- results %>%
  #group_by(variable) %>%
  #mutate(estimate = remove_outliers(estimate)) %>%
  #ungroup() %>%
  #filter(variable != "mean_h")

# Prepare data for plotting
plot_data <- results %>%
  filter(variable != "(Intercept)") %>%
  filter(!is.na(estimate)) %>%  # Remove NAs resulting from outlier removal
  mutate(variable = dplyr::recode(variable,
                           "built" = "Impervious Cover",
                           "mean_h" = "Mean Building Height",
                           "bare" = "Bare",
                           "grass" = "Grass",
                           "shrub_and_scrub" = "Shrub and Scrub",
                           "water" = "Water",
                           "trees" = "Trees")) %>%
  mutate(variable = factor(variable, levels = c("Bare",
                                                "Grass",
                                                "Shrub and Scrub",
                                                "Water",
                                                "Trees",
                                                "Impervious Cover",
                                                "Mean Building Height",
                                                "NDVI")))

# Create the plot
ggplot(plot_data, aes(x = estimate, y = variable, color = p_value < 0.05)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(position = position_jitter(height = 0.2), alpha = 0.5) +
  scale_color_manual(values = c("gray", "red"), labels = c("p ≥ 0.05", "p < 0.05")) +
  theme_minimal() +
  labs(
    title = "Coefficient Estimates for Urban Landscape Variables",
    x = "Coefficient Estimate",
    y = "Variable",
    color = "Significance"
  ) +
  theme(legend.position = "bottom")


#########################################################################################

# Test which variables have statistically significant trends
# Test which variables are significantly different from 0 (i.e., significant positive or negative trend)
ggplot(plot_data, aes(x = estimate)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", alpha = 0.7, color = "black") +  # Adjust binwidth as needed
  facet_wrap(~ variable, scales = "free_y") +  # Facet by variable, each with its own y-axis scale
  theme_minimal() +
  labs(
    title = "Histograms of Estimate Values by Variable",
    x = "Estimate",
    y = "Frequency"
  ) +
  theme(
    strip.text = element_text(size = 12),  # Adjust facet label size
    axis.text = element_text(size = 10),   # Adjust axis text size
    axis.title = element_text(size = 12)   # Adjust axis title size
  )

# format data for plotting and statistical tests
plot_data <- results %>%
  filter(variable != "(Intercept)") %>%
  filter(!is.na(estimate)) %>%  # Remove NAs
  mutate(variable = dplyr::recode(variable,
                                  "built" = "Impervious Cover",
                                  "sd_h" = "Standard Deviation\nof Building Height",
                                  "bare" = "Bare",
                                  "grass" = "Grass",
                                  "shrub_and_scrub" = "Shrub and Scrub",
                                  "water" = "Water",
                                  "trees" = "Trees")) %>%
  mutate(variable = factor(variable, levels = c("Bare",
                                                "Grass",
                                                "Shrub and Scrub",
                                                "Water",
                                                "Trees",
                                                "NDVI",
                                                "Impervious Cover",
                                                "Standard Deviation\nof Building Height")))

# Get the mean/SD of each variable
test_plot <- plot_data %>%
  group_by(variable) %>%
  dplyr::summarise(mean=mean(estimate, na.rm=TRUE), 
                   se = sd(estimate, na.rm = TRUE) / sqrt(n()),
                   n = n(),
                   t_stat = (mean(estimate, na.rm = TRUE) - 0) / (sd(estimate, na.rm = TRUE) / sqrt(n())),
                   p_value = 2 * (1 - pt(abs(t_stat), df = n - 1))) %>%
  dplyr::mutate(sig_class = case_when(
    p_value < 0.05 & mean > 0  ~ "Positive",
    p_value < 0.05 & mean < 0  ~ "Negative",
    TRUE                       ~ "Not Significant"))
test_plot

# Let's see which cities have a significant positive or negative trend with each variable.
# We can start by defining different levels of p-value
results_examine <- results %>%
  mutate(p_value_group=ifelse(p_value<0.05, "significant", "not-significant"),
         estimate_trend=ifelse(estimate>0, "positive", "negative")) %>%
  mutate(
    significance_trend = case_when(
      p_value_group == "significant" & estimate_trend == "positive" ~ "significant positive",
      p_value_group == "significant" & estimate_trend == "negative" ~ "significant negative",
      p_value_group != "significant" & estimate_trend == "positive" ~ "not significant positive",
      p_value_group != "significant" & estimate_trend == "negative" ~ "not significant negative"
    )
  )

trends <- results_examine %>%
  group_by(variable, significance_trend) %>%
  summarise(count = n()) %>%
  group_by(variable) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()
trends

trends %>%
  filter(variable!="(Intercept)") %>%
  pivot_wider(values_from=percentage, names_from=variable)

library(tidyr)
clean_trends <- trends %>%
  filter(variable != "(Intercept)") %>%
  mutate(percentage=round(percentage, 2)) %>%
  group_by(variable, significance_trend) %>%
  summarise(percentage = mean(percentage, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = significance_trend,
    values_from = percentage
  ) %>%
  select(variable, `significant negative`, `significant positive`) %>%
  rename("Negative trend - (%)" = `significant negative`, "Positive trend + (%)" = `significant positive`)
clean_trends

##################################################################################################

# now get significant class merged with the plot data
plot_data_merged <- plot_data %>%
  left_join(test_plot %>% dplyr::select(variable, sig_class), by = "variable")

# define custom colors to show significance
label_colors <- c(
  "Positive" = "#377eb8",       # blue
  "Negative" = "#e41a1c",       # red
  "Not Significant" = "gray30"  # neutral gray
)

# Map each variable to its significance color
var_label_colors <- setNames(
  label_colors[plot_data_merged %>% distinct(variable, sig_class) %>% arrange(variable) %>% pull(sig_class)],
  levels(plot_data_merged$variable)
)

# let's try plotting this as a box-and-whisker plot
ggplot(plot_data_merged, aes(x = estimate, y = variable, fill = variable)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(
    values = c(
      "Mean Building Height" = "#E5C494",  
      "Standard Deviation/nof Building Height" = "red4",
      "Impervious Cover" = "#FFD92F",      
      "NDVI" = "#A6D854",
      "Trees" = "#E78AC3", 
      "Water" = "#8DA0CB",
      "Shrub and Scrub" = "#FC8D62",
      "Grass" = "#66C2A5", 
      "Bare" = "#B3B3B3"
    )
  ) +
  theme_classic() +
  labs(x = "Coefficient Estimate", y = "") +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 14),
    axis.text.y = element_text(size = 12, color = var_label_colors, face="bold"),
    axis.text.x = element_text(size = 12)
  )

#####################################################################without outliers boxplot

# Modified boxplot without outliers
ggplot(plot_data_merged, aes(x = estimate, y = variable, fill = variable)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  scale_fill_manual(
    values = c(
      "Mean Building Height" = "#E5C494",  
      "Impervious Cover" = "#FFD92F",      
      "NDVI" = "#A6D854",
      "Trees" = "#E78AC3", 
      "Water" = "#8DA0CB",
      "Shrub and Scrub" = "#FC8D62",
      "Grass" = "#66C2A5", 
      "Bare" = "#B3B3B3"
    )
  ) +
  coord_cartesian(xlim = quantile(plot_data$estimate, c(0.05, 0.95)) * 1.1) +
  theme_classic() +
  labs(x = "Coefficient Estimate", y = "") +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 14),
    axis.text.y = element_text(size = 12, color = var_label_colors, face="bold"),
    axis.text.x = element_text(size = 12)
  )

ggsave("Figures_and_Tables/Figure_A11.png", 
       width = 10, height = 5, units = "in", dpi = 300)

# how many data points are we excluded from the visualization with our defined x limits
xlims <- quantile(plot_data$estimate, c(0.05, 0.95)) * 1.1

excluded <- plot_data %>%
  filter(estimate < xlims[1] | estimate > xlims[2])

included <- plot_data %>%
  filter(estimate >= xlims[1] & estimate <= xlims[2])

n_excluded <- nrow(excluded)
n_total <- nrow(plot_data)
pct_excluded <- round(n_excluded / n_total * 100, 2)

cat("Excluded observations:", n_excluded, "out of", n_total, 
    "(", pct_excluded, "%)\n")


#########################################################################

# let's plot by eco-region
name_eco <- all_city_data %>%
  group_by(name) %>%
  summarise(eco_name=first(eco_name))

results <- left_join(results, name_eco, by=c("city"="name"))

# now plot the data by ecoregion
# Prepare data for plotting (unchanged)
plot_data_eco <- results %>%
  filter(variable != "(Intercept)",
         complete.cases(eco_name)) %>%  # Remove NAs
  mutate(variable = dplyr::recode(variable,
                                  "built" = "Impervious Cover",
                                  "mean_h" = "Mean Building Height",
                                  "bare" = "Bare",
                                  "grass" = "Grass",
                                  "shrub_and_scrub" = "Shrub and Scrub",
                                  "water" = "Water",
                                  "trees" = "Trees")) %>%
  mutate(variable = factor(variable, levels = c("Bare",
                                                "Grass",
                                                "Shrub and Scrub",
                                                "Water",
                                                "Trees",
                                                "NDVI",
                                                "Impervious Cover",
                                                "Mean Building Height"))) %>%
  group_by(eco_name, variable) %>%
  filter(n() > 1) %>%   # Keep only city-model groups with more than 1 observation
  ungroup()

#######################################################################################

# Test which variables have statistically significant trends

# Get the mean/SD of each variable
t_test_eco <- plot_data_eco %>%
  group_by(variable, eco_name) %>%
  dplyr::summarise(mean=mean(estimate, na.rm=TRUE), 
                   se = sd(estimate, na.rm = TRUE) / sqrt(n()),
                   n = n(),
                   t_stat = (mean(estimate, na.rm = TRUE) - 0) / (sd(estimate, na.rm = TRUE) / sqrt(n())),
                   p_value = 2 * (1 - pt(abs(t_stat), df = n - 1))
  ) %>%
  filter(p_value <= 0.05)
t_test_eco

# Let's see which cities have a significant positive or negative trend with each variable.
# We can start by defining different levels of p-value
results_examine <- results %>%
  mutate(p_value_group=ifelse(p_value<0.05, "significant", "not-significant"),
         estimate_trend=ifelse(estimate>0, "positive", "negative")) %>%
  mutate(
    significance_trend = case_when(
      p_value_group == "significant" & estimate_trend == "positive" ~ "significant positive",
      p_value_group == "significant" & estimate_trend == "negative" ~ "significant negative",
      p_value_group != "significant" & estimate_trend == "positive" ~ "not significant positive",
      p_value_group != "significant" & estimate_trend == "negative" ~ "not significant negative"
    )
  )

trends_eco <- results_examine %>%
  group_by(variable, eco_name, significance_trend) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(variable, eco_name) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

################################################################################################################

# plot the data by ecoregion

# Count the number of unique cities per eco_name
eco_counts <- results %>%
  distinct(city, eco_name) %>%
  count(eco_name, name = "n_cities")

# Add "(N = n)" to eco_name
plot_data_eco_cities <- plot_data_eco %>%
  left_join(eco_counts, by = "eco_name") %>%
  mutate(eco_name = paste(eco_name, " (N=", n_cities, ")", sep=""))

# Wrap legend text before plotting
plot_data_eco_cities <- plot_data_eco_cities %>%
  mutate(eco_name = str_wrap(eco_name, width = 20),
         eco_name = case_when(
           eco_name == "OZARK/OUACHITA-APPALACHIAN\nFORESTS (N=16)" ~ "OZARK/OUACHITA-\nAPPALACHIAN\nFORESTS (N=16)",
           TRUE ~ eco_name
         ))

# Modified boxplot without outliers
ggplot(plot_data_eco_cities, aes(x = estimate, y = variable, fill = variable)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30") +
  geom_boxplot(
    alpha = 0.7,
    outlier.shape = NA  # Removes outliers from display
  ) +
  facet_wrap(~eco_name, scale="free_x") +
  scale_fill_manual(
    values = c(
      "Mean Building Height" = "#E5C494",  
      "Impervious Cover" = "#FFD92F",      
      "NDVI" = "#A6D854",
      "Trees" = "#E78AC3", 
      "Water" = "#8DA0CB",
      "Shrub and Scrub" = "#FC8D62",
      "Grass" = "#66C2A5", 
      "Bare" = "#B3B3B3"
    )
  ) +
  theme_classic() +
  labs(
    x = "Coefficient Estimate",
    y = "Variable"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 10)
  )

# how many data points are we excluded from the visualization with our defined x limits
xlims <- quantile(plot_data_eco$estimate, c(0.05, 0.95)) * 3

excluded <- plot_data_eco %>%
  filter(estimate < xlims[1] | estimate > xlims[2])

included <- plot_data_eco %>%
  filter(estimate >= xlims[1] & estimate <= xlims[2])

n_excluded <- nrow(excluded)
n_total <- nrow(plot_data_eco)
pct_excluded <- round(n_excluded / n_total * 100, 2)

cat("Excluded observations:", n_excluded, "out of", n_total, 
    "(", pct_excluded, "%)\n")

# 23 data points are excluded
