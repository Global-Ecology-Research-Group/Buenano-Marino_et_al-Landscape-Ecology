This repository contains all the code and files used in the analyses reported in "Does building height influence bird diversity in urban landscapes?" article, which is published in Landscape Ecology.

# Figures and Tables Folder

This folder contains all the main figures and supplemental figures and tables.

# Intermediate Data Folder

This folder contains all intermediate data, including summarized eBird data, building height data, and model results. The following is a description of each subfolder:

**bird_richness_by_city** – XXXX

**building_height** – Several files that summarize the building height data and urban areas for inclusion in the study.

**city_level_ebird_data** – XXXX

**compiled_data** – This folder contains 298 cities with eBird checklist information including species richness and summarized data within 250 meter buffer around checklists including percent land cover and mean, min, max, and standard deviation of building height. This data was used in the statistical analyses scripts.

**ecoregion_ebird_ndvi** – Contains mean NDVI value for the 250 meter buffer around each checklist used in this study.

**land_cover_GEE** – Percentage of each land cover type in the 250 meter buffer around each checklist.

**model_results** – Files containing model results from the statistical analyses scripts.

# R Folder

R scripts are organized into 6 sub-folders which are numbered to follow the order of the methods described in the paper. Below is a description of each sub-folder:

**1_Getting Cities shapefiles** – These scripts are used to extract building height shapefiles for each city and prepare the data to create the study area figure. Some of these scripts cannot be run because they require large data files that cannot be pushed to the respository. However, for building height data, the user can use the `1_get_building_height_shapefiles.R` script to extract the shapefiles from the `Shapefiles/dataset-links-to-building-height-shapefile.csv` file. Additionally, we provide the filtered and summarized files that are outputted from these scripts.

**2_Getting eBird data** – XXX

**3_Land cover data** – This folder contains one script which combines the individual city files containing land cover information for each checklist, into one combined file. Due to storage limitations, we do not share the raw files or exported file, but the summarized eBird data with these variables is shared in the *compiled_data* folder.

**4_Preparing intermediate data** – XXX

**4_Statistical anlysis** – This folder contains two scripts, `Statistical_model_obj_1.R` and `Statistical_model_obj_2.R`. These scripts contain all the code used to conduct the statistical analyses for objective 1 and 2 in the paper.

**4_Supplementary analysis** – Scripts to conduct supplemental analyses, which are presented in the supplemental file of the paper. Specifically, there is a script to examine the linear relationship between each land cover variable and species richness, the the relationship between land cover values calculated at different resolutions, the relationship between building height and number of buildings, and objective 2 repeated with the standard deviation of building height as a predictor variable.

# Shapefiles folder

This folder contains all the relevant shapefiles including:

**building_height_shapefiles:** All the building height shapefiles extracted from `Shapefiles/dataset-links-to-building-height-shapefile.csv.`

**urban_areas:** Shapefiles of all urban areas in the continental United States from the U.S. Census Bureau (U.S. Census Bureau. (2023). 2020 Census Urban Area TIGER/Line Shapefiles. Retrieved 09/2024 from <https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html>) and associated files.

**dataset-links-to-building-height-shapefile:** This is the CSV file of urban areas, that was obtained from <https://catalog.data.gov/dataset/urban-areas2>. This file provides URL links to access each building height shapefile.
