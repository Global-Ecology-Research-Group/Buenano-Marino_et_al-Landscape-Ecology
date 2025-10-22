##### Combining all land cover outputs from GEE into 1 RDS

library(dplyr)
library(readr)
library(tidyverse)

# The land cover GEE files are too large to share to the repository, but we do share the outputted gee_land_cover_combined.RDS

# creating a list of file names and reading them in
# total we have 427,963 LOCALITY_IDs, so that's how many rows we have
file_list <- list.files(path = "Intermediate_data/land_cover_GEE/", pattern = "\\.csv", full.names = TRUE)
r <- lapply(file_list, read_csv)
s <- bind_rows(r, .id = "column_id")
s <- as.data.frame(s)

# for easier merge with the checklist data
s <- rename(s, LOCALITY_ID = "LOCALIT")

# getting rid of the first two columns that we don't need
gee_varbs <- s %>%
  select(LOCALITY_ID:.geo)

# saving an RDS of the new combined file
saveRDS(gee_varbs, file = "Intermediate_data/land_cover_GEE/gee_land_cover_combined.RDS")





