# Author: Zhaozhe Chen
# Date: 2025.10.9

# -------- Global -----------
library(dplyr)
library(lubridate)
library(dataRetrieval) # Used to extract USGS ppt data


# Data paths =====
# Cleaned EOF dataframe
eof_df <- read.csv("00_Data/Processed_data/Cleaned_data/DF_EOF_cleaned.csv")
# PRISM ppt
ppt_df <- read.csv("00_Data/Processed_data/DF_PRISM_ppt.csv")

# ------- Main ----------


# Compare PRISM vs USGS daily P





plot(ppt_df$KP3)

# USGS station ID
USGS_ID <- "451021089064901"
# code for precipitation variable, Unit: inches
var_code <- "00045"
start <- "2019-01-01"
end <- "2023-12-31"

# Get ppt at min resolution
ppt <- readNWISuv(siteNumbers = USGS_ID,
                  parameterCd = var_code,
                  startDate = start,
                  endDate = end,
                  tz="UTC")
# Filter out non-published 
