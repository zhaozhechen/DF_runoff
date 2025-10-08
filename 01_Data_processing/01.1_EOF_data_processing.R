# Author: Zhaozhe Chen
# Date: 2025.10.7

# This code processes raw USGS EOF dataset
# Filtering out sites that should not be included
# Extract variables related to Q
# This data processing code is adapted from Ellen Albright (personal communication)

# -------- Global -----------
library(dplyr)
library(lfstat) # assign dates into USGS water years
library(sf)
library(lubridate)
library(readxl)

# Data paths ======
# USGS raw EOF Storm event data
usgs_eof <- read.csv("00_Data/USGS raw/All_EOF_StormEventLoadsFormatted.csv")
# DF Site info
