# Author: Zhaozhe Chen
# Date: 2025.10.9

# This code is to compare daily P from USGS sites vs PRISM

# -------- Global -----------
library(dplyr)
library(lubridate)
library(dataRetrieval) # Used to extract USGS ppt data
# https://doi-usgs.github.io/dataRetrieval/index.html
# https://cran.r-project.org/web/packages/dataRetrieval/vignettes/dataRetrieval.html
library(stringr)

# Data paths =====
# Cleaned EOF dataframe
eof_df <- read.csv("00_Data/Processed_data/Cleaned_data/DF_EOF_cleaned.csv")
# PRISM ppt
ppt_df <- read.csv("00_Data/Processed_data/DF_PRISM_ppt.csv")
# Source functions
source("Functions/Data_processing_functions.R")

# Global parameters =======
# Code for precipitation. Unit: in 
P_code <- "00045"
# ------------ Main ----------
# Get Site info for USGS P extraction
Site_ls <- eof_df %>%
  distinct(Field_Name,USGS_Station_Number,StudyPeriod) %>%
  arrange(Field_Name) %>%
  rename(Site_ID = Field_Name,
         USGS_ID = USGS_Station_Number) %>%
  mutate(USGS_ID = str_remove(USGS_ID,"USGS-")) %>%
  # Extract start and end date
  mutate(
    WY_Start = as.integer(str_extract(StudyPeriod,"((?<=WY)\\d{4})")),
    WY_End = as.integer(str_extract(StudyPeriod,"((?<=-WY)\\d{4})")),
    Start = paste0(WY_Start - 1,"-10-01"),
    End = paste0(WY_End,"-09-30")
  )



arrayid <- 1
# Extract sub daily USGS P
USGS_subd_P <- USGS_ppt(arrayid, Site_ls)



# Aggregate to daily sum P

USGS_subd_P %>%
  renameNWISColumns()






