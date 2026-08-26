# Author: Zhaozhe Chen
# Update Date: 2026.8.10

# This code processes sediment and phosphorus observations for later analyses
# Only measured runoff and water-quality observations are retained

# The runoff-event filters are consistent with 01_Data_processing.R:
# 1. Discovery Farms project
# 2. Surface monitoring sites retained in DF_site_info.csv
# 3. Storm-associated runoff events
# 4. Valid runoff-event start and end times

# Two additional filters are applied:
# 1. estimated_flow_fraction equals 0
# 2. estimated equals 0

# -------- Global -----------
library(dplyr)
library(lubridate)
library(stringr)

# Confirm that the script is run from the project root
Project_path <- normalizePath(getwd(),winslash="/",mustWork=TRUE)

if(!dir.exists(file.path(Project_path,"00_Data","Raw"))){
  stop(
    "Run this script from the DF_runoff_v2 project root. ",
    "The folder 00_Data/Raw was not found."
  )
}

# Source functions ======
source(file.path(Project_path,"01_Functions","01_General_data_functions.R"))
source(file.path(Project_path,"01_Functions","02_Management_data_functions.R"))
source(file.path(Project_path,"01_Functions","06_Water_quality_data_functions.R"))

# Data paths ======
Raw_path <- file.path(Project_path,"00_Data","Raw")
Processed_path <- file.path(Project_path,"00_Data","Processed")

Runoff_path <- file.path(
  Raw_path,
  "All_EOF_StormEventLoadsRainCalculated.csv"
)

Site_info_path <- file.path(Processed_path,"DF_site_info.csv")
Management_path <- file.path(
  Processed_path,
  "Management_site_water_year.csv"
)

required_files <- c(Runoff_path,Site_info_path,Management_path)
missing_files <- required_files[!file.exists(required_files)]

if(length(missing_files) > 0){
  stop(
    "Required input files were not found. Run 01_Data_processing.R first. ",
    "Missing: ",
    paste(missing_files,collapse=", ")
  )
}

# ------- Main ---------
# Step 1. Import the previously processed explanatory variables =========
DF_site_info <- read.csv(
  Site_info_path,
  check.names=FALSE,
  stringsAsFactors=FALSE,
  na.strings=c("","NA")
) %>%
  remove_bom_names()

Management_df <- read.csv(
  Management_path,
  check.names=FALSE,
  stringsAsFactors=FALSE,
  na.strings=c("","NA")
) %>%
  remove_bom_names()

# Step 2. Filter and process measured water-quality events ==============
Water_quality_events <- process_sediment_phosphorus_events(
  runoff_path=Runoff_path,
  site_df=DF_site_info
)

if(nrow(Water_quality_events) == 0){
  stop("No events remained after applying the requested filters.")
}

# Step 3. Add site-level explanatory variables ==========================
Site_variables <- DF_site_info %>%
  dplyr::select(
    Field_Name,
    Monitoring,
    FarmEnterprise,
    CropRotation,
    LandCover,
    LandCover_Updated,
    Tile,
    Tile_Notes,
    SoilType,
    Hydrologic_Group,
    DrainageClass,
    MeanSlope_per,
    Clay_Fraction
  )

Water_quality_analysis <- Water_quality_events %>%
  dplyr::left_join(Site_variables,by="Field_Name") %>%
  add_seasonal_management(
    event_date=Q_start,
    management_df=Management_df
  )

# Step 4. Confirm the requested filter conditions =======================
if(any(Water_quality_analysis$estimated_flow_fraction != 0,na.rm=TRUE)){
  stop("The output contains an event with estimated_flow_fraction != 0.")
}

if(any(Water_quality_analysis$estimated != 0,na.rm=TRUE)){
  stop("The output contains an event with estimated != 0.")
}

if(any(Water_quality_analysis$Monitoring != "Surface",na.rm=TRUE)){
  stop("The output contains a non-surface monitoring site.")
}

# Step 5. Output the analysis-ready dataset =============================
Output_path <- file.path(
  Processed_path,
  "Sediment_phosphorus_analysis.csv"
)

write.csv(
  Water_quality_analysis,
  Output_path,
  row.names=FALSE,
  na=""
)

message("Sediment and phosphorus data processing complete.")
message("Events retained: ",nrow(Water_quality_analysis))
message("Sites retained: ",dplyr::n_distinct(Water_quality_analysis$Field_Name))
message("Processed data: ",Output_path)
