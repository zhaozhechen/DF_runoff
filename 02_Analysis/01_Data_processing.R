# Author: Zhaozhe Chen
# Update Date: 2026.7.27

# This code processes the raw Discovery Farms and USGS datasets
# The output includes analysis-ready precipitation and runoff event datasets

# Only surface monitoring outlets are included
# Site-level tile drainage is retained as an agricultural practice variable

# Seasons:
# Spring: January-May
# Summer: June-September
# Fall: October-December

# -------- Global -----------
library(dplyr)
library(tidyr)
library(lubridate)
library(readxl)
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
source(file.path(Project_path,"01_Functions","03_Event_data_functions.R"))

# Data paths ======
Raw_path <- file.path(Project_path,"00_Data","Raw")
Reference_path <- file.path(Project_path,"00_Data","Reference")
Processed_path <- file.path(Project_path,"00_Data","Processed")
Report_path <- file.path(Project_path,"03_Reports")

dir.create(Processed_path,recursive=TRUE,showWarnings=FALSE)
dir.create(Report_path,recursive=TRUE,showWarnings=FALSE)

# Raw event data
Runoff_path <- file.path(
  Raw_path,
  "All_EOF_StormEventLoadsRainCalculated.csv"
)
Precipitation_path <- file.path(
  Raw_path,
  "All_EOF_RainEvents.csv"
)
PRISM_T_path <- file.path(
  Raw_path,
  "DF_PRISM_tmean.csv"
)

# Site and management data
Site_metadata_path <- file.path(
  Raw_path,
  "DF EOF Site & Year Metadata (2004-2023)-Site_Update.csv"
)
Site_location_path <- file.path(
  Raw_path,
  "DiscoveryFarms_SiteLocations.xlsx"
)
Tile_path <- file.path(
  Raw_path,
  "DF Surface EOF Tile Info.csv"
)
Tillage_path <- file.path(
  Raw_path,
  "DF Surface EOF Tillage Info.csv"
)
Crop_residue_path <- file.path(
  Raw_path,
  "DF Surface EOF Crop Residue Info.csv"
)

# Reference data
Site_time_path <- file.path(
  Reference_path,
  "EOF_Site_Table.csv"
)
Soil_path <- file.path(
  Reference_path,
  "Soil_Texture_Lookup_table.csv"
)

# ------- Main ---------
# Step 1. Process site-level information =====================
DF_site_info <- process_site_info(
  site_metadata_path=Site_metadata_path,
  site_location_path=Site_location_path,
  site_time_path=Site_time_path,
  soil_path=Soil_path,
  tile_path=Tile_path
)

Target_sites <- DF_site_info$Field_Name

# Step 2. Process management information =====================
# Tillage is weighted by the percentage of the monitored basin
Tillage_output <- process_tillage_info(
  tillage_path=Tillage_path,
  target_sites=Target_sites
)

# Crop and residue information are provided in the updated residue workbook
Crop_residue_output <- process_crop_residue_info(
  residue_path=Crop_residue_path,
  target_sites=Target_sites
)

Management_df <- assemble_management_info(
  tillage_df=Tillage_output$summary,
  crop_residue_df=Crop_residue_output$summary,
  target_sites=Target_sites
)

# Step 3. Process USGS runoff events ==========================
Q_events <- process_runoff_events(
  runoff_path=Runoff_path,
  site_df=DF_site_info
)

# Step 4. Process USGS precipitation events ==================
P_events <- process_precipitation_events(
  precipitation_path=Precipitation_path,
  prism_temperature_path=PRISM_T_path,
  site_df=DF_site_info
)

# Step 5. Match precipitation and runoff events ===============
PQ_output <- match_precipitation_runoff(
  precipitation_df=P_events,
  runoff_df=Q_events
)

P_events <- PQ_output$precipitation
Q_events <- PQ_output$runoff

# Step 6. Include site and seasonal management variables ======
Site_variables <- DF_site_info %>%
  select(
    Field_Name,
    Monitoring,
    FarmEnterprise,
    CropRotation,
    LandCover,
    LandCover_Updated,
    Tile,
    Tile_Notes,
    SoilType,
    HydrologicGroup,
    DrainageClass,
    MeanSlope_per,
    Clay_Fraction
  )

P_analysis_all <- P_events %>%
  left_join(Site_variables,by="Field_Name") %>%
  add_seasonal_management(
    event_date=P_start,
    management_df=Management_df
  )

Q_analysis_all <- Q_events %>%
  left_join(Site_variables,by="Field_Name") %>%
  add_seasonal_management(
    event_date=Q_start,
    management_df=Management_df
  )

# Only keep non-frozen events in the primary analysis datasets
P_analysis_nonfrozen <- P_analysis_all %>%
  filter(P_frozen == FALSE)

Q_analysis_nonfrozen <- Q_analysis_all %>%
  filter(frozen == "Non-Frozen")

# Step 7. Output processed datasets ===========================
write.csv(
  DF_site_info,
  file.path(Processed_path,"DF_site_info.csv"),
  row.names=FALSE,
  na=""
)

write.csv(
  Tillage_output$raw,
  file.path(Processed_path,"Tillage_field_clean.csv"),
  row.names=FALSE,
  na=""
)

write.csv(
  Crop_residue_output$raw,
  file.path(Processed_path,"Crop_residue_field_clean.csv"),
  row.names=FALSE,
  na=""
)

write.csv(
  Management_df,
  file.path(Processed_path,"Management_site_water_year.csv"),
  row.names=FALSE,
  na=""
)

write.csv(
  P_analysis_all,
  file.path(Processed_path,"All_P_events.csv"),
  row.names=FALSE,
  na=""
)

write.csv(
  Q_analysis_all,
  file.path(Processed_path,"All_Q_events.csv"),
  row.names=FALSE,
  na=""
)

write.csv(
  P_analysis_nonfrozen,
  file.path(Processed_path,"NonFrozen_P_analysis.csv"),
  row.names=FALSE,
  na=""
)

write.csv(
  Q_analysis_nonfrozen,
  file.path(Processed_path,"NonFrozen_Q_analysis.csv"),
  row.names=FALSE,
  na=""
)

# Step 8. Generate data-processing report =====================
P_season_n <- P_analysis_nonfrozen %>%
  count(Season,name="n")

Q_season_n <- Q_analysis_nonfrozen %>%
  count(Season,name="n")

Report_lines <- c(
  "# Data Processing Summary",
  "",
  paste0("Generated: ",Sys.Date()),
  "",
  "## Processing scope",
  "",
  paste0("- Surface monitoring sites retained: ",nrow(DF_site_info)),
  paste0("- Sites with site-level tile drainage: ",sum(DF_site_info$Tile == "Yes",na.rm=TRUE)),
  paste0("- All precipitation events: ",nrow(P_analysis_all)),
  paste0("- Non-frozen precipitation events: ",nrow(P_analysis_nonfrozen)),
  paste0("- All storm-associated runoff events: ",nrow(Q_analysis_all)),
  paste0("- Non-frozen runoff events: ",nrow(Q_analysis_nonfrozen)),
  "",
  "## Non-frozen events by season",
  "",
  "### Precipitation",
  "",
  paste0("- ",P_season_n$Season,": ",P_season_n$n),
  "",
  "### Runoff",
  "",
  paste0("- ",Q_season_n$Season,": ",Q_season_n$n),
  "",
  "## Management data availability",
  "",
  paste0(
    "- Precipitation events missing perennial fraction: ",
    sum(is.na(P_analysis_nonfrozen$PerennialFrac))
  ),
  paste0(
    "- Precipitation events missing seasonal tillage passes: ",
    sum(is.na(P_analysis_nonfrozen$Tillage_Passes))
  ),
  paste0(
    "- Fall/spring precipitation events missing residue fraction: ",
    sum(
      is.na(P_analysis_nonfrozen$Residue_Frac) &
        P_analysis_nonfrozen$Season %in% c("Fall","Spring")
    )
  ),
  paste0(
    "- Runoff events missing perennial fraction: ",
    sum(is.na(Q_analysis_nonfrozen$PerennialFrac))
  ),
  paste0(
    "- Runoff events missing seasonal tillage passes: ",
    sum(is.na(Q_analysis_nonfrozen$Tillage_Passes))
  ),
  paste0(
    "- Fall/spring runoff events missing residue fraction: ",
    sum(
      is.na(Q_analysis_nonfrozen$Residue_Frac) &
        Q_analysis_nonfrozen$Season %in% c("Fall","Spring")
    )
  ),
  "",
  "## Important definitions",
  "",
  "- Water year runs from October 1 through September 30.",
  "- Spring is January-May, summer is June-September, and fall is October-December.",
  "- Fall tillage combines the previous water year's summer passes with the current water year's fall passes.",
  "- Spring tillage combines current-water-year fall and spring passes.",
  "- Summer tillage combines current-water-year spring and summer passes.",
  "- Fall and spring use the previous crop; summer uses the current crop.",
  "- Residue is included for fall and spring only.",
  "- Cover crops are not included because the available record is incomplete and temporally biased."
)

writeLines(
  Report_lines,
  file.path(Report_path,"01_Data_processing_summary.md"),
  useBytes=TRUE
)

message("Data processing complete.")
message("Processed data: ",Processed_path)
message("Report: ",file.path(Report_path,"01_Data_processing_summary.md"))
