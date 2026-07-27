# Author: Zhaozhe Chen
# Update Date: 2026.7.27

# This code validates the processed datasets used for analyses
# Run 01_Data_processing.R before running this code

# -------- Global -----------
library(dplyr)
library(lubridate)

# Confirm that the script is run from the project root
Project_path <- normalizePath(getwd(),winslash="/",mustWork=TRUE)

if(!dir.exists(file.path(Project_path,"00_Data","Processed"))){
  stop(
    "Run this script from the DF_runoff_v2 project root. ",
    "The folder 00_Data/Processed was not found."
  )
}

# Source functions ======
source(file.path(Project_path,"01_Functions","01_General_data_functions.R"))

# Data paths ======
Processed_path <- file.path(Project_path,"00_Data","Processed")
Report_path <- file.path(Project_path,"03_Reports")

# Read processed data
DF_site_info <- read.csv(
  file.path(Processed_path,"DF_site_info.csv"),
  stringsAsFactors=FALSE
)

Management_df <- read.csv(
  file.path(Processed_path,"Management_site_water_year.csv"),
  stringsAsFactors=FALSE
)

P_df <- read.csv(
  file.path(Processed_path,"NonFrozen_P_analysis.csv"),
  stringsAsFactors=FALSE
) %>%
  mutate(P_start=as.Date(substr(P_start,1,10)))

Q_df <- read.csv(
  file.path(Processed_path,"NonFrozen_Q_analysis.csv"),
  stringsAsFactors=FALSE
) %>%
  mutate(Q_start=as.Date(substr(Q_start,1,10)))

Crop_residue_field <- read.csv(
  file.path(Processed_path,"Crop_residue_field_clean.csv"),
  stringsAsFactors=FALSE
)

# ------- Main ---------
# Step 1. Validate site selection =============================
stopifnot(
  nrow(DF_site_info) == 28,
  !anyDuplicated(DF_site_info$Field_Name),
  all(DF_site_info$Monitoring == "Surface"),
  all(DF_site_info$Tile %in% c("Yes","No")),
  sum(DF_site_info$Tile == "Yes") == 6
)

# Step 2. Validate keys and event sites =======================
stopifnot(
  !anyDuplicated(Management_df[c("Field_Name","Water_Year")]),
  all(P_df$Field_Name %in% DF_site_info$Field_Name),
  all(Q_df$Field_Name %in% DF_site_info$Field_Name)
)

# Step 3. Validate water years and seasons ====================
stopifnot(
  all(P_df$Water_Year == assign_water_year(P_df$P_start)),
  all(Q_df$Water_Year == assign_water_year(Q_df$Q_start)),
  all(as.character(P_df$Season) == as.character(assign_season(P_df$P_start))),
  all(as.character(Q_df$Season) == as.character(assign_season(Q_df$Q_start)))
)

# Step 4. Validate seasonal management definitions ============
P_tillage_expected <- case_when(
  P_df$Season == "Fall" ~ P_df$Tillage_Summer_Previous + P_df$Tillage_Fall,
  P_df$Season == "Spring" ~ P_df$Tillage_Fall + P_df$Tillage_Spring,
  P_df$Season == "Summer" ~ P_df$Tillage_Spring + P_df$Tillage_Summer
)

Q_tillage_expected <- case_when(
  Q_df$Season == "Fall" ~ Q_df$Tillage_Summer_Previous + Q_df$Tillage_Fall,
  Q_df$Season == "Spring" ~ Q_df$Tillage_Fall + Q_df$Tillage_Spring,
  Q_df$Season == "Summer" ~ Q_df$Tillage_Spring + Q_df$Tillage_Summer
)

stopifnot(
  all.equal(P_df$Tillage_Passes,P_tillage_expected,check.attributes=FALSE),
  all.equal(Q_df$Tillage_Passes,Q_tillage_expected,check.attributes=FALSE),
  all(is.na(P_df$Residue_Frac[P_df$Season == "Summer"])),
  all(is.na(Q_df$Residue_Frac[Q_df$Season == "Summer"])),
  all(P_df$Crop_Source[P_df$Season %in% c("Fall","Spring")] == "Previous crop"),
  all(Q_df$Crop_Source[Q_df$Season %in% c("Fall","Spring")] == "Previous crop"),
  all(P_df$Crop_Source[P_df$Season == "Summer"] == "Current crop"),
  all(Q_df$Crop_Source[Q_df$Season == "Summer"] == "Current crop")
)

# Step 5. Validate runoff response time =======================
stopifnot(
  all(Q_df$Q_response_time_hr >= 0,na.rm=TRUE)
)

# Step 6. Summarize source-data limitations ===================
Tillage_low_coverage <- Management_df %>%
  filter(
    !is.na(Tillage_Basin_Coverage),
    Tillage_Basin_Coverage < 0.999
  )

Crop_low_coverage <- Management_df %>%
  filter(
    !is.na(Crop_Basin_Coverage),
    Crop_Basin_Coverage < 0.999
  )

Unclassified_previous <- Crop_residue_field %>%
  filter(
    !is.na(Previous_Crop),
    Previous_Crop != "",
    is.na(Previous_Crop_Type)
  ) %>%
  distinct(Previous_Crop) %>%
  arrange(Previous_Crop)

Unclassified_current <- Crop_residue_field %>%
  filter(
    !is.na(Current_Crop),
    Current_Crop != "",
    is.na(Current_Crop_Type)
  ) %>%
  distinct(Current_Crop) %>%
  arrange(Current_Crop)

# Step 7. Generate validation report ==========================
Report_lines <- c(
  "# Data Validation Summary",
  "",
  paste0("Generated: ",Sys.Date()),
  "",
  "## Validation result",
  "",
  "**All structural validation checks passed.**",
  "",
  paste0("- Unique surface monitoring sites: ",nrow(DF_site_info)),
  paste0("- Surface sites with site-level tile drainage: ",sum(DF_site_info$Tile == "Yes")),
  paste0("- Duplicate management site-water-year keys: ",sum(duplicated(Management_df[c("Field_Name","Water_Year")]))),
  "- Water-year assignment: passed",
  "- Season assignment: passed",
  "- Seasonal tillage formulas: passed",
  "- Seasonal crop selection: passed",
  "- Summer residue exclusion: passed",
  "- Non-negative runoff response time: passed",
  "",
  "## Source-data coverage flags",
  "",
  paste0(
    "- Site-water-years with tillage coverage below 100%: ",
    nrow(Tillage_low_coverage)
  ),
  paste0(
    "- Site-water-years with crop/residue coverage below 100%: ",
    nrow(Crop_low_coverage)
  ),
  paste0(
    "- Unique unclassified previous-crop descriptions: ",
    nrow(Unclassified_previous)
  ),
  paste0(
    "- Unique unclassified current-crop descriptions: ",
    nrow(Unclassified_current)
  )
)

if(nrow(Unclassified_previous) > 0){
  Report_lines <- c(
    Report_lines,
    "",
    "### Unclassified previous crops",
    "",
    paste0("- ",Unclassified_previous$Previous_Crop)
  )
}

if(nrow(Unclassified_current) > 0){
  Report_lines <- c(
    Report_lines,
    "",
    "### Unclassified current crops",
    "",
    paste0("- ",Unclassified_current$Current_Crop)
  )
}

writeLines(
  Report_lines,
  file.path(Report_path,"02_Data_validation_summary.md"),
  useBytes=TRUE
)

message("All data validation checks passed.")
message("Report: ",file.path(Report_path,"02_Data_validation_summary.md"))
