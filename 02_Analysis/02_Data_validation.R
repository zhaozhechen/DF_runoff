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
source(file.path(Project_path,"01_Functions","04_Reporting_plotting_functions.R"))

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

Tillage_field <- read.csv(
  file.path(Processed_path,"Tillage_field_clean.csv"),
  stringsAsFactors=FALSE
)

# ------- Main ---------
# Step 1. Validate site selection =============================
stopifnot(
  nrow(DF_site_info) == 28,
  !anyDuplicated(DF_site_info$Field_Name),
  "Monitoring" %in% names(DF_site_info),
  all(DF_site_info$Monitoring == "Surface"),
  all(DF_site_info$Tile %in% c("Yes","No")),
  sum(DF_site_info$Tile == "Yes") == 6,
  all(DF_site_info$Clay_Fraction >= 0,na.rm=TRUE),
  all(DF_site_info$Clay_Fraction <= 1,na.rm=TRUE),
  all(
    as.character(DF_site_info$Hydrologic_Group) ==
      as.character(
        group_hydrologic_class(DF_site_info$HydrologicGroup)
      )
  )
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
  all.equal(
    Tillage_field$Tillage_Total,
    Tillage_field$Tillage_Fall +
      Tillage_field$Tillage_Spring +
      Tillage_field$Tillage_Summer,
    check.attributes=FALSE
  ),
  all(is.na(P_df$Residue_Frac[P_df$Season == "Summer"])),
  all(is.na(Q_df$Residue_Frac[Q_df$Season == "Summer"])),
  all(P_df$Crop_Source[P_df$Season %in% c("Fall","Spring")] == "Previous crop"),
  all(Q_df$Crop_Source[Q_df$Season %in% c("Fall","Spring")] == "Previous crop"),
  all(P_df$Crop_Source[P_df$Season == "Summer"] == "Current crop"),
  all(Q_df$Crop_Source[Q_df$Season == "Summer"] == "Current crop")
)

# Check that each site-water-year total is the basin-weighted sum of fields
Tillage_weighted_expected <- Tillage_field %>%
  group_by(Field_Name,Water_Year) %>%
  summarise(
    Tillage_Total_Expected=sum(
      Tillage_Total*Basin_Percentage/100
    ),
    Tillage_Fall_Expected=sum(
      Tillage_Fall*Basin_Percentage/100
    ),
    Tillage_Spring_Expected=sum(
      Tillage_Spring*Basin_Percentage/100
    ),
    Tillage_Summer_Expected=sum(
      Tillage_Summer*Basin_Percentage/100
    ),
    .groups="drop"
  ) %>%
  left_join(
    Management_df %>%
      select(
        Field_Name,
        Water_Year,
        Tillage_Total,
        Tillage_Fall,
        Tillage_Spring,
        Tillage_Summer
      ),
    by=c("Field_Name","Water_Year")
  )

stopifnot(
  all.equal(
    Tillage_weighted_expected$Tillage_Total,
    Tillage_weighted_expected$Tillage_Total_Expected,
    check.attributes=FALSE
  ),
  all.equal(
    Tillage_weighted_expected$Tillage_Fall,
    Tillage_weighted_expected$Tillage_Fall_Expected,
    check.attributes=FALSE
  ),
  all.equal(
    Tillage_weighted_expected$Tillage_Spring,
    Tillage_weighted_expected$Tillage_Spring_Expected,
    check.attributes=FALSE
  ),
  all.equal(
    Tillage_weighted_expected$Tillage_Summer,
    Tillage_weighted_expected$Tillage_Summer_Expected,
    check.attributes=FALSE
  )
)

# Step 5. Validate hydrologic units and runoff response time ===
Inch_field_pattern <- paste0(
  "(^rain$|^rain_in$|^runoff_in$|",
  "^I(event|5|10|15|30|60)$|",
  "^ARFdays(1|2|7|14)$|_in$)"
)

stopifnot(
  !any(grepl(Inch_field_pattern,names(P_df))),
  !any(grepl(Inch_field_pattern,names(Q_df))),
  all(
    c(
      "rain_mm",
      "I30_mm_hr",
      "ARFdays7_mm",
      "Q_total_mm"
    ) %in% names(P_df)
  ),
  all(
    c(
      "rain_mm",
      "I30_mm_hr",
      "ARFdays7_mm",
      "runoff_mm"
    ) %in% names(Q_df)
  ),
  all.equal(
    Q_df$runoff_mm,
    inch_to_mm(Q_df$runoff_volume/Q_df$area_ft2*12),
    check.attributes=FALSE
  ),
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
Validation_table <- data.frame(
  Check=c(
    "Unique surface monitoring sites",
    "Surface sites with site-level tile drainage",
    "Duplicate management site-water-year keys",
    "Water-year assignment",
    "Season assignment",
    "Basin-weighted total tillage passes",
    "Seasonal tillage formulas",
    "Seasonal crop selection",
    "Summer residue exclusion",
    "Inch-to-millimetre conversion",
    "Non-negative runoff response time"
  ),
  Result=c(
    nrow(DF_site_info),
    sum(DF_site_info$Tile == "Yes"),
    sum(duplicated(Management_df[c("Field_Name","Water_Year")])),
    "Passed",
    "Passed",
    "Passed",
    "Passed",
    "Passed",
    "Passed",
    "Passed",
    "Passed"
  )
)

Coverage_table <- data.frame(
  Flag=c(
    "Site-water-years with tillage coverage below 100%",
    "Site-water-years with crop/residue coverage below 100%",
    "Unique unclassified previous-crop descriptions",
    "Unique unclassified current-crop descriptions"
  ),
  Count=c(
    nrow(Tillage_low_coverage),
    nrow(Crop_low_coverage),
    nrow(Unclassified_previous),
    nrow(Unclassified_current)
  )
)

Report_body <- c(
  "<div class=\"callout\"><strong>All structural validation checks passed.</strong></div>",
  "<h2>Validation result</h2>",
  data_frame_to_html(Validation_table,digits=0),
  "<h2>Source-data coverage flags</h2>",
  "<p>Coverage below 100% reflects the supplied basin percentages. Weighted values remain fractions of the full monitored basin and are not renormalized.</p>",
  data_frame_to_html(Coverage_table,digits=0)
)

if(nrow(Unclassified_previous) > 0){
  Report_body <- c(
    Report_body,
    "<h3>Unclassified previous crops</h3>",
    data_frame_to_html(Unclassified_previous)
  )
}

if(nrow(Unclassified_current) > 0){
  Report_body <- c(
    Report_body,
    "<h3>Unclassified current crops</h3>",
    data_frame_to_html(Unclassified_current)
  )
}

Validation_report <- file.path(
  Report_path,
  "02_Data_validation_summary.html"
)

write_html_report(
  title="Data Validation Summary",
  subtitle=paste0("Generated: ",Sys.Date()),
  body_html=Report_body,
  output_path=Validation_report
)

message("All data validation checks passed.")
message("Report: ",Validation_report)
