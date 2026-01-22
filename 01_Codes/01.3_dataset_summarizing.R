# Author: Zhaozhe Chen
# Update Date: 2026.1.22

# This code is to synthesize all data to get final dataset for RF
# Only keep non-frozen P and non-frozen Q

# ---------- Global -----------
library(stringr)
library(dplyr)
library(lubridate)

# Data paths =======
# All P events
P_all_df <- read.csv("00_Data/Processed_data_v2/All_P_events_df.csv") %>%
  select(-X)
# All Q events
Q_all_df <- read.csv("00_Data/Processed_data_v2/All_Q_events_df.csv") %>%
  select(-X)
# Processed crop df
Crop_df <- read.csv("00_Data/Processed_data_v2/Crop_df.csv") %>%
  select(-X)
# Updated site info
DF_site_info <- read.csv("00_Data/Processed_data_v2/DF_site_info.csv") %>%
  select(-X)
# Tillage for each site at each site
DF_Tillage <- read.csv("00_Data/Metadata/DF_EOF_Tillage.csv")

# Output path
Output_path <- "00_Data/Processed_data_v2/"

# ------- Main --------
# Process site info to include only target variables
DF_site_df <- DF_site_info %>%
  select(Field_Name,
         Monitoring,
         FarmEnterprise,
         CropRotation,
         Tillage,
         Tile,
         SoilType,
         HydrologicGroup,
         DrainageClass,
         MeanSlope_per,
         Clay_Fraction)

# Process Annual Tillage Data
DF_Tillage <- DF_Tillage %>%
  mutate(Annual_Tillage = ifelse(Tillage == "Pasture Renovation","Pasture",Tillage),
         Annual_Tillage = ifelse(Tillage == "None","No-Till",Tillage)) %>%
  select(-Tillage)

# Join P df ==============
P_joint_df <- P_all_df %>%
  # Include site characteristics
  left_join(DF_site_df,by="Field_Name") %>%
  mutate(Field_Year = year(P_start)) %>%
  # Include Crop variables
  left_join(Crop_df,by=c("Field_Name","Field_Year")) %>%
  # Calculate Days since planting (DSP)
  mutate(DSP = date(P_start) - as.Date(Start_Date_wt)) %>%
  # If DSP < 0 (planting not started), change DSP to 0
  mutate(DSP = ifelse(DSP<0,0,DSP)) %>%
  # Include Tillage for each year
  left_join(DF_Tillage,
            by = c("Field_Name" = "SiteID",
                   "Field_Year" = "Year")) %>%
  # Only keep non-frozen events
  filter(P_frozen == "FALSE")

# Output this df
write.csv(P_joint_df,paste0(Output_path,"Non-Frozen_P_joint_df.csv"))

# Join Q df ================
Q_joint_df <- Q_all_df %>%
  # Include site characteristics
  left_join(DF_site_df,by="Field_Name") %>%
  mutate(Field_Year = year(Q_start)) %>%
  # Include Crop variables
  left_join(Crop_df,by=c("Field_Name","Field_Year")) %>%
  # Calculate Days since planting (DSP)
  mutate(DSP = date(Q_start) - as.Date(Start_Date_wt)) %>%
  # If DSP < 0 (planting not started), change DSP to 0
  mutate(DSP = ifelse(DSP<0,0,DSP)) %>%
  # Include Tillage for each year
  left_join(DF_Tillage,
            by = c("Field_Name" = "SiteID",
                   "Field_Year" = "Year")) %>%
  # Only keep non-frozen events
  filter(frozen == "Non-Frozen")

# Output this df
write.csv(Q_joint_df,paste0(Output_path,"Non-Frozen_Q_joint_df.csv"))


