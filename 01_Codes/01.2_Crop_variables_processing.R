# Author: Zhaozhe Chen
# Update Date: 2026.1.22

# This code is to model crop cover based on crop type and plant timing

# ---------- Global -----------
library(stringr)
library(dplyr)
library(lubridate)

# Data paths =======
# Raw DF EOF Crop dataset
Crop_raw_df <- read.csv("00_Data/Metadata/DF_EOF_Crop.csv")
# processed DF Site info
DF_site_info <- read.csv("00_Data/Processed_data_v2/DF_site_info.csv")
# This is output path for data
Output_path <- "00_Data/Processed_data_v2/"

# -------- Main --------
# Data cleaning
Crop_raw_df <- Crop_raw_df %>%
  # Clean Site ID
  mutate(Field_Name = str_replace(Site.ID,"r\\d*$","")) %>%
  filter(Field_Name %in% DF_site_info$Field_Name ) %>%
  select(-c(Site.ID,Field.ID))

# P4 and P5 are missing, they are paired sites to P1 and P3, so add them
P4_df <- Crop_raw_df %>%
  filter(Field_Name == "P1") %>%
  mutate(Field_Name = "P4")

P5_df <- Crop_raw_df %>%
  filter(Field_Name == "P3") %>%
  mutate(Field_Name = "P5")

# Bind them together
Crop_raw_df <- bind_rows(Crop_raw_df,P4_df,P5_df) %>%
  select(Field_Name,
         Percentage_of_basin = Percentage.of.basin,
         Field_Year = Field.year,
         Previous_Crop = Previous.Crop,
         Planting_Date = Planting.date..this.year.s.crop.,
         Current_Crop = Crop.grown.this.year)

# Classify Crop types
Crop_raw_df <- Crop_raw_df %>%
  mutate(
    # normalize crop strings (lowercase, trim, collapse spaces)
    Previous_Crop = str_to_lower(str_squish(as.character(Previous_Crop))),
    Current_Crop  = str_to_lower(str_squish(as.character(Current_Crop))),
    # Previous crop type 
    Previous_Crop_Type = case_when(
      str_detect(Previous_Crop, "alfalfa|grass|pasture|red clover|ryegrass forage") ~ "Perennial",
      str_detect(Previous_Crop, "potato|corn|silage|wheat|soybean|oats|pea|bean|cover crop mix") ~ "Annual"
    ),
    # Current crop type
    Current_Crop_Type = case_when(
      str_detect(Current_Crop, "alfalfa|grass|pasture|red clover|ryegrass forage") ~ "Perennial",
      str_detect(Current_Crop, "potato|corn|silage|wheat|soybean|oats|pea|bean|cover crop mix|sorghum") ~ "Annual"
    )
  )

# If the previous crop is perennial and it is still the same perennial this year, use 5/2 as the planting date
# Ref: https://mrcc.purdue.edu/mw_climate/climateSummaries/climSummOut_grow?stnId=USW00014837
Crop_raw_df <- Crop_raw_df %>%
  mutate(Planting_Date = mdy(Planting_Date)) %>%
  # When year of Planting Date does not match that of Field_Year, overwrite the year
  mutate(Planting_Date = make_date(
    year = Field_Year,
    month = month(Planting_Date),
    day = day(Planting_Date)
  )) %>%
  mutate(
    # If perennial and no planting date, use May 2 of the Field_Year
    Planting_Date = if_else(
      Current_Crop_Type == "Perennial" & is.na(Planting_Date),
      as.Date(paste0(Field_Year, "-05-02")),
      Planting_Date
    )
  )

# Summarize the Crop dataset for each Field Year
Crop_df <- Crop_raw_df %>%
  mutate(
    is_perennial = Current_Crop_Type == "Perennial"
  ) %>%
  group_by(Field_Name,Field_Year) %>%
  summarise(
    # Fraction of basin in perennial crops
    PerennialFrac = sum(Percentage_of_basin[is_perennial]/100,na.rm=TRUE),
    # Weighted mean starting date
    Start_Date_wt = as.Date(
      sum(Percentage_of_basin/100*as.numeric(Planting_Date),na.rm=TRUE)/sum(Percentage_of_basin[!is.na(Planting_Date)]/100,na.rm=TRUE),
      origin = "1970-01-01"
    )
  )

# Output this df
write.csv(Crop_df,paste0(Output_path,"Crop_df.csv"))
