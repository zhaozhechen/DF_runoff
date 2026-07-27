# Author: Zhaozhe Chen
# Update Date: 2026.7.27

# This code includes functions to process site and management information

# Process site-level information
process_site_info <- function(site_metadata_path,
                              site_location_path,
                              site_time_path,
                              soil_path,
                              tile_path){
  
  # Updated tile table defines the surface monitoring sites used in this project
  tile_df <- read.csv(
    tile_path,
    check.names=FALSE,
    stringsAsFactors=FALSE
  ) %>%
    remove_bom_names() %>%
    dplyr::transmute(
      Field_Name = normalize_site_id(Site_ID),
      LandCover_Updated = stringr::str_squish(LandCover),
      Tile = standardize_yes_no(Tile),
      Tile_Notes = dplyr::na_if(stringr::str_squish(`Tile Notes`),"")
    ) %>%
    dplyr::distinct(Field_Name,.keep_all=TRUE)
  
  # General site metadata
  site_metadata <- read.csv(
    site_metadata_path,
    check.names=FALSE,
    stringsAsFactors=FALSE
  ) %>%
    remove_bom_names() %>%
    dplyr::filter(Field_Name %in% tile_df$Field_Name)
  
  # Updated site locations and monitoring types
  site_location <- read_xlsx_safe(site_location_path) %>%
    dplyr::transmute(
      Field_Name = normalize_site_id(`Site ID`),
      Site_Type = as.character(`Site Type`),
      LAT_Updated = as.numeric(`GPS Lat`),
      LONG_Updated = as.numeric(`GPS Lon`)
    ) %>%
    dplyr::filter(Site_Type == "Surface") %>%
    dplyr::distinct(Field_Name,.keep_all=TRUE)
  
  # Approximate monitoring periods
  site_time <- read.csv(
    site_time_path,
    check.names=FALSE,
    stringsAsFactors=FALSE
  ) %>%
    remove_bom_names() %>%
    dplyr::filter(Project == "DiscoveryFarms") %>%
    dplyr::transmute(
      Field_Name = normalize_site_id(Field_Name),
      Approximate_Start_Date = lubridate::mdy(Approximate_Start_Date,quiet=TRUE),
      Approximate_End_Date = lubridate::mdy(Approximate_End_Date,quiet=TRUE)
    ) %>%
    dplyr::filter(Field_Name %in% tile_df$Field_Name) %>%
    dplyr::distinct(Field_Name,.keep_all=TRUE)
  
  # Soil texture lookup table
  soil_df <- read.csv(
    soil_path,
    check.names=FALSE,
    stringsAsFactors=FALSE
  ) %>%
    remove_bom_names() %>%
    dplyr::select(Soil_Type,Clay_Fraction)
  
  # Combine site-level information
  site_df <- site_metadata %>%
    dplyr::select(-dplyr::any_of(c("Tile","Monitoring"))) %>%
    dplyr::left_join(site_location,by="Field_Name") %>%
    dplyr::left_join(tile_df,by="Field_Name") %>%
    dplyr::left_join(site_time,by="Field_Name") %>%
    dplyr::left_join(
      soil_df,
      by=c("SoilType"="Soil_Type")
    ) %>%
    dplyr::mutate(
      # All retained monitoring outlets are surface outlets
      Monitoring = "Surface",
      LAT_approx = dplyr::coalesce(LAT_Updated,as.numeric(LAT_approx)),
      LONG_approx = dplyr::coalesce(LONG_Updated,as.numeric(LONG_approx)),
      MeanSlope_per = as.numeric(MeanSlope_per),
      BasinArea_ac = as.numeric(BasinArea_ac),
      Clay_Fraction = as.numeric(Clay_Fraction)
    ) %>%
    dplyr::select(-LAT_Updated,-LONG_Updated)
  
  # Include slope for Site AR2
  # Ref: Rock County Discovery Farms report (2024)
  site_df$MeanSlope_per[site_df$Field_Name == "AR2"] <- 3.4
  
  site_df %>%
    dplyr::arrange(Field_Name)
}

# Process field-level tillage-pass information
process_tillage_info <- function(tillage_path,target_sites){
  
  tillage_raw <- read.csv(
    tillage_path,
    check.names=FALSE,
    stringsAsFactors=FALSE,
    na.strings=c("","NA")
  ) %>%
    remove_bom_names() %>%
    dplyr::transmute(
      Field_Name = normalize_site_id(SiteID),
      Field_ID = as.character(FieldID),
      Basin_Percentage = as.numeric(Percentage_of_basin),
      Water_Year = as.integer(Water_Year),
      Tillage_Total = as.numeric(total_passes),
      Tillage_Fall = as.numeric(fall_passes_OctDec),
      Tillage_Spring = as.numeric(spring_passes_JanMay),
      Tillage_Summer = as.numeric(summer_passes_JunSept)
    ) %>%
    dplyr::filter(Field_Name %in% target_sites)
  
  # Summarize field-level tillage using the percentage of monitored basin
  tillage_df <- tillage_raw %>%
    dplyr::group_by(Field_Name,Water_Year) %>%
    dplyr::summarise(
      Tillage_Basin_Coverage = basin_coverage(Basin_Percentage),
      Tillage_Total = weighted_sum_complete(Tillage_Total,Basin_Percentage),
      Tillage_Fall = weighted_sum_complete(Tillage_Fall,Basin_Percentage),
      Tillage_Spring = weighted_sum_complete(Tillage_Spring,Basin_Percentage),
      Tillage_Summer = weighted_sum_complete(Tillage_Summer,Basin_Percentage),
      .groups="drop"
    ) %>%
    dplyr::arrange(Field_Name,Water_Year) %>%
    dplyr::group_by(Field_Name) %>%
    dplyr::mutate(
      # Fall uses the preceding summer, which belongs to the prior water year
      Tillage_Summer_Previous = dplyr::if_else(
        Water_Year - dplyr::lag(Water_Year) == 1L,
        dplyr::lag(Tillage_Summer),
        NA_real_
      )
    ) %>%
    dplyr::ungroup()
  
  list(raw=tillage_raw,summary=tillage_df)
}

# Process crop and residue information
process_crop_residue_info <- function(residue_path,target_sites){
  
  if(tolower(tools::file_ext(residue_path)) == "csv"){
    residue_source <- read.csv(
      residue_path,
      check.names=FALSE,
      stringsAsFactors=FALSE,
      na.strings=c("","NA")
    ) %>%
      remove_bom_names()
  }else{
    residue_source <- read_xlsx_safe(residue_path)
  }
  
  residue_raw <- residue_source %>%
    dplyr::transmute(
      Field_Name = normalize_site_id(`Site ID`),
      Field_ID = as.character(`Field ID`),
      Basin_Percentage = as.numeric(`Percentage of basin`),
      Water_Year = as.integer(`Field year`),
      Previous_Crop = stringr::str_squish(as.character(`Previous Water Year Crop`)),
      Residue_Fall = standardize_yes_no(
        `Residue left in fall from previous crop?`
      ),
      Residue_Spring = standardize_yes_no(
        `Residue left in spring from previous crop?`
      ),
      Current_Crop = stringr::str_squish(
        as.character(`Crop grown this water year`)
      ),
      Residue_Notes = as.character(Notes)
    ) %>%
    dplyr::filter(Field_Name %in% target_sites) %>%
    dplyr::mutate(
      Previous_Crop_Type = classify_crop_type(Previous_Crop),
      Current_Crop_Type = classify_crop_type(Current_Crop)
    )
  
  # Summarize field-level crop and residue data by monitored basin
  crop_residue_df <- residue_raw %>%
    dplyr::group_by(Field_Name,Water_Year) %>%
    dplyr::summarise(
      Crop_Basin_Coverage = basin_coverage(Basin_Percentage),
      Previous_PerennialFrac = weighted_fraction_complete(
        Previous_Crop_Type == "Perennial",
        Basin_Percentage
      ),
      Current_PerennialFrac = weighted_fraction_complete(
        Current_Crop_Type == "Perennial",
        Basin_Percentage
      ),
      Residue_Fall_Frac = weighted_fraction_complete(
        Residue_Fall,
        Basin_Percentage
      ),
      Residue_Spring_Frac = weighted_fraction_complete(
        Residue_Spring,
        Basin_Percentage
      ),
      .groups="drop"
    )
  
  list(raw=residue_raw,summary=crop_residue_df)
}

# Combine tillage, crop, and residue information
assemble_management_info <- function(tillage_df,crop_residue_df,target_sites){
  dplyr::full_join(
    tillage_df,
    crop_residue_df,
    by=c("Field_Name","Water_Year")
  ) %>%
    dplyr::filter(Field_Name %in% target_sites) %>%
    dplyr::arrange(Field_Name,Water_Year)
}

# Add the season-specific management variables to event data
add_seasonal_management <- function(event_df,event_date,management_df){
  
  event_date <- rlang::ensym(event_date)
  
  event_df %>%
    dplyr::mutate(
      Water_Year = assign_water_year(!!event_date),
      Season = assign_season(!!event_date)
    ) %>%
    dplyr::left_join(
      management_df,
      by=c("Field_Name","Water_Year")
    ) %>%
    dplyr::mutate(
      # Fall and spring use the previous crop; summer uses the current crop
      PerennialFrac = dplyr::case_when(
        Season %in% c("Fall","Spring") ~ Previous_PerennialFrac,
        Season == "Summer" ~ Current_PerennialFrac,
        TRUE ~ NA_real_
      ),
      Crop_Source = dplyr::case_when(
        Season %in% c("Fall","Spring") ~ "Previous crop",
        Season == "Summer" ~ "Current crop",
        TRUE ~ NA_character_
      ),
      # Each seasonal model includes tillage during that season and the preceding season
      Tillage_Passes = dplyr::case_when(
        Season == "Fall" ~ Tillage_Summer_Previous + Tillage_Fall,
        Season == "Spring" ~ Tillage_Fall + Tillage_Spring,
        Season == "Summer" ~ Tillage_Spring + Tillage_Summer,
        TRUE ~ NA_real_
      ),
      Tillage_Window = dplyr::case_when(
        Season == "Fall" ~ "Previous summer + current fall",
        Season == "Spring" ~ "Current fall + spring",
        Season == "Summer" ~ "Current spring + summer",
        TRUE ~ NA_character_
      ),
      # Residue is included in fall and spring but not summer
      Residue_Frac = dplyr::case_when(
        Season == "Fall" ~ Residue_Fall_Frac,
        Season == "Spring" ~ Residue_Spring_Frac,
        Season == "Summer" ~ NA_real_,
        TRUE ~ NA_real_
      ),
      Residue = dplyr::case_when(
        is.na(Residue_Frac) ~ NA_character_,
        Residue_Frac == 0 ~ "No",
        Residue_Frac == 1 ~ "Yes",
        Residue_Frac > 0 & Residue_Frac < 1 ~ "Partial",
        TRUE ~ NA_character_
      )
    )
}
