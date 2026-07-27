# Author: Zhaozhe Chen
# Update Date: 2026.7.27

# This code includes general functions used to process the DF runoff dataset

# Normalize field/site identifiers used in the management datasets
normalize_site_id <- function(x){
  x <- stringr::str_squish(as.character(x))
  dplyr::case_when(
    x %in% c("K5r1","K5r2") ~ "K5",
    x == "K7r" ~ "K7",
    TRUE ~ x
  )
}

# Assign USGS water year
# October-December are assigned to the following calendar year
assign_water_year <- function(x){
  x <- as_local_date(x)
  dplyr::if_else(
    lubridate::month(x) >= 10,
    lubridate::year(x) + 1L,
    lubridate::year(x)
  )
}

# Assign seasons agreed upon by the project team
assign_season <- function(x){
  m <- lubridate::month(as_local_date(x))
  season <- dplyr::case_when(
    m %in% 1:5 ~ "Spring",
    m %in% 6:9 ~ "Summer",
    m %in% 10:12 ~ "Fall",
    TRUE ~ NA_character_
  )
  factor(season,levels=c("Spring","Summer","Fall"))
}

# Convert date-time values to their local Central calendar date
as_local_date <- function(x){
  if(inherits(x,"POSIXt")){
    as.Date(x,tz="America/Chicago")
  }else{
    as.Date(x)
  }
}

# Parse USGS date-time values
# Some source values contain only a date, so midnight is added
parse_usgs_datetime <- function(x){
  x <- as.character(x)
  x <- ifelse(
    grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$",x),
    paste(x,"00:00"),
    x
  )
  lubridate::mdy_hm(x,quiet=TRUE,tz="America/Chicago")
}

# Read an Excel workbook from OneDrive
# A temporary local copy is used if readxl cannot unzip the cloud-hosted path
read_xlsx_safe <- function(path,...){
  tryCatch(
    readxl::read_xlsx(path,...),
    error=function(original_error){
      temp_path <- tempfile(fileext=".xlsx")
      copied <- file.copy(path,temp_path,overwrite=TRUE)
      
      if(!copied){
        stop(original_error)
      }
      
      on.exit(unlink(temp_path),add=TRUE)
      readxl::read_xlsx(temp_path,...)
    }
  )
}

# Remove a possible UTF-8 byte-order mark from column names
remove_bom_names <- function(df){
  # Remove a byte-preserved UTF-8 BOM without locale conversion
  names(df) <- sub("^\xEF\xBB\xBF","",names(df),useBytes=TRUE)
  df
}

# Convert common yes/no values to a consistent notation
standardize_yes_no <- function(x){
  x <- stringr::str_to_lower(stringr::str_squish(as.character(x)))
  dplyr::case_when(
    x %in% c("yes","y","true","1") ~ "Yes",
    x %in% c("no","n","false","0") ~ "No",
    TRUE ~ NA_character_
  )
}

# Classify crop descriptions as perennial or annual
classify_crop_type <- function(x){
  x <- stringr::str_to_lower(stringr::str_squish(as.character(x)))
  dplyr::case_when(
    stringr::str_detect(
      x,
      "alfalfa|grass|pasture|red clover|ryegrass forage|clover"
    ) ~ "Perennial",
    stringr::str_detect(
      x,
      "potato|corn|silage|wheat|soybean|oat|pea|bean|sorghum|cover crop"
    ) ~ "Annual",
    TRUE ~ NA_character_
  )
}

# Calculate a basin-weighted total
# Return NA if any positive-weight component has a missing value
weighted_sum_complete <- function(x,percentage){
  x <- suppressWarnings(as.numeric(x))
  weight <- suppressWarnings(as.numeric(percentage))/100
  keep <- !is.na(weight) & weight > 0
  
  if(!any(keep)){
    return(NA_real_)
  }
  
  if(any(is.na(x[keep]))){
    return(NA_real_)
  }
  
  sum(x[keep]*weight[keep])
}

# Calculate a basin-weighted fraction for a yes/no or logical variable
# The percentage is retained as a fraction of the whole monitored basin
weighted_fraction_complete <- function(x,percentage){
  weight <- suppressWarnings(as.numeric(percentage))/100
  keep <- !is.na(weight) & weight > 0
  
  if(!any(keep)){
    return(NA_real_)
  }
  
  if(is.logical(x)){
    indicator <- as.numeric(x)
  }else{
    x <- standardize_yes_no(x)
    indicator <- dplyr::case_when(
      x == "Yes" ~ 1,
      x == "No" ~ 0,
      TRUE ~ NA_real_
    )
  }
  
  if(any(is.na(indicator[keep]))){
    return(NA_real_)
  }
  
  sum(indicator[keep]*weight[keep])
}

# Calculate the percentage of the monitored basin represented by records
basin_coverage <- function(percentage){
  sum(suppressWarnings(as.numeric(percentage)),na.rm=TRUE)/100
}
