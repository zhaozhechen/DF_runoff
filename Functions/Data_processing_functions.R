# Author: Zhaozhe Chen
# Date: 2025.8.6

# This code includes functions for data processing

# This function reads in the tif file
# Input is the path to the zip file folder
# Output the tif file as a raster
read_tif <- function(zip_folder){
  # Get the files names in this folder
  zip_filenames <- unzip(zip_folder,list=TRUE)
  # Get the tif file
  tif_filename <- zip_filenames$Name[grepl("\\.tif$",zip_filenames$Name)]
  # Create a temporary file path fo this tif
  tmp_tif_path <- tempfile(fileext = ".tif")
  # Extract this tif file to this temporary file
  unzip(zip_folder,files=tif_filename,exdir = dirname(tmp_tif_path))
  tif_full_path <- file.path(dirname(tmp_tif_path),tif_filename)
  # Read the tif
  r <- rast(tif_full_path)
  return(list(r,tif_full_path))
}

# This function is to extract subdaily P from USGS station
# Input include arrayid, which indicates which site to process
# And Site_ls, which includes Site_ID, USGS_ID, Start and End time
USGS_ppt <- function(USGS_ID,start,end){
  # Extract subdaily P from USGS station
  ppt <- readNWISuv(siteNumbers = USGS_ID,
                    parameterCd = P_code,
                    startDate = start,
                    endDate = end,
                    tz = "UTC") %>%
    # Standardize variable name
    renameNWISColumns() %>%
    # Convert unit from in to mm
    mutate(Precip_Inst_mm = Precip_Inst * 2.54*10)
  
  return(ppt)
}

# This function is to separate rainfall events, and give each individual event a event_ID
# Only rainfall events separated by at least MIT (hours) are considered separate rainfall
# P_event_ID is also assigned to dry periods within an event. i.e., between the first and last rain obs of each event
# Input includes:
# P_df, which should be the df of full P time series
# P_th: threshold to define if it rains or not
# MIT: # Minimum inter-event time (MIT) to separate rainfall event
Assign_P_event_ID <- function(P_df,P_th,MIT){
  event_P <- P_df %>%
    arrange(dateTime) %>%
    mutate(
      # Label dry obs
      is_rain = Precip_Inst_mm > P_th,
      # Get time difference between each two obs in hours
      time_diff_hr = as.numeric(difftime(dateTime,lag(dateTime),units = "hours")),
      # Remove the first NA in time_diff_hr
      time_diff_hr = replace_na(time_diff_hr,0),
      
      # Define dry groups: each time a dry period starts, increase ID
      dry_start = (!is_rain) & (lag(is_rain,default = TRUE)),
      # These dry groups are consecutive dry obs without rain interruption
      dry_group = if_else(!is_rain,cumsum(dry_start),NA)
    ) %>%
    group_by(dry_group) %>%
    mutate(
      # Get cumulative dry period duration in hours
      cum_dry_time_hr = if_else(!is_rain,cumsum(time_diff_hr),NA)
    )%>%
    ungroup() %>%
    mutate(
      # Get cumulative dry duration before rain
      dry_time_before_rain = lag(cum_dry_time_hr),
      # Define new rainfall events, if it has been dried for time longer than MIT
      new_event = if_else(
        is_rain & 
          (is.na(lag(is_rain)) | lag(is_rain) == FALSE) &
          (dry_time_before_rain > MIT | is.na(dry_time_before_rain)),
        TRUE,FALSE
      ),
      # Assign rainfall event ID only to rain obs
      Rainfall_ID = if_else(is_rain,cumsum(new_event),NA)
    ) %>%
    fill(Rainfall_ID,.direction = "down") %>%
    # Mask out dry obs that are after the last rain obs of each event
    group_by(Rainfall_ID) %>%
    mutate(
      has_rain = any(is_rain),
      last_rain_time = max(dateTime[is_rain],na.rm=TRUE),
      P_event_ID = if_else(dateTime > last_rain_time,NA,Rainfall_ID)
    ) %>%
    ungroup() %>%
    select(dateTime,Precip_Inst_mm,P_event_ID)  
  return(event_P)
}




