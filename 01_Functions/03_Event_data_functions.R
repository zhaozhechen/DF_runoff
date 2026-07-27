# Author: Zhaozhe Chen
# Update Date: 2026.7.27

# This code includes functions to process USGS EOF precipitation and runoff events

# Process USGS EOF dataset runoff events (Q and associated P)
process_runoff_events <- function(runoff_path,site_df){
  
  target_sites <- site_df$Field_Name
  
  runoff_df <- read.csv(
    runoff_path,
    check.names=FALSE,
    stringsAsFactors=FALSE,
    na.strings=c("","NA")
  ) %>%
    remove_bom_names() %>%
    dplyr::filter(
      project == "DiscoveryFarms",
      Field_Name %in% target_sites,
      storm == 1
    ) %>%
    dplyr::transmute(
      Field_Name,
      frozen = dplyr::if_else(frozen == 1,"Frozen","Non-Frozen"),
      # Storm = 0: The flow in this event is associated with baseflow, groundwater flow, melting snow, 
      # or melting frozen ground and not a monitored precipitation event
      # Filter out Q events that are not associated with storm
      storm = "Storm",
      unique_storm_number,
      Q_start = parse_usgs_datetime(storm_start),
      Q_end = parse_usgs_datetime(storm_end),
      runoff_volume = as.numeric(runoff_volume),
      peak_discharge = as.numeric(peak_discharge),
      # Note: below the notes indicate Q event, which is correct. Not P event. Because multiple P events are combined within this Q event.
      # Total rain during this Q event (Unit: in)
      rain_in = as.numeric(rain),
      # Duration of all P events overlapping with this Q event.  (Unit: hour)
      duration = as.numeric(duration),
      # Mean Intensity during this Q event (Unit: in/hour)
      Ievent = as.numeric(Ievent),
      # Maximum 5-min intensity during this Q event (Unit: in/hour)
      I5 = as.numeric(I5),
      I10 = as.numeric(I10),
      I30 = as.numeric(I30),
      I60 = as.numeric(I60),
      # Antecedent rainfall (ARF) for each event was calculated by taking the sum of the total amount of rain 
      # for a period of days (not events) before the beginning of the event associated with the flow event and reported in inches.
      ARFdays1 = as.numeric(ARFdays1),
      ARFdays2 = as.numeric(ARFdays2),
      ARFdays7 = as.numeric(ARFdays7),
      ARFdays14 = as.numeric(ARFdays14)
    ) %>%
    dplyr::filter(!is.na(Q_start),!is.na(Q_end)) %>%
    dplyr::arrange(Field_Name,Q_start,Q_end) %>%
    dplyr::mutate(Q_event_id=dplyr::row_number()) %>%
    dplyr::left_join(
      site_df %>%
        dplyr::select(Field_Name,BasinArea_ac),
      by="Field_Name"
    ) %>%
    dplyr::mutate(
      area_ft2 = BasinArea_ac*43560,
      runoff_in = runoff_volume/area_ft2*12
    )
  
  runoff_df
}

# Process USGS precipitation events
# This is wrapper function to process P at each site
# Including keeping only P events during the monitoring period of site
# Label whether a P event is associated with a Q event or not
# Label whether a P event is frozen or not
process_precipitation_events <- function(precipitation_path,
                                         prism_temperature_path,
                                         site_df){
  
  target_sites <- site_df$Field_Name
  
  precipitation_df <- read.csv(
    precipitation_path,
    check.names=FALSE,
    stringsAsFactors=FALSE,
    na.strings=c("","NA")
  ) %>%
    remove_bom_names() %>%
    dplyr::filter(
      project == "DiscoveryFarms",
      rain > 0.01
    ) %>%
    dplyr::rename(
      USGS_Station_Number = USGS_Station_Number_for_Precipitation,
      P_start = StartDate,
      P_end = EndDate
    ) %>%
    tidyr::separate_rows(All_Field_Names,sep="\\|") %>%
    dplyr::mutate(
      Field_Name = normalize_site_id(All_Field_Names),
      P_start = parse_usgs_datetime(P_start),
      P_end = parse_usgs_datetime(P_end)
    ) %>%
    dplyr::filter(
      Field_Name %in% target_sites,
      !is.na(P_start),
      !is.na(P_end)
    ) %>%
    dplyr::select(
      -project,
      -All_Field_Names
    ) %>%
    dplyr::left_join(
      site_df %>%
        dplyr::select(
          Field_Name,
          Approximate_Start_Date,
          Approximate_End_Date,
          BasinArea_ac
        ),
      by="Field_Name"
    ) %>%
    # Keep precipitation events during the site's monitoring period
    dplyr::filter(
      as_local_date(P_start) >= Approximate_Start_Date,
      # No end date: keep everything from start onward
      # Both start and end defined → keep only within window
      is.na(Approximate_End_Date) |
        as_local_date(P_start) <= Approximate_End_Date
    ) %>%
    dplyr::select(-Approximate_Start_Date,-Approximate_End_Date)
  
  # Convert PRISM daily temperature data to a site-date table
  prism_temperature <- read.csv(
    prism_temperature_path,
    check.names=FALSE,
    stringsAsFactors=FALSE,
    na.strings=c("","NA")
  ) %>%
    remove_bom_names()
  
  # Remove the unnamed row-index column written by the legacy workflow
  prism_temperature <- prism_temperature[
    ,
    nzchar(names(prism_temperature)),
    drop=FALSE
  ] %>%
    dplyr::mutate(Date=lubridate::ymd(Date)) %>%
    dplyr::select(Date,dplyr::any_of(target_sites)) %>%
    tidyr::pivot_longer(
      cols=-Date,
      names_to="Field_Name",
      values_to="Tmp"
    ) %>%
    dplyr::mutate(Tmp=as.numeric(Tmp))
  
  precipitation_df %>%
    dplyr::mutate(Date=as_local_date(P_start)) %>%
    dplyr::left_join(
      prism_temperature,
      by=c("Field_Name","Date")
    ) %>%
    dplyr::mutate(
      P_frozen = dplyr::case_when(
        is.na(Tmp) ~ NA,
        Tmp <= 0 ~ TRUE,
        Tmp > 0 ~ FALSE
      ),
      area_ft2 = BasinArea_ac*43560
    ) %>%
    dplyr::arrange(Field_Name,P_start,P_end) %>%
    dplyr::mutate(P_event_id=dplyr::row_number())
}

# Match precipitation and runoff events using site and time overlap
match_precipitation_runoff <- function(precipitation_df,runoff_df){
  
  precipitation_df$Associated_Q <- FALSE
  precipitation_df$Q_event_count <- 0L
  precipitation_df$Q_total_volume_raw <- 0
  precipitation_df$Q_total_volume <- 0
  precipitation_df$Q_event_ids <- NA_character_
  precipitation_df$Shared_Q <- FALSE
  
  runoff_df$first_P_start <- as.POSIXct(
    NA,
    tz="America/Chicago",
    origin="1970-01-01"
  )
  
  # Process one site at a time to prevent cross-site event matches
  for(site_id in intersect(
    unique(precipitation_df$Field_Name),
    unique(runoff_df$Field_Name)
  )){
    
    p_idx <- which(precipitation_df$Field_Name == site_id)
    q_idx <- which(runoff_df$Field_Name == site_id)
    
    # Store precipitation events overlapping each runoff event
    q_to_p <- vector("list",length(q_idx))
    
    for(i in seq_along(p_idx)){
      pi <- p_idx[i]
      overlap_local <- which(
        runoff_df$Q_end[q_idx] >= precipitation_df$P_start[pi] &
          runoff_df$Q_start[q_idx] <= precipitation_df$P_end[pi]
      )
      
      if(length(overlap_local) > 0){
        overlap_q <- q_idx[overlap_local]
        # If a P event is associated with a Q event, Associated_Q is TRUE, otherwise FALSE
        precipitation_df$Associated_Q[pi] <- TRUE
        precipitation_df$Q_event_count[pi] <- length(overlap_q)
        # Also get the total Q volume for each P event
        precipitation_df$Q_total_volume_raw[pi] <- sum(
          runoff_df$runoff_volume[overlap_q],
          na.rm=TRUE
        )
        precipitation_df$Q_event_ids[pi] <- paste(
          runoff_df$Q_event_id[overlap_q],
          collapse="|"
        )
        
        for(j in overlap_local){
          q_to_p[[j]] <- c(q_to_p[[j]],pi)
        }
      }
    }
    
    # Assign each runoff volume once to the earliest overlapping precipitation event
    for(j in seq_along(q_idx)){
      qi <- q_idx[j]
      matching_p <- q_to_p[[j]]
      
      if(length(matching_p) > 0){
        first_p <- matching_p[
          which.min(precipitation_df$P_start[matching_p])
        ]
        precipitation_df$Q_total_volume[first_p] <-
          precipitation_df$Q_total_volume[first_p] +
          runoff_df$runoff_volume[qi]
        
        if(length(matching_p) > 1){
          precipitation_df$Shared_Q[matching_p] <- TRUE
        }
      }
      
      # Calculate response time using precipitation at the same site
      response_p <- p_idx[
        precipitation_df$P_start[p_idx] <= runoff_df$Q_start[qi] &
          precipitation_df$P_end[p_idx] >= runoff_df$Q_start[qi]
      ]
      
      if(length(response_p) > 0){
        runoff_df$first_P_start[qi] <- min(
          precipitation_df$P_start[response_p],
          na.rm=TRUE
        )
      }
    }
  }
  
  precipitation_df <- precipitation_df %>%
    dplyr::mutate(
      Q_total_in_raw = Q_total_volume_raw/area_ft2*12,
      Q_total_in = Q_total_volume/area_ft2*12
    )
  
  runoff_df <- runoff_df %>%
    dplyr::mutate(
      Q_response_time_hr = as.numeric(
        difftime(Q_start,first_P_start,units="hours")
      )
    )
  
  list(precipitation=precipitation_df,runoff=runoff_df)
}
