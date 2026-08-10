# Author: Zhaozhe Chen
# Update Date: 2026.8.10

# This code includes functions to process sediment and phosphorus data
# from the USGS EOF storm-event dataset

# Process measured sediment and phosphorus observations from runoff events
process_sediment_phosphorus_events <- function(runoff_path,site_df){

  target_sites <- site_df$Field_Name

  target_measure_columns <- c(
    "suspended_sediment_conc_mgL",
    "suspended_sediment_load_pounds",
    "suspended_sediment_yield_pounds_per_acre",
    "total_dissolved_solids_conc_mgL",
    "total_dissolved_solids_load_pounds",
    "total_dissolved_solids_yield_pounds_per_acre",
    "orthophosphate_conc_mgL",
    "orthophosphate_load_pounds",
    "orthophosphate_yield_pounds_per_acre",
    "total_phosphorus_unfiltered_conc_mgL",
    "total_phosphorus_unfiltered_load_pounds",
    "total_phosphorus_unfiltered_yield_pounds_per_acre"
  )

  target_remark_columns <- c(
    "remark_suspended_sediment",
    "remark_total_dissolved_solids",
    "remark_orthophosphate",
    "remark_total_phosphorus_unfiltered"
  )

  required_columns <- c(
    "USGS_Station_Number",
    "Field_Name",
    "project",
    "discrete",
    "estimated",
    "estimated_flow_fraction",
    "frozen",
    "storm",
    "unique_storm_number",
    "n_sub_flow_events",
    "storm_start",
    "storm_end",
    "runoff_volume",
    "peak_discharge",
    "rain",
    "duration",
    "Ievent",
    "I5",
    "I10",
    "I15",
    "I30",
    "I60",
    "energy_m1",
    "erosivity_m1",
    "energy_m2",
    "erosivity_m2",
    "ARFdays1",
    "ARFdays2",
    "ARFdays7",
    "ARFdays14",
    target_measure_columns,
    target_remark_columns
  )

  runoff_source <- read.csv(
    runoff_path,
    check.names=FALSE,
    stringsAsFactors=FALSE,
    na.strings=c("","NA")
  ) %>%
    remove_bom_names()

  missing_columns <- setdiff(required_columns,names(runoff_source))

  if(length(missing_columns) > 0){
    stop(
      "The USGS runoff file is missing required columns: ",
      paste(missing_columns,collapse=", ")
    )
  }

  water_quality_df <- runoff_source %>%
    dplyr::filter(
      project == "DiscoveryFarms",
      Field_Name %in% target_sites,
      storm == 1,
      estimated_flow_fraction == 0,
      estimated == 0
    ) %>%
    dplyr::transmute(
      USGS_Station_Number,
      Field_Name,
      discrete=as.integer(discrete),
      estimated=as.integer(estimated),
      estimated_flow_fraction=as.numeric(estimated_flow_fraction),
      frozen=dplyr::if_else(frozen == 1,"Frozen","Non-Frozen"),
      storm="Storm",
      unique_storm_number,
      n_sub_flow_events=as.integer(n_sub_flow_events),
      Q_start=parse_usgs_datetime(storm_start),
      Q_end=parse_usgs_datetime(storm_end),
      runoff_volume=as.numeric(runoff_volume),
      peak_discharge=as.numeric(peak_discharge),
      rain_mm=inch_to_mm(rain),
      duration_hr=as.numeric(duration),
      Ievent_mm_hr=inch_to_mm(Ievent),
      I5_mm_hr=inch_to_mm(I5),
      I10_mm_hr=inch_to_mm(I10),
      I15_mm_hr=inch_to_mm(I15),
      I30_mm_hr=inch_to_mm(I30),
      I60_mm_hr=inch_to_mm(I60),
      energy_m1=as.numeric(energy_m1),
      erosivity_m1=as.numeric(erosivity_m1),
      energy_m2=as.numeric(energy_m2),
      erosivity_m2=as.numeric(erosivity_m2),
      ARFdays1_mm=inch_to_mm(ARFdays1),
      ARFdays2_mm=inch_to_mm(ARFdays2),
      ARFdays7_mm=inch_to_mm(ARFdays7),
      ARFdays14_mm=inch_to_mm(ARFdays14),
      suspended_sediment_conc_mgL=as.numeric(suspended_sediment_conc_mgL),
      suspended_sediment_load_pounds=as.numeric(suspended_sediment_load_pounds),
      suspended_sediment_yield_pounds_per_acre=as.numeric(
        suspended_sediment_yield_pounds_per_acre
      ),
      total_dissolved_solids_conc_mgL=as.numeric(
        total_dissolved_solids_conc_mgL
      ),
      total_dissolved_solids_load_pounds=as.numeric(
        total_dissolved_solids_load_pounds
      ),
      total_dissolved_solids_yield_pounds_per_acre=as.numeric(
        total_dissolved_solids_yield_pounds_per_acre
      ),
      orthophosphate_conc_mgL=as.numeric(orthophosphate_conc_mgL),
      orthophosphate_load_pounds=as.numeric(orthophosphate_load_pounds),
      orthophosphate_yield_pounds_per_acre=as.numeric(
        orthophosphate_yield_pounds_per_acre
      ),
      total_phosphorus_unfiltered_conc_mgL=as.numeric(
        total_phosphorus_unfiltered_conc_mgL
      ),
      total_phosphorus_unfiltered_load_pounds=as.numeric(
        total_phosphorus_unfiltered_load_pounds
      ),
      total_phosphorus_unfiltered_yield_pounds_per_acre=as.numeric(
        total_phosphorus_unfiltered_yield_pounds_per_acre
      ),
      remark_suspended_sediment=as.character(remark_suspended_sediment),
      remark_total_dissolved_solids=as.character(
        remark_total_dissolved_solids
      ),
      remark_orthophosphate=as.character(remark_orthophosphate),
      remark_total_phosphorus_unfiltered=as.character(
        remark_total_phosphorus_unfiltered
      )
    ) %>%
    dplyr::filter(!is.na(Q_start),!is.na(Q_end)) %>%
    dplyr::arrange(Field_Name,Q_start,Q_end) %>%
    dplyr::mutate(WQ_event_id=dplyr::row_number()) %>%
    dplyr::left_join(
      site_df %>%
        dplyr::select(Field_Name,BasinArea_ac),
      by="Field_Name"
    ) %>%
    dplyr::mutate(
      area_ft2=BasinArea_ac*43560,
      runoff_mm=inch_to_mm(runoff_volume/area_ft2*12)
    )

  water_quality_df
}
