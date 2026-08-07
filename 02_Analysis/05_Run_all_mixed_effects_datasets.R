# Author: Zhaozhe Chen
# Update Date: 2026.8.7

# This code runs the mixed-effects workflow for all frozen and
# non-frozen events combined. Frozen-soil condition is included
# as a binary precipitation-characteristic predictor.

Project_path <- normalizePath(
  getwd(),
  winslash="/",
  mustWork=TRUE
)

Analysis_script <- file.path(
  Project_path,
  "02_Analysis",
  "04_Mixed_effects_models.R"
)

if(!file.exists(Analysis_script)){
  stop(
    "Run this script from the DF_runoff_v2 project root. ",
    "The mixed-effects analysis script was not found."
  )
}

Rscript_path <- file.path(
  R.home("bin"),
  if(.Platform$OS.type == "windows"){
    "Rscript.exe"
  }else{
    "Rscript"
  }
)

# Number of bootstrapping for Q occurence
Occurrence_replications <- Sys.getenv(
  "DF_OCCURRENCE_REPLICATIONS",
  "50"
)
RC_replications <- Sys.getenv(
  "DF_RC_REPLICATIONS",
  "200"
)

for(dataset_key in "All"){
  message(
    "Starting mixed-effects models: ",
    dataset_key
  )
  
  Sys.setenv(
    DF_EVENT_DATASET=dataset_key,
    DF_OCCURRENCE_REPLICATIONS=Occurrence_replications,
    DF_RC_REPLICATIONS=RC_replications
  )
  
  exit_status <- system2(
    command=Rscript_path,
    args=shQuote(Analysis_script)
  )
  
  if(!identical(exit_status,0L)){
    stop(
      "Mixed-effects model workflow failed for ",
      dataset_key,
      "."
    )
  }
}

message("All-event mixed-effects analysis is complete.")
