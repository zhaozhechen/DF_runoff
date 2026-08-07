# Author: Zhaozhe Chen
# Update Date: 2026.8.7

# This code reproduces the seasonal mixed-effects model workflow
# Part 1 uses a Mixed-effects logistic model for Runoff occurrence
# Part 2 uses a Mixed-effects linear regression model for Runoff magnitude
# Random-forest analyses are not included

# ---------- Global -----------
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
  library(lme4)
  library(performance)
  library(pROC)
  library(ggeffects)
  library(RColorBrewer)
})

# Confirm that the script is run from the project root
Project_path <- normalizePath(
  getwd(),
  winslash="/",
  mustWork=TRUE
)

if(!dir.exists(file.path(Project_path,"00_Data","Processed"))){
  stop(
    "Run this script from the DF_runoff_v2 project root. ",
    "The folder 00_Data/Processed was not found."
  )
}

# Source functions ======
source(
  file.path(
    Project_path,
    "01_Functions",
    "04_Reporting_plotting_functions.R"
  )
)
source(
  file.path(
    Project_path,
    "01_Functions",
    "05_Mixed_model_functions.R"
  )
)

# Paths ======
Processed_path <- file.path(Project_path,"00_Data","Processed")
Dataset_key <- Sys.getenv("DF_EVENT_DATASET","All")
Dataset_key <- match.arg(
  Dataset_key,
  c("Frozen","NonFrozen","All")
)

Dataset_labels <- c(
  Frozen="Frozen soil conditions",
  NonFrozen="Non-frozen soil conditions",
  All="All soil conditions"
)

Dataset_short_labels <- c(
  Frozen="Frozen",
  NonFrozen="Non-frozen",
  All="All events"
)

Result_path <- file.path(
  Project_path,
  "04_Results",
  "Mixed_Effects",
  Dataset_key
)
Figure_path <- file.path(Result_path,"Figures")
Table_path <- file.path(Result_path,"Tables")
Model_path <- file.path(Result_path,"Models")
Report_path <- file.path(Project_path,"03_Reports")

dir.create(Figure_path,recursive=TRUE,showWarnings=FALSE)
dir.create(Table_path,recursive=TRUE,showWarnings=FALSE)
dir.create(Model_path,recursive=TRUE,showWarnings=FALSE)
dir.create(Report_path,recursive=TRUE,showWarnings=FALSE)

# Reproduce the previous uncertainty settings by default
# Environment variables allow short diagnostic runs without changing the code
Occurrence_Replications <- as.integer(
  Sys.getenv("DF_OCCURRENCE_REPLICATIONS","50")
)
RC_Replications <- as.integer(
  Sys.getenv("DF_RC_REPLICATIONS","200")
)

if(
  !is.finite(Occurrence_Replications) ||
  Occurrence_Replications < 1 ||
  !is.finite(RC_Replications) ||
  RC_Replications < 1
){
  stop("Replication settings must be positive integers.")
}

set.seed(123)

# Model variable definitions ======
All_season_levels <- c(
  "Pre-growing season",
  "Growing season",
  "Post-growing season"
)

# No frozen events occur during June-September
Season_levels <- if(Dataset_key == "Frozen"){
  c("Pre-growing season","Post-growing season")
}else{
  All_season_levels
}

# Precipitation-characteristic predictors
# Frozen soil is included when both frozen and non-frozen observations occur
Storm_variables <- c(
  "log_I30",
  "log_ARFdays7",
  "Frozen"
)

# Agricultural predictors vary by season
# Residue is excluded from the growing-season models
Agricultural_variables <- list(
  "Pre-growing season"=c(
    "Tillage_Passes",
    "PerennialFrac",
    "Residue_Frac"
  ),
  "Growing season"=c(
    "Tillage_Passes",
    "PerennialFrac"
  ),
  "Post-growing season"=c(
    "Tillage_Passes",
    "PerennialFrac",
    "Residue_Frac"
  )
)

# Tile represents site-level tile drainage, not monitoring type
Site_variables <- c(
  "MeanSlope_per",
  "Hydrologic_Group",
  "Tile"
)

Continuous_variables <- c(
  "log_I30",
  "log_ARFdays7",
  "Tillage_Passes",
  "PerennialFrac",
  "Residue_Frac",
  "MeanSlope_per"
)

# Friendly labels used in figures and the report
Variable_labels <- c(
  log_I30="Log 30-minute intensity",
  log_ARFdays7="Log 7-day antecedent rainfall",
  Frozen="Frozen soil condition",
  Tillage_Passes="Seasonal tillage passes",
  PerennialFrac="Perennial crop fraction",
  Residue_Frac="Crop-residue fraction",
  MeanSlope_per="Mean slope",
  Hydrologic_Group="Soil infiltration group",
  Tile="Site-level tile drainage"
)

Comparison_labels <- c(
  Storm_Agricultural="Storm-only vs. Agricultural model",
  Storm_Site="Storm-only vs. Site physics model",
  Agricultural_Full="Agricultural vs. Full model",
  Site_Full="Site physics vs. Full model"
)

Model_labels <- c(
  Storm="Storm-only model",
  Agricultural="Agricultural model",
  Site="Site physics model",
  Full="Full model"
)

Variable_group_labels <- c(
  Agricultural="Agricultural management",
  Site="Physical site properties"
)

Model_colors <- c(
  Storm=DF_colors[3],
  Agricultural=DF_colors[2],
  Site=DF_colors[1],
  Full=DF_colors[4]
)

Variable_group_colors <- c(
  Agricultural=DF_colors[2],
  Site=DF_colors[1]
)

Season_colors <- setNames(
  RColorBrewer::brewer.pal(7,"Set2")[c(3,1,2)],
  All_season_levels
)

# ------- Data import and preprocessing ---------
keep_precipitation_condition <- function(frozen_flag){
  if(Dataset_key == "Frozen"){
    frozen_flag %in% TRUE
  }else if(Dataset_key == "NonFrozen"){
    frozen_flag %in% FALSE
  }else{
    frozen_flag %in% c(TRUE,FALSE)
  }
}

keep_runoff_condition <- function(frozen_status){
  if(Dataset_key == "Frozen"){
    frozen_status == "Frozen"
  }else if(Dataset_key == "NonFrozen"){
    frozen_status == "Non-Frozen"
  }else{
    frozen_status %in% c("Frozen","Non-Frozen")
  }
}

P_df <- read.csv(
  file.path(Processed_path,"All_P_events.csv"),
  stringsAsFactors=FALSE,
  check.names=FALSE
) %>%
  filter(Monitoring == "Surface") %>%
  mutate(
    Q_Occurred=as.integer(
      Associated_Q %in% c(TRUE,"TRUE","True","true",1,"1")
    ),
    Field_Name=factor(Field_Name),
    Season=factor(Season,levels=Season_levels),
    Hydrologic_Group=factor(
      Hydrologic_Group,
      levels=c(
        "Slow-infiltration",
        "Moderate-infiltration",
        "High-infiltration"
      )
    ),
    Tile=factor(Tile,levels=c("No","Yes")),
    Frozen=factor(
      if_else(P_frozen,"Frozen","Non-Frozen"),
      levels=c("Non-Frozen","Frozen")
    ),
    log_I30=log(I30_mm_hr),
    log_ARFdays7=log(ARFdays7_mm+0.1)
  ) %>%
  filter(
    keep_precipitation_condition(P_frozen),
    is.finite(log_I30),
    is.finite(log_ARFdays7)
  )

Q_df <- read.csv(
  file.path(Processed_path,"All_Q_events.csv"),
  stringsAsFactors=FALSE,
  check.names=FALSE
) %>%
  filter(
    Monitoring == "Surface",
    keep_runoff_condition(frozen),
    rain_mm > 0,
    runoff_mm > 0
  ) %>%
  mutate(
    Runoff_Coefficient=runoff_mm/rain_mm
  ) %>%
  # Retain the runoff-coefficient quality screen from the previous model
  filter(
    is.finite(Runoff_Coefficient),
    Runoff_Coefficient < 5
  ) %>%
  mutate(
    log_RC=log(Runoff_Coefficient),
    Field_Name=factor(Field_Name),
    Season=factor(Season,levels=Season_levels),
    Hydrologic_Group=factor(
      Hydrologic_Group,
      levels=c(
        "Slow-infiltration",
        "Moderate-infiltration",
        "High-infiltration"
      )
    ),
    Tile=factor(Tile,levels=c("No","Yes")),
    Frozen=factor(
      frozen,
      levels=c("Non-Frozen","Frozen")
    ),
    log_I30=log(I30_mm_hr),
    log_ARFdays7=log(ARFdays7_mm+0.1)
  ) %>%
  filter(
    is.finite(log_RC),
    is.finite(log_I30),
    is.finite(log_ARFdays7)
  )

# Verify the revised agricultural-variable definitions before modeling
stopifnot(
  all(
    P_df$Crop_Source[
      P_df$Season %in% c(
        "Post-growing season",
        "Pre-growing season"
      )
    ] == "Previous crop"
  ),
  all(
    Q_df$Crop_Source[
      Q_df$Season %in% c(
        "Post-growing season",
        "Pre-growing season"
      )
    ] == "Previous crop"
  ),
  all(
    P_df$Crop_Source[P_df$Season == "Growing season"] ==
      "Current crop"
  ),
  all(
    Q_df$Crop_Source[Q_df$Season == "Growing season"] ==
      "Current crop"
  ),
  all(
    is.na(
      P_df$Residue_Frac[P_df$Season == "Growing season"]
    )
  ),
  all(
    is.na(
      Q_df$Residue_Frac[Q_df$Season == "Growing season"]
    )
  )
)

# Prepare one complete-case dataset for one season
prepare_season_model_data <- function(
    df,
    response,
    season){
  agricultural_terms <- Agricultural_variables[[season]]
  required_variables <- unique(
    c(
      response,
      "Field_Name",
      Storm_variables,
      agricultural_terms,
      Site_variables
    )
  )
  
  df %>%
    filter(Season == season) %>%
    select(all_of(required_variables)) %>%
    drop_na() %>%
    filter(
      if_all(
        all_of(
          intersect(
            Continuous_variables,
            required_variables
          )
        ),
        is.finite
      )
    ) %>%
    mutate(
      Field_Name=droplevels(factor(Field_Name)),
      Hydrologic_Group=droplevels(
        factor(Hydrologic_Group)
      ),
      Tile=droplevels(factor(Tile)),
      Frozen=droplevels(factor(Frozen))
    )
}

# Run the balanced bootstrap workflow for one response
run_mixed_model_analysis <- function(
    df,
    response,
    response_type,
    replications,
    analysis_name){
  season_data <- setNames(
    lapply(
      Season_levels,
      function(season){
        prepare_season_model_data(
          df,
          response,
          season
        )
      }
    ),
    Season_levels
  )
  
  season_counts <- data.frame(
    Analysis=analysis_name,
    Season=Season_levels,
    Complete_Events=vapply(
      season_data,
      nrow,
      integer(1)
    )
  )
  
  if(any(season_counts$Complete_Events == 0)){
    stop(
      analysis_name,
      " has a season with no complete observations."
    )
  }
  
  balanced_sample_n <- min(season_counts$Complete_Events)
  season_counts$Balanced_Sample_n <- balanced_sample_n
  
  message(
    analysis_name,
    ": balanced bootstrap n per season = ",
    balanced_sample_n
  )
  
  metric_results <- list()
  metric_long_results <- list()
  comparison_results <- list()
  drop_results <- list()
  
  for(season in Season_levels){
    agricultural_terms <- Agricultural_variables[[season]]
    scale_terms <- intersect(
      Continuous_variables,
      c(
        Storm_variables,
        agricultural_terms,
        Site_variables
      )
    )
    
    for(replication in seq_len(replications)){
      sampled_df <- season_data[[season]] %>%
        slice_sample(
          n=balanced_sample_n,
          replace=TRUE
        )
      
      replication_result <- fit_model_replication(
        sampled_df=sampled_df,
        response=response,
        response_type=response_type,
        season=season,
        replication=replication,
        storm_terms=Storm_variables,
        agricultural_terms=agricultural_terms,
        site_terms=Site_variables,
        scale_terms=scale_terms
      )
      
      result_name <- paste(
        season,
        replication,
        sep="_"
      )
      
      metric_results[[result_name]] <-
        replication_result$Metrics
      metric_long_results[[result_name]] <-
        replication_result$Metrics_Long
      comparison_results[[result_name]] <-
        replication_result$Comparisons
      drop_results[[result_name]] <-
        replication_result$Drop_One
      
      if(replication %% 10 == 0){
        message(
          analysis_name,
          ": complete ",
          season,
          " replication ",
          replication,
          " of ",
          replications
        )
      }
    }
  }
  
  metric_df <- bind_rows(metric_results)
  metric_long_df <- bind_rows(metric_long_results)
  comparison_df <- bind_rows(comparison_results)
  drop_df <- bind_rows(drop_results)
  
  list(
    Season_Data=season_data,
    Season_Counts=season_counts,
    Balanced_Sample_n=balanced_sample_n,
    Metrics=metric_df,
    Metrics_Long=metric_long_df,
    Comparisons=comparison_df,
    Drop_One=drop_df,
    Metric_Summary=summarize_model_metrics(
      metric_long_df,
      response_type
    ),
    Comparison_Summary=summarize_model_comparisons(
      comparison_df
    ),
    Drop_Summary=summarize_drop_one(drop_df)
  )
}

Refresh_only <- tolower(
  Sys.getenv("DF_REFRESH_MODEL_OUTPUTS","false")
) %in% c("true","1","yes")

read_saved_results <- function(file_prefix){
  read_result_table <- function(table_name){
    read.csv(
      file.path(
        Table_path,
        paste0(file_prefix,"_",table_name,".csv")
      ),
      stringsAsFactors=FALSE,
      check.names=FALSE
    )
  }

  list(
    Metrics=read_result_table("Model_metrics_replications"),
    Metrics_Long=read_result_table(
      "Model_metrics_long_replications"
    ),
    Comparisons=read_result_table(
      "Model_comparisons_replications"
    ),
    Drop_One=read_result_table("Drop_one_replications"),
    Metric_Summary=read_result_table("Model_metric_summary"),
    Comparison_Summary=read_result_table(
      "Model_comparison_summary"
    ),
    Drop_Summary=read_result_table("Drop_one_summary"),
    Season_Counts=read_result_table("Seasonal_sample_sizes")
  )
}

if(Refresh_only){
  message("Refreshing figures and reports from saved model results.")
  Occurrence_results <- read_saved_results("Occurrence")
  RC_results <- read_saved_results("RC")
}else{
  # Part 1. Surface-runoff occurrence ==========================
  Occurrence_results <- run_mixed_model_analysis(
    df=P_df,
    response="Q_Occurred",
    response_type="occurrence",
    replications=Occurrence_Replications,
    analysis_name="Runoff occurrence"
  )

  # Part 2. Runoff magnitude ===================================
  RC_results <- run_mixed_model_analysis(
    df=Q_df,
    response="log_RC",
    response_type="continuous",
    replications=RC_Replications,
    analysis_name="Runoff magnitude"
  )
}

# Save model result tables =====================================
write_model_tables <- function(results,file_prefix){
  output_tables <- list(
    Model_metrics_replications=results$Metrics,
    Model_metrics_long_replications=results$Metrics_Long,
    Model_metric_summary=results$Metric_Summary,
    Model_comparisons_replications=results$Comparisons,
    Model_comparison_summary=results$Comparison_Summary,
    Drop_one_replications=results$Drop_One,
    Drop_one_summary=results$Drop_Summary,
    Seasonal_sample_sizes=results$Season_Counts
  )
  
  for(table_name in names(output_tables)){
    write.csv(
      output_tables[[table_name]],
      file.path(
        Table_path,
        paste0(
          file_prefix,
          "_",
          table_name,
          ".csv"
        )
      ),
      row.names=FALSE,
      na=""
    )
  }
}

if(!Refresh_only){
  write_model_tables(
    Occurrence_results,
    "Occurrence"
  )
  write_model_tables(
    RC_results,
    "RC"
  )
}

# Fit final full models using all complete observations ========
fit_final_models <- function(
    results,
    response,
    response_type,
    analysis_name,
    file_prefix){
  final_models <- list()
  coefficient_tables <- list()
  specification_rows <- list()
  
  for(season in Season_levels){
    final_df <- results$Season_Data[[season]]
    agricultural_terms <- Agricultural_variables[[season]]
    supported_storm_terms <- Storm_variables
    supported_site_terms <- Site_variables

    if(
      "Frozen" %in% supported_storm_terms &&
      !factor_has_support(final_df,"Frozen",min_n=5)
    ){
      supported_storm_terms <- setdiff(
        supported_storm_terms,
        "Frozen"
      )
    }
    
    for(variable in intersect(
        c("Hydrologic_Group","Tile"),
        supported_site_terms)){
      if(!factor_has_support(
          final_df,
          variable,
          min_n=5
        )){
        supported_site_terms <- setdiff(
          supported_site_terms,
          variable
        )
      }
    }
    
    full_terms <- unique(
      c(
        supported_storm_terms,
        agricultural_terms,
        supported_site_terms
      )
    )
    scale_terms <- intersect(
      Continuous_variables,
      full_terms
    )
    model_df <- scale_model_variables(
      final_df,
      scale_terms
    )
    final_model <- fit_mixed_model(
      model_df,
      response=response,
      fixed_terms=full_terms,
      response_type=response_type
    )
    
    if(is.null(final_model)){
      stop(
        "The final ",
        analysis_name,
        " model failed for ",
        season,
        "."
      )
    }
    
    final_models[[season]] <- list(
      Model=final_model,
      Data=model_df,
      Terms=full_terms
    )
    
    model_output_file <- file.path(
      Model_path,
      paste0(
        file_prefix,
        "_Full_",
        season,
        ".rds"
      )
    )
    
    tryCatch(
      saveRDS(
        final_models[[season]],
        model_output_file
      ),
      error=function(e){
        message(
          "Model object could not be saved at ",
          model_output_file,
          ": ",
          conditionMessage(e)
        )
      }
    )
    
    coefficient_tables[[season]] <-
      extract_fixed_effects(
        final_model,
        season,
        analysis_name
      )
    
    specification_rows[[season]] <- data.frame(
      Analysis=analysis_name,
      Season=season,
      Observations=nrow(model_df),
      Sites=dplyr::n_distinct(model_df$Field_Name),
      Storm_Variables=paste(
        supported_storm_terms,
        collapse=" + "
      ),
      Agricultural_Variables=paste(
        agricultural_terms,
        collapse=" + "
      ),
      Site_Variables=paste(
        supported_site_terms,
        collapse=" + "
      ),
      Formula=paste(
        deparse(stats::formula(final_model)),
        collapse=""
      ),
      Singular=lme4::isSingular(final_model,tol=1e-4)
    )
  }
  
  list(
    Models=final_models,
    Coefficients=bind_rows(coefficient_tables),
    Specifications=bind_rows(specification_rows)
  )
}

if(Refresh_only){
  load_final_models <- function(file_prefix){
    model_list <- lapply(
      Season_levels,
      function(season){
        readRDS(
          file.path(
            Model_path,
            paste0(file_prefix,"_Full_",season,".rds")
          )
        )
      }
    )
    names(model_list) <- Season_levels
    list(Models=model_list)
  }

  Occurrence_final <- load_final_models("Occurrence")
  RC_final <- load_final_models("RC")
  Final_coefficients <- read.csv(
    file.path(Table_path,"Final_full_model_coefficients.csv"),
    stringsAsFactors=FALSE,
    check.names=FALSE
  )
  Final_specifications <- read.csv(
    file.path(Table_path,"Final_full_model_specifications.csv"),
    stringsAsFactors=FALSE,
    check.names=FALSE
  )
}else{
  Occurrence_final <- fit_final_models(
    Occurrence_results,
    response="Q_Occurred",
    response_type="occurrence",
    analysis_name="Runoff occurrence",
    file_prefix="Occurrence"
  )

  RC_final <- fit_final_models(
    RC_results,
    response="log_RC",
    response_type="continuous",
    analysis_name="Runoff magnitude",
    file_prefix="RC"
  )

  Final_coefficients <- bind_rows(
    Occurrence_final$Coefficients,
    RC_final$Coefficients
  )
  Final_specifications <- bind_rows(
    Occurrence_final$Specifications,
    RC_final$Specifications
  )

  write.csv(
    Final_coefficients,
    file.path(Table_path,"Final_full_model_coefficients.csv"),
    row.names=FALSE,
    na=""
  )
  write.csv(
    Final_specifications,
    file.path(Table_path,"Final_full_model_specifications.csv"),
    row.names=FALSE,
    na=""
  )
}

Singular_diagnostic <- bind_rows(
  Occurrence_results$Metrics_Long %>%
    mutate(Analysis="Runoff occurrence"),
  RC_results$Metrics_Long %>%
    mutate(Analysis="Runoff magnitude")
) %>%
  group_by(Analysis,Season,Model) %>%
  summarise(
    Replications=n(),
    Singular_Replications=sum(Singular %in% TRUE),
    Singular_Percent=100*mean(Singular %in% TRUE),
    Missing_Conditional_R2=sum(is.na(R2c)),
    .groups="drop"
  )

write.csv(
  Singular_diagnostic,
  file.path(Table_path,"Singular_fit_diagnostic.csv"),
  row.names=FALSE,
  na=""
)

# Plotting functions ===========================================
model_performance_figure <- function(
    results,
    response_type){
  metric_order <- if(response_type == "occurrence"){
    c("AUC","R2m","R2c","Random_R2")
  }else{
    c("RMSE","R2m","R2c","Random_R2")
  }
  
  metric_labels <- c(
    AUC="AUC",
    RMSE="RMSE of log(RC)",
    R2m="Marginal R-squared",
    R2c="Conditional R-squared",
    Random_R2="Random-effect R-squared"
  )
  
  plot_df <- results$Metrics_Long %>%
    mutate(
      Random_R2=R2c-R2m,
      Season=factor(Season,levels=Season_levels),
      Model=factor(
        Model,
        levels=c("Storm","Agricultural","Site","Full")
      )
    ) %>%
    pivot_longer(
      cols=all_of(metric_order),
      names_to="Metric",
      values_to="Value"
    ) %>%
    mutate(
      Metric=factor(Metric,levels=metric_order)
    )
  
  metric_plots <- lapply(
    metric_order,
    function(metric_name){
      ggplot(
        filter(plot_df,Metric == metric_name),
        aes(Model,Value,fill=Model)
      ) +
        geom_boxplot(
          color="black",
          outlier.shape=NA
        ) +
        facet_wrap(~Season,nrow=1) +
        scale_x_discrete(labels=Model_labels) +
        scale_fill_manual(
          values=Model_colors,
          labels=Model_labels
        ) +
        labs(
          x=NULL,
          y=metric_labels[[metric_name]],
          fill="Model"
        ) +
        DF_plot_theme +
        theme(
          legend.position="bottom",
          axis.text.x=element_text(angle=25,hjust=1)
        )
    }
  )
  
  wrap_plots(metric_plots,ncol=2,guides="collect") &
    theme(legend.position="bottom")
}

model_comparison_figure <- function(results){
  comparison_df <- results$Comparison_Summary %>%
    mutate(
      Season=factor(Season,levels=Season_levels),
      Comparison=factor(
        Comparison,
        levels=names(Comparison_labels),
        labels=unname(Comparison_labels)
      ),
      Chisq_Label_Y=Mean_Chisq+SD_Chisq
    )
  
  delta_aic_plot <- ggplot(
    comparison_df,
    aes(Season,Mean_Delta_AIC,fill=Season)
  ) +
    geom_col(color="black") +
    geom_errorbar(
      aes(
        ymin=Mean_Delta_AIC-SD_Delta_AIC,
        ymax=Mean_Delta_AIC+SD_Delta_AIC
      ),
      width=0.2
    ) +
    geom_hline(yintercept=0,linetype="dashed") +
    facet_wrap(~Comparison,nrow=1) +
    scale_fill_manual(values=Season_colors) +
    labs(
      x=NULL,
      y="Mean Delta AIC (smaller - larger)",
      fill="Season"
    ) +
    DF_plot_theme +
    theme(
      legend.position="bottom",
      axis.text.x=element_text(angle=25,hjust=1)
    )
  
  chisq_plot <- ggplot(
    comparison_df,
    aes(Season,Mean_Chisq,fill=Season)
  ) +
    geom_col(color="black") +
    geom_errorbar(
      aes(
        ymin=pmax(0,Mean_Chisq-SD_Chisq),
        ymax=Mean_Chisq+SD_Chisq
      ),
      width=0.2
    ) +
    geom_text(
      aes(
        y=Chisq_Label_Y,
        label=paste0(
          "p<0.05: ",
          round(Significant_Percent),
          "%"
        )
      ),
      vjust=-0.45,
      size=4
    ) +
    facet_wrap(~Comparison,nrow=1) +
    scale_fill_manual(values=Season_colors) +
    scale_y_continuous(expand=expansion(mult=c(0,0.22))) +
    labs(
      x=NULL,
      y="Mean likelihood-ratio chi-squared",
      fill="Season"
    ) +
    DF_plot_theme +
    theme(
      legend.position="bottom",
      axis.text.x=element_text(angle=25,hjust=1)
    )
  
  delta_aic_plot / chisq_plot +
    plot_layout(guides="collect") &
    theme(legend.position="bottom")
}

drop_one_figure <- function(results){
  plot_df <- results$Drop_Summary %>%
    mutate(
      Season=factor(Season,levels=Season_levels),
      Variable_Label=ifelse(
        Dropped %in% names(Variable_labels),
        Variable_labels[Dropped],
        Dropped
      )
    )
  
  delta_aic_order <- plot_df %>%
    arrange(Season,Mean_Delta_AIC) %>%
    transmute(
      Variable_Season=paste(
        Variable_Label,
        Season,
        sep="___"
      )
    ) %>%
    pull(Variable_Season)

  chisq_order <- plot_df %>%
    arrange(Season,Mean_Chisq) %>%
    transmute(
      Variable_Season=paste(
        Variable_Label,
        Season,
        sep="___"
      )
    ) %>%
    pull(Variable_Season)
  
  plot_df <- plot_df %>%
    mutate(
      Variable_Season_Delta_AIC=factor(
        paste(Variable_Label,Season,sep="___"),
        levels=unique(delta_aic_order)
      ),
      Variable_Season_Chisq=factor(
        paste(Variable_Label,Season,sep="___"),
        levels=unique(chisq_order)
      ),
      Chisq_Label_Y=Mean_Chisq+SD_Chisq
    )
  
  delta_aic_plot <- ggplot(
    plot_df,
    aes(
      Variable_Season_Delta_AIC,
      Mean_Delta_AIC,
      fill=Variable_Group
    )
  ) +
    geom_col(color="black") +
    geom_errorbar(
      aes(
        ymin=Mean_Delta_AIC-SD_Delta_AIC,
        ymax=Mean_Delta_AIC+SD_Delta_AIC
      ),
      width=0.2
    ) +
    geom_hline(yintercept=0,linetype="dashed") +
    coord_flip() +
    facet_wrap(~Season,nrow=1,scales="free_y") +
    scale_x_discrete(
      labels=function(x) sub("___.*$","",x)
    ) +
    scale_fill_manual(
      values=Variable_group_colors,
      labels=Variable_group_labels
    ) +
    labs(
      x=NULL,
      y="Mean Delta AIC (dropped - full)",
      fill="Variable category"
    ) +
    DF_plot_theme +
    theme(legend.position="bottom")
  
  chisq_plot <- ggplot(
    plot_df,
    aes(
      Variable_Season_Chisq,
      Mean_Chisq,
      fill=Variable_Group
    )
  ) +
    geom_col(color="black") +
    geom_errorbar(
      aes(
        ymin=pmax(0,Mean_Chisq-SD_Chisq),
        ymax=Mean_Chisq+SD_Chisq
      ),
      width=0.2
    ) +
    geom_text(
      aes(
        y=Chisq_Label_Y,
        label=paste0(
          "p<0.05: ",
          round(Significant_Percent),
          "%"
        )
      ),
      hjust=-0.15,
      size=4
    ) +
    coord_flip(clip="off") +
    facet_wrap(~Season,nrow=1,scales="free_y") +
    scale_x_discrete(
      labels=function(x) sub("___.*$","",x)
    ) +
    scale_fill_manual(
      values=Variable_group_colors,
      labels=Variable_group_labels
    ) +
    scale_y_continuous(expand=expansion(mult=c(0,0.38))) +
    labs(
      x=NULL,
      y="Mean likelihood-ratio chi-squared",
      fill="Variable category"
    ) +
    DF_plot_theme +
    theme(legend.position="bottom")
  
  delta_aic_plot / chisq_plot +
    plot_layout(guides="collect") &
    theme(legend.position="bottom")
}

marginal_effect_panel <- function(
    final_results,
    variable,
    response_type){
  prediction_rows <- list()
  is_numeric_predictor <- FALSE
  
  for(season in Season_levels){
    model_result <- final_results$Models[[season]]
    
    if(!variable %in% model_result$Terms){
      next
    }
    
    is_numeric_predictor <- is.numeric(
      model_result$Data[[variable]]
    )
    
    prediction <- tryCatch(
      suppressMessages(
        as.data.frame(
          ggeffects::ggpredict(
            model_result$Model,
            terms=variable
          )
        )
      ),
      error=function(e) NULL
    )
    
    if(!is.null(prediction)){
      prediction$Season <- season
      prediction_rows[[season]] <- prediction
    }
  }
  
  if(length(prediction_rows) == 0){
    return(ggplot() + theme_void())
  }
  
  prediction_df <- bind_rows(prediction_rows) %>%
    mutate(Season=factor(Season,levels=Season_levels))
  
  variable_label <- if(variable %in% names(Variable_labels)){
    Variable_labels[[variable]]
  }else{
    variable
  }
  
  x_title <- if(is_numeric_predictor){
    paste0(variable_label," (Standardized)")
  }else{
    variable_label
  }
  
  y_title <- if(response_type == "occurrence"){
    "Runoff probability"
  }else{
    "Runoff magnitude: log(RC)"
  }
  
  if(is_numeric_predictor){
    effect_plot <- ggplot(
      prediction_df,
      aes(
        x=x,
        y=predicted,
        color=Season,
        fill=Season
      )
    ) +
      geom_ribbon(
        aes(
          ymin=conf.low,
          ymax=conf.high,
          group=Season
        ),
        alpha=0.22,
        color=NA
      ) +
      geom_line(
        aes(group=Season),
        linewidth=1
      )
  }else{
    dodge <- position_dodge(width=0.45)
    
    effect_plot <- ggplot(
      prediction_df,
      aes(
        x=x,
        y=predicted,
        color=Season,
        fill=Season,
        group=Season
      )
    ) +
      geom_errorbar(
        aes(ymin=conf.low,ymax=conf.high),
        width=0.12,
        position=dodge
      ) +
      geom_point(
        shape=21,
        size=3.5,
        color="black",
        position=dodge
      )
  }
  
  effect_plot +
    scale_color_manual(values=Season_colors,drop=FALSE) +
    scale_fill_manual(
      values=Season_colors,
      drop=FALSE,
      guide="none"
    ) +
    labs(
      x=x_title,
      y=y_title,
      color="Season",
      fill="Season"
    ) +
    DF_plot_theme +
    theme(
      legend.position="bottom",
      axis.text.x=element_text(
        angle=if(is_numeric_predictor) 0 else 25,
        hjust=if(is_numeric_predictor) 0.5 else 1
      )
    )
}

marginal_effect_figure <- function(
    final_results,
    response_type){
  variable_order <- c(
    Storm_variables,
    "Tillage_Passes",
    "PerennialFrac",
    "Residue_Frac",
    Site_variables
  )
  
  variable_plots <- lapply(
    variable_order,
    function(variable){
      marginal_effect_panel(
        final_results,
        variable,
        response_type
      )
    }
  )
  
  for(plot_number in seq_along(variable_plots)){
    if(plot_number > 1){
      variable_plots[[plot_number]] <- variable_plots[[plot_number]] +
        theme(legend.position="none")
    }
  }
  
  wrap_plots(
    variable_plots,
    ncol=3,
    guides="collect"
  ) +
    plot_annotation(
      theme=theme(legend.position="bottom")
    )
}

# Generate occurrence figures =================================
Occurrence_performance <- model_performance_figure(
  Occurrence_results,
  "occurrence"
)
Occurrence_comparison <- model_comparison_figure(
  Occurrence_results
)
Occurrence_drop <- drop_one_figure(Occurrence_results)
Occurrence_marginal <- marginal_effect_figure(
  Occurrence_final,
  "occurrence"
)

save_figure_pair(
  Occurrence_performance,
  file.path(Figure_path,"01_Occurrence_model_performance"),
  width=16,
  height=10
)
save_figure_pair(
  Occurrence_comparison,
  file.path(Figure_path,"02_Occurrence_model_comparisons"),
  width=20,
  height=10
)
save_figure_pair(
  Occurrence_drop,
  file.path(Figure_path,"03_Occurrence_drop_one"),
  width=18,
  height=12
)
save_figure_pair(
  Occurrence_marginal,
  file.path(Figure_path,"04_Occurrence_marginal_effects"),
  width=18,
  height=15
)

# Generate runoff-coefficient figures =========================
RC_performance <- model_performance_figure(
  RC_results,
  "continuous"
)
RC_comparison <- model_comparison_figure(RC_results)
RC_drop <- drop_one_figure(RC_results)
RC_marginal <- marginal_effect_figure(
  RC_final,
  "continuous"
)

save_figure_pair(
  RC_performance,
  file.path(Figure_path,"05_RC_model_performance"),
  width=16,
  height=10
)
save_figure_pair(
  RC_comparison,
  file.path(Figure_path,"06_RC_model_comparisons"),
  width=20,
  height=10
)
save_figure_pair(
  RC_drop,
  file.path(Figure_path,"07_RC_drop_one"),
  width=18,
  height=12
)
save_figure_pair(
  RC_marginal,
  file.path(Figure_path,"08_RC_marginal_effects"),
  width=18,
  height=15
)

# Generate the HTML report =====================================
Model_variable_table <- data.frame(
  Component=c(
    "Response",
    "Precipitation-characteristics",
    "Agricultural management",
    "Physical site properties",
    "Random effect"
  ),
  Definition=c(
    "Runoff occurrence is a binary response analyzed with a Mixed-effects logistic model. Runoff magnitude is defined as log(RC) and analyzed with a Mixed-effects linear regression model.",
    "Log 30-minute precipitation intensity, log seven-day antecedent precipitation, and binary frozen-soil condition. Frozen-soil condition is omitted when a season contains only one observed level.",
    "Seasonal tillage passes, continuous perennial crop fraction, and the crop-residue fraction when applicable.",
    "Mean slope, grouped soil infiltration, and binary site-level tile drainage",
    "Site random intercept"
  ),
  check.names=FALSE
)

Agricultural_rule_table <- data.frame(
  Season=c(
    "Pre-growing season (January-May)",
    "Growing season (June-September)",
    "Post-growing season (October-December)"
  ),
  `Precipitation-characteristics`=rep(
    "Log 30-minute intensity, log seven-day antecedent precipitation, and frozen-soil condition when both levels occur",
    3
  ),
  `Agricultural management`=c(
    "Post-growing- plus pre-growing-season tillage passes; previous summer crop perennial fraction; spring crop-residue fraction",
    "Pre-growing- plus growing-season tillage passes; current summer crop perennial fraction; crop residue excluded",
    "Growing- plus post-growing-season tillage passes; previous summer crop perennial fraction; fall crop-residue fraction"
  ),
  `Physical site properties`=rep(
    "Mean slope, grouped soil infiltration, and binary site-level tile drainage",
    3
  ),
  check.names=FALSE
)

Full_metric_report <- bind_rows(
  Occurrence_results$Metric_Summary %>%
    filter(Model == "Full") %>%
    mutate(Analysis="Runoff occurrence"),
  RC_results$Metric_Summary %>%
    filter(Model == "Full") %>%
    mutate(Analysis="Runoff magnitude")
) %>%
  select(
    Analysis,
    Season,
    Metric,
    Replications,
    Mean,
    SD,
    Median,
    Q25,
    Q75
  )

Most_important_variables <- bind_rows(
  Occurrence_results$Drop_Summary %>%
    mutate(Analysis="Runoff occurrence"),
  RC_results$Drop_Summary %>%
    mutate(Analysis="Runoff magnitude")
) %>%
  group_by(Analysis,Season) %>%
  slice_max(
    Mean_Delta_AIC,
    n=3,
    with_ties=FALSE
  ) %>%
  ungroup() %>%
  mutate(
    Variable=ifelse(
      Dropped %in% names(Variable_labels),
      Variable_labels[Dropped],
      Dropped
    ),
    Variable_Group=dplyr::recode(
      Variable_Group,
      !!!Variable_group_labels
    )
  ) %>%
  select(
    Analysis,
    Season,
    Variable,
    Variable_Group,
    Mean_Delta_AIC,
    SD_Delta_AIC,
    Significant_Percent
  )

Final_specifications_report <- Final_specifications %>%
  mutate(
    Analysis=dplyr::recode(
      Analysis,
      `Runoff coefficient`="Runoff magnitude"
    )
  ) %>%
  select(-any_of("Singular")) %>%
  rename(
    `Precipitation-characteristics`=Storm_Variables,
    `Agricultural management`=Agricultural_Variables,
    `Physical site properties`=Site_Variables
  )

Final_coefficients_report <- Final_coefficients %>%
  mutate(
    Analysis=dplyr::recode(
      Analysis,
      `Runoff coefficient`="Runoff magnitude"
    )
  )

Report_body <- c(
  paste0(
    "<div class=\"callout\"><strong>Model scope:</strong> Mixed-effects models are fitted for Runoff occurrence and Runoff magnitude, defined as log(RC), using all frozen and non-frozen surface-runoff events. Runoff occurrence is analyzed with a Mixed-effects logistic model, and Runoff magnitude is analyzed with a Mixed-effects linear regression model",
    ". The balanced-bootstrap, nested-model comparison, drop-one-variable, and marginal-effect logic is retained. Random-forest analyses are excluded.</div>"
  ),
  "<h2>Model definitions</h2>",
  data_frame_to_html(Model_variable_table,digits=2),
  "<h2>Season-specific variables and rules</h2>",
  data_frame_to_html(Agricultural_rule_table,digits=2),
  "<h2>Final full-model specifications</h2>",
  "<p>Continuous predictors are standardized within each bootstrap sample and within each final seasonal model. Frozen-soil condition is included as a binary precipitation-characteristic predictor whenever both frozen and non-frozen observations occur within a season. Every model includes a site random intercept.</p>",
  data_frame_to_html(Final_specifications_report,digits=2),
  "<h2>Part 1. Runoff occurrence</h2>",
  paste0(
    "<p>The response is whether a precipitation event under ",
    tolower(Dataset_labels[[Dataset_key]]),
    " produced surface runoff. This response is analyzed with a Mixed-effects logistic model. AUC evaluates discrimination; marginal and conditional R-squared values quantify fixed-effect and total model variation.</p>"
  ),
  embedded_figure_html(
    file.path(Figure_path,"01_Occurrence_model_performance.png"),
    "Figure 1. Performance of the Storm-only model, Agricultural model, Site physics model, and Full model for Runoff occurrence across balanced bootstrap replications."
  ),
  embedded_figure_html(
    file.path(Figure_path,"02_Occurrence_model_comparisons.png"),
    "Figure 2. Nested Runoff occurrence model comparisons using Delta AIC and likelihood-ratio tests."
  ),
  embedded_figure_html(
    file.path(Figure_path,"03_Occurrence_drop_one.png"),
    "Figure 3. Change in Runoff occurrence model support when each Agricultural management or Physical site properties variable is removed from the Full model."
  ),
  embedded_figure_html(
    file.path(Figure_path,"04_Occurrence_marginal_effects.png"),
    "Figure 4. Marginal effects from the final seasonal Mixed-effects logistic models for Runoff occurrence."
  ),
  "<h2>Part 2. Runoff magnitude</h2>",
  "<p>Runoff magnitude is defined as log(RC) and analyzed with a Mixed-effects linear regression model. RMSE is reported on the log(RC) scale.</p>",
  embedded_figure_html(
    file.path(Figure_path,"05_RC_model_performance.png"),
    "Figure 5. Performance of the Storm-only model, Agricultural model, Site physics model, and Full model for Runoff magnitude across balanced bootstrap replications."
  ),
  embedded_figure_html(
    file.path(Figure_path,"06_RC_model_comparisons.png"),
    "Figure 6. Nested Runoff magnitude model comparisons using Delta AIC and likelihood-ratio tests."
  ),
  embedded_figure_html(
    file.path(Figure_path,"07_RC_drop_one.png"),
    "Figure 7. Change in Runoff magnitude model support when each Agricultural management or Physical site properties variable is removed from the Full model."
  ),
  embedded_figure_html(
    file.path(Figure_path,"08_RC_marginal_effects.png"),
    "Figure 8. Marginal effects from the final seasonal Mixed-effects linear regression models for Runoff magnitude."
  ),
  "<h2>Full-model performance summary</h2>",
  "<p>Replications is the number of non-missing estimates available for each metric, not the number of model fits attempted. RMSE, AUC, and marginal R-squared can remain available when the site random-effect variance is estimated at the boundary. Conditional R-squared and the derived random-effect R-squared are unavailable for those fits, so their replication counts can be smaller.</p>",
  data_frame_to_html(Full_metric_report,digits=3),
  "<h2>Variables with the largest drop-one Delta AIC</h2>",
  "<p>Positive Delta AIC indicates that removing the variable reduced model support. The table lists the three largest mean values within each response and season.</p>",
  data_frame_to_html(Most_important_variables,digits=2),
  "<h2>Final full-model coefficients</h2>",
  "<p>Continuous-predictor coefficients are standardized. Mixed-effects logistic model p-values are Wald tests. The Mixed-effects linear regression models report estimates, standard errors, and t statistics; coefficient p-values are unavailable because lme4::lmer does not define denominator degrees of freedom or calculate them by default. Satterthwaite p-values require an explicitly selected method such as lmerTest.</p>",
  data_frame_to_html(Final_coefficients_report,digits=4),
  "<h2>Output files</h2>",
  "<p>All figures are saved as PNG and PDF files. Bootstrap results, model summaries, specifications, and coefficients are saved as CSV files. Final fitted seasonal models are saved as RDS files.</p>"
)

Mixed_model_report <- file.path(
  Report_path,
  paste0(
    "04_Mixed_effects_model_report_",
    Dataset_key,
    ".html"
  )
)

write_html_report(
  title=paste0(
    "Discovery Farms Seasonal Mixed-Effects Models: ",
    Dataset_labels[[Dataset_key]]
  ),
  subtitle=paste0(
    Dataset_short_labels[[Dataset_key]],
    " dataset; generated: ",
    Sys.Date()
  ),
  body_html=Report_body,
  output_path=Mixed_model_report
)

message("Mixed-effects model analysis complete.")
message("Figures: ",Figure_path)
message("Tables: ",Table_path)
message("Models: ",Model_path)
message("Report: ",Mixed_model_report)
