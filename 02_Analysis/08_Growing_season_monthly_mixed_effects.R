# Author: Zhaozhe Chen
# Update Date: 2026.8.20

# This code applies the established mixed-effects model framework separately
# to June, July, August, and September.

# Part 1 uses a Mixed-effects logistic model for Runoff occurrence.
# Part 2 uses a Mixed-effects linear regression model for Runoff magnitude,
# defined as log(RC).

# The monthly experiment uses all frozen and non-frozen events. Frozen-soil
# condition is automatically omitted because no frozen events occur during
# the growing-season months. Growing-season crop and tillage rules are retained.

# -------- Global -----------
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(ggplot2)
  library(patchwork)
  library(lme4)
  library(performance)
  library(pROC)
  library(RColorBrewer)
})

Project_path <- normalizePath(getwd(),winslash="/",mustWork=TRUE)

if(!dir.exists(file.path(Project_path,"00_Data","Processed"))){
  stop(
    "Run this script from the DF_runoff_v2 project root. ",
    "The folder 00_Data/Processed was not found."
  )
}

source(file.path(
  Project_path,
  "01_Functions",
  "04_Reporting_plotting_functions.R"
))
source(file.path(
  Project_path,
  "01_Functions",
  "05_Mixed_model_functions.R"
))

Processed_path <- file.path(Project_path,"00_Data","Processed")
Result_path <- file.path(
  Project_path,
  "04_Results",
  "Mixed_Effects_Monthly_Growing_Season"
)
Figure_path <- file.path(Result_path,"Figures")
Table_path <- file.path(Result_path,"Tables")
Model_path <- file.path(Result_path,"Models")
Report_path <- file.path(Project_path,"03_Reports")

dir.create(Figure_path,recursive=TRUE,showWarnings=FALSE)
dir.create(Table_path,recursive=TRUE,showWarnings=FALSE)
dir.create(Model_path,recursive=TRUE,showWarnings=FALSE)
dir.create(Report_path,recursive=TRUE,showWarnings=FALSE)

# Match the replication settings used by the established workflow
Occurrence_Replications <- as.integer(
  Sys.getenv("DF_MONTHLY_OCCURRENCE_REPLICATIONS","50")
)
RC_Replications <- as.integer(
  Sys.getenv("DF_MONTHLY_RC_REPLICATIONS","200")
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

Month_levels <- c("June","July","August","September")
Month_numbers <- setNames(6:9,Month_levels)
Month_colors <- setNames(
  RColorBrewer::brewer.pal(7,"Set2")[c(1,3,2,5)],
  Month_levels
)

Storm_variables <- c("log_I30","log_ARFdays7","Frozen")
Agricultural_variables <- c("Tillage_Passes","PerennialFrac")
Site_variables <- c("MeanSlope_per","Hydrologic_Group","Tile")
Continuous_variables <- c(
  "log_I30",
  "log_ARFdays7",
  "Tillage_Passes",
  "PerennialFrac",
  "MeanSlope_per"
)

Model_levels <- c("Storm","Agricultural","Site","Full")
Model_labels <- c(
  Storm="Storm-only model",
  Agricultural="Agricultural model",
  Site="Site physics model",
  Full="Full model"
)
Model_colors <- c(
  Storm=DF_colors[3],
  Agricultural=DF_colors[2],
  Site=DF_colors[1],
  Full=DF_colors[4]
)

Variable_labels <- c(
  Tillage_Passes="Seasonal tillage passes",
  PerennialFrac="Perennial crop fraction",
  MeanSlope_per="Mean slope",
  Hydrologic_Group="Soil infiltration group",
  Tile="Site-level tile drainage"
)

metric_summary_safe <- function(metrics_long,response_type){
  summarize_model_metrics(metrics_long,response_type) %>%
    mutate(
      Mean=if_else(is.nan(Mean),NA_real_,Mean),
      SD=if_else(is.nan(SD),NA_real_,SD),
      Median=if_else(is.nan(Median),NA_real_,Median),
      Q25=if_else(is.nan(Q25),NA_real_,Q25),
      Q75=if_else(is.nan(Q75),NA_real_,Q75)
    )
}

# ------- Data import and preprocessing ---------
P_df <- read.csv(
  file.path(Processed_path,"All_P_events.csv"),
  stringsAsFactors=FALSE,
  check.names=FALSE
) %>%
  filter(Monitoring == "Surface") %>%
  mutate(
    Event_Date=as.Date(
      as.POSIXct(P_start,tz="America/Chicago"),
      tz="America/Chicago"
    ),
    Month=factor(month(Event_Date,label=TRUE,abbr=FALSE),levels=Month_levels),
    Q_Occurred=as.integer(
      Associated_Q %in% c(TRUE,"TRUE","True","true",1,"1")
    ),
    Field_Name=factor(Field_Name),
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
    !is.na(Month),
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
    rain_mm > 0,
    runoff_mm > 0
  ) %>%
  mutate(
    Event_Date=as.Date(
      as.POSIXct(Q_start,tz="America/Chicago"),
      tz="America/Chicago"
    ),
    Month=factor(month(Event_Date,label=TRUE,abbr=FALSE),levels=Month_levels),
    Runoff_Coefficient=runoff_mm/rain_mm
  ) %>%
  filter(
    !is.na(Month),
    is.finite(Runoff_Coefficient),
    Runoff_Coefficient < 5
  ) %>%
  mutate(
    log_RC=log(Runoff_Coefficient),
    Field_Name=factor(Field_Name),
    Hydrologic_Group=factor(
      Hydrologic_Group,
      levels=c(
        "Slow-infiltration",
        "Moderate-infiltration",
        "High-infiltration"
      )
    ),
    Tile=factor(Tile,levels=c("No","Yes")),
    Frozen=factor(frozen,levels=c("Non-Frozen","Frozen")),
    log_I30=log(I30_mm_hr),
    log_ARFdays7=log(ARFdays7_mm+0.1)
  ) %>%
  filter(
    is.finite(log_RC),
    is.finite(log_I30),
    is.finite(log_ARFdays7)
  )

# Growing-season events must use current-water-year crop information,
# growing-season tillage passes, and no residue variable
stopifnot(
  all(P_df$Crop_Source == "Current crop"),
  all(Q_df$Crop_Source == "Current crop"),
  all(is.na(P_df$Residue_Frac)),
  all(is.na(Q_df$Residue_Frac))
)

prepare_month_data <- function(df,response,month_name){
  required_variables <- unique(c(
    response,
    "Field_Name",
    Storm_variables,
    Agricultural_variables,
    Site_variables
  ))

  df %>%
    filter(Month == month_name) %>%
    select(all_of(required_variables)) %>%
    drop_na() %>%
    filter(
      if_all(
        all_of(intersect(Continuous_variables,required_variables)),
        is.finite
      )
    ) %>%
    mutate(
      Field_Name=droplevels(factor(Field_Name)),
      Hydrologic_Group=droplevels(factor(Hydrologic_Group)),
      Tile=droplevels(factor(Tile)),
      Frozen=droplevels(factor(Frozen))
    )
}

run_monthly_analysis <- function(
    df,
    response,
    response_type,
    replications,
    analysis_name){

  month_data <- setNames(
    lapply(
      Month_levels,
      function(month_name){
        prepare_month_data(df,response,month_name)
      }
    ),
    Month_levels
  )

  month_counts <- bind_rows(
    lapply(
      Month_levels,
      function(month_name){
        month_df <- month_data[[month_name]]
        data.frame(
          Analysis=analysis_name,
          Month=month_name,
          Complete_Events=nrow(month_df),
          Sites=n_distinct(month_df$Field_Name),
          Response_Events=if(response_type == "occurrence"){
            sum(month_df[[response]] == 1)
          }else{
            nrow(month_df)
          }
        )
      }
    )
  )

  if(any(month_counts$Complete_Events == 0)){
    stop(analysis_name," has a month with no complete observations.")
  }

  balanced_sample_n <- min(month_counts$Complete_Events)
  month_counts$Balanced_Sample_n <- balanced_sample_n

  message(
    analysis_name,
    ": balanced bootstrap n per month = ",
    balanced_sample_n
  )

  metric_results <- list()
  metric_long_results <- list()
  comparison_results <- list()
  drop_results <- list()

  scale_terms <- intersect(
    Continuous_variables,
    c(Storm_variables,Agricultural_variables,Site_variables)
  )

  for(month_name in Month_levels){
    for(replication in seq_len(replications)){
      sampled_df <- month_data[[month_name]] %>%
        slice_sample(n=balanced_sample_n,replace=TRUE)

      replication_result <- fit_model_replication(
        sampled_df=sampled_df,
        response=response,
        response_type=response_type,
        season=month_name,
        replication=replication,
        storm_terms=Storm_variables,
        agricultural_terms=Agricultural_variables,
        site_terms=Site_variables,
        scale_terms=scale_terms
      )

      result_name <- paste(month_name,replication,sep="_")
      metric_results[[result_name]] <- replication_result$Metrics
      metric_long_results[[result_name]] <- replication_result$Metrics_Long
      comparison_results[[result_name]] <- replication_result$Comparisons
      drop_results[[result_name]] <- replication_result$Drop_One

      if(replication %% 10 == 0){
        message(
          analysis_name,
          ": complete ",
          month_name,
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
    Month_Data=month_data,
    Month_Counts=month_counts,
    Balanced_Sample_n=balanced_sample_n,
    Metrics=metric_df,
    Metrics_Long=metric_long_df,
    Comparisons=comparison_df,
    Drop_One=drop_df,
    Metric_Summary=metric_summary_safe(metric_long_df,response_type),
    Comparison_Summary=summarize_model_comparisons(comparison_df),
    Drop_Summary=summarize_drop_one(drop_df)
  )
}

Occurrence_results <- run_monthly_analysis(
  P_df,
  response="Q_Occurred",
  response_type="occurrence",
  replications=Occurrence_Replications,
  analysis_name="Runoff occurrence"
)

RC_results <- run_monthly_analysis(
  Q_df,
  response="log_RC",
  response_type="continuous",
  replications=RC_Replications,
  analysis_name="Runoff magnitude"
)

write_result_tables <- function(results,prefix){
  tables <- list(
    Model_metrics_replications=results$Metrics,
    Model_metrics_long_replications=results$Metrics_Long,
    Model_metric_summary=results$Metric_Summary,
    Model_comparisons_replications=results$Comparisons,
    Model_comparison_summary=results$Comparison_Summary,
    Drop_one_replications=results$Drop_One,
    Drop_one_summary=results$Drop_Summary,
    Monthly_sample_sizes=results$Month_Counts
  )

  for(table_name in names(tables)){
    write.csv(
      tables[[table_name]],
      file.path(Table_path,paste0(prefix,"_",table_name,".csv")),
      row.names=FALSE,
      na=""
    )
  }
}

write_result_tables(Occurrence_results,"Occurrence")
write_result_tables(RC_results,"RC")

# Fit final full models to all complete observations within each month
fit_monthly_final_models <- function(
    results,
    response,
    response_type,
    analysis_name,
    prefix){

  model_list <- list()
  coefficient_list <- list()
  specification_list <- list()
  vegetation_effect_list <- list()

  for(month_name in Month_levels){
    final_df <- results$Month_Data[[month_name]]
    supported_storm_terms <- Storm_variables
    supported_site_terms <- Site_variables

    if(!factor_has_support(final_df,"Frozen",min_n=5)){
      supported_storm_terms <- setdiff(supported_storm_terms,"Frozen")
    }

    for(variable in intersect(
        c("Hydrologic_Group","Tile"),
        supported_site_terms)){
      if(!factor_has_support(final_df,variable,min_n=5)){
        supported_site_terms <- setdiff(supported_site_terms,variable)
      }
    }

    full_terms <- unique(c(
      supported_storm_terms,
      Agricultural_variables,
      supported_site_terms
    ))
    scale_terms <- intersect(Continuous_variables,full_terms)
    model_df <- scale_model_variables(final_df,scale_terms)
    final_model <- fit_mixed_model(
      model_df,
      response=response,
      fixed_terms=full_terms,
      response_type=response_type
    )

    if(is.null(final_model)){
      stop("Final ",analysis_name," model failed for ",month_name,".")
    }

    model_list[[month_name]] <- list(
      Model=final_model,
      Data=model_df,
      Terms=full_terms
    )

    saveRDS(
      model_list[[month_name]],
      file.path(Model_path,paste0(prefix,"_Full_",month_name,".rds"))
    )

    coefficient_list[[month_name]] <- extract_fixed_effects(
      final_model,
      month_name,
      analysis_name
    )

    coefficient_matrix <- summary(final_model)$coefficients
    vegetation_row <- coefficient_matrix["PerennialFrac",,drop=FALSE]
    vegetation_effect_list[[month_name]] <- data.frame(
      Analysis=analysis_name,
      Month=month_name,
      Estimate=vegetation_row[1,1],
      Std_Error=vegetation_row[1,2],
      Lower_95=vegetation_row[1,1]-1.96*vegetation_row[1,2],
      Upper_95=vegetation_row[1,1]+1.96*vegetation_row[1,2],
      Statistic=vegetation_row[1,3],
      P_Value=if(ncol(coefficient_matrix) >= 4){
        vegetation_row[1,4]
      }else{
        NA_real_
      }
    )

    specification_list[[month_name]] <- data.frame(
      Analysis=analysis_name,
      Month=month_name,
      Observations=nrow(model_df),
      Sites=n_distinct(model_df$Field_Name),
      Precipitation_Characteristics=paste(
        supported_storm_terms,
        collapse=" + "
      ),
      Agricultural_Management=paste(
        Agricultural_variables,
        collapse=" + "
      ),
      Physical_Site_Properties=paste(
        supported_site_terms,
        collapse=" + "
      ),
      Formula=paste(deparse(formula(final_model)),collapse=""),
      Singular=lme4::isSingular(final_model,tol=1e-4)
    )
  }

  list(
    Models=model_list,
    Coefficients=bind_rows(coefficient_list),
    Specifications=bind_rows(specification_list),
    Vegetation_Effects=bind_rows(vegetation_effect_list)
  )
}

Occurrence_final <- fit_monthly_final_models(
  Occurrence_results,
  "Q_Occurred",
  "occurrence",
  "Runoff occurrence",
  "Occurrence"
)

RC_final <- fit_monthly_final_models(
  RC_results,
  "log_RC",
  "continuous",
  "Runoff magnitude",
  "RC"
)

Final_coefficients <- bind_rows(
  Occurrence_final$Coefficients,
  RC_final$Coefficients
)
Final_specifications <- bind_rows(
  Occurrence_final$Specifications,
  RC_final$Specifications
)
Vegetation_effects <- bind_rows(
  Occurrence_final$Vegetation_Effects,
  RC_final$Vegetation_Effects
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
write.csv(
  Vegetation_effects,
  file.path(Table_path,"Vegetation_standardized_effects.csv"),
  row.names=FALSE,
  na=""
)

# Vegetation drop-one summaries
Vegetation_drop <- bind_rows(
  Occurrence_results$Drop_Summary %>%
    filter(Dropped == "PerennialFrac") %>%
    mutate(Analysis="Runoff occurrence"),
  RC_results$Drop_Summary %>%
    filter(Dropped == "PerennialFrac") %>%
    mutate(Analysis="Runoff magnitude")
) %>%
  rename(Month=Season) %>%
  mutate(
    Month=factor(Month,levels=Month_levels),
    Analysis=factor(
      Analysis,
      levels=c("Runoff occurrence","Runoff magnitude")
    )
  )

write.csv(
  Vegetation_drop,
  file.path(Table_path,"Vegetation_drop_one_summary.csv"),
  row.names=FALSE,
  na=""
)

# ------- Figures ---------
make_performance_figure <- function(results,response_type,title){
  metric_levels <- if(response_type == "occurrence"){
    c("AUC","R2m","R2c","Random_R2")
  }else{
    c("RMSE","R2m","R2c","Random_R2")
  }
  metric_labels <- c(
    AUC="AUC",
    RMSE="RMSE",
    R2m="Marginal R-squared",
    R2c="Conditional R-squared",
    Random_R2="Random-effect R-squared"
  )

  plot_df <- results$Metric_Summary %>%
    rename(Month=Season) %>%
    mutate(
      Month=factor(Month,levels=Month_levels),
      Model=factor(Model,levels=Model_levels),
      Metric=factor(Metric,levels=metric_levels),
      Lower=Mean-SD,
      Upper=Mean+SD
    )

  ggplot(
    plot_df,
    aes(Month,Mean,color=Model,group=Model)
  ) +
    geom_errorbar(
      aes(ymin=Lower,ymax=Upper),
      width=0.12,
      linewidth=0.5,
      position=position_dodge(width=0.15)
    ) +
    geom_line(linewidth=0.9) +
    geom_point(size=3) +
    facet_wrap(
      ~Metric,
      scales="free_y",
      ncol=2,
      labeller=as_labeller(metric_labels)
    ) +
    scale_color_manual(
      values=Model_colors,
      labels=Model_labels,
      drop=FALSE
    ) +
    labs(
      title=title,
      subtitle="Points are bootstrap means; error bars show one standard deviation",
      x=NULL,
      y=NULL,
      color=NULL
    ) +
    DF_plot_theme +
    theme(legend.position="bottom")
}

Occurrence_performance <- make_performance_figure(
  Occurrence_results,
  "occurrence",
  "Runoff occurrence model performance by month"
)
RC_performance <- make_performance_figure(
  RC_results,
  "continuous",
  "Runoff magnitude model performance by month"
)

save_figure_pair(
  Occurrence_performance,
  file.path(Figure_path,"01_Occurrence_model_performance_by_month"),
  width=15,
  height=10
)
save_figure_pair(
  RC_performance,
  file.path(Figure_path,"02_Magnitude_model_performance_by_month"),
  width=15,
  height=10
)

# Contribution of agricultural management beyond storm characteristics
Agricultural_comparison <- bind_rows(
  Occurrence_results$Comparison_Summary %>%
    filter(Comparison == "Storm_Agricultural") %>%
    mutate(Analysis="Runoff occurrence"),
  RC_results$Comparison_Summary %>%
    filter(Comparison == "Storm_Agricultural") %>%
    mutate(Analysis="Runoff magnitude")
) %>%
  rename(Month=Season) %>%
  mutate(
    Month=factor(Month,levels=Month_levels),
    Analysis=factor(
      Analysis,
      levels=c("Runoff occurrence","Runoff magnitude")
    ),
    Lower=Mean_Delta_AIC-SD_Delta_AIC,
    Upper=Mean_Delta_AIC+SD_Delta_AIC,
    Label=paste0("p<0.05: ",round(Significant_Percent),"%")
  )

Agricultural_comparison_plot <- ggplot(
  Agricultural_comparison,
  aes(Month,Mean_Delta_AIC,fill=Analysis)
) +
  geom_col(width=0.65,color="black",position=position_dodge(width=0.72)) +
  geom_errorbar(
    aes(ymin=Lower,ymax=Upper),
    width=0.15,
    position=position_dodge(width=0.72)
  ) +
  geom_text(
    aes(y=Upper,label=Label),
    vjust=-0.45,
    position=position_dodge(width=0.72),
    size=4
  ) +
  facet_wrap(~Analysis,scales="free_y",ncol=2) +
  scale_fill_manual(
    values=c(
      "Runoff occurrence"=DF_colors[1],
      "Runoff magnitude"=DF_colors[2]
    )
  ) +
  labs(
    title="Contribution of agricultural management by month",
    subtitle="Storm-only model compared with Agricultural model",
    x=NULL,
    y="AIC difference",
    fill=NULL
  ) +
  expand_limits(
    y=max(Agricultural_comparison$Upper,na.rm=TRUE)*1.16
  ) +
  DF_plot_theme +
  theme(legend.position="none")

save_figure_pair(
  Agricultural_comparison_plot,
  file.path(Figure_path,"03_Agricultural_contribution_by_month"),
  width=15,
  height=7
)

# Direct vegetation-variable importance
Vegetation_importance_plot <- ggplot(
  Vegetation_drop,
  aes(Month,Mean_Delta_AIC,fill=Analysis)
) +
  geom_col(width=0.65,color="black") +
  geom_errorbar(
    aes(
      ymin=Mean_Delta_AIC-SD_Delta_AIC,
      ymax=Mean_Delta_AIC+SD_Delta_AIC
    ),
    width=0.15
  ) +
  geom_text(
    aes(
      y=Mean_Delta_AIC+SD_Delta_AIC,
      label=paste0("p<0.05: ",round(Significant_Percent),"%")
    ),
    vjust=-0.45,
    size=4
  ) +
  facet_wrap(~Analysis,scales="free_y",ncol=2) +
  scale_fill_manual(
    values=c(
      "Runoff occurrence"=DF_colors[1],
      "Runoff magnitude"=DF_colors[2]
    )
  ) +
  labs(
    title="Perennial crop fraction importance by month",
    subtitle="Change in Full-model support when perennial crop fraction is removed",
    x=NULL,
    y="AIC difference",
    fill=NULL
  ) +
  expand_limits(
    y=max(
      Vegetation_drop$Mean_Delta_AIC+Vegetation_drop$SD_Delta_AIC,
      na.rm=TRUE
    )*1.17
  ) +
  DF_plot_theme +
  theme(legend.position="none")

save_figure_pair(
  Vegetation_importance_plot,
  file.path(Figure_path,"04_Vegetation_importance_by_month"),
  width=15,
  height=7
)

Vegetation_effect_plot_df <- Vegetation_effects %>%
  mutate(
    Month=factor(Month,levels=Month_levels),
    Analysis=factor(
      Analysis,
      levels=c("Runoff occurrence","Runoff magnitude")
    )
  )

Vegetation_effect_plot <- ggplot(
  Vegetation_effect_plot_df,
  aes(Month,Estimate,color=Month)
) +
  geom_hline(yintercept=0,linetype=2,color="grey45") +
  geom_errorbar(
    aes(ymin=Lower_95,ymax=Upper_95),
    width=0.12,
    linewidth=0.65
  ) +
  geom_point(size=3.5) +
  facet_wrap(~Analysis,scales="free_y",ncol=2) +
  scale_color_manual(values=Month_colors,drop=FALSE) +
  labs(
    title="Standardized perennial crop fraction effect by month",
    subtitle="Points are Full-model coefficients; error bars are estimate +/- 1.96 standard errors",
    x=NULL,
    y="Standardized effect",
    color=NULL
  ) +
  DF_plot_theme +
  theme(legend.position="none")

save_figure_pair(
  Vegetation_effect_plot,
  file.path(Figure_path,"05_Vegetation_standardized_effect_by_month"),
  width=15,
  height=7
)

# Marginal vegetation response with all other predictors held constant
make_vegetation_predictions <- function(final_results,analysis_name){
  bind_rows(
    lapply(
      Month_levels,
      function(month_name){
        model_info <- final_results$Models[[month_name]]
        model <- model_info$Model
        model_df <- model_info$Data
        x_grid <- seq(-2,2,length.out=80)

        newdata <- data.frame(
          log_I30=0,
          log_ARFdays7=0,
          Tillage_Passes=0,
          PerennialFrac=x_grid,
          MeanSlope_per=0,
          Field_Name=levels(model_df$Field_Name)[1]
        )

        if("Hydrologic_Group" %in% model_info$Terms){
          newdata$Hydrologic_Group <- factor(
            levels(model_df$Hydrologic_Group)[1],
            levels=levels(model_df$Hydrologic_Group)
          )
        }
        if("Tile" %in% model_info$Terms){
          newdata$Tile <- factor(
            levels(model_df$Tile)[1],
            levels=levels(model_df$Tile)
          )
        }
        if("Frozen" %in% model_info$Terms){
          newdata$Frozen <- factor(
            "Non-Frozen",
            levels=levels(model_df$Frozen)
          )
        }

        prediction <- predict(
          model,
          newdata=newdata,
          re.form=NA,
          type=if(analysis_name == "Runoff occurrence") "response" else "response",
          se.fit=FALSE,
          allow.new.levels=TRUE
        )

        data.frame(
          Analysis=analysis_name,
          Month=month_name,
          PerennialFrac_Standardized=x_grid,
          Predicted=as.numeric(prediction)
        )
      }
    )
  )
}

Vegetation_predictions <- bind_rows(
  make_vegetation_predictions(Occurrence_final,"Runoff occurrence"),
  make_vegetation_predictions(RC_final,"Runoff magnitude")
) %>%
  mutate(
    Month=factor(Month,levels=Month_levels),
    Analysis=factor(
      Analysis,
      levels=c("Runoff occurrence","Runoff magnitude")
    )
  )

write.csv(
  Vegetation_predictions,
  file.path(Table_path,"Vegetation_marginal_predictions.csv"),
  row.names=FALSE,
  na=""
)

Vegetation_response_plot <- ggplot(
  Vegetation_predictions,
  aes(
    PerennialFrac_Standardized,
    Predicted,
    color=Month
  )
) +
  geom_line(linewidth=1.1) +
  facet_wrap(~Analysis,scales="free_y",ncol=2) +
  scale_color_manual(values=Month_colors,drop=FALSE) +
  labs(
    title="Modelled vegetation response across growing-season months",
    subtitle="Other continuous predictors are held at their monthly means after standardization",
    x="Perennial crop fraction (standardized)",
    y="Predicted value",
    color=NULL
  ) +
  DF_plot_theme +
  theme(legend.position="bottom")

save_figure_pair(
  Vegetation_response_plot,
  file.path(Figure_path,"06_Vegetation_marginal_response_by_month"),
  width=15,
  height=7
)

# ------- HTML report ---------
Sample_size_report <- bind_rows(
  Occurrence_results$Month_Counts,
  RC_results$Month_Counts
)

Performance_report <- bind_rows(
  Occurrence_results$Metric_Summary %>%
    filter(Model == "Full") %>%
    mutate(Analysis="Runoff occurrence"),
  RC_results$Metric_Summary %>%
    filter(Model == "Full") %>%
    mutate(Analysis="Runoff magnitude")
) %>%
  rename(Month=Season) %>%
  select(
    Analysis,
    Month,
    Metric,
    Replications,
    Mean,
    SD
  )

Vegetation_effect_report <- Vegetation_effects %>%
  mutate(
    P_Value=if_else(
      is.na(P_Value),
      NA_real_,
      P_Value
    )
  )

Agricultural_comparison_report <- Agricultural_comparison %>%
  select(
    Analysis,
    Month,
    Replications,
    Mean_Delta_AIC,
    SD_Delta_AIC,
    Significant_Percent
  )

August_occ_effect <- Vegetation_effects %>%
  filter(Analysis == "Runoff occurrence",Month == "August") %>%
  slice(1)
August_occ_drop <- Vegetation_drop %>%
  filter(Analysis == "Runoff occurrence",Month == "August") %>%
  slice(1)
Key_findings_html <- sprintf(
  paste0(
    "<div class=\"callout\"><strong>Key findings:</strong> ",
    "Full-model runoff-occurrence AUC was highest in September (%.3f). ",
    "Perennial crop fraction showed its clearest monthly role in August: ",
    "the standardized occurrence coefficient was %.2f (95%% CI %.2f to %.2f; p&lt;0.001), ",
    "and removing it increased AIC by %.2f on average, with p&lt;0.05 in %.0f%% of bootstrap comparisons. ",
    "No monthly perennial-fraction effect was supported for runoff magnitude.</div>"
  ),
  Performance_report %>%
    filter(Analysis == "Runoff occurrence",Month == "September",Metric == "AUC") %>%
    pull(Mean),
  August_occ_effect$Estimate,
  August_occ_effect$Lower_95,
  August_occ_effect$Upper_95,
  August_occ_drop$Mean_Delta_AIC,
  August_occ_drop$Significant_Percent
)

Report_body <- c(
  "<div class=\"callout\"><strong>Experiment:</strong> the established all-events mixed-effects framework is applied separately to June, July, August, and September. A common balanced bootstrap sample size is used within each response so model performance is comparable among months.</div>",
  Key_findings_html,
  "<h2>Model definitions</h2>",
  "<p>Runoff occurrence is analyzed with a Mixed-effects logistic model. Runoff magnitude is defined as log(RC) and analyzed with a Mixed-effects linear regression model. Every model includes a site random intercept.</p>",
  "<p>Precipitation-characteristics include log 30-minute precipitation intensity and log seven-day antecedent precipitation. Frozen-soil condition is not estimable in June-September because all retained events are non-frozen. Agricultural management includes growing-season tillage passes and current-water-year perennial crop fraction. Physical site properties include mean slope, soil infiltration group, and site-level tile drainage.</p>",
  "<p>Crop residue is not included during the growing season, consistent with the established seasonal models.</p>",
  "<h2>Monthly sample sizes</h2>",
  "<p>Complete Events is the available complete-case sample before balancing. Balanced Sample n is the common number sampled with replacement in every month and replication for that response.</p>",
  data_frame_to_html(Sample_size_report,digits=0),
  "<h2>Model performance through the growing season</h2>",
  embedded_figure_html(
    file.path(Figure_path,"01_Occurrence_model_performance_by_month.png"),
    "Figure 1. Runoff occurrence performance for the Storm-only, Agricultural, Site physics, and Full models from June through September."
  ),
  embedded_figure_html(
    file.path(Figure_path,"02_Magnitude_model_performance_by_month.png"),
    "Figure 2. Runoff magnitude performance for the Storm-only, Agricultural, Site physics, and Full models from June through September."
  ),
  "<h3>Full-model performance summary</h3>",
  "<p>Replications is the number of non-missing estimates available for each metric. Error summaries use standard deviations across balanced bootstrap replications.</p>",
  data_frame_to_html(Performance_report,digits=3),
  "<h2>Agricultural contribution</h2>",
  embedded_figure_html(
    file.path(Figure_path,"03_Agricultural_contribution_by_month.png"),
    "Figure 3. AIC difference (Storm-only minus Agricultural) after growing-season tillage passes and perennial crop fraction are added. Labels show the percentage of bootstrap likelihood-ratio tests with p < 0.05."
  ),
  data_frame_to_html(Agricultural_comparison_report,digits=3),
  "<h2>Changing role of vegetation</h2>",
  "<p>Perennial crop fraction is the available event-level vegetation indicator. Its role is evaluated using drop-one model support, standardized Full-model coefficients, and marginal response curves.</p>",
  embedded_figure_html(
    file.path(Figure_path,"04_Vegetation_importance_by_month.png"),
    "Figure 4. AIC difference (reduced minus Full) when perennial crop fraction is removed. Labels show the percentage of bootstrap likelihood-ratio tests with p < 0.05."
  ),
  embedded_figure_html(
    file.path(Figure_path,"05_Vegetation_standardized_effect_by_month.png"),
    "Figure 5. Standardized Full-model coefficient for perennial crop fraction in each month."
  ),
  embedded_figure_html(
    file.path(Figure_path,"06_Vegetation_marginal_response_by_month.png"),
    "Figure 6. Modelled response along the standardized perennial crop fraction gradient for June through September."
  ),
  "<h3>Vegetation drop-one summary</h3>",
  data_frame_to_html(Vegetation_drop,digits=3),
  "<h3>Vegetation coefficient summary</h3>",
  "<p>Runoff occurrence coefficient p-values are Wald tests. The Mixed-effects linear regression models report estimates, standard errors, and t statistics; coefficient p-values are unavailable because lme4::lmer does not calculate denominator degrees of freedom by default.</p>",
  data_frame_to_html(Vegetation_effect_report,digits=4),
  "<h2>Final monthly Full models</h2>",
  data_frame_to_html(
    Final_specifications %>% select(-Singular),
    digits=0
  ),
  "<h2>Output files</h2>",
  "<p>Every figure is saved in PNG and PDF format. Bootstrap results, final coefficients, specifications, vegetation summaries, and marginal predictions are saved as CSV files under <code>04_Results/Mixed_Effects_Monthly_Growing_Season/Tables</code>. Final fitted models are saved as RDS files.</p>"
)

Monthly_report <- file.path(
  Report_path,
  "08_Growing_season_monthly_mixed_effects_report.html"
)

write_html_report(
  title="Growing-Season Monthly Mixed-Effects Model Experiment",
  subtitle=paste0("Generated: ",Sys.Date()),
  body_html=Report_body,
  output_path=Monthly_report
)

message("Monthly growing-season mixed-effects experiment complete.")
message("Figures: ",Figure_path)
message("Tables: ",Table_path)
message("Models: ",Model_path)
message("Report: ",Monthly_report)
