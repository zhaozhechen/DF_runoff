# Author: Zhaozhe Chen
# Companion marginal-effect figures with predictors in original units
# Uses the saved final seasonal models; model fitting is unchanged.

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(lme4)
  library(ggeffects)
  library(RColorBrewer)
})

Project_path <- normalizePath(getwd(),winslash="/",mustWork=TRUE)
source(file.path(Project_path,"01_Functions","04_Reporting_plotting_functions.R"))

Season_levels <- c(
  "Pre-growing season","Growing season","Post-growing season"
)
Season_colors <- setNames(
  RColorBrewer::brewer.pal(7,"Set2")[c(3,1,2)],
  Season_levels
)
Variable_order <- c(
  "log_I30","log_ARFdays7","Frozen","Tillage_Passes",
  "PerennialFrac","Residue_Frac","MeanSlope_per",
  "Hydrologic_Group","Tile"
)
Continuous_variables <- c(
  "log_I30","log_Dur","log_ARFdays7","Tillage_Passes",
  "PerennialFrac","Residue_Frac","MeanSlope_per"
)
Original_labels <- c(
  log_I30="30-minute precipitation intensity\n(mm/hour; log scale)",
  log_Dur="Precipitation event duration (hours)",
  log_ARFdays7="7-day antecedent rainfall\n(mm; log scale)",
  Frozen="Frozen soil condition",
  Tillage_Passes="Seasonal tillage passes",
  PerennialFrac="Perennial crop fraction (0-1)",
  Residue_Frac="Crop-residue fraction (0-1)",
  MeanSlope_per="Mean slope (%)",
  Hydrologic_Group="Soil infiltration group",
  Tile="Site-level tile drainage"
)

read_original_model_data <- function(dataset_key,response_type){
  if(response_type == "occurrence"){
    df <- read.csv(
      file.path(Project_path,"00_Data","Processed","All_P_events.csv"),
      stringsAsFactors=FALSE,check.names=FALSE
    )
    df <- df[df$Monitoring == "Surface",,drop=FALSE]
    df$Q_Occurred <- as.integer(
      df$Associated_Q %in% c(TRUE,"TRUE","True","true",1,"1")
    )
    if(dataset_key == "Frozen"){
      df <- df[df$P_frozen %in% TRUE,,drop=FALSE]
    }else if(dataset_key == "NonFrozen"){
      df <- df[df$P_frozen %in% FALSE,,drop=FALSE]
    }else{
      df <- df[df$P_frozen %in% c(TRUE,FALSE),,drop=FALSE]
    }
    df$Frozen <- ifelse(df$P_frozen,"Frozen","Non-Frozen")
  }else{
    df <- read.csv(
      file.path(Project_path,"00_Data","Processed","All_Q_events.csv"),
      stringsAsFactors=FALSE,check.names=FALSE
    )
    keep <- df$Monitoring == "Surface" &
      df$rain_mm > 0 & df$runoff_mm > 0
    if(dataset_key == "Frozen"){
      keep <- keep & df$frozen == "Frozen"
    }else if(dataset_key == "NonFrozen"){
      keep <- keep & df$frozen == "Non-Frozen"
    }else{
      keep <- keep & df$frozen %in% c("Frozen","Non-Frozen")
    }
    df <- df[which(keep),,drop=FALSE]
    df$Runoff_Coefficient <- df$runoff_mm/df$rain_mm
    df <- df[
      is.finite(df$Runoff_Coefficient) & df$Runoff_Coefficient < 5,
      ,drop=FALSE
    ]
    df$log_RC <- log(df$Runoff_Coefficient)
    df$Frozen <- df$frozen
  }
  df$log_I30 <- log(df$I30_mm_hr)
  df$log_Dur <- log(df$duration_hr)
  df$log_ARFdays7 <- log(df$ARFdays7_mm+0.1)
  df[
    is.finite(df$log_I30) & is.finite(df$log_ARFdays7),
    ,drop=FALSE
  ]
}

original_season_data <- function(df,season,stored_df){
  required <- names(stored_df)
  raw <- df[df$Season == season,required,drop=FALSE]
  raw <- raw[stats::complete.cases(raw),,drop=FALSE]
  numeric_terms <- intersect(Continuous_variables,required)
  for(variable in numeric_terms){
    raw <- raw[is.finite(raw[[variable]]),,drop=FALSE]
  }
  if(nrow(raw) != nrow(stored_df)){
    stop("Raw and fitted model rows differ for ",season,".")
  }
  for(variable in numeric_terms){
    value_sd <- stats::sd(raw[[variable]])
    expected <- if(is.finite(value_sd) && value_sd > 0){
      as.numeric(scale(raw[[variable]]))
    }else{
      raw[[variable]]
    }
    if(!isTRUE(all.equal(
      expected,stored_df[[variable]],
      tolerance=1e-7,check.attributes=FALSE
    ))){
      stop("Original values do not reproduce saved model scaling: ",
           season,", ",variable)
    }
  }
  raw
}

back_transform_x <- function(x,variable,raw){
  value_sd <- stats::sd(raw[[variable]])
  transformed <- if(is.finite(value_sd) && value_sd > 0){
    x*value_sd+mean(raw[[variable]])
  }else{
    x
  }
  if(variable == "log_I30"){
    exp(transformed)
  }else if(variable == "log_Dur"){
    exp(transformed)
  }else if(variable == "log_ARFdays7"){
    pmax(0,exp(transformed)-0.1)
  }else{
    transformed
  }
}

original_unit_predictions <- function(model_path,raw_df,response_type){
  prediction_rows <- list()
  for(season in Season_levels){
    model_file <- file.path(model_path,paste0(
      if(response_type == "occurrence") "Occurrence" else "RC",
      "_Full_",season,".rds"
    ))
    if(!file.exists(model_file)) next
    model_result <- readRDS(model_file)
    raw <- original_season_data(raw_df,season,model_result$Data)
    variables <- if("log_Dur" %in% model_result$Terms){
      replace(Variable_order,3,"log_Dur")
    }else{
      Variable_order
    }
    for(variable in variables){
      if(!variable %in% model_result$Terms) next
      prediction <- suppressMessages(as.data.frame(
        ggeffects::ggpredict(model_result$Model,terms=variable)
      ))
      numeric_predictor <- variable %in% Continuous_variables
      original_x <- if(numeric_predictor){
        back_transform_x(as.numeric(prediction$x),variable,raw)
      }else{
        as.character(prediction$x)
      }
      prediction_rows[[length(prediction_rows)+1]] <- data.frame(
        Response=response_type,
        Season=season,
        Variable=variable,
        Standardized_x=if(numeric_predictor){
          as.numeric(prediction$x)
        }else{
          NA_real_
        },
        Original_x=as.character(original_x),
        Predicted=prediction$predicted,
        CI_low=prediction$conf.low,
        CI_high=prediction$conf.high,
        stringsAsFactors=FALSE
      )
    }
  }
  dplyr::bind_rows(prediction_rows)
}

plot_original_unit_panel <- function(
    predictions,variable,response_type,x_scale="log"){
  plot_df <- predictions[predictions$Variable == variable,,drop=FALSE]
  if(nrow(plot_df) == 0) return(ggplot()+theme_void())
  plot_df$Season <- factor(plot_df$Season,levels=Season_levels)
  if(variable %in% Continuous_variables){
    plot_df$x <- as.numeric(plot_df$Original_x)
    p <- ggplot(
      plot_df,
      aes(x=x,y=Predicted,color=Season,fill=Season)
    ) +
      geom_ribbon(
        aes(ymin=CI_low,ymax=CI_high,group=Season),
        alpha=0.22,color=NA
      ) +
      geom_line(aes(group=Season),linewidth=1)
    if(x_scale == "log" && variable == "log_I30"){
      p <- p+scale_x_log10(
        breaks=c(0.3,1,3,10,30,100,300)
      )
    }else if(x_scale == "log" && variable == "log_ARFdays7"){
      # The model uses log(rainfall + 0.1); the offset retains zero rainfall.
      p <- p+scale_x_continuous(
        trans=scales::trans_new(
          "log10_rainfall_plus_0.1",
          transform=function(x) log10(x+0.1),
          inverse=function(x) 10^x-0.1,
          domain=c(0,Inf)
        ),
        breaks=c(0,1,10,100,400)
      )
    }
  }else{
    plot_df$x <- factor(plot_df$Original_x,levels=unique(plot_df$Original_x))
    dodge <- position_dodge(width=0.45)
    p <- ggplot(
      plot_df,
      aes(x=x,y=Predicted,color=Season,fill=Season,group=Season)
    ) +
      geom_errorbar(aes(ymin=CI_low,ymax=CI_high),
                    width=0.12,position=dodge) +
      geom_point(shape=21,size=3.5,color="black",position=dodge)
  }
  p+
    scale_color_manual(values=Season_colors,drop=FALSE)+
    scale_fill_manual(values=Season_colors,drop=FALSE,guide="none")+
    labs(
      x=if(x_scale == "linear"){
        sub("; log scale","",Original_labels[[variable]],fixed=TRUE)
      }else{
        Original_labels[[variable]]
      },
      y=if(response_type == "occurrence"){
        "Runoff probability"
      }else{
        "Runoff magnitude: log(RC)"
      },
      color="Season",fill="Season"
    )+
    DF_plot_theme+
    theme(
      legend.position="none",
      axis.text.x=element_text(
        angle=if(variable %in% Continuous_variables) 0 else 25,
        hjust=if(variable %in% Continuous_variables) 0.5 else 1
      )
    )
}

plot_original_unit_figure <- function(
    predictions,response_type,x_scale="log"){
  variables <- if("log_Dur" %in% predictions$Variable){
    replace(Variable_order,3,"log_Dur")
  }else{
    Variable_order
  }
  panels <- lapply(
    variables,
    function(variable){
      plot_original_unit_panel(
        predictions,variable,response_type,x_scale=x_scale
      )
    }
  )
  legend_data <- data.frame(
    Season=factor(Season_levels,levels=Season_levels),
    x=c(0.6,1.8,3)
  )
  legend_panel <- ggplot(legend_data)+
    geom_segment(
      aes(x=x,xend=x+0.16,y=0,yend=0,color=Season),
      linewidth=1.4
    )+
    geom_text(
      aes(x=x+0.2,y=0,label=Season),
      hjust=0,size=5
    )+
    scale_color_manual(values=Season_colors,guide="none")+
    coord_cartesian(xlim=c(0.45,4.1),ylim=c(-0.1,0.1),clip="off")+
    theme_void()
  patchwork::wrap_plots(panels,ncol=3)/legend_panel+
    patchwork::plot_layout(heights=c(1,0.055))
}

i30_probability_thresholds <- function(predictions){
  rows <- lapply(Season_levels,function(season){
    curve <- predictions[
      predictions$Response == "occurrence" &
        predictions$Variable == "log_I30" &
        predictions$Season == season,
      ,drop=FALSE
    ]
    if(nrow(curve) < 2){
      stop("Insufficient I30 predictions for ",season,".")
    }
    intensity <- as.numeric(curve$Original_x)
    order_x <- order(intensity)
    intensity <- intensity[order_x]
    probability <- curve$Predicted[order_x]
    if(!all(is.finite(intensity)) ||
       !all(is.finite(probability)) ||
       !(all(diff(probability) > 0) ||
         all(diff(probability) < 0))){
      stop("I30 prediction curve is not strictly monotonic for ",season,".")
    }
    crossings <- stats::approx(
      x=probability,y=intensity,xout=c(0.5,0.7),rule=1
    )$y
    data.frame(
      Season=season,
      I30_at_P0_50_mm_hr=round(crossings[1],2),
      I30_at_P0_70_mm_hr=round(crossings[2],2),
      stringsAsFactors=FALSE
    )
  })
  dplyr::bind_rows(rows)
}

arf_runoff_probabilities <- function(predictions){
  rows <- lapply(Season_levels,function(season){
    curve <- predictions[
      predictions$Response == "occurrence" &
        predictions$Variable == "log_ARFdays7" &
        predictions$Season == season,
      ,drop=FALSE
    ]
    if(nrow(curve) < 2){
      stop("Insufficient antecedent-rainfall predictions for ",season,".")
    }
    rainfall <- as.numeric(curve$Original_x)
    order_x <- order(rainfall)
    rainfall <- rainfall[order_x]
    probability <- curve$Predicted[order_x]
    if(!all(is.finite(rainfall)) ||
       !all(is.finite(probability)) ||
       any(diff(rainfall) <= 0)){
      stop("Invalid antecedent-rainfall curve for ",season,".")
    }
    values <- stats::approx(
      x=rainfall,y=probability,xout=c(0,50,100),rule=1
    )$y
    data.frame(
      Season=season,
      Runoff_probability_at_ARF_0_mm=round(values[1],5),
      Runoff_probability_at_ARF_50_mm=round(values[2],5),
      Runoff_probability_at_ARF_100_mm=round(values[3],5),
      stringsAsFactors=FALSE
    )
  })
  dplyr::bind_rows(rows)
}

insert_original_unit_figures <- function(
    report_file,figure_path,i30_thresholds,arf_probabilities){
  report <- paste(readLines(report_file,warn=FALSE,encoding="UTF-8"),
                  collapse="\n")
  figures <- list(
    list(
      number="4",
      stem="04B_Occurrence_marginal_effects_original_units",
      caption="Figure 4B. The Figure 4 runoff-occurrence marginal effects with continuous predictors shown in their original units. Model predictions and confidence intervals use the same final seasonal models.",
      description=paste0(
        "<p>The two log-transformed model inputs are shown as physical ",
        "30-minute precipitation intensity (mm/hour) and 7-day antecedent ",
        "rainfall (mm), both on logarithmic x axes. The rainfall axis uses ",
        "log(rainfall + 0.1), matching the model and retaining zero rainfall. ",
        "Where applicable, event duration is shown in hours. ",
        "The other continuous inputs use their recorded units. Conversion ",
        "uses the mean and standard deviation from each season's complete ",
        "model data.</p>"
      )
    ),
    list(
      number="4B",
      stem="04C_Occurrence_marginal_effects_linear_original_units",
      caption="Figure 4C. The Figure 4 runoff-occurrence marginal effects with 30-minute precipitation intensity and 7-day antecedent rainfall on linear axes in their original units. Predictions and confidence intervals are identical to Figure 4B.",
      description=paste0(
        "<p>Figure 4C retains the physical units of Figure 4B, but shows ",
        "intensity (mm/hour) and antecedent rainfall (mm) on linear x axes. ",
        "The model still uses the same log-transformed predictors; only ",
        "the display scale differs.</p>"
      )
    ),
    list(
      number="8",
      stem="08B_RC_marginal_effects_original_units",
      caption="Figure 8B. The Figure 8 runoff-magnitude marginal effects with continuous predictors shown in their original units. Model predictions and confidence intervals use the same final seasonal models.",
      description=paste0(
        "<p>The two log-transformed model inputs are shown as physical ",
        "30-minute precipitation intensity (mm/hour) and 7-day antecedent ",
        "rainfall (mm), both on logarithmic x axes. The rainfall axis uses ",
        "log(rainfall + 0.1), matching the model and retaining zero rainfall. ",
        "Where applicable, event duration is shown in hours. ",
        "The other continuous inputs use their recorded units. Conversion ",
        "uses the mean and standard deviation from each season's complete ",
        "model data.</p>"
      )
    )
  )
  for(figure in figures){
    begin <- paste0("<!-- ",figure$stem," START -->")
    end <- paste0("<!-- ",figure$stem," END -->")
    if(grepl(begin,report,fixed=TRUE)){
      pattern <- paste0(begin,"[\\s\\S]*?",end)
      report <- sub(pattern,"",report,perl=TRUE)
    }
    caption_html <- embedded_figure_html(
      file.path(figure_path,paste0(figure$stem,".png")),
      figure$caption
    )
    threshold_html <- if(figure$stem ==
                         "04C_Occurrence_marginal_effects_linear_original_units"){
      display <- i30_thresholds
      names(display) <- c(
        "Season",
        "I30 at runoff probability 0.50 (mm/hour)",
        "I30 at runoff probability 0.70 (mm/hour)"
      )
      paste0(
        "<h3>30-minute intensity at runoff-probability thresholds</h3>",
        "<p>Values are linearly interpolated between adjacent prediction ",
        "points in Figure 4C. NA means the curve did not reach that ",
        "probability within the plotted I30 range; no extrapolation ",
        "was used.</p>",
        data_frame_to_html(display,digits=2),
        "<h3>Runoff probability at 7-day antecedent rainfall levels</h3>",
        "<p>Probabilities are read from the seasonal curves in Figure 4C ",
        "at 0, 50, and 100 mm of 7-day antecedent rainfall. Values between ",
        "plotted points use linear interpolation on the original rainfall ",
        "axis; no extrapolation was used.</p>",
        data_frame_to_html(
          setNames(
            arf_probabilities,
            c("Season","Runoff probability at 0 mm",
              "Runoff probability at 50 mm",
              "Runoff probability at 100 mm")
          ),
          digits=5
        )
      )
    }else{
      ""
    }
    anchor_match <- regexpr(
      paste0("<figcaption>Figure ",figure$number,
             "\\.[^<]+</figcaption></figure>"),
      report,perl=TRUE
    )
    if(anchor_match[1] < 0){
      stop("Could not find the report insertion point for ",
           figure$stem,".")
    }
    anchor <- regmatches(report,anchor_match)
    report <- sub(
      anchor,
      paste0(
        anchor,"\n",begin,
        figure$description,
        caption_html,threshold_html,end
      ),
      report,fixed=TRUE
    )
  }
  writeLines(report,report_file,useBytes=TRUE)
}

run_figure_4c_tables <- function(dataset_keys=c("All","NonFrozen")){
  for(dataset_key in dataset_keys){
    result_path <- file.path(
      Project_path,"04_Results","Mixed_Effects",dataset_key
    )
    table_path <- file.path(result_path,"Tables")
    prediction_file <- file.path(
      table_path,"Marginal_effects_original_units_predictions.csv"
    )
    report_file <- file.path(
      Project_path,"03_Reports",
      paste0("04_Mixed_effects_model_report_",dataset_key,".html")
    )
    predictions <- read.csv(prediction_file)
    thresholds <- i30_probability_thresholds(predictions)
    arf_probabilities <- arf_runoff_probabilities(predictions)
    write.csv(
      thresholds,
      file.path(table_path,"I30_runoff_probability_thresholds.csv"),
      row.names=FALSE,na="NA"
    )
    write.csv(
      arf_probabilities,
      file.path(table_path,"ARF7_runoff_probability_at_0_50_100_mm.csv"),
      row.names=FALSE,na="NA"
    )
    insert_original_unit_figures(
      report_file,file.path(result_path,"Figures"),
      thresholds,arf_probabilities
    )
    message("Figure 4C tables complete: ",dataset_key)
  }
  invisible(TRUE)
}

run_i30_threshold_report <- run_figure_4c_tables

run_original_unit_marginal_effects <- function(
    dataset_keys=c("All","NonFrozen")){
  for(dataset_key in dataset_keys){
    result_path <- file.path(
      Project_path,"04_Results","Mixed_Effects",dataset_key
    )
    figure_path <- file.path(result_path,"Figures")
    table_path <- file.path(result_path,"Tables")
    model_path <- file.path(result_path,"Models")
    report_file <- file.path(
      Project_path,"03_Reports",
      paste0("04_Mixed_effects_model_report_",dataset_key,".html")
    )
    if(!file.exists(report_file)){
      message("Skipping ",dataset_key,": no report found.")
      next
    }
    occurrence_predictions <- original_unit_predictions(
      model_path,
      read_original_model_data(dataset_key,"occurrence"),
      "occurrence"
    )
    rc_predictions <- original_unit_predictions(
      model_path,
      read_original_model_data(dataset_key,"continuous"),
      "continuous"
    )
    predictions <- dplyr::bind_rows(
      occurrence_predictions,rc_predictions
    )
    write.csv(
      predictions,
      file.path(table_path,"Marginal_effects_original_units_predictions.csv"),
      row.names=FALSE,na=""
    )
    save_figure_pair(
      plot_original_unit_figure(occurrence_predictions,"occurrence"),
      file.path(
        figure_path,"04B_Occurrence_marginal_effects_original_units"
      ),
      width=18,height=15
    )
    save_figure_pair(
      plot_original_unit_figure(
        occurrence_predictions,"occurrence",x_scale="linear"
      ),
      file.path(
        figure_path,"04C_Occurrence_marginal_effects_linear_original_units"
      ),
      width=18,height=15
    )
    save_figure_pair(
      plot_original_unit_figure(rc_predictions,"continuous"),
      file.path(figure_path,"08B_RC_marginal_effects_original_units"),
      width=18,height=15
    )
    run_figure_4c_tables(dataset_key)
    message("Original-unit marginal effects complete: ",dataset_key)
  }
  invisible(TRUE)
}

if(!isTRUE(getOption("df_runoff.skip_original_unit_marginal_main"))){
  run_original_unit_marginal_effects()
}
