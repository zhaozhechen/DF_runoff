# Author: Zhaozhe Chen
# Update Date: 2026.8.24

# This code conducts exploratory analysis of measured sediment and phosphorus
# observations from Discovery Farms surface-runoff monitoring sites

# Concentrations are retained in mg/L
# Loads are retained in pounds
# Yields are retained in pounds per acre
# Hydrologic depths and rainfall intensities are in millimetres

# -------- Global -----------
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(ggplot2)
  library(patchwork)
  library(RColorBrewer)
  library(gghalves)
  library(scales)
})

# Confirm that the script is run from the project root
Project_path <- normalizePath(getwd(),winslash="/",mustWork=TRUE)

if(!dir.exists(file.path(Project_path,"00_Data","Processed"))){
  stop(
    "Run this script from the DF_runoff_v2 project root. ",
    "The folder 00_Data/Processed was not found."
  )
}

# Source functions ======
source(file.path(Project_path,"01_Functions","04_Reporting_plotting_functions.R"))

# Data paths ======
Processed_path <- file.path(Project_path,"00_Data","Processed")
Input_path <- file.path(
  Processed_path,
  "Sediment_phosphorus_analysis.csv"
)
Result_path <- file.path(
  Project_path,
  "04_Results",
  "Sediment_Phosphorus_Exploratory"
)
Figure_path <- file.path(Result_path,"Figures")
Table_path <- file.path(Result_path,"Tables")
Report_path <- file.path(Project_path,"03_Reports")

dir.create(Figure_path,recursive=TRUE,showWarnings=FALSE)
dir.create(Table_path,recursive=TRUE,showWarnings=FALSE)
dir.create(Report_path,recursive=TRUE,showWarnings=FALSE)

if(!file.exists(Input_path)){
  stop(
    "Sediment_phosphorus_analysis.csv was not found. ",
    "Run 06_Sediment_phosphorus_data_processing.R first."
  )
}

# Reproducible jitter
set.seed(1)

# Global figure settings ======
Season_levels <- c(
  "Pre-growing season",
  "Growing season",
  "Post-growing season"
)

Season_colors <- setNames(
  RColorBrewer::brewer.pal(7,"Set2")[c(3,1,2)],
  Season_levels
)

Constituent_levels <- c(
  "Suspended sediment",
  "Total dissolved solids",
  "Orthophosphate",
  "Total phosphorus"
)

Constituent_colors <- setNames(
  DF_colors[c(1,6,2,3)],
  Constituent_levels
)

Metric_levels <- c("Concentration","Load","Yield")

Metric_units <- c(
  Concentration="mg/L",
  Load="lb",
  Yield="lb/acre"
)

safe_stat <- function(x,fun){
  x <- x[is.finite(x)]
  if(length(x) == 0){
    NA_real_
  }else{
    fun(x)
  }
}

sd_or_zero <- function(x){
  x <- x[is.finite(x)]
  if(length(x) > 1){
    stats::sd(x)
  }else{
    0
  }
}

format_p_value <- function(x){
  ifelse(
    is.na(x),
    "NA",
    ifelse(x < 0.001,"<0.001",format(round(x,3),nsmall=3))
  )
}

# Compare site-level median measurements among categorical groups
pairwise_measure_tests <- function(
    df,
    metric_name,
    group_var,
    grouping_label,
    group_levels,
    paired_comparison=FALSE){

  site_group_values <- df %>%
    filter(
      Metric == metric_name,
      is.finite(Value),
      Value > 0,
      !is.na(Field_Name),
      !is.na(.data[[group_var]]),
      as.character(.data[[group_var]]) != ""
    ) %>%
    transmute(
      Field_Name=as.character(Field_Name),
      Constituent=as.character(Constituent),
      Group=as.character(.data[[group_var]]),
      Value
    ) %>%
    filter(Group %in% group_levels) %>%
    group_by(Field_Name,Constituent,Group) %>%
    summarise(Site_Median=median(Value),.groups="drop")

  test_rows <- lapply(
    Constituent_levels,
    function(constituent_name){
      constituent_df <- site_group_values %>%
        filter(Constituent == constituent_name)

      observed_levels <- group_levels[
        group_levels %in% constituent_df$Group
      ]

      if(length(observed_levels) < 2){
        return(data.frame())
      }

      comparison_pairs <- combn(observed_levels,2,simplify=FALSE)

      bind_rows(
        lapply(
          comparison_pairs,
          function(comparison_pair){
            group_1 <- comparison_pair[1]
            group_2 <- comparison_pair[2]

            if(paired_comparison){
              paired_values <- constituent_df %>%
                filter(Group %in% comparison_pair) %>%
                pivot_wider(names_from=Group,values_from=Site_Median) %>%
                filter(
                  !is.na(.data[[group_1]]),
                  !is.na(.data[[group_2]])
                )

              sample_1 <- paired_values[[group_1]]
              sample_2 <- paired_values[[group_2]]
              sample_size <- nrow(paired_values)
              test_name <- "Paired Wilcoxon signed-rank test"
            }else{
              sample_1 <- constituent_df$Site_Median[
                constituent_df$Group == group_1
              ]
              sample_2 <- constituent_df$Site_Median[
                constituent_df$Group == group_2
              ]
              sample_size <- min(length(sample_1),length(sample_2))
              test_name <- "Wilcoxon rank-sum test"
            }

            p_value <- if(
              length(sample_1) >= 3 &&
              length(sample_2) >= 3
            ){
              tryCatch(
                suppressWarnings(
                  wilcox.test(
                    sample_1,
                    sample_2,
                    paired=paired_comparison,
                    exact=FALSE
                  )$p.value
                ),
                error=function(e) NA_real_
              )
            }else{
              NA_real_
            }

            data.frame(
              Metric=metric_name,
              Constituent=constituent_name,
              Grouping=grouping_label,
              Group_1=group_1,
              Group_2=group_2,
              X_1=match(group_1,group_levels),
              X_2=match(group_2,group_levels),
              Test=test_name,
              Sites=sample_size,
              P_Value=p_value
            )
          }
        )
      )
    }
  )

  bind_rows(test_rows) %>%
    group_by(Metric,Constituent,Grouping) %>%
    mutate(
      P_Adjusted=p.adjust(P_Value,method="BH"),
      Significance=case_when(
        P_Adjusted < 0.001 ~ "***",
        P_Adjusted < 0.01 ~ "**",
        P_Adjusted < 0.05 ~ "*",
        TRUE ~ ""
      )
    ) %>%
    ungroup()
}

# Add significant pairwise comparisons above log-scaled boxplots
add_measurement_brackets <- function(
    plot,
    plot_df,
    test_df){

  significant_tests <- test_df %>%
    filter(!is.na(P_Adjusted),P_Adjusted < 0.05)

  if(nrow(significant_tests) == 0){
    return(plot)
  }

  panel_maximum <- plot_df %>%
    group_by(Constituent,Grouping) %>%
    summarise(Panel_Max=max(Value,na.rm=TRUE),.groups="drop")

  bracket_df <- significant_tests %>%
    left_join(panel_maximum,by=c("Constituent","Grouping")) %>%
    mutate(
      Constituent=factor(
        as.character(Constituent),
        levels=Constituent_levels
      )
    ) %>%
    group_by(Constituent,Grouping) %>%
    arrange(P_Adjusted,.by_group=TRUE) %>%
    mutate(
      Bracket_Order=row_number(),
      Y=Panel_Max*1.28^Bracket_Order,
      Tip=Y/1.08,
      Label_Y=Y*1.05
    ) %>%
    ungroup()

  plot +
    geom_segment(
      data=bracket_df,
      aes(x=X_1,xend=X_2,y=Y,yend=Y),
      inherit.aes=FALSE,
      linewidth=0.5
    ) +
    geom_segment(
      data=bracket_df,
      aes(x=X_1,xend=X_1,y=Y,yend=Tip),
      inherit.aes=FALSE,
      linewidth=0.5
    ) +
    geom_segment(
      data=bracket_df,
      aes(x=X_2,xend=X_2,y=Y,yend=Tip),
      inherit.aes=FALSE,
      linewidth=0.5
    ) +
    geom_text(
      data=bracket_df,
      aes(x=(X_1+X_2)/2,y=Label_Y,label=Significance),
      inherit.aes=FALSE,
      size=5
    )
}

# ------- Data import and organization ---------
WQ_df <- read.csv(
  Input_path,
  stringsAsFactors=FALSE,
  check.names=FALSE,
  na.strings=c("","NA")
) %>%
  mutate(
    Q_start=as.POSIXct(Q_start,tz="America/Chicago"),
    Q_end=as.POSIXct(Q_end,tz="America/Chicago"),
    Event_Date=as.Date(Q_start,tz="America/Chicago"),
    Calendar_Year=year(Event_Date),
    Month_Number=month(Event_Date),
    Month=factor(month.abb[Month_Number],levels=month.abb),
    Season=factor(Season,levels=Season_levels),
    frozen=factor(frozen,levels=c("Non-Frozen","Frozen")),
    Hydrologic_Group=factor(
      Hydrologic_Group,
      levels=c(
        "Slow-infiltration",
        "Moderate-infiltration",
        "High-infiltration"
      )
    ),
    Tile=factor(Tile,levels=c("No","Yes"))
  )

if(
  any(WQ_df$estimated != 0,na.rm=TRUE) ||
  any(WQ_df$estimated_flow_fraction != 0,na.rm=TRUE)
){
  stop("The input contains estimated water-quality or runoff observations.")
}

Measurement_lookup <- data.frame(
  Column=c(
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
  ),
  Constituent=rep(Constituent_levels,each=3),
  Metric=rep(Metric_levels,times=4),
  stringsAsFactors=FALSE
)

missing_measurements <- setdiff(Measurement_lookup$Column,names(WQ_df))

if(length(missing_measurements) > 0){
  stop(
    "The processed dataset is missing required measurements: ",
    paste(missing_measurements,collapse=", ")
  )
}

Measurement_long <- WQ_df %>%
  select(
    WQ_event_id,
    Field_Name,
    Event_Date,
    Calendar_Year,
    Month_Number,
    Month,
    Season,
    frozen,
    Hydrologic_Group,
    Tile,
    Residue,
    PerennialFrac,
    Tillage_Passes,
    MeanSlope_per,
    runoff_mm,
    rain_mm,
    I30_mm_hr,
    ARFdays7_mm,
    all_of(Measurement_lookup$Column)
  ) %>%
  pivot_longer(
    cols=all_of(Measurement_lookup$Column),
    names_to="Column",
    values_to="Value"
  ) %>%
  left_join(Measurement_lookup,by="Column") %>%
  mutate(
    Constituent=factor(Constituent,levels=Constituent_levels),
    Metric=factor(Metric,levels=Metric_levels)
  )

# ------- Main ---------
# Step 1. Data and measurement summaries ====================
Data_summary <- data.frame(
  Description=c(
    "Measured storm-associated water-quality events",
    "Surface monitoring sites",
    "Calendar years represented",
    "Water years represented",
    "Non-frozen events",
    "Frozen events"
  ),
  Value=c(
    nrow(WQ_df),
    n_distinct(WQ_df$Field_Name),
    n_distinct(WQ_df$Calendar_Year),
    n_distinct(WQ_df$Water_Year),
    sum(WQ_df$frozen == "Non-Frozen",na.rm=TRUE),
    sum(WQ_df$frozen == "Frozen",na.rm=TRUE)
  )
)

Constituent_availability <- Measurement_long %>%
  group_by(Constituent,Metric) %>%
  summarise(
    Available_Events=sum(is.finite(Value)),
    Total_Events=n_distinct(WQ_event_id),
    Availability_Percent=100*Available_Events/Total_Events,
    Sites_with_Data=n_distinct(Field_Name[is.finite(Value)]),
    .groups="drop"
  )

Constituent_summary <- Measurement_long %>%
  group_by(Constituent,Metric) %>%
  summarise(
    Unit=Metric_units[as.character(first(Metric))],
    Events=sum(is.finite(Value)),
    Mean=safe_stat(Value,mean),
    Standard_Deviation=sd_or_zero(Value),
    Median=safe_stat(Value,median),
    Minimum=safe_stat(Value,min),
    Maximum=safe_stat(Value,max),
    .groups="drop"
  )

Site_constituent_summary <- Measurement_long %>%
  group_by(Field_Name,Hydrologic_Group,Constituent,Metric) %>%
  summarise(
    Events=sum(is.finite(Value)),
    Mean=safe_stat(Value,mean),
    Standard_Deviation=sd_or_zero(Value),
    Median=safe_stat(Value,median),
    .groups="drop"
  )

Season_constituent_summary <- Measurement_long %>%
  group_by(Season,Constituent,Metric) %>%
  summarise(
    Events=sum(is.finite(Value)),
    Mean=safe_stat(Value,mean),
    Standard_Deviation=sd_or_zero(Value),
    Median=safe_stat(Value,median),
    .groups="drop"
  )

# Pairwise tests use site-level medians to avoid treating repeated events
# at the same site as independent replicates
Residue_levels <- c("No","Yes","Partial")
Frozen_levels <- c("Non-Frozen","Frozen")

Season_tests <- bind_rows(
  lapply(
    Metric_levels,
    function(metric_name){
      pairwise_measure_tests(
        Measurement_long,
        metric_name,
        "Season",
        "Season",
        Season_levels,
        paired_comparison=TRUE
      )
    }
  )
)

Categorical_group_tests <- bind_rows(
  lapply(
    c("Concentration","Load"),
    function(metric_name){
      bind_rows(
        pairwise_measure_tests(
          Measurement_long,
          metric_name,
          "Season",
          "Season",
          Season_levels,
          paired_comparison=TRUE
        ),
        pairwise_measure_tests(
          Measurement_long,
          metric_name,
          "frozen",
          "Frozen soil condition",
          Frozen_levels,
          paired_comparison=TRUE
        ),
        pairwise_measure_tests(
          Measurement_long,
          metric_name,
          "Hydrologic_Group",
          "Soil infiltration group",
          names(DF_infiltration_colors),
          paired_comparison=FALSE
        ),
        pairwise_measure_tests(
          Measurement_long,
          metric_name,
          "Tile",
          "Site-level tile drainage",
          names(DF_tile_colors),
          paired_comparison=FALSE
        ),
        pairwise_measure_tests(
          Measurement_long %>%
            filter(Season %in% c(
              "Pre-growing season",
              "Post-growing season"
            )),
          metric_name,
          "Residue",
          "Crop residue",
          Residue_levels,
          paired_comparison=TRUE
        )
      )
    }
  )
)

# Step 2. Measurement availability by site ==================
Site_order <- WQ_df %>%
  distinct(Field_Name,Hydrologic_Group) %>%
  arrange(Hydrologic_Group,Field_Name) %>%
  pull(Field_Name)

Site_availability <- Measurement_long %>%
  filter(Metric == "Concentration") %>%
  group_by(Field_Name,Hydrologic_Group,Constituent) %>%
  summarise(Available_Events=sum(is.finite(Value)),.groups="drop") %>%
  complete(
    Field_Name=Site_order,
    Constituent=factor(Constituent_levels,levels=Constituent_levels),
    fill=list(Available_Events=0)
  ) %>%
  left_join(
    WQ_df %>% distinct(Field_Name,Hydrologic_Group),
    by="Field_Name",
    suffix=c("",".site")
  ) %>%
  mutate(
    Hydrologic_Group=coalesce(Hydrologic_Group,Hydrologic_Group.site),
    Field_Name=factor(Field_Name,levels=rev(Site_order)),
    Constituent=factor(Constituent,levels=Constituent_levels)
  ) %>%
  select(-Hydrologic_Group.site)

Figure_availability <- ggplot(
  Site_availability,
  aes(Constituent,Field_Name,fill=Available_Events)
) +
  geom_tile(color="black",linewidth=0.35) +
  geom_text(aes(label=Available_Events),size=3.5) +
  scale_fill_distiller(
    palette="YlGnBu",
    direction=1,
    guide=guide_colorbar(
      frame.colour="black",
      frame.linewidth=0.4,
      ticks.colour="black",
      title.position="top",
      barwidth=grid::unit(7,"cm")
    )
  ) +
  labs(
    x=NULL,
    y="Site ID",
    fill="Events with data",
    title="Measured water-quality observations by site"
  ) +
  DF_plot_theme +
  theme(
    axis.text.x=element_text(angle=25,hjust=1),
    legend.position="bottom"
  )

save_figure_pair(
  Figure_availability,
  file.path(Figure_path,"01_Measurement_availability_by_site"),
  width=12,
  height=11
)

# Step 3. Event distributions by season =====================
make_metric_distribution <- function(metric_name,figure_number){
  plot_df <- Measurement_long %>%
    filter(
      Metric == metric_name,
      is.finite(Value),
      Value > 0,
      !is.na(Season)
    ) %>%
    mutate(Grouping="Season")

  metric_plot <- ggplot(
    plot_df,
    aes(Season,Value,fill=Season,color=Season)
  ) +
    geom_half_violin(
      side="l",
      alpha=0.5,
      trim=TRUE,
      color=NA
    ) +
    geom_boxplot(
      color="black",
      width=0.14,
      outlier.shape=NA,
      alpha=0.8
    ) +
    geom_jitter(
      aes(x=as.numeric(Season)+0.2),
      width=0.10,
      height=0,
      size=1.4,
      alpha=0.45
    ) +
    facet_wrap(~Constituent,scales="free_y",ncol=2) +
    scale_y_log10(labels=label_number()) +
    scale_fill_manual(values=Season_colors,drop=FALSE) +
    scale_color_manual(values=Season_colors,drop=FALSE) +
    guides(color="none",fill=guide_legend(nrow=1)) +
    labs(
      x=NULL,
      y=paste0(metric_name," (",Metric_units[[metric_name]],", log scale)"),
      fill=NULL,
      title=paste(metric_name,"distributions by season")
    ) +
    DF_plot_theme +
    theme(
      axis.text.x=element_text(angle=20,hjust=1),
      legend.position="bottom"
    )

  metric_plot <- add_measurement_brackets(
    metric_plot,
    plot_df,
    Season_tests %>% filter(Metric == metric_name)
  )

  save_figure_pair(
    metric_plot,
    file.path(
      Figure_path,
      paste0(sprintf("%02d",figure_number),"_",metric_name,"_by_season")
    ),
    width=14,
    height=10
  )

  metric_plot
}

Figure_concentration <- make_metric_distribution("Concentration",2)
Figure_load <- make_metric_distribution("Load",3)
Figure_yield <- make_metric_distribution("Yield",4)

# Step 4. Monthly concentration patterns ====================
# Event concentrations are first averaged within site-months
# The plotted uncertainty is one standard deviation across site-months
Monthly_concentration <- Measurement_long %>%
  filter(Metric == "Concentration",is.finite(Value)) %>%
  group_by(
    Field_Name,
    Calendar_Year,
    Month_Number,
    Month,
    Constituent
  ) %>%
  summarise(Site_Month_Mean=mean(Value),.groups="drop") %>%
  group_by(Month_Number,Month,Constituent) %>%
  summarise(
    Mean_Concentration=mean(Site_Month_Mean),
    SD_Concentration=sd_or_zero(Site_Month_Mean),
    Site_Months=n(),
    .groups="drop"
  ) %>%
  mutate(
    Lower=pmax(0,Mean_Concentration-SD_Concentration),
    Upper=Mean_Concentration+SD_Concentration
  )

Figure_monthly <- ggplot(
  Monthly_concentration,
  aes(Month,Mean_Concentration,group=Constituent,color=Constituent)
) +
  geom_errorbar(
    aes(ymin=Lower,ymax=Upper),
    width=0.18,
    color="black",
    linewidth=0.5
  ) +
  geom_line(linewidth=0.9) +
  geom_point(
    aes(fill=Constituent),
    shape=21,
    color="black",
    stroke=0.45,
    size=3.2
  ) +
  facet_wrap(~Constituent,scales="free_y",ncol=2) +
  scale_color_manual(values=Constituent_colors) +
  scale_fill_manual(values=Constituent_colors) +
  guides(color="none",fill="none") +
  labs(
    x=NULL,
    y="Mean concentration (mg/L)",
    title="Monthly concentration patterns",
    subtitle="Error bars show one standard deviation across site-months"
  ) +
  DF_plot_theme +
  theme(axis.text.x=element_text(angle=45,hjust=1))

save_figure_pair(
  Figure_monthly,
  file.path(Figure_path,"05_Monthly_concentration_patterns"),
  width=14,
  height=10
)

# Step 4B. Monthly target variables by frozen-soil condition =
# Concentrations are averaged within each observed site-month
# Loads and yields are totaled within each observed site-month
# Months without a qualifying measured observation are not assigned zero
Frozen_monthly_site <- Measurement_long %>%
  filter(
    is.finite(Value),
    !is.na(frozen),
    !is.na(Month)
  ) %>%
  group_by(
    Field_Name,
    Calendar_Year,
    Month_Number,
    Month,
    frozen,
    Constituent,
    Metric
  ) %>%
  summarise(
    Site_Month_Value=if_else(
      first(Metric) == "Concentration",
      mean(Value),
      sum(Value)
    ),
    Events=n(),
    .groups="drop"
  )

Frozen_monthly_summary <- Frozen_monthly_site %>%
  group_by(
    Month_Number,
    Month,
    frozen,
    Constituent,
    Metric
  ) %>%
  summarise(
    Mean=mean(Site_Month_Value),
    Standard_Deviation=sd_or_zero(Site_Month_Value),
    Site_Months=n(),
    Events=sum(Events),
    .groups="drop"
  ) %>%
  mutate(
    Lower=pmax(0,Mean-Standard_Deviation),
    Upper=Mean+Standard_Deviation,
    Metric_Label=paste0(
      as.character(Metric),
      " (",
      Metric_units[as.character(Metric)],
      ")"
    ),
    Panel=paste(Metric_Label,Constituent,sep=" | ")
  )

Frozen_panel_levels <- unlist(
  lapply(
    Metric_levels,
    function(metric_name){
      paste(
        paste0(metric_name," (",Metric_units[[metric_name]],")"),
        Constituent_levels,
        sep=" | "
      )
    }
  )
)

Frozen_monthly_summary <- Frozen_monthly_summary %>%
  mutate(Panel=factor(Panel,levels=Frozen_panel_levels))

Frozen_dodge <- position_dodge(width=0.78)

Figure_frozen_targets <- ggplot(
  Frozen_monthly_summary,
  aes(
    Month,
    Mean,
    ymin=Lower,
    ymax=Upper,
    fill=frozen
  )
) +
  geom_col(
    position=Frozen_dodge,
    width=0.70,
    color="black",
    linewidth=0.35
  ) +
  geom_errorbar(
    position=Frozen_dodge,
    width=0.16,
    linewidth=0.45,
    color="black"
  ) +
  facet_wrap(
    ~Panel,
    scales="free_y",
    ncol=4,
    labeller=labeller(
      Panel=function(x) gsub(" \\| ","\n",x)
    )
  ) +
  scale_fill_manual(values=DF_frozen_colors,drop=FALSE) +
  labs(
    x=NULL,
    y="Mean observed site-month value",
    fill=NULL,
    title="Monthly target variables by frozen-soil condition",
    subtitle="Error bars show one standard deviation across observed site-months"
  ) +
  DF_plot_theme +
  theme(
    axis.text.x=element_text(angle=45,hjust=1,size=11),
    legend.position="top"
  )

save_figure_pair(
  Figure_frozen_targets,
  file.path(Figure_path,"05B_Target_variables_frozen_nonfrozen"),
  width=21,
  height=16
)

# Step 5. Correlation matrices ===============================
make_correlation_long <- function(df,variables,metric_name){
  correlation_df <- df %>%
    select(all_of(unname(variables)))

  names(correlation_df) <- names(variables)

  correlation_matrix <- stats::cor(
    correlation_df,
    use="pairwise.complete.obs",
    method="spearman"
  )

  n_matrix <- outer(
    seq_along(correlation_df),
    seq_along(correlation_df),
    Vectorize(
      function(i,j){
        sum(
          is.finite(correlation_df[[i]]) &
            is.finite(correlation_df[[j]])
        )
      }
    )
  )

  dimnames(n_matrix) <- dimnames(correlation_matrix)

  correlation_long <- as.data.frame(
    as.table(correlation_matrix),
    stringsAsFactors=FALSE
  )
  names(correlation_long) <- c("Variable_Y","Variable_X","Correlation")

  n_long <- as.data.frame(as.table(n_matrix),stringsAsFactors=FALSE)
  names(n_long) <- c("Variable_Y","Variable_X","Pairwise_Events")

  left_join(
    correlation_long,
    n_long,
    by=c("Variable_Y","Variable_X")
  ) %>%
    mutate(
      Metric=metric_name,
      Variable_X=factor(Variable_X,levels=names(variables)),
      Variable_Y=factor(Variable_Y,levels=rev(names(variables)))
    )
}

Correlation_variable_sets <- list(
  Concentration=c(
    "Suspended sediment"="suspended_sediment_conc_mgL",
    "Dissolved solids"="total_dissolved_solids_conc_mgL",
    "Orthophosphate"="orthophosphate_conc_mgL",
    "Total phosphorus"="total_phosphorus_unfiltered_conc_mgL",
    "Runoff depth"="runoff_mm",
    "Precipitation depth"="rain_mm"
  ),
  Load=c(
    "Suspended sediment"="suspended_sediment_load_pounds",
    "Dissolved solids"="total_dissolved_solids_load_pounds",
    "Orthophosphate"="orthophosphate_load_pounds",
    "Total phosphorus"="total_phosphorus_unfiltered_load_pounds",
    "Runoff depth"="runoff_mm",
    "Precipitation depth"="rain_mm"
  ),
  Yield=c(
    "Suspended sediment"="suspended_sediment_yield_pounds_per_acre",
    "Dissolved solids"="total_dissolved_solids_yield_pounds_per_acre",
    "Orthophosphate"="orthophosphate_yield_pounds_per_acre",
    "Total phosphorus"="total_phosphorus_unfiltered_yield_pounds_per_acre",
    "Runoff depth"="runoff_mm",
    "Precipitation depth"="rain_mm"
  )
)

Correlation_long <- bind_rows(
  lapply(
    names(Correlation_variable_sets),
    function(metric_name){
      make_correlation_long(
        WQ_df,
        Correlation_variable_sets[[metric_name]],
        metric_name
      )
    }
  )
) %>%
  mutate(Metric=factor(Metric,levels=Metric_levels))

Figure_correlation <- ggplot(
  Correlation_long,
  aes(Variable_X,Variable_Y,fill=Correlation)
) +
  geom_tile(color="black",linewidth=0.35) +
  geom_text(
    aes(label=sprintf("%.2f",Correlation)),
    size=3.5
  ) +
  facet_wrap(~Metric,nrow=1) +
  scale_fill_gradient2(
    low="#4575B4",
    mid="white",
    high="#D73027",
    midpoint=0,
    limits=c(-1,1),
    guide=guide_colorbar(
      frame.colour="black",
      frame.linewidth=0.4,
      ticks.colour="black",
      title.position="top",
      barwidth=grid::unit(7,"cm")
    )
  ) +
  coord_equal() +
  labs(
    x=NULL,
    y=NULL,
    fill="Spearman correlation",
    title="Correlations among water-quality and hydrologic variables"
  ) +
  DF_plot_theme +
  theme(
    axis.text.x=element_text(angle=45,hjust=1,size=11),
    axis.text.y=element_text(size=11),
    legend.position="bottom"
  )

save_figure_pair(
  Figure_correlation,
  file.path(Figure_path,"06_Target_variable_correlation_matrices"),
  width=19,
  height=7.5
)

# Step 6. Constituent load versus runoff depth ===============
Load_lookup <- Measurement_lookup %>%
  filter(Metric == "Load")

Load_long <- WQ_df %>%
  select(
    WQ_event_id,
    Field_Name,
    Season,
    frozen,
    runoff_mm,
    all_of(Load_lookup$Column)
  ) %>%
  pivot_longer(
    cols=all_of(Load_lookup$Column),
    names_to="Column",
    values_to="Load"
  ) %>%
  left_join(Load_lookup,by="Column") %>%
  mutate(Constituent=factor(Constituent,levels=Constituent_levels)) %>%
  filter(
    is.finite(runoff_mm),
    runoff_mm > 0,
    is.finite(Load),
    Load > 0,
    !is.na(Season)
  )

Load_runoff_correlations <- Load_long %>%
  group_by(Constituent) %>%
  group_modify(
    ~{
      correlation_test <- suppressWarnings(
        cor.test(.x$runoff_mm,.x$Load,method="spearman",exact=FALSE)
      )
      log_model <- lm(log10(Load)~log10(runoff_mm),data=.x)
      model_summary <- summary(log_model)

      data.frame(
        Events=nrow(.x),
        Spearman_Rho=unname(correlation_test$estimate),
        Spearman_P=correlation_test$p.value,
        Log_Log_Slope=unname(coef(log_model)[2]),
        Adjusted_R2=model_summary$adj.r.squared,
        Log_Log_Slope_P=coef(model_summary)[2,4]
      )
    }
  ) %>%
  ungroup()

Scatter_annotations <- Load_runoff_correlations %>%
  mutate(
    Label=paste0(
      "Spearman rho = ",sprintf("%.2f",Spearman_Rho),
      "\np ",ifelse(Spearman_P < 0.001,"< 0.001",paste0("= ",sprintf("%.3f",Spearman_P)))
    )
  )

Figure_load_runoff <- ggplot(
  Load_long,
  aes(runoff_mm,Load,color=Season)
) +
  geom_smooth(
    aes(group=Constituent),
    method="lm",
    formula=y~x,
    se=TRUE,
    color="black",
    fill="grey75",
    linewidth=0.8
  ) +
  geom_point(
    aes(shape=frozen),
    size=2.2,
    alpha=0.65
  ) +
  geom_label(
    data=Scatter_annotations,
    aes(x=Inf,y=Inf,label=Label),
    inherit.aes=FALSE,
    hjust=1.05,
    vjust=1.15,
    size=3.8,
    linewidth=0.25,
    fill=alpha("white",0.85)
  ) +
  facet_wrap(~Constituent,scales="free_y",ncol=2) +
  scale_x_log10(labels=label_number()) +
  scale_y_log10(labels=label_number()) +
  scale_color_manual(values=Season_colors,drop=FALSE) +
  scale_shape_manual(values=c("Non-Frozen"=16,"Frozen"=17),drop=FALSE) +
  guides(
    color=guide_legend(nrow=1,order=1),
    shape=guide_legend(nrow=1,order=2)
  ) +
  labs(
    x="Runoff depth (mm, log scale)",
    y="Constituent load (lb, log scale)",
    color=NULL,
    shape="Soil condition",
    title="Constituent load versus runoff depth"
  ) +
  DF_plot_theme +
  theme(legend.position="bottom")

save_figure_pair(
  Figure_load_runoff,
  file.path(Figure_path,"07_Load_vs_runoff_depth"),
  width=14,
  height=10
)

# Step 7. Concentration distributions across sites ===========
Site_concentration <- Measurement_long %>%
  filter(
    Metric == "Concentration",
    is.finite(Value),
    Value > 0,
    !is.na(Hydrologic_Group)
  ) %>%
  mutate(Field_Name=factor(Field_Name,levels=Site_order))

Figure_site_concentration <- ggplot(
  Site_concentration,
  aes(
    Field_Name,
    Value,
    fill=Hydrologic_Group,
    color=Hydrologic_Group
  )
) +
  geom_half_violin(
    side="l",
    alpha=0.5,
    trim=TRUE,
    color=NA
  ) +
  geom_boxplot(
    color="black",
    width=0.14,
    outlier.shape=NA,
    alpha=0.8
  ) +
  geom_jitter(
    aes(x=as.numeric(Field_Name)+0.2),
    width=0.08,
    height=0,
    size=1.1,
    alpha=0.4
  ) +
  facet_wrap(~Constituent,scales="free_y",ncol=2) +
  scale_y_log10(labels=label_number()) +
  scale_fill_manual(values=DF_infiltration_colors,drop=FALSE) +
  scale_color_manual(values=DF_infiltration_colors,drop=FALSE) +
  guides(color="none",fill=guide_legend(nrow=1)) +
  labs(
    x="Site ID",
    y="Concentration (mg/L, log scale)",
    fill="Soil infiltration group",
    title="Water-quality concentrations across sites"
  ) +
  DF_plot_theme +
  theme(
    axis.text.x=element_text(angle=55,hjust=1,size=10),
    legend.position="bottom"
  )

save_figure_pair(
  Figure_site_concentration,
  file.path(Figure_path,"08_Concentration_distributions_by_site"),
  width=18,
  height=12
)

# Step 8. Concentrations and loads across explanatory groups =
Residue_colors <- setNames(
  DF_colors[c(1,2,3)],
  Residue_levels
)

Frozen_group_colors <- setNames(
  DF_frozen_colors[c("Non-Frozen","Frozen")],
  Frozen_levels
)

make_single_group_plot <- function(
    metric_name,
    group_var,
    grouping_label,
    group_levels,
    group_colors){

  plot_df <- Measurement_long %>%
    filter(
      Metric == metric_name,
      is.finite(Value),
      Value > 0,
      !is.na(.data[[group_var]]),
      as.character(.data[[group_var]]) != ""
    )

  if(group_var == "Residue"){
    plot_df <- plot_df %>%
      filter(Season %in% c(
        "Pre-growing season",
        "Post-growing season"
      ))
  }

  plot_df <- plot_df %>%
    mutate(
      Plot_Group=factor(
        as.character(.data[[group_var]]),
        levels=group_levels
      ),
      Grouping=grouping_label
    ) %>%
    filter(!is.na(Plot_Group))

  group_plot <- ggplot(
    plot_df,
    aes(Plot_Group,Value,fill=Plot_Group,color=Plot_Group)
  ) +
    geom_half_violin(
      side="l",
      color=NA,
      alpha=0.5,
      trim=TRUE
    ) +
    geom_boxplot(
      color="black",
      width=0.14,
      outlier.shape=NA,
      alpha=0.8
    ) +
    geom_jitter(
      aes(x=as.numeric(Plot_Group)+0.2),
      width=0.08,
      height=0,
      size=1.2,
      alpha=0.4,
      show.legend=FALSE
    ) +
    facet_wrap(~Constituent,scales="free_y",nrow=1) +
    scale_y_log10(labels=label_number()) +
    scale_fill_manual(values=group_colors,drop=FALSE) +
    scale_color_manual(values=group_colors,drop=FALSE) +
    guides(fill="none",color="none") +
    labs(
      title=grouping_label,
      x=NULL,
      y=paste0(metric_name," (",Metric_units[[metric_name]],", log scale)")
    ) +
    DF_plot_theme +
    theme(
      axis.text.x=element_text(angle=28,hjust=1),
      plot.title=element_text(size=16,face="bold")
    )

  add_measurement_brackets(
    group_plot,
    plot_df,
    Categorical_group_tests %>%
      filter(
        Metric == metric_name,
        Grouping == grouping_label
      )
  )
}

make_categorical_group_figure <- function(metric_name,figure_number){
  group_plots <- list(
    make_single_group_plot(
      metric_name,
      "Season",
      "Season",
      Season_levels,
      Season_colors
    ),
    make_single_group_plot(
      metric_name,
      "frozen",
      "Frozen soil condition",
      Frozen_levels,
      Frozen_group_colors
    ),
    make_single_group_plot(
      metric_name,
      "Hydrologic_Group",
      "Soil infiltration group",
      names(DF_infiltration_colors),
      DF_infiltration_colors
    ),
    make_single_group_plot(
      metric_name,
      "Tile",
      "Site-level tile drainage",
      names(DF_tile_colors),
      DF_tile_colors
    ),
    make_single_group_plot(
      metric_name,
      "Residue",
      "Crop residue",
      Residue_levels,
      Residue_colors
    )
  )

  combined_plot <- wrap_plots(group_plots,ncol=1) +
    plot_annotation(
      title=paste(metric_name,"across categorical explanatory groups"),
      subtitle="Brackets identify significant Benjamini-Hochberg-adjusted pairwise comparisons of site-level medians"
    )

  save_figure_pair(
    combined_plot,
    file.path(
      Figure_path,
      paste0(
        sprintf("%02d",figure_number),
        "_",
        metric_name,
        "_across_categorical_groups"
      )
    ),
    width=22,
    height=27
  )

  combined_plot
}

Figure_concentration_groups <- make_categorical_group_figure(
  "Concentration",
  9
)
Figure_load_groups <- make_categorical_group_figure("Load",10)

# Step 9. Continuous agricultural variables =================
make_continuous_group_plot <- function(
    metric_name,
    predictor,
    predictor_title,
    x_title,
    percent_axis=FALSE){

  plot_df <- Measurement_long %>%
    filter(
      Metric == metric_name,
      is.finite(Value),
      Value > 0,
      is.finite(.data[[predictor]]),
      !is.na(Season)
    )

  continuous_plot <- ggplot(
    plot_df,
    aes(.data[[predictor]],Value,color=Season)
  ) +
    geom_point(size=1.5,alpha=0.4) +
    geom_smooth(
      aes(group=Constituent),
      method="loess",
      color="black",
      fill="grey80",
      linewidth=0.8,
      se=TRUE
    ) +
    facet_wrap(~Constituent,scales="free_y",nrow=1) +
    scale_y_log10(labels=label_number()) +
    scale_color_manual(values=Season_colors,drop=FALSE) +
    labs(
      title=predictor_title,
      x=x_title,
      y=paste0(metric_name," (",Metric_units[[metric_name]],", log scale)"),
      color=NULL
    ) +
    DF_plot_theme +
    theme(legend.position="bottom")

  if(percent_axis){
    continuous_plot <- continuous_plot +
      scale_x_continuous(labels=percent,limits=c(0,1))
  }

  continuous_plot
}

make_continuous_group_figure <- function(metric_name,figure_number){
  perennial_plot <- make_continuous_group_plot(
    metric_name,
    "PerennialFrac",
    "Seasonal perennial crop fraction",
    "Perennial crop fraction",
    percent_axis=TRUE
  )

  tillage_plot <- make_continuous_group_plot(
    metric_name,
    "Tillage_Passes",
    "Seasonal tillage passes",
    "Seasonal tillage passes"
  )

  combined_plot <- (perennial_plot/tillage_plot) +
    plot_annotation(
      title=paste(metric_name,"across continuous agricultural variables")
    )

  save_figure_pair(
    combined_plot,
    file.path(
      Figure_path,
      paste0(
        sprintf("%02d",figure_number),
        "_",
        metric_name,
        "_across_continuous_management"
      )
    ),
    width=21,
    height=12
  )

  combined_plot
}

Figure_concentration_management <- make_continuous_group_figure(
  "Concentration",
  11
)
Figure_load_management <- make_continuous_group_figure("Load",12)

# Step 10. Phosphorus versus sediment and solids =============
Phosphorus_solids_pairs <- data.frame(
  Phosphorus=c(
    "Orthophosphate",
    "Orthophosphate",
    "Total phosphorus",
    "Total phosphorus"
  ),
  Solids=c(
    "Suspended sediment",
    "Total dissolved solids",
    "Suspended sediment",
    "Total dissolved solids"
  ),
  stringsAsFactors=FALSE
) %>%
  mutate(Combination=paste(Phosphorus,"vs",Solids))

Phosphorus_solids_long <- bind_rows(
  lapply(
    Metric_levels,
    function(metric_name){
      metric_lookup <- Measurement_lookup %>%
        filter(Metric == metric_name)

      bind_rows(
        lapply(
          seq_len(nrow(Phosphorus_solids_pairs)),
          function(i){
            pair_row <- Phosphorus_solids_pairs[i,]
            phosphorus_column <- metric_lookup$Column[
              metric_lookup$Constituent == pair_row$Phosphorus
            ]
            solids_column <- metric_lookup$Column[
              metric_lookup$Constituent == pair_row$Solids
            ]

            data.frame(
              WQ_event_id=WQ_df$WQ_event_id,
              Field_Name=WQ_df$Field_Name,
              Season=WQ_df$Season,
              frozen=WQ_df$frozen,
              Metric=metric_name,
              Combination=pair_row$Combination,
              Phosphorus=pair_row$Phosphorus,
              Solids=pair_row$Solids,
              X=WQ_df[[solids_column]],
              Y=WQ_df[[phosphorus_column]]
            )
          }
        )
      )
    }
  )
) %>%
  mutate(
    Metric=factor(Metric,levels=Metric_levels),
    Combination=factor(
      Combination,
      levels=Phosphorus_solids_pairs$Combination
    )
  ) %>%
  filter(
    is.finite(X),
    X > 0,
    is.finite(Y),
    Y > 0,
    !is.na(Season)
  )

Phosphorus_solids_correlations <- Phosphorus_solids_long %>%
  group_by(Metric,Combination,Phosphorus,Solids) %>%
  summarise(
    Events=n(),
    Spearman_Rho=suppressWarnings(cor(X,Y,method="spearman")),
    Spearman_P=suppressWarnings(
      cor.test(X,Y,method="spearman",exact=FALSE)$p.value
    ),
    .groups="drop"
  )

make_phosphorus_solids_figure <- function(metric_name,figure_number){
  plot_df <- Phosphorus_solids_long %>%
    filter(Metric == metric_name)

  annotation_df <- Phosphorus_solids_correlations %>%
    filter(Metric == metric_name) %>%
    mutate(
      Label=paste0(
        "Spearman rho = ",sprintf("%.2f",Spearman_Rho),
        "\np ",
        ifelse(
          Spearman_P < 0.001,
          "< 0.001",
          paste0("= ",sprintf("%.3f",Spearman_P))
        )
      )
    )

  scatter_plot <- ggplot(plot_df,aes(X,Y,color=Season)) +
    geom_smooth(
      aes(group=Combination),
      method="lm",
      formula=y~x,
      se=TRUE,
      color="black",
      fill="grey75",
      linewidth=0.8
    ) +
    geom_point(aes(shape=frozen),size=2.1,alpha=0.65) +
    geom_label(
      data=annotation_df,
      aes(x=Inf,y=Inf,label=Label),
      inherit.aes=FALSE,
      hjust=1.05,
      vjust=1.15,
      size=3.8,
      linewidth=0.25,
      fill=alpha("white",0.85)
    ) +
    facet_wrap(~Combination,scales="free",ncol=2) +
    scale_x_log10(labels=label_number()) +
    scale_y_log10(labels=label_number()) +
    scale_color_manual(values=Season_colors,drop=FALSE) +
    scale_shape_manual(
      values=c("Non-Frozen"=16,"Frozen"=17),
      drop=FALSE
    ) +
    guides(
      color=guide_legend(nrow=1,order=1),
      shape=guide_legend(nrow=1,order=2)
    ) +
    labs(
      x=paste0("Sediment or solids ",tolower(metric_name)," (",Metric_units[[metric_name]],", log scale)"),
      y=paste0("Phosphorus ",tolower(metric_name)," (",Metric_units[[metric_name]],", log scale)"),
      color=NULL,
      shape="Soil condition",
      title=paste("Phosphorus versus sediment and solids:",tolower(metric_name))
    ) +
    DF_plot_theme +
    theme(legend.position="bottom")

  save_figure_pair(
    scatter_plot,
    file.path(
      Figure_path,
      paste0(
        sprintf("%02d",figure_number),
        "_Phosphorus_vs_sediment_",
        metric_name
      )
    ),
    width=14,
    height=10
  )

  scatter_plot
}

Figure_phosphorus_sediment_concentration <-
  make_phosphorus_solids_figure("Concentration",13)
Figure_phosphorus_sediment_load <-
  make_phosphorus_solids_figure("Load",14)
Figure_phosphorus_sediment_yield <-
  make_phosphorus_solids_figure("Yield",15)

# Step 11. Phosphorus-to-sediment ratios =====================
# Ratios are expressed as pounds of phosphorus per ton of suspended sediment
# The non-orthophosphate fraction is an operational difference (total P -
# orthophosphate), not a directly measured particulate-phosphorus variable
Ratio_levels <- c(
  "Total phosphorus / suspended sediment",
  "Orthophosphate / suspended sediment",
  "Non-orthophosphate P / suspended sediment"
)

P_sediment_ratio_events <- bind_rows(
  WQ_df %>%
    transmute(
      WQ_event_id,
      Field_Name,
      Event_Date,
      Calendar_Year,
      Month_Number,
      Month,
      Season,
      frozen,
      Ratio=Ratio_levels[1],
      P_Load_lb=total_phosphorus_unfiltered_load_pounds,
      Sediment_Load_lb=suspended_sediment_load_pounds
    ),
  WQ_df %>%
    transmute(
      WQ_event_id,
      Field_Name,
      Event_Date,
      Calendar_Year,
      Month_Number,
      Month,
      Season,
      frozen,
      Ratio=Ratio_levels[2],
      P_Load_lb=orthophosphate_load_pounds,
      Sediment_Load_lb=suspended_sediment_load_pounds
    ),
  WQ_df %>%
    transmute(
      WQ_event_id,
      Field_Name,
      Event_Date,
      Calendar_Year,
      Month_Number,
      Month,
      Season,
      frozen,
      Ratio=Ratio_levels[3],
      P_Load_lb=total_phosphorus_unfiltered_load_pounds-
        orthophosphate_load_pounds,
      Sediment_Load_lb=suspended_sediment_load_pounds
    )
) %>%
  filter(
    is.finite(P_Load_lb),
    P_Load_lb > 0,
    is.finite(Sediment_Load_lb),
    Sediment_Load_lb > 0
  ) %>%
  mutate(
    Ratio=factor(Ratio,levels=Ratio_levels),
    P_per_Sediment_lb_per_ton=2000*P_Load_lb/Sediment_Load_lb
  )

P_sediment_ratio_monthly_site_year <- P_sediment_ratio_events %>%
  group_by(
    Field_Name,
    Calendar_Year,
    Month_Number,
    Month,
    Season,
    Ratio
  ) %>%
  summarise(
    Events=n(),
    Median_lb_P_per_ton_sediment=median(P_per_Sediment_lb_per_ton),
    .groups="drop"
  )

P_sediment_ratio_monthly_summary <- P_sediment_ratio_monthly_site_year %>%
  group_by(Month_Number,Month,Season,Ratio) %>%
  summarise(
    Site_Years=n(),
    Median=median(Median_lb_P_per_ton_sediment),
    Q25=quantile(Median_lb_P_per_ton_sediment,0.25),
    Q75=quantile(Median_lb_P_per_ton_sediment,0.75),
    .groups="drop"
  )

P_sediment_ratio_annual_site <- P_sediment_ratio_events %>%
  group_by(Field_Name,Calendar_Year,Ratio) %>%
  summarise(
    Events=n(),
    Median_lb_P_per_ton_sediment=median(P_per_Sediment_lb_per_ton),
    .groups="drop"
  )

P_sediment_ratio_annual_summary <- P_sediment_ratio_annual_site %>%
  group_by(Calendar_Year,Ratio) %>%
  summarise(
    Sites=n(),
    Median=median(Median_lb_P_per_ton_sediment),
    Q25=quantile(Median_lb_P_per_ton_sediment,0.25),
    Q75=quantile(Median_lb_P_per_ton_sediment,0.75),
    .groups="drop"
  )

Figure_ratio_month <- ggplot(
  P_sediment_ratio_monthly_summary,
  aes(Month_Number,Median,color=Season,group=1)
) +
  geom_linerange(aes(ymin=Q25,ymax=Q75),linewidth=0.7) +
  geom_line(color="grey35",linewidth=0.7) +
  geom_point(size=3.2,alpha=0.8) +
  facet_wrap(~Ratio,scales="free_y",ncol=1) +
  scale_x_continuous(breaks=1:12,labels=month.abb) +
  scale_y_log10(labels=label_number()) +
  scale_color_manual(values=Season_colors,drop=FALSE) +
  labs(
    x=NULL,
    y="Phosphorus per suspended sediment (lb/ton, log scale)",
    color=NULL,
    title="Monthly phosphorus-to-sediment ratios",
    subtitle="Points are medians and vertical lines are interquartile ranges across site-years"
  ) +
  DF_plot_theme +
  theme(
    axis.text.x=element_text(angle=35,hjust=1),
    legend.position="bottom"
  )

Figure_ratio_year <- ggplot(
  P_sediment_ratio_annual_summary,
  aes(Calendar_Year,Median,color=Ratio,fill=Ratio)
) +
  geom_ribbon(aes(ymin=Q25,ymax=Q75),alpha=0.15,color=NA) +
  geom_line(linewidth=0.9) +
  geom_point(size=2.8,alpha=0.8) +
  scale_y_log10(labels=label_number()) +
  scale_color_manual(
    values=setNames(DF_colors[c(3,2,6)],Ratio_levels),
    drop=FALSE
  ) +
  scale_fill_manual(
    values=setNames(DF_colors[c(3,2,6)],Ratio_levels),
    drop=FALSE
  ) +
  labs(
    x="Calendar year",
    y="Phosphorus per suspended sediment (lb/ton, log scale)",
    color=NULL,
    fill=NULL,
    title="Annual phosphorus-to-sediment ratios",
    subtitle="Lines are medians and ribbons are interquartile ranges across sites"
  ) +
  DF_plot_theme +
  theme(legend.position="bottom") +
  guides(color=guide_legend(nrow=2),fill="none")

Figure_ratio_temporal <- Figure_ratio_month / Figure_ratio_year +
  plot_layout(heights=c(1.7,1))

save_figure_pair(
  Figure_ratio_temporal,
  file.path(Figure_path,"16_Phosphorus_to_sediment_temporal_patterns"),
  width=15,
  height=17
)

P_sediment_ratio_group_summary <- bind_rows(
  P_sediment_ratio_events %>%
    group_by(Field_Name,Ratio,Season) %>%
    summarise(Site_Median=median(P_per_Sediment_lb_per_ton),.groups="drop") %>%
    rename(Group=Season) %>%
    mutate(Grouping="Season"),
  P_sediment_ratio_events %>%
    group_by(Field_Name,Ratio,frozen) %>%
    summarise(Site_Median=median(P_per_Sediment_lb_per_ton),.groups="drop") %>%
    rename(Group=frozen) %>%
    mutate(Grouping="Soil condition")
) %>%
  mutate(
    Group=as.character(Group),
    Group=factor(
      Group,
      levels=c(Season_levels,"Non-Frozen","Frozen")
    )
  )

ratio_pairwise_tests <- function(grouping_name,group_levels){
  ratio_df <- P_sediment_ratio_group_summary %>%
    filter(Grouping == grouping_name) %>%
    mutate(Group=as.character(Group))

  bind_rows(
    lapply(
      Ratio_levels,
      function(ratio_name){
        current_df <- ratio_df %>% filter(Ratio == ratio_name)
        comparison_pairs <- combn(group_levels,2,simplify=FALSE)

        bind_rows(
          lapply(
            comparison_pairs,
            function(comparison_pair){
              paired_values <- current_df %>%
                filter(Group %in% comparison_pair) %>%
                select(Field_Name,Group,Site_Median) %>%
                pivot_wider(names_from=Group,values_from=Site_Median) %>%
                filter(
                  !is.na(.data[[comparison_pair[1]]]),
                  !is.na(.data[[comparison_pair[2]]])
                )

              p_value <- if(nrow(paired_values) >= 3){
                suppressWarnings(
                  wilcox.test(
                    paired_values[[comparison_pair[1]]],
                    paired_values[[comparison_pair[2]]],
                    paired=TRUE,
                    exact=FALSE
                  )$p.value
                )
              }else{
                NA_real_
              }

              data.frame(
                Ratio=ratio_name,
                Grouping=grouping_name,
                Group_1=comparison_pair[1],
                Group_2=comparison_pair[2],
                X_1=match(comparison_pair[1],group_levels),
                X_2=match(comparison_pair[2],group_levels),
                Test="Paired Wilcoxon signed-rank test",
                Sites=nrow(paired_values),
                P_Value=p_value
              )
            }
          )
        )
      }
    )
  ) %>%
    group_by(Ratio,Grouping) %>%
    mutate(
      P_Adjusted=p.adjust(P_Value,method="BH"),
      Significance=case_when(
        P_Adjusted < 0.001 ~ "***",
        P_Adjusted < 0.01 ~ "**",
        P_Adjusted < 0.05 ~ "*",
        TRUE ~ ""
      )
    ) %>%
    ungroup()
}

P_sediment_ratio_group_tests <- bind_rows(
  ratio_pairwise_tests("Season",Season_levels),
  ratio_pairwise_tests("Soil condition",c("Non-Frozen","Frozen"))
)

add_ratio_brackets <- function(plot,plot_df,test_df){
  significant_tests <- test_df %>%
    filter(!is.na(P_Adjusted),P_Adjusted < 0.05)

  if(nrow(significant_tests) == 0){
    return(plot)
  }

  bracket_df <- plot_df %>%
    group_by(Ratio) %>%
    summarise(Panel_Max=max(Site_Median,na.rm=TRUE),.groups="drop") %>%
    right_join(significant_tests,by="Ratio") %>%
    mutate(Ratio=factor(Ratio,levels=Ratio_levels)) %>%
    group_by(Ratio) %>%
    arrange(P_Adjusted,.by_group=TRUE) %>%
    mutate(
      Bracket_Order=row_number(),
      Y=Panel_Max*1.45^Bracket_Order,
      Tip=Y/1.10,
      Label_Y=Y*1.07
    ) %>%
    ungroup()

  plot +
    geom_segment(
      data=bracket_df,
      aes(x=X_1,xend=X_2,y=Y,yend=Y),
      inherit.aes=FALSE,
      linewidth=0.5
    ) +
    geom_segment(
      data=bracket_df,
      aes(x=X_1,xend=X_1,y=Y,yend=Tip),
      inherit.aes=FALSE,
      linewidth=0.5
    ) +
    geom_segment(
      data=bracket_df,
      aes(x=X_2,xend=X_2,y=Y,yend=Tip),
      inherit.aes=FALSE,
      linewidth=0.5
    ) +
    geom_text(
      data=bracket_df,
      aes(x=(X_1+X_2)/2,y=Label_Y,label=Significance),
      inherit.aes=FALSE,
      size=5
    )
}

Ratio_season_plot <- P_sediment_ratio_group_summary %>%
  filter(Grouping == "Season") %>%
  ggplot(aes(Group,Site_Median,fill=Group,color=Group)) +
  geom_half_violin(side="l",alpha=0.5,trim=TRUE,color=NA) +
  geom_boxplot(color="black",width=0.14,outlier.shape=NA,alpha=0.8) +
  geom_jitter(
    aes(x=as.numeric(Group)+0.2),
    width=0.08,
    size=1.6,
    alpha=0.6
  ) +
  facet_wrap(~Ratio,scales="free_y",ncol=1) +
  scale_y_log10(labels=label_number()) +
  scale_fill_manual(values=Season_colors,drop=TRUE) +
  scale_color_manual(values=Season_colors,drop=TRUE) +
  labs(
    x=NULL,
    y="Site median (lb P/ton sediment, log scale)",
    fill=NULL,
    title="Ratios across seasons"
  ) +
  DF_plot_theme +
  theme(axis.text.x=element_text(angle=25,hjust=1),legend.position="none") +
  guides(color="none")

Frozen_colors <- setNames(
  RColorBrewer::brewer.pal(7,"Set2")[c(3,2)],
  c("Non-Frozen","Frozen")
)

Ratio_frozen_plot <- P_sediment_ratio_group_summary %>%
  filter(Grouping == "Soil condition") %>%
  mutate(
    Group=factor(
      as.character(Group),
      levels=c("Non-Frozen","Frozen")
    )
  ) %>%
  ggplot(aes(Group,Site_Median,fill=Group,color=Group)) +
  geom_half_violin(side="l",alpha=0.5,trim=TRUE,color=NA) +
  geom_boxplot(color="black",width=0.14,outlier.shape=NA,alpha=0.8) +
  geom_jitter(
    aes(x=as.numeric(Group)+0.2),
    width=0.08,
    size=1.6,
    alpha=0.6
  ) +
  facet_wrap(~Ratio,scales="free_y",ncol=1) +
  scale_y_log10(labels=label_number()) +
  scale_fill_manual(values=Frozen_colors,drop=TRUE) +
  scale_color_manual(values=Frozen_colors,drop=TRUE) +
  labs(
    x=NULL,
    y="Site median (lb P/ton sediment, log scale)",
    fill=NULL,
    title="Ratios under frozen and non-frozen conditions"
  ) +
  DF_plot_theme +
  theme(axis.text.x=element_text(angle=20,hjust=1),legend.position="none") +
  guides(color="none")

Ratio_season_plot <- add_ratio_brackets(
  Ratio_season_plot,
  P_sediment_ratio_group_summary %>% filter(Grouping == "Season"),
  P_sediment_ratio_group_tests %>% filter(Grouping == "Season")
)

Ratio_frozen_plot <- add_ratio_brackets(
  Ratio_frozen_plot,
  P_sediment_ratio_group_summary %>% filter(Grouping == "Soil condition"),
  P_sediment_ratio_group_tests %>% filter(Grouping == "Soil condition")
)

Figure_ratio_groups <- Ratio_season_plot | Ratio_frozen_plot

save_figure_pair(
  Figure_ratio_groups,
  file.path(Figure_path,"17_Phosphorus_to_sediment_across_groups"),
  width=18,
  height=14
)

# Step 12. Constituent concentration versus runoff depth =====
Concentration_lookup <- Measurement_lookup %>%
  filter(Metric == "Concentration")

Concentration_runoff_long <- WQ_df %>%
  select(
    WQ_event_id,
    Field_Name,
    Season,
    frozen,
    runoff_mm,
    all_of(Concentration_lookup$Column)
  ) %>%
  pivot_longer(
    cols=all_of(Concentration_lookup$Column),
    names_to="Column",
    values_to="Concentration"
  ) %>%
  left_join(Concentration_lookup,by="Column") %>%
  mutate(Constituent=factor(Constituent,levels=Constituent_levels)) %>%
  filter(
    is.finite(runoff_mm),
    runoff_mm > 0,
    is.finite(Concentration),
    Concentration > 0,
    !is.na(Season)
  )

Concentration_runoff_correlations <- Concentration_runoff_long %>%
  group_by(Constituent) %>%
  group_modify(
    ~{
      correlation_test <- suppressWarnings(
        cor.test(.x$runoff_mm,.x$Concentration,method="spearman",exact=FALSE)
      )
      log_model <- lm(log10(Concentration)~log10(runoff_mm),data=.x)
      model_summary <- summary(log_model)
      data.frame(
        Events=nrow(.x),
        Spearman_Rho=unname(correlation_test$estimate),
        Spearman_P=correlation_test$p.value,
        Log_Log_Slope=unname(coef(log_model)[2]),
        Adjusted_R2=model_summary$adj.r.squared,
        Log_Log_Slope_P=coef(model_summary)[2,4]
      )
    }
  ) %>%
  ungroup()

Concentration_scatter_annotations <- Concentration_runoff_correlations %>%
  mutate(
    Label=paste0(
      "Spearman rho = ",sprintf("%.2f",Spearman_Rho),
      "\np ",ifelse(
        Spearman_P < 0.001,
        "< 0.001",
        paste0("= ",sprintf("%.3f",Spearman_P))
      )
    )
  )

Figure_concentration_runoff <- ggplot(
  Concentration_runoff_long,
  aes(runoff_mm,Concentration,color=Season)
) +
  geom_smooth(
    aes(group=Constituent),
    method="lm",
    formula=y~x,
    se=TRUE,
    color="black",
    fill="grey75",
    linewidth=0.8
  ) +
  geom_point(aes(shape=frozen),size=2.2,alpha=0.65) +
  geom_label(
    data=Concentration_scatter_annotations,
    aes(x=Inf,y=Inf,label=Label),
    inherit.aes=FALSE,
    hjust=1.05,
    vjust=1.15,
    size=3.8,
    linewidth=0.25,
    fill=alpha("white",0.85)
  ) +
  facet_wrap(~Constituent,scales="free_y",ncol=2) +
  scale_x_log10(labels=label_number()) +
  scale_y_log10(labels=label_number()) +
  scale_color_manual(values=Season_colors,drop=FALSE) +
  scale_shape_manual(values=c("Non-Frozen"=16,"Frozen"=17),drop=FALSE) +
  guides(
    color=guide_legend(nrow=1,order=1),
    shape=guide_legend(nrow=1,order=2)
  ) +
  labs(
    x="Runoff depth (mm, log scale)",
    y="Constituent concentration (mg/L, log scale)",
    color=NULL,
    shape="Soil condition",
    title="Constituent concentration versus runoff depth"
  ) +
  DF_plot_theme +
  theme(legend.position="bottom")

save_figure_pair(
  Figure_concentration_runoff,
  file.path(Figure_path,"18_Concentration_vs_runoff_depth"),
  width=14,
  height=10
)

# Step 13. Monthly variability and candidate drivers =========
# Log10 IQR is a robust spread measure that can be compared among variables
# spanning different concentration and load scales
Monthly_variability_summary <- Measurement_long %>%
  filter(
    Metric %in% c("Concentration","Load"),
    is.finite(Value),
    Value > 0,
    !is.na(Month)
  ) %>%
  group_by(Metric,Constituent,Month_Number,Month,Season) %>%
  summarise(
    Events=n(),
    Sites=n_distinct(Field_Name),
    Log10_SD=sd_or_zero(log10(Value)),
    Log10_IQR=IQR(log10(Value)),
    Log10_MAD=mad(log10(Value)),
    Q75_to_Q25_Ratio=10^Log10_IQR,
    .groups="drop"
  )

Figure_monthly_variability <- ggplot(
  Monthly_variability_summary,
  aes(Month_Number,Log10_IQR,color=Season,group=1)
) +
  geom_line(color="grey35",linewidth=0.7) +
  geom_point(size=3,alpha=0.8) +
  facet_grid(Metric~Constituent,scales="free_y") +
  scale_x_continuous(breaks=1:12,labels=month.abb) +
  scale_color_manual(values=Season_colors,drop=FALSE) +
  labs(
    x=NULL,
    y="Monthly variability (IQR of log10 values)",
    color=NULL,
    title="Monthly variability in sediment and phosphorus",
    subtitle="Larger values indicate a wider event distribution within a month"
  ) +
  DF_plot_theme +
  theme(
    axis.text.x=element_text(angle=45,hjust=1),
    legend.position="bottom"
  )

save_figure_pair(
  Figure_monthly_variability,
  file.path(Figure_path,"19_Monthly_target_variability"),
  width=20,
  height=10
)

Driver_lookup <- c(
  runoff_mm="Runoff depth",
  rain_mm="Precipitation depth",
  I30_mm_hr="Maximum 30-minute intensity",
  ARFdays7_mm="Seven-day antecedent precipitation",
  MeanSlope_per="Mean slope",
  PerennialFrac="Perennial crop fraction",
  Tillage_Passes="Tillage passes",
  frozen_numeric="Frozen soil"
)

Target_driver_long <- Measurement_long %>%
  filter(
    Metric %in% c("Concentration","Load"),
    is.finite(Value),
    Value > 0
  ) %>%
  mutate(frozen_numeric=ifelse(frozen == "Frozen",1,0)) %>%
  pivot_longer(
    cols=all_of(names(Driver_lookup)),
    names_to="Driver_column",
    values_to="Driver_value"
  ) %>%
  mutate(
    Driver=unname(Driver_lookup[Driver_column]),
    Driver=factor(Driver,levels=unname(Driver_lookup))
  ) %>%
  filter(is.finite(Driver_value))

safe_spearman_summary <- function(target,driver){
  keep <- is.finite(target) & is.finite(driver)
  target <- target[keep]
  driver <- driver[keep]
  if(
    length(target) < 10 ||
    length(unique(target)) < 2 ||
    length(unique(driver)) < 2
  ){
    return(data.frame(Events=length(target),Spearman_Rho=NA_real_,P_Value=NA_real_))
  }
  result <- suppressWarnings(
    cor.test(target,driver,method="spearman",exact=FALSE)
  )
  data.frame(
    Events=length(target),
    Spearman_Rho=unname(result$estimate),
    P_Value=result$p.value
  )
}

Monthly_target_driver_associations <- Target_driver_long %>%
  group_by(Metric,Constituent,Month_Number,Month,Driver) %>%
  group_modify(
    ~safe_spearman_summary(log10(.x$Value),.x$Driver_value)
  ) %>%
  ungroup() %>%
  group_by(Metric,Constituent,Month) %>%
  mutate(P_Adjusted=p.adjust(P_Value,method="BH")) %>%
  ungroup()

Figure_monthly_driver_associations <- Monthly_target_driver_associations %>%
  filter(is.finite(Spearman_Rho)) %>%
  ggplot(aes(Month,Driver,fill=Spearman_Rho)) +
  geom_tile(color="black",linewidth=0.25) +
  geom_point(
    data=~filter(.x,!is.na(P_Adjusted),P_Adjusted < 0.05),
    shape=8,
    size=2.2,
    color="black"
  ) +
  facet_grid(Metric~Constituent) +
  scale_fill_gradient2(
    low="#2166AC",
    mid="white",
    high="#B2182B",
    midpoint=0,
    limits=c(-1,1),
    guide=guide_colorbar(
      frame.colour="black",
      frame.linewidth=0.4,
      ticks.colour="black",
      title.position="top",
      barwidth=grid::unit(7,"cm")
    )
  ) +
  labs(
    x=NULL,
    y=NULL,
    fill="Spearman correlation",
    title="Monthly associations with candidate drivers",
    subtitle="Asterisks identify Benjamini-Hochberg-adjusted p < 0.05 within each target and month"
  ) +
  DF_plot_theme +
  theme(
    axis.text.x=element_text(angle=45,hjust=1,size=10),
    axis.text.y=element_text(size=10),
    legend.position="bottom"
  )

save_figure_pair(
  Figure_monthly_driver_associations,
  file.path(Figure_path,"20_Monthly_target_driver_associations"),
  width=22,
  height=13
)

Variability_driver_long <- Target_driver_long %>%
  group_by(Metric,Constituent,Month_Number) %>%
  mutate(
    Log10_Value=log10(Value),
    Absolute_Log_Deviation=abs(Log10_Value-median(Log10_Value,na.rm=TRUE))
  ) %>%
  ungroup()

Variability_driver_associations <- Variability_driver_long %>%
  group_by(Metric,Constituent,Season,Driver) %>%
  group_modify(
    ~safe_spearman_summary(.x$Absolute_Log_Deviation,.x$Driver_value)
  ) %>%
  ungroup() %>%
  group_by(Metric,Constituent,Season) %>%
  mutate(P_Adjusted=p.adjust(P_Value,method="BH")) %>%
  ungroup()

Figure_variability_drivers <- Variability_driver_associations %>%
  filter(is.finite(Spearman_Rho)) %>%
  ggplot(aes(Season,Driver,fill=Spearman_Rho)) +
  geom_tile(color="black",linewidth=0.3) +
  geom_point(
    data=~filter(.x,!is.na(P_Adjusted),P_Adjusted < 0.05),
    shape=8,
    size=2.3,
    color="black"
  ) +
  facet_grid(Metric~Constituent) +
  scale_fill_gradient2(
    low="#2166AC",
    mid="white",
    high="#B2182B",
    midpoint=0,
    limits=c(-1,1),
    guide=guide_colorbar(
      frame.colour="black",
      frame.linewidth=0.4,
      ticks.colour="black",
      title.position="top",
      barwidth=grid::unit(7,"cm")
    )
  ) +
  labs(
    x=NULL,
    y=NULL,
    fill="Spearman correlation",
    title="Associations with within-month event variability",
    subtitle="Positive values indicate greater departure from the monthly median; asterisks identify adjusted p < 0.05"
  ) +
  DF_plot_theme +
  theme(
    axis.text.x=element_text(angle=30,hjust=1,size=10),
    axis.text.y=element_text(size=10),
    legend.position="bottom"
  )

save_figure_pair(
  Figure_variability_drivers,
  file.path(Figure_path,"21_Variability_driver_associations"),
  width=22,
  height=13
)

# Step 14. Output machine-readable tables ====================
write.csv(
  Data_summary,
  file.path(Table_path,"Data_summary.csv"),
  row.names=FALSE,
  na=""
)

write.csv(
  Constituent_availability,
  file.path(Table_path,"Constituent_availability.csv"),
  row.names=FALSE,
  na=""
)

write.csv(
  Constituent_summary,
  file.path(Table_path,"Constituent_summary_statistics.csv"),
  row.names=FALSE,
  na=""
)

write.csv(
  Site_constituent_summary,
  file.path(Table_path,"Site_constituent_summary.csv"),
  row.names=FALSE,
  na=""
)

write.csv(
  Season_constituent_summary,
  file.path(Table_path,"Season_constituent_summary.csv"),
  row.names=FALSE,
  na=""
)

write.csv(
  Frozen_monthly_summary %>% select(-Panel,-Metric_Label),
  file.path(Table_path,"Frozen_nonfrozen_monthly_summary.csv"),
  row.names=FALSE,
  na=""
)

write.csv(
  Correlation_long,
  file.path(Table_path,"Target_variable_correlations.csv"),
  row.names=FALSE,
  na=""
)

write.csv(
  Load_runoff_correlations,
  file.path(Table_path,"Load_runoff_correlations.csv"),
  row.names=FALSE,
  na=""
)

write.csv(
  bind_rows(
    Season_tests,
    Categorical_group_tests
  ) %>%
    distinct(),
  file.path(Table_path,"Constituent_group_pairwise_tests.csv"),
  row.names=FALSE,
  na=""
)

write.csv(
  Phosphorus_solids_correlations,
  file.path(Table_path,"Phosphorus_sediment_correlations.csv"),
  row.names=FALSE,
  na=""
)

write.csv(
  P_sediment_ratio_events,
  file.path(Table_path,"P_sediment_ratio_events.csv"),
  row.names=FALSE,
  na=""
)

write.csv(
  P_sediment_ratio_monthly_site_year,
  file.path(Table_path,"P_sediment_ratio_monthly_site_year.csv"),
  row.names=FALSE,
  na=""
)

write.csv(
  P_sediment_ratio_monthly_summary,
  file.path(Table_path,"P_sediment_ratio_monthly_summary.csv"),
  row.names=FALSE,
  na=""
)

write.csv(
  P_sediment_ratio_annual_summary,
  file.path(Table_path,"P_sediment_ratio_annual_summary.csv"),
  row.names=FALSE,
  na=""
)

write.csv(
  P_sediment_ratio_group_tests,
  file.path(Table_path,"P_sediment_ratio_group_tests.csv"),
  row.names=FALSE,
  na=""
)

write.csv(
  Concentration_runoff_correlations,
  file.path(Table_path,"Concentration_runoff_correlations.csv"),
  row.names=FALSE,
  na=""
)

write.csv(
  Monthly_variability_summary,
  file.path(Table_path,"Monthly_variability_summary.csv"),
  row.names=FALSE,
  na=""
)

write.csv(
  Monthly_target_driver_associations,
  file.path(Table_path,"Monthly_target_driver_associations.csv"),
  row.names=FALSE,
  na=""
)

write.csv(
  Variability_driver_associations,
  file.path(Table_path,"Variability_driver_associations.csv"),
  row.names=FALSE,
  na=""
)

# Step 15. Generate the HTML report ==========================
Strongest_load_relationship <- Load_runoff_correlations %>%
  filter(is.finite(Spearman_Rho)) %>%
  slice_max(abs(Spearman_Rho),n=1,with_ties=FALSE)

Strongest_concentration_relationship <- Concentration_runoff_correlations %>%
  filter(is.finite(Spearman_Rho)) %>%
  slice_max(abs(Spearman_Rho),n=1,with_ties=FALSE)

Strongest_variability_association <- Variability_driver_associations %>%
  filter(
    is.finite(Spearman_Rho),
    !is.na(P_Adjusted),
    P_Adjusted < 0.05
  ) %>%
  slice_max(abs(Spearman_Rho),n=1,with_ties=FALSE)

Key_findings <- c(
  "<div class=\"callout\"><strong>Dataset scope:</strong> only storm-associated surface-runoff events with <code>estimated_flow_fraction = 0</code> and <code>estimated = 0</code> are included.</div>",
  paste0(
    "<p>The strongest rank correlation between constituent load and runoff depth was observed for ",
    html_escape(as.character(Strongest_load_relationship$Constituent)),
    " (Spearman rho = ",
    sprintf("%.2f",Strongest_load_relationship$Spearman_Rho),
    ", p ",
    ifelse(
      Strongest_load_relationship$Spearman_P < 0.001,
      "&lt; 0.001",
      paste0("= ",format_p_value(Strongest_load_relationship$Spearman_P))
    ),
    ").</p>"
  ),
  paste0(
    "<p>The strongest concentration-runoff association was observed for ",
    html_escape(as.character(Strongest_concentration_relationship$Constituent)),
    " (Spearman rho = ",
    sprintf("%.2f",Strongest_concentration_relationship$Spearman_Rho),
    ", p ",
    ifelse(
      Strongest_concentration_relationship$Spearman_P < 0.001,
      "&lt; 0.001",
      paste0("= ",format_p_value(Strongest_concentration_relationship$Spearman_P))
    ),
    "). This relationship is evaluated separately from the load-runoff relationship because concentration is not mathematically multiplied by runoff volume.</p>"
  ),
  paste0(
    "<p>The largest supported association with within-month event variability was for ",
    html_escape(as.character(Strongest_variability_association$Driver)),
    " and ",
    tolower(html_escape(as.character(Strongest_variability_association$Constituent))),
    " ",
    tolower(html_escape(as.character(Strongest_variability_association$Metric))),
    " during the ",
    tolower(html_escape(as.character(Strongest_variability_association$Season))),
    " (Spearman rho = ",
    sprintf("%.2f",Strongest_variability_association$Spearman_Rho),
    ", adjusted p ",
    ifelse(
      Strongest_variability_association$P_Adjusted < 0.001,
      "&lt; 0.001",
      paste0("= ",format_p_value(Strongest_variability_association$P_Adjusted))
    ),
    "). These exploratory associations do not establish causal effects.</p>"
  )
)

Availability_report <- Constituent_availability %>%
  select(
    Constituent,
    Metric,
    Available_Events,
    Availability_Percent,
    Sites_with_Data
  )

Load_correlation_report <- Load_runoff_correlations %>%
  mutate(
    Spearman_P=format_p_value(Spearman_P),
    Log_Log_Slope_P=format_p_value(Log_Log_Slope_P)
  )

Concentration_correlation_report <- Concentration_runoff_correlations %>%
  mutate(
    Spearman_P=format_p_value(Spearman_P),
    Log_Log_Slope_P=format_p_value(Log_Log_Slope_P)
  )

Ratio_group_tests_report <- P_sediment_ratio_group_tests %>%
  mutate(
    P_Value=format_p_value(P_Value),
    P_Adjusted=format_p_value(P_Adjusted)
  )

Largest_monthly_variability_report <- Monthly_variability_summary %>%
  group_by(Metric,Constituent) %>%
  slice_max(Log10_IQR,n=1,with_ties=FALSE) %>%
  ungroup() %>%
  select(
    Metric,
    Constituent,
    Month,
    Events,
    Sites,
    Log10_IQR,
    Q75_to_Q25_Ratio
  )

Significant_group_tests_report <- Categorical_group_tests %>%
  filter(!is.na(P_Adjusted),P_Adjusted < 0.05) %>%
  select(
    Metric,
    Constituent,
    Grouping,
    Group_1,
    Group_2,
    Test,
    Sites,
    P_Adjusted,
    Significance
  )

Phosphorus_solids_report <- Phosphorus_solids_correlations %>%
  mutate(Spearman_P=format_p_value(Spearman_P))

Report_body <- c(
  Key_findings,
  "<h2>Data summary</h2>",
  data_frame_to_html(Data_summary,digits=0),
  "<h3>Constituent availability</h3>",
  "<p>Availability is the number and percentage of retained events containing a non-missing measurement for each constituent and measurement form. Concentration, load, and yield are evaluated separately, and Sites with Data is the number of monitoring sites represented by those measurements.</p>",
  data_frame_to_html(Availability_report,digits=1),
  embedded_figure_html(
    file.path(Figure_path,"01_Measurement_availability_by_site.png"),
    "Figure 1. Number of measured concentration observations at each site. Sites are ordered by soil infiltration group and then alphabetically."
  ),
  "<h2>Distributions of measured constituents</h2>",
  "<p>Boxplot lines are black, fills and jittered observations use the established seasonal colors, and axes are log scaled because the event measurements span several orders of magnitude.</p>",
  embedded_figure_html(
    file.path(Figure_path,"02_Concentration_by_season.png"),
    "Figure 2. Event concentration distributions during the pre-growing, growing, and post-growing seasons. Brackets show significant Benjamini-Hochberg-adjusted pairwise comparisons of site-level medians."
  ),
  embedded_figure_html(
    file.path(Figure_path,"03_Load_by_season.png"),
    "Figure 3. Event load distributions during the pre-growing, growing, and post-growing seasons. Brackets show significant Benjamini-Hochberg-adjusted pairwise comparisons of site-level medians."
  ),
  embedded_figure_html(
    file.path(Figure_path,"04_Yield_by_season.png"),
    "Figure 4. Event yield distributions during the pre-growing, growing, and post-growing seasons. Brackets show significant Benjamini-Hochberg-adjusted pairwise comparisons of site-level medians."
  ),
  "<h3>Summary statistics</h3>",
  data_frame_to_html(Constituent_summary,digits=3),
  "<h2>Monthly concentration patterns</h2>",
  "<p>Concentrations are first averaged within each monitored site-month. Monthly points are means across site-months, and error bars show one standard deviation rather than bootstrap intervals.</p>",
  embedded_figure_html(
    file.path(Figure_path,"05_Monthly_concentration_patterns.png"),
    "Figure 5A. Monthly mean concentrations with one-standard-deviation error bars across site-months."
  ),
  "<h3>Frozen and non-frozen conditions</h3>",
  "<p>Concentrations are averaged within each observed site-month, while loads and yields are totaled within each observed site-month. Bars are means across observed site-months and error bars show one standard deviation. A month without a qualifying measured observation is not treated as zero.</p>",
  embedded_figure_html(
    file.path(Figure_path,"05B_Target_variables_frozen_nonfrozen.png"),
    "Figure 5B. Monthly concentrations, loads, and yields under frozen and non-frozen soil conditions. Error bars show one standard deviation across observed site-months."
  ),
  "<h2>Correlations among target and hydrologic variables</h2>",
  "<p>Matrices use pairwise-complete Spearman correlations. Runoff and precipitation depths are included to show how concentration, load, and yield relate to event hydrology. Pairwise sample sizes are retained in the correlation CSV.</p>",
  embedded_figure_html(
    file.path(Figure_path,"06_Target_variable_correlation_matrices.png"),
    "Figure 6. Spearman correlation matrices for concentrations, loads, and yields, together with runoff and precipitation depth."
  ),
  "<h2>Constituent load and runoff depth</h2>",
  "<p>Both axes are log scaled. Points are colored by season and shaped by frozen-soil condition. Black lines show linear fits on the displayed log-log scales; labels report untransformed Spearman rank correlations.</p>",
  embedded_figure_html(
    file.path(Figure_path,"07_Load_vs_runoff_depth.png"),
    "Figure 7. Event constituent loads versus surface-runoff depth."
  ),
  "<h3>Load-runoff relationship statistics</h3>",
  data_frame_to_html(Load_correlation_report,digits=3),
  "<h2>Concentrations across sites</h2>",
  "<p>Sites are ordered by soil infiltration group and alphabetically within each group. The site panels use half-violin plots, black-outlined boxplots, and matching jittered-point colors.</p>",
  embedded_figure_html(
    file.path(Figure_path,"08_Concentration_distributions_by_site.png"),
    "Figure 8. Event concentration distributions across sites, colored by soil infiltration group."
  ),
  "<h2>Concentrations and loads across explanatory groups</h2>",
  "<p>Categorical comparisons use site-level medians. Season, frozen-soil condition, and crop-residue comparisons use paired Wilcoxon signed-rank tests where the same sites contribute to both groups. Soil-infiltration and tile-drainage comparisons use Wilcoxon rank-sum tests. P-values are adjusted within each constituent and grouping variable using the Benjamini-Hochberg method. Only significant comparisons are drawn as brackets.</p>",
  embedded_figure_html(
    file.path(Figure_path,"09_Concentration_across_categorical_groups.png"),
    "Figure 9. Concentrations across season, frozen-soil condition, soil infiltration group, site-level tile drainage, and crop-residue groups."
  ),
  embedded_figure_html(
    file.path(Figure_path,"10_Load_across_categorical_groups.png"),
    "Figure 10. Loads across season, frozen-soil condition, soil infiltration group, site-level tile drainage, and crop-residue groups."
  ),
  "<h3>Significant categorical-group comparisons</h3>",
  data_frame_to_html(Significant_group_tests_report,digits=4),
  "<h2>Continuous agricultural variables</h2>",
  "<p>Perennial crop fraction remains continuous. Black curves show overall smooth trends, while event points retain the established seasonal colors.</p>",
  embedded_figure_html(
    file.path(Figure_path,"11_Concentration_across_continuous_management.png"),
    "Figure 11. Concentrations in relation to seasonal perennial crop fraction and seasonal tillage passes."
  ),
  embedded_figure_html(
    file.path(Figure_path,"12_Load_across_continuous_management.png"),
    "Figure 12. Loads in relation to seasonal perennial crop fraction and seasonal tillage passes."
  ),
  "<h2>Phosphorus relationships with sediment and solids</h2>",
  "<p>Orthophosphate and total phosphorus are compared with suspended sediment and total dissolved solids for concentration, load, and yield. Both axes are log scaled. Points are colored by season and shaped by frozen-soil condition; labels report Spearman rank correlations.</p>",
  embedded_figure_html(
    file.path(Figure_path,"13_Phosphorus_vs_sediment_Concentration.png"),
    "Figure 13. Phosphorus concentrations versus suspended-sediment and total-dissolved-solids concentrations."
  ),
  embedded_figure_html(
    file.path(Figure_path,"14_Phosphorus_vs_sediment_Load.png"),
    "Figure 14. Phosphorus loads versus suspended-sediment and total-dissolved-solids loads."
  ),
  embedded_figure_html(
    file.path(Figure_path,"15_Phosphorus_vs_sediment_Yield.png"),
    "Figure 15. Phosphorus yields versus suspended-sediment and total-dissolved-solids yields."
  ),
  "<h3>Phosphorus-sediment and phosphorus-solids correlations</h3>",
  data_frame_to_html(Phosphorus_solids_report,digits=3),
  "<h2>Phosphorus relative to suspended-sediment export</h2>",
  "<p>Phosphorus-to-sediment ratios address the proposed characterization of nutrient composition and nutrient relationships with soil loss. Ratios are calculated from event loads and expressed as pounds of phosphorus per ton of suspended sediment. Total phosphorus and orthophosphate ratios use measured variables. Non-orthophosphate P is calculated as total phosphorus minus orthophosphate only when that difference is positive; it is an operational residual and should not be interpreted as a directly measured particulate-phosphorus fraction.</p>",
  embedded_figure_html(
    file.path(Figure_path,"16_Phosphorus_to_sediment_temporal_patterns.png"),
    "Figure 16. Monthly and annual changes in phosphorus-to-suspended-sediment load ratios. Monthly estimates summarize site-year medians, and annual estimates summarize site medians. Interquartile ranges describe spatial and interannual variation without annualizing partial monitoring years."
  ),
  embedded_figure_html(
    file.path(Figure_path,"17_Phosphorus_to_sediment_across_groups.png"),
    "Figure 17. Site-level median phosphorus-to-suspended-sediment ratios across seasons and frozen-soil conditions."
  ),
  "<h3>Overall ratio comparisons</h3>",
  "<p>Paired Wilcoxon signed-rank tests compare site-level median ratios among seasons and between frozen and non-frozen conditions. P-values are adjusted within each ratio and grouping using the Benjamini-Hochberg method. Significant comparisons are shown with brackets.</p>",
  data_frame_to_html(Ratio_group_tests_report,digits=3),
  "<h2>Constituent concentration and runoff depth</h2>",
  "<p>Concentration-runoff relationships are shown separately from load-runoff relationships. Both axes are log scaled; points are colored by season and shaped by frozen-soil condition. Black lines show linear fits on the displayed log-log scales, while labels report Spearman rank correlations.</p>",
  embedded_figure_html(
    file.path(Figure_path,"18_Concentration_vs_runoff_depth.png"),
    "Figure 18. Event constituent concentrations versus surface-runoff depth."
  ),
  "<h3>Concentration-runoff relationship statistics</h3>",
  data_frame_to_html(Concentration_correlation_report,digits=3),
  "<h2>Monthly variability and candidate explanatory variables</h2>",
  "<p>Monthly variability is summarized using the interquartile range of log10-transformed event values. This robust measure describes the spread among events without bootstrapping and permits comparison among constituents with very different numerical scales. The Q75-to-Q25 ratio translates the log-scale spread into a multiplicative range.</p>",
  embedded_figure_html(
    file.path(Figure_path,"19_Monthly_target_variability.png"),
    "Figure 19. Monthly variability of sediment and phosphorus concentrations and loads."
  ),
  "<h3>Month with greatest variability for each target</h3>",
  data_frame_to_html(Largest_monthly_variability_report,digits=3),
  "<p>The monthly association analysis evaluates runoff depth, precipitation depth, maximum 30-minute intensity, seven-day antecedent precipitation, mean slope, perennial crop fraction, tillage passes, and frozen-soil condition. Spearman correlations are calculated separately by month, constituent, and measurement form. Asterisks identify associations with Benjamini-Hochberg-adjusted p &lt; 0.05. Management variables are included as descriptive candidate explanations, not as estimates of causal management effects.</p>",
  embedded_figure_html(
    file.path(Figure_path,"20_Monthly_target_driver_associations.png"),
    "Figure 20. Month-specific associations between constituent concentrations or loads and candidate explanatory variables."
  ),
  "<p>Within-month event variability is defined as the absolute distance of each log10-transformed value from its constituent-specific monthly median. Positive correlations indicate that larger values of an explanatory variable are associated with observations farther from the typical value for that month.</p>",
  embedded_figure_html(
    file.path(Figure_path,"21_Variability_driver_associations.png"),
    "Figure 21. Seasonal associations between candidate explanatory variables and within-month event variability."
  ),
  "<h2>Output files</h2>",
  "<p>Every figure is saved in PNG and PDF format. Machine-readable summary tables are saved under <code>04_Results/Sediment_Phosphorus_Exploratory/Tables</code>.</p>"
)

Exploratory_report <- file.path(
  Report_path,
  "07_Sediment_phosphorus_exploratory_analysis_report.html"
)

write_html_report(
  title="Discovery Farms Sediment and Phosphorus Exploratory Analysis",
  subtitle=paste0("Generated: ",Sys.Date()),
  body_html=Report_body,
  output_path=Exploratory_report
)

message("Sediment and phosphorus exploratory analysis complete.")
message("Figures: ",Figure_path)
message("Tables: ",Table_path)
message("Report: ",Exploratory_report)
