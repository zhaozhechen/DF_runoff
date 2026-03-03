# Author: Zhaozhe Chen
# Date: 2026.3.2

# This code is to model Q event runoff coefficient (RC)

# Seasons to consider
# All
# Growing season (GS): May - Sep 
# Spring shoulder (SS): Jan - April 
# Fall shoulder (FS): Oct - Dec

# ---------- Global -----------
library(stringr)
library(dplyr)
library(lubridate)
library(cowplot)
library(lme4)
library(GGally)
library(ggeffects)
library(performance)
library(pROC)
library(tibble)

# Data path =======
# Joint non-frozen Q events
Q_df <- read.csv("00_Data/Processed_data_v2/Non-Frozen_Q_joint_df.csv")

# Source functions
source("01_Codes/Plotting_functions.R")
source("01_Codes/Analyses_functions.R")

# Colors for plotting 
my_color <- RColorBrewer::brewer.pal(7,"Set2")

# Decide if only focus on Surface monitoring sites and filter out Tile sites
Tile_Y <- "NOTile"

# Agricultural practices to test
ag_vars <- c("Annual_Tillage","DSP","PerennialFrac")

# Output path
Output_path <- "D:/OneDrive - UW-Madison/Research/Discovery Farms/DF Runoff Generation/Results/RC_modeling_log/"

# -------- Main --------
# Preprocessing Q dataset ============
# If Tile_Y is TRUE, keep Tile sites, otherwise, filter them out
if(Tile_Y == "NOTile"){
  Q_df <- Q_df %>%
    filter(Monitoring == "Surface")
}

Q_df <- Q_df %>%
  # Runoff coefficient is the response
  mutate(RC = runoff_in/rain_in,
         log_RC = log(RC)) %>%
  # Filter out tiny rain events
  filter(rain_in > 0) %>%
  # Filter out extreme RC values
  filter(RC < 5) %>%
  # Convert factors and transform skewed predictors
  mutate(Field_Name = factor(Field_Name),
         Tile = factor(Tile),
         Tillage = factor(Tillage),
         Annual_Tillage = factor(Annual_Tillage,levels=c("Conventional","Reduced","No-Till","Pasture")),
         SoilType = factor(SoilType),
         Crop = factor(Crop),
         HydrologicGroup = factor(HydrologicGroup),
         log_P = log(rain_in),
         log_Dur = log(duration),
         log_I30 = log(I30),
         log_ARFdays7 = log(ARFdays7+0.1)) %>%
  mutate(Season = case_when(month(Q_start) %in% 5:9 ~"GS",
                            month(Q_start) %in% 1:4 ~ "SS",
                            month(Q_start) %in% 10:12 ~ "FS"),
         Season = factor(Season,levels = c("SS","GS","FS")))

# Fit Mixed-effect linear regression model =================
# Initialize a list to store model comparison plots for different seasons
g_model_ls <- list()
for(season in c("SS","GS","FS")){
  Q_df_season <- Q_df %>%
    filter(Season == season)
  
  # Use a common dataset to ensure all models have the same sample size
  vars_all <- c("log_RC","Field_Name","log_I30","log_Dur","log_ARFdays7",
                "Annual_Tillage","DSP","PerennialFrac","MeanSlope_per","SoilType")
  
  # Only keep target variables
  Q_df_season <- Q_df_season %>%
    select(all_of(vars_all)) %>%
    na.omit()
  
  # Precipitation-only baseline model ==========
  m0 <- MER(Q_df_season,
            vars_to_scale = c("log_I30", "log_Dur", "log_ARFdays7"),
            main_varls = c("log_I30", "log_Dur", "log_ARFdays7"),
            random_varls = c("Field_Name"),
            res_varname = "log_RC",
            model_title = paste0("RC model - ", season),
            REML = FALSE)$model
  
  # Compare model performance
  df_tmp <- data.frame(obs = Q_df_season$log_RC,
                       fit = predict(m0))
  RMSE_m0 <- round(sqrt(mean((df_tmp$obs - df_tmp$fit)^2)),2)
  g_m0 <- plot_scatter_lm(df_tmp,x="obs",y="fit",my_colors = my_color[1],label_x = -0.2,label_y=1.5)+
    labs(x = "Observed RC",y="Fitted RC")+
    ggtitle(paste(season,"P-only MER","RMSE:",RMSE_m0))
  g_model_ls[[length(g_model_ls)+1]] <- g_m0
  
  # Add agricultural management as main effects ===========
  m_ag <- MER(Q_df_season,
              vars_to_scale = c("log_I30", "log_Dur", "log_ARFdays7","DSP","PerennialFrac"),
              main_varls = c("log_I30", "log_Dur", "log_ARFdays7",
                             "Annual_Tillage","DSP","PerennialFrac"),
              random_varls = c("Field_Name"),
              res_varname = "log_RC",
              model_title = paste0("RC model - ", season),
              REML = FALSE)$model
  # Compare model performance
  df_tmp <- data.frame(obs = Q_df_season$log_RC,
                       fit = predict(m_ag))
  RMSE_ag <- round(sqrt(mean((df_tmp$obs - df_tmp$fit)^2)),2)
  g_mag <- plot_scatter_lm(df_tmp,x="obs",y="fit",my_colors = my_color[1],label_x = -0.2,label_y=1.5)+
    labs(x = "Observed RC",y="Fitted RC")+
    ggtitle(paste(season,"P + Ag MER","RMSE:",RMSE_ag))  
  g_model_ls[[length(g_model_ls)+1]] <- g_mag
  
  # Add interactions in the model ===================
  m_ag_int <- MER(Q_df_season,
                  vars_to_scale = c("log_I30","log_Dur","log_ARFdays7","DSP","PerennialFrac"),
                  main_varls = c("log_I30","log_Dur","log_ARFdays7",
                                 "Annual_Tillage","DSP","PerennialFrac"),
                  interaction_varls = c(                         
                    "Annual_Tillage:log_I30",
                    "Annual_Tillage:log_ARFdays7",
                    "PerennialFrac:log_ARFdays7",
                    "DSP:log_ARFdays7"
                  ),
                  random_varls = c("Field_Name"),
                  res_varname = "log_RC",
                  model_title = paste0("RC model - ", season),
                  REML = FALSE)$model
  # Compare model performance
  df_tmp <- data.frame(obs = Q_df_season$log_RC,
                       fit = predict(m_ag_int))
  RMSE_ag <- round(sqrt(mean((df_tmp$obs - df_tmp$fit)^2)),2)
  g_mag_int <- plot_scatter_lm(df_tmp,x="obs",y="fit",my_colors = my_color[1],label_x = -0.2,label_y=1.5)+
    labs(x = "Observed RC",y="Fitted RC")+
    ggtitle(paste(season,"P + Ag +interactions MER","RMSE:",RMSE_ag))  
  
  # Random slopes by field ======================
  m0_rs <- MER(Q_df_season,
               vars_to_scale = c("log_I30","log_Dur","log_ARFdays7"),
               main_varls = c("log_I30","log_Dur","log_ARFdays7"),
               random_varls = c("Field_Name"),
               random_slope_varls = c("log_I30","log_ARFdays7","log_Dur"),
               res_varname = "log_RC",
               model_title = paste0("RC model - ", season),
               REML = FALSE)$model
  # Compare model performance
  df_tmp <- data.frame(obs = Q_df_season$log_RC,
                       fit = predict(m0_rs))
  RMSE_ag <- round(sqrt(mean((df_tmp$obs - df_tmp$fit)^2)),2)
  g_m0_rs <- plot_scatter_lm(df_tmp,x="obs",y="fit",my_colors = my_color[1],label_x = -0.2,label_y=1.5)+
    labs(x = "Observed RC",y="Fitted RC")+
    ggtitle(paste(season,"P + Ag +interactions MER","RMSE:",RMSE_ag))  
  
  # Compare model_performance
  performance::r2_nakagawa(m0)
  performance::r2_nakagawa(m_ag)
  performance::r2_nakagawa(m_ag_int)
  performance::r2_nakagawa(m0_rs)
  
  anova(m0, m_ag)
  anova(m_ag, m_ag_int)
  anova(m0, m0_rs)
  
  
  
  # Partition random effect ================
  m0_re <- ranef(m0)$Field_Name %>%
    as.data.frame() %>%
    rownames_to_column(var = "Field_Name") %>%
    rename(random_effect = "(Intercept)") %>%
    left_join(Q_df %>%
            select(Field_Name,DrainageClass,SoilType,MeanSlope_per,Clay_Fraction) %>%
              distinct(),by="Field_Name")
  #mag_re <- ranef(m_ag)$Field_Name
  
  ggplot(m0_re,aes(x=SoilType,y=random_effect))+
    geom_boxplot()+
    my_theme2
  
  ggplot(m0_re,aes(x=MeanSlope_per,random_effect))+
    geom_point()+
    my_theme2
  
  ggplot(m0_re,aes(x=MeanSlope_per,y=Clay_Fraction,color=random_effect))+
    geom_point(size=4)+
    my_theme2+
    theme(legend.position = 'right')+
    scale_color_distiller(palette = "RdYlBu")

}

# Combine these plots
g_model <- plot_grid(plotlist = g_model_ls,ncol=2,align="hv")

print_g(g_model,"Model_comparison",9,12)



