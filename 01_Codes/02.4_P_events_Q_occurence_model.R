# Author: Zhaozhe Chen (zhaozhe.chen@wisc.edu)
# Date: 2025.1.22

# This code is to model Q occurrence (when P produced Q or not, focusing on non-frozen events only)
# Question to answer: What controls whether a precipitation event produces runoff across Wisconsin farms?

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

# Data path =======
# Joint non-frozen P events
P_df_all <- read.csv("00_Data/Processed_data_v2/Non-Frozen_P_joint_df.csv")

# Source functions
source("01_Codes/Plotting_functions.R")
source("01_Codes/Analyses_functions.R")

# Colors for plotting 
my_color <- RColorBrewer::brewer.pal(7,"Set2")

# Decide if only focus on Surface monitoring sites and filter out Tile sites
Tile_Y <- "NOTile"

Output_path <- "D:/OneDrive - UW-Madison/Research/Discovery Farms/DF Runoff Generation/Results/Q_occurrence2/"
  
# ------- Main -------
# Initialize a list to store plots
g_ls <- list()
# Initialize a list to store AIC results
AIC_ls <- list()

# Loop over seasons
for(season in c("SS","GS","FS")){
  # Figure main name
  g_name <- paste0(Tile_Y,"_",season)
  
  # Preprocessing P dataset ====================================
  P_df <- P_df_all
  
  # If Tile_Y is TRUE, keep Tile sites, otherwise, filter them out
  if(Tile_Y == "NOTile"){
    P_df <- P_df %>%
      filter(Monitoring == "Surface")
  }
  
  # Trunk to the target season
  if(season == "GS"){
    # Growing season: May - Sep
    P_df <- P_df %>%
      filter(month(P_df$P_start) < 10 & month(P_df$P_start) > 4)
  }else if(season == "SS"){
    # Spring shoulder season: Jan - April
    P_df <- P_df %>%
      filter(month(P_df$P_start) <  5)
  }else if(season == "FS"){
    # Fall shoulder season: Oct - Dec
    P_df <- P_df %>%
      filter(month(P_df$P_start) > 9)
  }
  
  P_df <- P_df %>%
    # Q occurrence or not is the response
    mutate(Q_Occurred = as.integer(Associated_Q == TRUE)) %>%
    # Convert factors and transform skewed predictors
    mutate(Field_Name = factor(Field_Name),
           Tile = factor(Tile),
           Tillage = factor(Tillage),
           Annual_Tillage = factor(Annual_Tillage,levels=c("Conventional","Reduced","No-Till","Pasture")),
           Associated_Q = factor(Associated_Q,levels = c("TRUE","FALSE")),
           SoilType = factor(SoilType),
           Crop = factor(Crop),
           HydrologicGroup = factor(HydrologicGroup),
           log_P = log(rain),
           log_Dur = log(duration),
           log_I30 = log(I30),
           log_ARFdays7 = log(ARFdays7+0.1))

  # Fit Mixed-effects logistic regression model ==========================
  # Use a common dataset to ensure all models have the same sample size
  vars_all <- c("Q_Occurred","Field_Name","log_I30","log_Dur","log_ARFdays7",
                "Annual_Tillage","Tile","DSP","PerennialFrac")
  P_df_common <- P_df %>%
    select(all_of(vars_all)) %>%
    na.omit()
  
  # Rainfall-only baseline model --------------------------
  # logit(P(Q=1)) = b0 + b1log(I30) + b2log(Duration) + b3log(ARF7) + (1|Site)
  # How much variance rainfall alone explains
  MELR_result0 <- MELR(P_df_common,vars_to_scale = c("log_I30","log_Dur","log_ARFdays7"),main_varls = c("log_I30","log_Dur","log_ARFdays7"),
                       random_varls = "Field_Name",res_varname = "Q_Occurred",model_title = paste(season,"Rainfall-only"))
  
  # Add agricultural management as main effects -----------------
  # logit(P(Q=1)) = b0 + b1log(I30) + b2log(Duration) + b3log(ARF7) + (1|Site) + b4Tillage + b5 PerennialFrac + b6DSP
  MELR_result_ag <- MELR(P_df_common,vars_to_scale = c("log_I30","log_Dur","log_ARFdays7","DSP","PerennialFrac"),
                         main_varls = c("log_I30","log_Dur","log_ARFdays7","Annual_Tillage","DSP","PerennialFrac"),
                         random_varls = "Field_Name",res_varname = "Q_Occurred",
                         model_title = paste(season,"Rainfall+Agricultural"))
  
  # Store model validation figures
  g_ls[[season]] <- plot_grid(MELR_result0$g,MELR_result_ag$g,align="hv",nrow=2)
  
  # Calculate AIC
  AIC0 <- AIC(MELR_result0$model)
  AIC_ag <- AIC(MELR_result_ag$model)

  # Also compare models, ANOVA
  LRT <- anova(MELR_result0$model,MELR_result_ag$model,test="Chisq")
  Chisq <- LRT$Chisq[2]
  p_value <- LRT$`Pr(>Chisq)`[2]
  
  # Put these into data frame
  AIC_df <- data.frame(Season = season,
                       n = nrow(P_df_common),
                       AIC0 = AIC0,
                       AIC_ag = AIC_ag,
                       Delta_AIC = AIC0 - AIC_ag,
                       Chisq = Chisq,
                       p_value = p_value)    
  AIC_ls[[season]] <- AIC_df
  
  message("Complete ",season)


# Combine all figures
g_all_models <- plot_grid(plotlist = g_ls,nrow=1,align="v")
# Combine metrics
AIC_df <- bind_rows(AIC_ls)

ggplot(AIC_df, aes(x = Season, y = Chisq)) +
  geom_col(fill = "grey70", color = "black") +
  geom_text(aes(label = round(Delta_AIC, 1)), vjust = -0.4, size = 5) +
  labs(x = "", y = expression(Delta*AIC~"(AIC_rain - AIC_rain+ag)")) +
  my_theme2 +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))






  

