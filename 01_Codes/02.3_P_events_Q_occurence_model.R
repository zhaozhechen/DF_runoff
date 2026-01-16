# Author: Zhaozhe Chen (zhaozhe.chen@wisc.edu)
# Date: 2025.1.16

# This code is to model Q occurrence (when P produced Q or not, focusing on non-frozen events only)
# Question to answer: What controls whether a precipitation event produces runoff across Wisconsin farms?

# ---------- Global -----------
library(stringr)
library(dplyr)
library(lubridate)
library(cowplot)
library(lme4)
library(GGally)

# Data path =======
# Joint non-frozen P events
P_df <- read.csv("00_Data/Processed_data_v2/Non-Frozen_P_joint_df.csv")

# Source functions
source("01_Codes/Plotting_functions.R")

# Colors for plotting 
my_color <- RColorBrewer::brewer.pal(7,"Set2")

# ------- Functions ---------
# Function to create 4 quantile bins
quantile_bin <- function(x, n = 4) {
  cut(
    x,
    breaks = quantile(x, probs = seq(0, 1, length.out = n + 1), na.rm = TRUE),
    include.lowest = TRUE,
    labels = paste0("Q", 1:n)
  )
}

# ------- Main -------
# Preprocessing
P_df <- P_df %>%
  # Q occurrence or not is the response
  mutate(Q_Occurred = as.integer(Associated_Q == TRUE)) %>%
  # Convert factors and transform skewed predictors
  mutate(Field_Name = factor(Field_Name),
         Tile = factor(Tile),
         Tillage = factor(Tillage,levels=c("Conventional","Reduced","No-Till","Pasture")),
         SoilType = factor(SoilType),
         Crop = factor(Crop),
         HydrologicGroup = factor(HydrologicGroup),
         log_P = log(rain),
         log_Dur = log(duration),
         log_I30 = log(I30))

# Bin P depth, intensity, duration, etc.
P_df <- P_df %>%
  mutate(
    Dur_bin = quantile_bin(duration),
    P_bin = quantile_bin(rain),
    I30_bin = quantile_bin(I30),
    ARFdays7_bin = quantile_bin(ARFdays7),
    Slope_bin = quantile_bin(MeanSlope_per)
  )

# Standardize continuous predictors
P_df_stand <- P_df
vars_to_scale <- c("log_P","log_Dur","log_I30",
                   "ARFdays7","MeanSlope_per","DSP","PerennialFrac")
P_df_stand[vars_to_scale] <- scale(P_df_stand[vars_to_scale])

# Explore the dataset =============

# Distributions of continuous explanatory variables -------



# Correlations, and scatter plots among continuous explanatory variables, to see if there is any obvious relationship ----



# Probability of Q occurrence across different groups ------
# Q occurrence across I30
g_pQ_Tillage1 <- plot_Qprob(P_df,varname1 = "I30_bin",varname2 = "Q_Occurred",vargroup = "Tillage",
                    xtitle = "I30 Level",ytitle = "P(Q generated)",grouptitle = "",mycolor = my_color[c(1,2,3,4)])
# Q occurrence across P depth
g_pQ_Tillage2 <- plot_Qprob(P_df,varname1 = "P_bin",varname2 = "Q_Occurred",vargroup = "Tillage",
                    xtitle = "P Depth Level",ytitle = "P(Q generated)",grouptitle = "",mycolor = my_color[c(1,2,3,4)])
# Q occurrence across P duration
g_pQ_Tillage3 <- plot_Qprob(P_df,varname1 = "Dur_bin",varname2 = "Q_Occurred",vargroup = "Tillage",
                    xtitle = "P Duration Level",ytitle = "P(Q generated)",grouptitle = "",mycolor = my_color[c(1,2,3,4)])
# Q occurrence across P ARFdays7
g_pQ_Tillage4 <- plot_Qprob(P_df,varname1 = "ARFdays7_bin",varname2 = "Q_Occurred",vargroup = "Tillage",
                            xtitle = "Antecendent P Level",ytitle = "P(Q generated)",grouptitle = "",mycolor = my_color[c(1,2,3,4)])


# Revise below!!!!!!!!!!!!!!!!!!!!!


# Q occurrence across I30
g_pQ_Tillage4 <- plot_Qprob(P_df,varname1 = "I30_bin",varname2 = "Q_Occurred",vargroup = "Tile",
                    xtitle = "I30 Level",ytitle = "P(Q generated)",grouptitle = "Tile",mycolor = my_color[c(1,2,3,4)])
g_pQ5 <- plot_Qprob(P_df,varname1 = "P_bin",varname2 = "Q_Occurred",vargroup = "Tile",
                    xtitle = "P Depth Level",ytitle = "P(Q generated)",grouptitle = "Tile",mycolor = my_color[c(1,2,3,4)])
g_pQ6 <- plot_Qprob(P_df,varname1 = "Dur_bin",varname2 = "Q_Occurred",vargroup = "Tile",
                    xtitle = "P Duration Level",ytitle = "P(Q generated)",grouptitle = "Tile",mycolor = my_color[c(1,2,3,4)])




ggplot(P_df, aes(x = rain, y = Q_Occurred)) +
  geom_point(alpha = 0.08) +
  stat_summary_bin(fun = mean, bins = 25, geom = "line") +
  labs(y = "P(runoff)", x = "Rain depth")




  
if(FALSE){
  # Fit Mixed-effects logistic regression model
  Q_occ_model <- glmer(
    Q_Occurred ~
      log_P +
      log_Dur +
      log_I30 +
      ARFdays7 +
      MeanSlope_per +
      Clay_Fraction +
      HydrologicGroup +
      Tile +
      Tillage +
      DSP +
      PerennialFrac +
      (1 | Field_Name),
    data = P_df,
    family = binomial,
    control = glmerControl(optimizer = "bobyqa")
  )
  
}  
  

  

