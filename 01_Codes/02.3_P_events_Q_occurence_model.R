# Author: Zhaozhe Chen (zhaozhe.chen@wisc.edu)
# Date: 2025.1.20

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
add_quantile_bin <- function(df, var, new_var = NULL, probs = c(0, 0.25, 0.5, 0.75, 1)) {
  if (is.null(new_var)) new_var <- paste0(var, "_bin")
  
  x <- df[[var]]
  qs <- stats::quantile(x, probs = probs, na.rm = TRUE, type = 7)
  
  # If quantile breakpoints repeat, cut() can't form the requested bins
  if (length(unique(qs)) < length(qs)) {
    stop(
      paste0(
        "Cannot create quantile bins for '", var, "': quantile breakpoints are not unique.\n",
        "This usually happens when the variable has many identical values.\n",
        "Quantiles: ", paste(names(qs), round(qs, 6), sep = "=", collapse = ", ")
      )
    )
  }
  
  # Labels like Q1-Q4 (based on number of intervals)
  n_bins <- length(probs) - 1
  labs <- paste0("Q", seq_len(n_bins))
  
  df[[new_var]] <- cut(
    x,
    breaks = qs,
    include.lowest = TRUE,
    right = TRUE,
    labels = labs
  )
  
  df[[new_var]] <- factor(df[[new_var]], levels = labs)
  return(df)
}
# ------- Main -------
# Preprocessing P dataset ====================================
P_df <- P_df %>%
  # Q occurrence or not is the response
  mutate(Q_Occurred = as.integer(Associated_Q == TRUE)) %>%
  # Convert factors and transform skewed predictors
  mutate(Field_Name = factor(Field_Name),
         Tile = factor(Tile),
         Tillage = factor(Tillage),
         Annual_Tillage = factor(Annual_Tillage,levels=c("Conventional","Reduced","No-Till","Pasture")),
         SoilType = factor(SoilType),
         Crop = factor(Crop),
         HydrologicGroup = factor(HydrologicGroup),
         log_P = log(rain),
         log_Dur = log(duration),
         log_I30 = log(I30))

# Standardize continuous predictors
P_df_stand <- P_df
vars_to_scale <- c("log_P","log_Dur","log_I30",
                   "ARFdays7","MeanSlope_per","DSP","PerennialFrac")
P_df_stand[vars_to_scale] <- scale(P_df_stand[vars_to_scale])

# Explore the dataset ==============================
# Target explanatory variable on the x axis
x_varname <- "I30"
x_title <- "I30"
P_df_tmp <- P_df

# Distributions of continuous explanatory variables
g_hist <- Dist_bar(P_df_tmp,x_varname,x_title)
# Convert the continuous data into quantiles
P_df_tmp <- add_quantile_bin(P_df_tmp,var = x_varname)
# Number of P events in each quantile bin
g_nP_bin <- plot_Pcount_by_bin(P_df_tmp,bin_var = paste0(x_varname,"_bin"),xtitle = x_title)
# Number of Q events in each quantile bin
g_nQ_bin <- plot_Qcount_by_bin(P_df_tmp,bin_var = paste0(x_varname,"_bin"),xtitle = x_title)
# Percentage of Q events in each quantile bin
g_PQ_bin <- plot_Qprob(P_df_tmp,varname1 = paste0(x_varname,"_bin"),varname2 = "Q_Occurred",vargroup = "P_frozen",
           xtitle=x_varname,ytitle = "Probability of Q Occurrence","",mycolor=my_color[1])+
  theme(legend.position = "none")
# Total Q depth in each quantile bin








# Correlations, and scatter plots among continuous explanatory variables, to see if there is any obvious relationship ----



# Probability of Q occurrence across different groups ---------------------------

# Number of Q events across different P levels


# Total Q depth across different P levels


# Grouped by Tillage (Yes or No) -=-=-=-=-=-=-=-=-=
# Q occurrence across I30
g_pQ_1 <- plot_Qprob(P_df,varname1 = "I30_bin",varname2 = "Q_Occurred",vargroup = "Tillage",
                     xtitle = "I30 Level",ytitle = "P(Q generated)",grouptitle = "Tillage",mycolor = my_color[c(1,2)])
# Q occurrence across P depth
g_pQ_2 <- plot_Qprob(P_df,varname1 = "P_bin",varname2 = "Q_Occurred",vargroup = "Tillage",
                     xtitle = "P Depth Level",ytitle = "P(Q generated)",grouptitle = "Tillage",mycolor = my_color[c(1,2)])
# Q occurrence across P duration
g_pQ_3 <- plot_Qprob(P_df,varname1 = "Dur_bin",varname2 = "Q_Occurred",vargroup = "Tillage",
                     xtitle = "P Duration Level",ytitle = "P(Q generated)",grouptitle = "Tillage",mycolor = my_color[c(1,2)])
# Q occurrence across P ARFdays7
g_pQ_4 <- plot_Qprob(P_df,varname1 = "ARFdays7_bin",varname2 = "Q_Occurred",vargroup = "Tillage",
                     xtitle = "Antecendent P Level",ytitle = "P(Q generated)",grouptitle = "Tillage",mycolor = my_color[c(1,2)])
# Combine these plots together
g_PQ <- plot_grid(g_pQ_1,g_pQ_2,g_pQ_3,g_pQ_4,ncol=2,align="hv")





# Q occurrence across I30
g_pQ_1 <- plot_Qprob(P_df,varname1 = "I30_bin",varname2 = "Q_Occurred",vargroup = "Tillage",
                    xtitle = "I30 Level",ytitle = "P(Q generated)",grouptitle = "",mycolor = my_color[c(1,2,3,4)])
# Q occurrence across P depth
g_pQ_2 <- plot_Qprob(P_df,varname1 = "P_bin",varname2 = "Q_Occurred",vargroup = "Tillage",
                    xtitle = "P Depth Level",ytitle = "P(Q generated)",grouptitle = "",mycolor = my_color[c(1,2,3,4)])
# Q occurrence across P duration
g_pQ_3 <- plot_Qprob(P_df,varname1 = "Dur_bin",varname2 = "Q_Occurred",vargroup = "Tillage",
                    xtitle = "P Duration Level",ytitle = "P(Q generated)",grouptitle = "",mycolor = my_color[c(1,2,3,4)])
# Q occurrence across P ARFdays7
g_pQ_4 <- plot_Qprob(P_df,varname1 = "ARFdays7_bin",varname2 = "Q_Occurred",vargroup = "Tillage",
                            xtitle = "Antecendent P Level",ytitle = "P(Q generated)",grouptitle = "",mycolor = my_color[c(1,2,3,4)])
# Combine these plots
g_Tillage <- plot_grid(g_pQ_Tillage1,g_pQ_Tillage2,g_pQ_Tillage3,g_pQ_Tillage4,
                       ncol=2,align = "hv")

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
  

  

