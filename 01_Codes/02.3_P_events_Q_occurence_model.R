# Author: Zhaozhe Chen (zhaozhe.chen@wisc.edu)
# Date: 2025.1.22

# This code is to model Q occurrence (when P produced Q or not, focusing on non-frozen events only)
# Question to answer: What controls whether a precipitation event produces runoff across Wisconsin farms?

# ---------- Global -----------
library(stringr)
library(dplyr)
library(lubridate)
library(cowplot)
library(lme4)
library(GGally)
library(ggeffects)

# Data path =======
# Joint non-frozen P events
P_df <- read.csv("00_Data/Processed_data_v2/Non-Frozen_P_joint_df.csv")

# Source functions
source("01_Codes/Plotting_functions.R")
source("01_Codes/Analyses_functions.R")

# Colors for plotting 
my_color <- RColorBrewer::brewer.pal(7,"Set2")

# Decide if only focus on Surface monitoring sites and filter out Tile sites
Tile_Y <- "NOTile"

# Determine the time period to analyze
# Options include 
# All
# Growing season (GS): May - Sep 
# Spring shoulder (SS): Jan - April 
# Fall shoulder (FS): Oct - Dec
season <- "GS"

# Figure main name
g_name <- paste0(Tile_Y,"_",season)

Output_path <- "D:/OneDrive - UW-Madison/Research/Discovery Farms/DF Runoff Generation/Results/Q_occurrence/"

# ------- Main -------
# Preprocessing P dataset ====================================
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

# Explore the dataset ==============================
# List of target explanatory variables
x_varname_ls <- c("I30","rain","duration","ARFdays7")
x_title_ls <- c("I30","P depth","P duration","ARFdays7")
for(i in 1:length(x_varname_ls)){
  x_varname <- x_varname_ls[i]
  x_title <- x_title_ls[i]
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
                         xtitle=x_title,ytitle = "P(Q Occurrence)","",mycolor=my_color[1])+
    theme(legend.position = "none")
  # Total Q depth in each quantile bin
  g_dQ_bin <- plot_Qdepth_by_bin(P_df_tmp,bin_var = paste0(x_varname,"_bin"),xtitle = x_title)
  # Probability of Q occurrence across continuous explanotary variable
  g_PQ_continuous <- plot_Qoccurence_x(P_df_tmp,x_varname = x_varname,xtitle = x_title,linecolor = my_color[1])
  # Probability of Q occurrence, grouped by Tillage (Yes or No)
  g_PQ_tillage <- plot_Qprob(P_df_tmp,varname1 = paste0(x_varname,"_bin"),varname2 = "Q_Occurred",vargroup = "Tillage",
                             xtitle = x_title,ytitle = "P(Q Occurrence)",grouptitle = "Tillage",mycolor = my_color[c(1,2)])
  # Probability of Q occurrence, grouped by Annual Tillage
  g_PQ_annual_tillage <- plot_Qprob(P_df_tmp,varname1 = paste0(x_varname,"_bin"),varname2 = "Q_Occurred",vargroup = "Annual_Tillage",
                                    xtitle = x_title,ytitle = "P(Q Occurrence)",grouptitle = "",mycolor = my_color[c(1,2,3,4)])+
    theme(legend.position = "bottom")
  # Probability of Q occurrence, grouped by Tile
  g_PQ_tile <- plot_Qprob(P_df_tmp,varname1 = paste0(x_varname,"_bin"),varname2 = "Q_Occurred",vargroup = "Tile",
                          xtitle = x_title,ytitle = "P(Q Occurrence)",grouptitle = "Tile",mycolor = my_color[c(1,2)])
  # Probability of Q occurrence, grouped by Monitoring
  g_PQ_Monitoring <- plot_Qprob(P_df_tmp,varname1 = paste0(x_varname,"_bin"),varname2 = "Q_Occurred",vargroup = "Monitoring",
                                xtitle = x_title,ytitle = "P(Q Occurrence)",grouptitle = "Monitoring",mycolor = my_color[c(3,4)])
  
  # Combine these plots together
  g_all <- plot_grid(g_hist,g_nP_bin,g_nQ_bin,g_PQ_bin,g_dQ_bin,
                     g_PQ_continuous,g_PQ_tile,g_PQ_Monitoring,g_PQ_tillage,g_PQ_annual_tillage,align="hv")
  
  # Output this figure
  print_g(g_all,paste0("Q_occurence_",x_varname,"_",g_name),15,10)
}

# Correlations, and scatter plots among continuous explanatory variables, to see if there is any obvious relationship ----
df_CM <- P_df %>%
  select(log_I30,log_Dur,log_P,log_ARFdays7,DSP)
g_CM <- ggpairs(df_CM)

# Fit Mixed-effects logistic regression model ==========================
# Rainfall-only baseline model --------------------------
# logit(P(Q=1)) = b0 + b1log(I30) + b2log(Duration) + b3log(ARF7) + (1|Site)
# How much variance rainfall alone explains
# Standardize continuous predictors
P_df_base <- P_df
vars_to_scale <- c("log_I30","log_Dur","log_ARFdays7")
P_df_base[vars_to_scale] <- scale(P_df_base[vars_to_scale])
P_df_base <- P_df_base %>%
  select(Field_Name,Q_Occurred,log_I30,log_Dur,log_ARFdays7) %>%
  na.omit()
# Fit the baseline model
model0 <- glmer(Q_Occurred ~ log_I30 + log_Dur + log_ARFdays7 + (1|Field_Name),
                data = P_df_base,
                family = binomial(link = "logit"),
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

# Compare modeled response vs observations
g_m0 <- compare_model(P_df_base,model0,var_res="Q_Occurred")
# Check marginal effect of each variable
# Names for x axis
x_title_ls <- c("Log I30 (standardized)","Log Duration (standardized)","Log ARF7 (standardized)")
# Initialize a list to store figures
g_ls <- list()
for(i in 1:length(vars_to_scale)){
  varname <- vars_to_scale[i]
  x_title <- x_title_ls[i]
  g <- marginal_plot(P_df_base,model0,varname,"Q_Occurred",x_title = x_title,y_title = "P(Q Occurrence)")
  g_ls[[i]] <- g
}
# Combine all plots
g0_all <- plot_grid(g_m0,g_ls[[1]],g_ls[[2]],g_ls[[3]],nrow=1,align="hv")
# Output this figure
print_g(g0_all,paste0("MELR0_",g_name,"_",GS),16,4)

# Add agricultural management as main effects -----------------
# logit(P(Q=1)) = b0 + b1log(I30) + b2log(Duration) + b3log(ARF7) + (1|Site) + b4Tillage + b5 PerennialFrac + b6Tile + b7DSP
P_df_ag <- P_df
vars_to_scale <- c("log_I30","log_Dur","log_ARFdays7","DSP","PerennialFrac")
P_df_ag[vars_to_scale] <- scale(P_df_ag[vars_to_scale])
P_df_ag <- P_df_ag %>%
  select(Field_Name,Q_Occurred,log_I30,log_Dur,log_ARFdays7,
         Annual_Tillage,Tile,DSP,PerennialFrac) %>%
  na.omit()
# Fit the baseline model
model_ag <- glmer(Q_Occurred ~ log_I30 + log_Dur + log_ARFdays7 + 
                    Annual_Tillage + Tile + DSP + PerennialFrac+(1|Field_Name),
                  data = P_df_ag,
                  family = binomial(link = "logit"),
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

# Compare modeled response vs observations
g_m_ag <- compare_model(P_df_ag,model_ag,var_res="Q_Occurred")

# Check marginal effect of each variable
var_ls <- c("log_I30","log_Dur","log_ARFdays7","DSP","PerennialFrac")
# Names for x axis
x_title_ls <- c("Log I30 (standardized)","Log Duration (standardized)","Log ARF7 (standardized)","Date since planting","Perennial Fraction")
# Initialize a list to store figures
g_ls <- list()
for(i in 1:length(var_ls)){
  varname <- var_ls[i]
  x_title <- x_title_ls[i]
  g <- marginal_plot(P_df_ag,model_ag,varname,"Q_Occurred",x_title = x_title,y_title = "P(Q Occurrence)")
  g_ls[[i]] <- g
}
# Combine all plots
g_ag_all <- plot_grid(plotlist = g_ls,align="hv")
# Output this figure
#print_g(g0_all,paste0("MELR0_",g_name,"_",GS),16,4)

# Add interactions
# Add log(I30) x Tillage
# logit(P(Q=1)) = b0 + b1log(I30) + b2log(Duration) + b3log(ARF7) + (1|Site) + b4Tillage + b5 PerennialFrac + b6Tile + b7DSP



# Site properties ----------------
P_df_ag <- P_df
vars_to_scale <- c("log_I30","log_Dur","log_ARFdays7","Clay_Fraction","MeanSlope_per")
P_df_ag[vars_to_scale] <- scale(P_df_ag[vars_to_scale])
P_df_ag <- P_df_ag %>%
  select(Field_Name,Q_Occurred,log_I30,log_Dur,log_ARFdays7,
         Clay_Fraction,MeanSlope_per) %>%
  na.omit()
# Fit the baseline model
model_ag <- glmer(Q_Occurred ~ log_I30 + log_Dur + log_ARFdays7 + 
                    Clay_Fraction+MeanSlope_per+(1|Field_Name),
                  data = P_df_ag,
                  family = binomial(link = "logit"),
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

# Compare modeled response vs observations
g_m_ag <- compare_model(P_df_ag,model_ag,var_res="Q_Occurred")

# Check marginal effect of each variable
var_ls <- c("log_I30","log_Dur","log_ARFdays7","Clay_Fraction","MeanSlope_per")
# Names for x axis
x_title_ls <- c("Log I30 (standardized)","Log Duration (standardized)","Log ARF7 (standardized)","Clay Fraction","Slope")
# Initialize a list to store figures
g_ls <- list()
for(i in 1:length(var_ls)){
  varname <- var_ls[i]
  x_title <- x_title_ls[i]
  g <- marginal_plot(P_df_ag,model_ag,varname,"Q_Occurred",x_title = x_title,y_title = "P(Q Occurrence)")
  g_ls[[i]] <- g
}
# Combine all plots
g_ag_all <- plot_grid(plotlist = g_ls,align="hv")





# Response surface of Q occurrence vs two P characteristics ==================
# Loop over different tillage
# Full dataset
df_tmp <- P_df
g1 <- scatter_Q_occurence(df_tmp,"log_I30","log_Dur",mycolor=my_color[c(1,2)],x_title = "Log I30",y_title = "Log Duration")
g2 <- scatter_Q_occurence(df_tmp,"log_I30","log_ARFdays7",mycolor=my_color[c(1,2)],x_title = "Log I30",y_title = "Log ARFdays7")
g3 <- scatter_Q_occurence(df_tmp,"log_ARFdays7","log_Dur",mycolor=my_color[c(1,2)],x_title = "Log ARFdays7",y_title = "Log Duration")

# Conventional only
df_tmp <- P_df %>%
  filter(Annual_Tillage == "Conventional")
g4 <- scatter_Q_occurence(df_tmp,"log_I30","log_Dur",mycolor=my_color[c(1,2)],x_title = "Log I30",y_title = "Log Duration")
g5 <- scatter_Q_occurence(df_tmp,"log_I30","log_ARFdays7",mycolor=my_color[c(1,2)],x_title = "Log I30",y_title = "Log ARFdays7")
g6 <- scatter_Q_occurence(df_tmp,"log_ARFdays7","log_Dur",mycolor=my_color[c(1,2)],x_title = "Log ARFdays7",y_title = "Log Duration")

# Reduced only
df_tmp <- P_df %>%
  filter(Annual_Tillage == "Reduced")
g7 <- scatter_Q_occurence(df_tmp,"log_I30","log_Dur",mycolor=my_color[c(1,2)],x_title = "Log I30",y_title = "Log Duration")
g8 <- scatter_Q_occurence(df_tmp,"log_I30","log_ARFdays7",mycolor=my_color[c(1,2)],x_title = "Log I30",y_title = "Log ARFdays7")
g9 <- scatter_Q_occurence(df_tmp,"log_ARFdays7","log_Dur",mycolor=my_color[c(1,2)],x_title = "Log ARFdays7",y_title = "Log Duration")

# No-Till only
df_tmp <- P_df %>%
  filter(Annual_Tillage == "No-Till")
g10 <- scatter_Q_occurence(df_tmp,"log_I30","log_Dur",mycolor=my_color[c(1,2)],x_title = "Log I30",y_title = "Log Duration")
g11 <- scatter_Q_occurence(df_tmp,"log_I30","log_ARFdays7",mycolor=my_color[c(1,2)],x_title = "Log I30",y_title = "Log ARFdays7")
g12 <- scatter_Q_occurence(df_tmp,"log_ARFdays7","log_Dur",mycolor=my_color[c(1,2)],x_title = "Log ARFdays7",y_title = "Log Duration")

# Pasture only
df_tmp <- P_df %>%
  filter(Annual_Tillage == "Pasture")
g13 <- scatter_Q_occurence(df_tmp,"log_I30","log_Dur",mycolor=my_color[c(1,2)],x_title = "Log I30",y_title = "Log Duration")
g14 <- scatter_Q_occurence(df_tmp,"log_I30","log_ARFdays7",mycolor=my_color[c(1,2)],x_title = "Log I30",y_title = "Log ARFdays7")
g15 <- scatter_Q_occurence(df_tmp,"log_ARFdays7","log_Dur",mycolor=my_color[c(1,2)],x_title = "Log ARFdays7",y_title = "Log Duration")

row_title <- function(text) {
  cowplot::ggdraw() +
    cowplot::draw_label(
      text,
      fontface = "bold",
      size = 14,
      angle = 90,
      x = 0.5,
      y = 0.5
    )
}

# All data
row1 <- plot_grid(g1, g2, g3, ncol = 3, align = "hv")
row1_labeled <- plot_grid(row_title("All sites"), row1, ncol = 2, rel_widths = c(0.05, 1))

# Conventional
row2 <- plot_grid(g4, g5, g6, ncol = 3, align = "hv")
row2_labeled <- plot_grid(row_title("Conventional"), row2, ncol = 2, rel_widths = c(0.05, 1))

# Reduced
row3 <- plot_grid(g7, g8, g9, ncol = 3, align = "hv")
row3_labeled <- plot_grid(row_title("Reduced"), row3, ncol = 2, rel_widths = c(0.05, 1))

# No-Till
row4 <- plot_grid(g10, g11, g12, ncol = 3, align = "hv")
row4_labeled <- plot_grid(row_title("No-Till"), row4, ncol = 2, rel_widths = c(0.05, 1))

# Pasture
row5 <- plot_grid(g13, g14, g15, ncol = 3, align = "hv")
row5_labeled <- plot_grid(row_title("Pasture"), row5, ncol = 2, rel_widths = c(0.05, 1))

g_all <- plot_grid(
  row1_labeled,
  row2_labeled,
  row3_labeled,
  row4_labeled,
  row5_labeled,
  ncol = 1,
  align = "v"
)


  

