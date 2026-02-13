# Author: Zhaozhe Chen (zhaozhe.chen@wisc.edu)
# Date: 2026.1.22

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
library(pROC)

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

set.seed(123)
# Sample size per season
n_sample <- 2000
# Number of random sampling
n_rep <- 50

# Agricultural practices to test
ag_vars <- c("Annual_Tillage","DSP","PerennialFrac")

Output_path <- "D:/OneDrive - UW-Madison/Research/Discovery Farms/DF Runoff Generation/Results/Q_occurrence_modeling/"
  
# ------- Main -------
# Preprocessing P dataset ====================================
# If Tile_Y is TRUE, keep Tile sites, otherwise, filter them out
if(Tile_Y == "NOTile"){
  P_df <- P_df %>%
    filter(Monitoring == "Surface")
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
         log_ARFdays7 = log(ARFdays7+0.1)) %>%
  mutate(Season = case_when(month(P_start) %in% 5:9 ~"GS",
                            month(P_start) %in% 1:4 ~ "SS",
                            month(P_start) %in% 10:12 ~ "FS"),
         Season = factor(Season,levels = c("SS","GS","FS")))

# Fit Mixed-effects logistic regression model ==========================
# Initialize a list to store replication results
res_ls <- list()
# Initialize a list to store replication results for dropped ag models
drop_ls <- list()
# Loop over seasons
for(season in c("SS","GS","FS")){
  # Only keep dataset for this season
  P_df_season <- P_df %>%
    filter(Season == season)
  
  # Use a common dataset to ensure all models have the same sample size
  vars_all <- c("Q_Occurred","Field_Name","log_I30","log_Dur","log_ARFdays7",
                "Annual_Tillage","DSP","PerennialFrac")
  
  # Only keep target variables
  P_df_season <- P_df_season %>%
    select(all_of(vars_all)) %>%
    na.omit()
  
  # Loop n_rep times, sample the same number for each season to keep balanced sample size
  for(r in 1:n_rep){
    # Random sample with equal n
    P_df_common <- P_df_season %>%
      slice_sample(n=n_sample)
    
    # Rainfall-only baseline model
    # logit(P(Q=1)) = b0 + b1log(I30) + b2log(Duration) + b3log(ARF7) + (1|Site)
    # How much variance rainfall alone explains
    m0 <- MELR(P_df_common,vars_to_scale = c("log_I30","log_Dur","log_ARFdays7"),main_varls = c("log_I30","log_Dur","log_ARFdays7"),
               random_varls = "Field_Name",res_varname = "Q_Occurred",model_title = paste(season,"Rainfall-only"))$model
    
    # Add agricultural management as main effects
    # logit(P(Q=1)) = b0 + b1log(I30) + b2log(Duration) + b3log(ARF7) + (1|Site) + b4Tillage + b5 PerennialFrac + b6DSP
    m_ag <- MELR(P_df_common,vars_to_scale = c("log_I30","log_Dur","log_ARFdays7","DSP","PerennialFrac"),
                 main_varls = c("log_I30","log_Dur","log_ARFdays7","Annual_Tillage","DSP","PerennialFrac"),
                 random_varls = "Field_Name",res_varname = "Q_Occurred",
                 model_title = paste(season,"Rainfall+Agricultural"))$model
    
    # Calculate AIC
    AIC0 <- AIC(m0)
    AIC_ag <- AIC(m_ag)
    
    # Also compare models, ANOVA
    LRT <- anova(m0,m_ag,test="Chisq")
    Chisq <- LRT$Chisq[2]
    p_value <- LRT$`Pr(>Chisq)`[2]
    
    # Calculate AUC
    AUC0 <- get_auc_glmer(m0,P_df_common,res_var = "Q_Occurred")
    AUC_ag <- get_auc_glmer(m_ag,P_df_common,res_var = "Q_Occurred")
    
    # Put these results into data frame
    res_ls[[length(res_ls)+1]] <- data.frame(Season = season,
                                             n = nrow(P_df_common),
                                             AIC0 = AIC0,
                                             AIC_ag = AIC_ag,
                                             Delta_AIC = AIC0 - AIC_ag,
                                             Chisq = Chisq,
                                             p_value = p_value,
                                             AUC0 = AUC0,
                                             AUC_ag = AUC_ag,
                                             Delta_AUC = AUC_ag-AUC0)    
    
    # Loop over agricultural practices, drop one variable at a time
    # Compare reduced model with full model
    for(v_drop in ag_vars){
      # The remaining main effects
      main_drop <- setdiff(c("log_I30","log_Dur","log_ARFdays7", ag_vars), v_drop)
      
      # scale vars for the dropped model: keep rainfall + any remaining continuous ag vars
      scale_drop <- intersect(c("log_I30","log_Dur","log_ARFdays7","DSP","PerennialFrac"), main_drop)
      
      # Reduced model with 1 agricultural variable dropped
      m_drop <- MELR(P_df_common,
                     vars_to_scale = scale_drop,
                     main_varls = main_drop,
                     random_varls = "Field_Name",
                     res_varname = "Q_Occurred",
                     model_title = paste(season, "Drop", v_drop))$model
      # Compare drop model vs full agriculture model (nested)
      LRT_drop <- anova(m_drop, m_ag, test = "Chisq")
      chisq_drop <- LRT_drop$Chisq[2]
      p_drop <- LRT_drop$`Pr(>Chisq)`[2]
      
      AIC_drop <- AIC(m_drop)
      # More positive means the dropped variable is more important
      dAIC_drop <- AIC_drop - AIC_ag
      
      drop_ls[[length(drop_ls)+1]] <- data.frame(
        Season = season,
        Rep = r,
        n = nrow(P_df_common),
        Dropped = v_drop,
        AIC_drop = AIC_drop,
        Delta_AIC_drop = dAIC_drop,
        Chisq_drop = chisq_drop,
        p_drop = p_drop
      )
    }
    
    message("Complete ",season," rep ",r)
  }
}

# Combine results
res_df <- bind_rows(res_ls)
write.csv(res_df,paste0(Output_path,"model_season_results.csv"))

summary_df <- res_df %>%
  group_by(Season) %>%
  summarize(
    n = first(n),
    mean_Delta_AIC = mean(Delta_AIC), sd_Delta_AIC = sd(Delta_AIC),
    mean_Chisq = mean(Chisq), sd_Chisq = sd(Chisq),
    prop_sig = mean(p_value < 0.05),
    mean_Delta_AUC = mean(Delta_AUC), sd_Delta_AUC = sd(Delta_AUC),
    .groups = "drop"
  ) %>%
  mutate(Season = factor(Season,levels = c("SS","GS","FS")))

drop_df <- bind_rows(drop_ls)
write.csv(drop_df, paste0(Output_path,"drop1_results.csv"))

drop_summary <- drop_df %>%
  group_by(Season,Dropped) %>%
  summarize(
    mean_dAIC = mean(Delta_AIC_drop),
    sd_dAIC = sd(Delta_AIC_drop),
    mean_chisq = mean(Chisq_drop),
    sd_chisq = sd(Chisq_drop),
    prop_sig = mean(p_drop < 0.05),
    .groups = "drop"
  ) %>%
  mutate(Season = factor(Season,levels=c("SS","GS","FS")),
         Dropped = factor(Dropped,levels=ag_vars))

# Delta_AIC = AIC0 - AICag
g_dAIC <- ggplot(summary_df, aes(x = Season, y = mean_Delta_AIC)) +
  geom_col(fill = "grey70", color = "black") +
  geom_errorbar(aes(ymin = mean_Delta_AIC - sd_Delta_AIC,
                    ymax = mean_Delta_AIC + sd_Delta_AIC),
                width = 0.2) +
  labs(x = "", y = expression(Delta*AIC~"(M0 - MAg)")) +
  my_theme2 +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

g_chi <- ggplot(summary_df, aes(x = Season, y = mean_Chisq)) +
  geom_col(fill = "grey70", color = "black") +
  geom_errorbar(aes(ymin = mean_Chisq - sd_Chisq,
                    ymax = mean_Chisq + sd_Chisq),
                width = 0.2) +
  geom_text(aes(label = paste0("p<0.05: ", round(prop_sig*100), "%")),
            vjust = -0.4, size = 5) +
  labs(x = "", y = "χ² (M0 vs MAg)") +
  my_theme2 +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18)))

# Drop1 plots
# Delta_AIC = AICdrop - AICag
g_drop_dAIC <- ggplot(drop_summary, aes(x = Dropped, y = mean_dAIC,fill=Dropped)) +
  geom_col(color = "black") +
  geom_errorbar(aes(ymin = mean_dAIC - sd_dAIC, ymax = mean_dAIC + sd_dAIC),
                width = 0.2) +
  facet_wrap(~Season, nrow = 1) +
  labs(x = "",
       y = expression(Delta*AIC~"(Drop - Full Ag)"),
       fill= "Dropped ag variable") +
  my_theme2 +
  theme(axis.text.x = element_blank(),
        legend.position = c(0.15,0.8))+
  scale_fill_manual(values = my_color[c(1,2,3)])+
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

g_drop_chi <- ggplot(drop_summary, aes(x = Dropped, y = mean_chisq,fill=Dropped)) +
  geom_col(color = "black") +
  geom_errorbar(aes(ymin = mean_chisq - sd_chisq, ymax = mean_chisq + sd_chisq),
                width = 0.2) +
  geom_text(aes(label = paste0("p<0.05: ", round(prop_sig*100), "%")),
            vjust = -0.4, size = 4) +
  facet_wrap(~Season, nrow = 1) +
  labs(x = "",
       y = expression(chi^2~"(Drop vs Full Ag)")) +
  my_theme2 +
  theme(axis.text.x = element_blank(),
        legend.position = c(0.15,0.8))+
  scale_fill_manual(values = my_color[c(1,2,3)])+
  scale_y_continuous(expand = expansion(mult = c(0, 0.20)))

# Combine these plots
g_model <- plot_grid(g_dAIC,g_chi,align="hv")
g_ag_drop <- plot_grid(g_drop_dAIC,g_drop_chi,align="hv")  
print_g(g_model,"Model_comparison",10,6)
print_g(g_ag_drop,"Ag_drop_comparison",18,6)

# Make marginal effect plots ===============
marginal_g_ls <- list()

# Only fit one model for one season
for(season in c("SS","GS","FS")){
  P_df_season <- P_df %>%
    filter(Season == season) %>%
    select(all_of(vars_all)) %>%
    na.omit()
  
  # Fit one "final" model for marginal effects (no resampling)
  m_ag_fit <- MELR(P_df_season,
                      vars_to_scale = c("log_I30","log_Dur","log_ARFdays7","DSP","PerennialFrac"),
                      main_varls = c("log_I30","log_Dur","log_ARFdays7","Annual_Tillage","DSP","PerennialFrac"),
                      random_varls = "Field_Name",
                      res_varname = "Q_Occurred",
                      model_title = paste0(season," Final"))
  m_ag <- m_ag_fit$model
  df_fit <- m_ag_fit$data
  
  # Marginal effects
  g_I30 <- plot(ggpredict(m_ag,terms="log_I30",data=df_fit))
  g_Dur <- plot(ggpredict(m_ag,terms="log_Dur",data=df_fit))
  g_ARF <- plot(ggpredict(m_ag,terms="log_ARFdays7",data=df_fit))
  g_DSP <- plot(ggpredict(m_ag,terms="DSP",data=df_fit))
  g_PerennialF <- plot(ggpredict(m_ag,terms="PerennialFrac",data=df_fit))
  g_Tillage <- plot(ggpredict(m_ag,terms="Annual_Tillage",data=df_fit))

  
  
  # Add season title
  g_row <- plot_grid(
    ggdraw() + draw_label(season, fontface = "bold", x = 0, hjust = 0),
    plot_grid(g_I30,g_Dur,g_ARF,g_DSP, g_PerennialF, g_Tillage, nrow = 1, align = "hv"),
    ncol = 1, rel_heights = c(0.15, 1)
  )
  
  marginal_g_ls[[season]] <- g_row
}
# Combine these plots
g_ag <- plot_grid(plotlist = marginal_g_ls,nrow=3)
print_g(g_ag,"Marginal_effect_ag",24,12)
