# Author: Zhaozhe Chen (zhaozhe.chen@wisc.edu)
# Date: 2026.3.6

# This code is to model Q occurrence (when P produced Q or not, focusing on non-frozen events only)
# Question to answer: What controls whether a precipitation event produces runoff across Wisconsin farms?
# Remove DSP or DOY from the model
# Include site physics

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
library(forcats)

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

# Agricultural practices to test
#ag_vars <- c("Annual_Tillage","DSP","PerennialFrac")
ag_vars <- c("Annual_Tillage","PerennialFrac")

# Site-level (time-invariant) variables to test
site_vars <- c("MeanSlope_per","HG")

# Precipitation-related variables
P_vars <- c("log_I30","log_Dur","log_ARFdays7")

set.seed(123)
# Number of random sampling
n_rep <- 50

Output_path <- "D:/OneDrive - UW-Madison/Research/Discovery Farms/DF Runoff Generation/Results/Q_occurence_modeling_noDOY/"
  
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
         MeanSlope_per = as.numeric(MeanSlope_per),
         log_P = log(rain),
         log_Dur = log(duration),
         log_I30 = log(I30),
         log_ARFdays7 = log(ARFdays7+0.1)) %>%
  # Group Hydrologic Group into fewer levels so that each group has sufficient samples
  mutate(HG = case_when(HydrologicGroup %in% c("A","B") ~ "High-infiltration",
                        HydrologicGroup %in% c("B/D","C/D","D") ~ "Slow-infiltration",
                        HydrologicGroup %in% c("C") ~ "Moderate-infiltration"),
         HG = factor(HG,levels=c("Slow-infiltration","Moderate-infiltration","High-infiltration"))) %>%
  mutate(Season = case_when(month(P_start) %in% 5:9 ~"GS",
                            month(P_start) %in% 1:4 ~ "SS",
                            month(P_start) %in% 10:12 ~ "FS"),
         Season = factor(Season,levels = c("SS","GS","FS")))

# Balance sample size across seasons ========
# All variables in the full model
vars_all <- c("Q_Occurred","Field_Name",P_vars,ag_vars,site_vars)
# Extract all required variables
P_df_clean <- P_df %>%
  select(all_of(c(vars_all, "Season"))) %>%
  na.omit()
# Check sample size in each season
season_counts <- P_df_clean %>%
  count(Season) %>%
  arrange(Season)
# Get the minimum sample size in any of the seasons
n_min <- min(season_counts$n)
n_sample <- n_min
message("Balanced bootstrap n_sample per season = ", n_sample)

# Fit Mixed-effects logistic regression model ==========================
# Initialize a list to store replication results
res_ls <- list()
# Initialize a list to store replication results for dropped ag models
drop_ls <- list()

# Loop over seasons
for(season in c("SS","GS","FS")){
  # Only keep dataset for this season and keep only target variables
  P_df_season <- P_df_clean %>%
    filter(Season == season)
  
  # Loop n_rep times, sample the same number for each season to keep balanced sample size
  for(r in 1:n_rep){
    # Random sample with equal n
    P_df_common <- P_df_season %>%
      slice_sample(n=n_sample,replace = TRUE)
    # Decide whether to include HG in the replicate
    use_HG <- hg_okay(P_df_common, min_n = 5)
    
    # Site fixed effects
    site_fx <- c("MeanSlope_per")
    if(use_HG) site_fx <- c(site_fx, "HG")
    
    # Storm-only baseline model ---------
    m_storm <- MELR(
      P_df_common,
      vars_to_scale = c(P_vars),
      main_varls = c(P_vars),
      random_varls = c("Field_Name"),
      res_varname = "Q_Occurred",
      model_title = paste(season, "Storm")
    )$model
    
    # Storm + agricultural model ------------
    m_ag <- MELR(
      P_df_common,
      vars_to_scale = c(P_vars, "PerennialFrac"),
      main_varls = c(P_vars, ag_vars),
      random_varls = c("Field_Name"),
      res_varname = "Q_Occurred",
      model_title = paste(season, "Storm + ag")
    )$model
    
    # Storm + site physics ----------
    m_site <- MELR(
      P_df_common,
      vars_to_scale = c(P_vars, "MeanSlope_per"),
      main_varls = c(P_vars, site_fx),
      random_varls = c("Field_Name"),
      res_varname = "Q_Occurred",
      model_title = paste(season, "Storm + Site")
    )$model
    
    # Full model: Storm + ag + Site physics ----------
    m_full <- MELR(
      P_df_common,
      vars_to_scale = c(P_vars, "PerennialFrac", "MeanSlope_per"),
      main_varls = c(P_vars, ag_vars, site_fx),
      random_varls = c("Field_Name"),
      res_varname = "Q_Occurred",
      model_title = paste(season, "Full")
    )$model
    
    # Model comparison ========
    AIC_storm <- AIC(m_storm)
    AIC_ag    <- AIC(m_ag)
    AIC_site  <- AIC(m_site)
    AIC_full  <- AIC(m_full)
    
    # LRTs
    LRT_storm_vs_ag   <- safe_anova(m_storm, m_ag)
    LRT_storm_vs_site <- safe_anova(m_storm, m_site)
    LRT_ag_vs_full    <- safe_anova(m_ag, m_full)
    LRT_site_vs_full  <- safe_anova(m_site, m_full)
    
    cp1 <- get_chi_p(LRT_storm_vs_ag)
    cp2 <- get_chi_p(LRT_storm_vs_site)
    cp3 <- get_chi_p(LRT_ag_vs_full)
    cp4 <- get_chi_p(LRT_site_vs_full)
    
    # AUC
    AUC_storm <- get_auc_glmer(m_storm, P_df_common, res_var = "Q_Occurred")
    AUC_ag    <- get_auc_glmer(m_ag,    P_df_common, res_var = "Q_Occurred")
    AUC_site  <- get_auc_glmer(m_site,  P_df_common, res_var = "Q_Occurred")
    AUC_full  <- get_auc_glmer(m_full,  P_df_common, res_var = "Q_Occurred")
    
    # R2 (Nakagawa)
    r2_storm <- performance::r2_nakagawa(m_storm)
    r2_ag    <- performance::r2_nakagawa(m_ag)
    r2_site  <- performance::r2_nakagawa(m_site)
    r2_full  <- performance::r2_nakagawa(m_full)
    
    # Random intercept variance
    re_storm <- get_re_var(m_storm)
    re_full  <- get_re_var(m_full)
    
    # Put these results into data frame
    res_ls[[length(res_ls)+1]] <- data.frame(
      Season = season,
      Rep = r,                              # >>> CHANGED
      n = nrow(P_df_common),
      use_HG = use_HG,                      # >>> CHANGED
      
      AIC_storm = AIC_storm,                # >>> CHANGED
      AIC_ag = AIC_ag,
      AIC_site = AIC_site,                  # >>> CHANGED
      AIC_full = AIC_full,                  # >>> CHANGED
      
      dAIC_storm_ag = AIC_storm - AIC_ag,       # >>> CHANGED
      dAIC_storm_site = AIC_storm - AIC_site,   # >>> CHANGED
      dAIC_ag_full = AIC_ag - AIC_full,         # >>> CHANGED
      dAIC_site_full = AIC_site - AIC_full,     # >>> CHANGED
      
      chisq_storm_ag = cp1$chisq, p_storm_ag = cp1$p,         # >>> CHANGED
      chisq_storm_site = cp2$chisq, p_storm_site = cp2$p,     # >>> CHANGED
      chisq_ag_full = cp3$chisq, p_ag_full = cp3$p,           # >>> CHANGED
      chisq_site_full = cp4$chisq, p_site_full = cp4$p,       # >>> CHANGED
      
      AUC_storm = AUC_storm,                  # >>> CHANGED
      AUC_ag = AUC_ag,
      AUC_site = AUC_site,                    # >>> CHANGED
      AUC_full = AUC_full,                    # >>> CHANGED
      
      dAUC_storm_ag = AUC_ag - AUC_storm,         # >>> CHANGED
      dAUC_storm_site = AUC_site - AUC_storm,     # >>> CHANGED
      dAUC_ag_full = AUC_full - AUC_ag,           # >>> CHANGED
      dAUC_site_full = AUC_full - AUC_site,       # >>> CHANGED
      
      R2m_storm = r2_storm$R2_marginal, R2c_storm = r2_storm$R2_conditional,   # >>> CHANGED
      R2m_ag    = r2_ag$R2_marginal,    R2c_ag    = r2_ag$R2_conditional,
      R2m_site  = r2_site$R2_marginal,  R2c_site  = r2_site$R2_conditional,
      R2m_full  = r2_full$R2_marginal,  R2c_full  = r2_full$R2_conditional,
      
      REvar_storm = re_storm,            # >>> CHANGED
      REvar_full  = re_full
    )  
    
    # Drop 1 variable and quantify contribution =============
    full_terms <- c(P_vars, ag_vars, site_fx)
    # List of variables to test
    drop_vars_ls <- c(ag_vars, "MeanSlope_per")
    if(use_HG) drop_vars_ls <- c(drop_vars_ls, "HG")
    
    for(v_drop in drop_vars_ls){
      # The remaining main effects
      main_drop <- setdiff(full_terms, v_drop)
      
      # scale vars present in dropped model
      scale_drop <- intersect(c(P_vars, "PerennialFrac", "MeanSlope_per"), main_drop)
      
      # Reduced model with 1 variable dropped
      m_drop <- MELR(
        P_df_common,
        vars_to_scale = scale_drop,
        main_varls = main_drop,
        random_varls = "Field_Name",
        res_varname = "Q_Occurred",
        model_title = paste(season, "Drop", v_drop)
      )$model
      
      # Compare drop model vs full model
      LRT_drop <- safe_anova(m_drop, m_full)              # >>> CHANGED
      cpD <- get_chi_p(LRT_drop)                          # >>> CHANGED
      
      AIC_drop <- AIC(m_drop)
      dAIC_drop <- AIC_drop - AIC_full                    # >>> CHANGED
      
      drop_ls[[length(drop_ls)+1]] <- data.frame(
        Season = season,
        Rep = r,
        n = nrow(P_df_common),
        use_HG = use_HG,                                  # >>> CHANGED
        Dropped = v_drop,
        AIC_drop = AIC_drop,
        dAIC_drop = dAIC_drop,                            # >>> CHANGED
        chisq_drop = cpD$chisq,                           # >>> CHANGED
        p_drop = cpD$p
      )
    }
    
    if (r %% 25 == 0) message("Complete ", season, " rep ", r)   # >>> CHANGED
  }
    message("Complete ",season)
}

# Combine results
res_df <- bind_rows(res_ls)
drop_df <- bind_rows(drop_ls)                                     # >>> CHANGED

write.csv(res_df, paste0(Output_path, "Q_occurrence_model_compare_rep.csv"), row.names = FALSE)   # >>> CHANGED
write.csv(drop_df, paste0(Output_path, "Q_occurrence_drop1_rep.csv"), row.names = FALSE)           # >>> CHANGED

# >>> REPLACED
# Summarize res_df
summary_df <- res_df %>%
  group_by(Season) %>%
  summarize(
    n = median(n),
    prop_use_HG = mean(use_HG),
    
    mean_dAIC_storm_ag = mean(dAIC_storm_ag), sd_dAIC_storm_ag = sd(dAIC_storm_ag),
    mean_dAIC_storm_site = mean(dAIC_storm_site), sd_dAIC_storm_site = sd(dAIC_storm_site),
    mean_dAIC_ag_full = mean(dAIC_ag_full), sd_dAIC_ag_full = sd(dAIC_ag_full),
    mean_dAIC_site_full = mean(dAIC_site_full), sd_dAIC_site_full = sd(dAIC_site_full),
    
    mean_chisq_storm_ag = mean(chisq_storm_ag, na.rm = TRUE), sd_chisq_storm_ag = sd(chisq_storm_ag, na.rm = TRUE),
    mean_chisq_storm_site = mean(chisq_storm_site, na.rm = TRUE), sd_chisq_storm_site = sd(chisq_storm_site, na.rm = TRUE),
    mean_chisq_ag_full = mean(chisq_ag_full, na.rm = TRUE), sd_chisq_ag_full = sd(chisq_ag_full, na.rm = TRUE),
    mean_chisq_site_full = mean(chisq_site_full, na.rm = TRUE), sd_chisq_site_full = sd(chisq_site_full, na.rm = TRUE),
    
    prop_sig_storm_ag = mean(p_storm_ag < 0.05, na.rm = TRUE),
    prop_sig_storm_site = mean(p_storm_site < 0.05, na.rm = TRUE),
    prop_sig_ag_full = mean(p_ag_full < 0.05, na.rm = TRUE),
    prop_sig_site_full = mean(p_site_full < 0.05, na.rm = TRUE),
    
    mean_AUC_storm = mean(AUC_storm), sd_AUC_storm = sd(AUC_storm),
    mean_AUC_ag = mean(AUC_ag), sd_AUC_ag = sd(AUC_ag),
    mean_AUC_site = mean(AUC_site), sd_AUC_site = sd(AUC_site),
    mean_AUC_full = mean(AUC_full), sd_AUC_full = sd(AUC_full),
    
    mean_dAUC_storm_ag = mean(dAUC_storm_ag), sd_dAUC_storm_ag = sd(dAUC_storm_ag),
    mean_dAUC_storm_site = mean(dAUC_storm_site), sd_dAUC_storm_site = sd(dAUC_storm_site),
    mean_dAUC_ag_full = mean(dAUC_ag_full), sd_dAUC_ag_full = sd(dAUC_ag_full),
    mean_dAUC_site_full = mean(dAUC_site_full), sd_dAUC_site_full = sd(dAUC_site_full),
    
    mean_R2m_storm = mean(R2m_storm), sd_R2m_storm = sd(R2m_storm),
    mean_R2c_storm = mean(R2c_storm), sd_R2c_storm = sd(R2c_storm),
    
    mean_R2m_ag = mean(R2m_ag), sd_R2m_ag = sd(R2m_ag),
    mean_R2c_ag = mean(R2c_ag), sd_R2c_ag = sd(R2c_ag),
    
    mean_R2m_site = mean(R2m_site), sd_R2m_site = sd(R2m_site),
    mean_R2c_site = mean(R2c_site), sd_R2c_site = sd(R2c_site),
    
    mean_R2m_full = mean(R2m_full), sd_R2m_full = sd(R2m_full),
    mean_R2c_full = mean(R2c_full), sd_R2c_full = sd(R2c_full),
    
    mean_REvar_storm = mean(REvar_storm, na.rm = TRUE),
    mean_REvar_full  = mean(REvar_full, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(Season = factor(Season, levels = c("SS","GS","FS")))

write.csv(summary_df, paste0(Output_path, "Q_occurrence_model_compare_summary.csv"), row.names = FALSE)   # >>> CHANGED

# >>> REPLACED
# Summarize drop_df
drop_summary <- drop_df %>%
  group_by(Season, Dropped) %>%
  summarize(
    mean_dAIC_drop = mean(dAIC_drop),
    sd_dAIC_drop = sd(dAIC_drop),
    
    mean_chisq_drop = mean(chisq_drop, na.rm = TRUE), 
    sd_chisq_drop   = sd(chisq_drop, na.rm = TRUE),  
    
    prop_sig_drop = mean(p_drop < 0.05, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Season = factor(Season, levels = c("SS","GS","FS")),
    Dropped = factor(Dropped, levels = c(ag_vars, "MeanSlope_per", "HG"))
  )

write.csv(drop_summary, paste0(Output_path, "Q_occurrence_drop1_summary.csv"), row.names = FALSE)   # >>> CHANGED

# Visualize model performance =================
# >>> CHANGED
perf_long <- res_df %>%
  select(Season, Rep,
         AUC_storm, AUC_ag, AUC_site, AUC_full,
         R2m_storm, R2m_ag, R2m_site, R2m_full,
         R2c_storm, R2c_ag, R2c_site, R2c_full) %>%
  tidyr::pivot_longer(
    cols = -c(Season, Rep),
    names_to = c("Metric", "Model"),
    names_pattern = "(AUC|R2m|R2c)_(storm|ag|site|full)",
    values_to = "Value"
  ) %>%
  mutate(
    Model = factor(Model, levels = c("storm","ag","site","full")),
    Metric = factor(Metric, levels = c("AUC","R2m","R2c")),
    Season = factor(Season, levels = c("SS","GS","FS"))
  )

# >>> CHANGED
rand_share <- res_df %>%
  transmute(
    Season, Rep,
    storm = R2c_storm - R2m_storm,
    ag    = R2c_ag    - R2m_ag,
    site  = R2c_site  - R2m_site,
    full  = R2c_full  - R2m_full
  ) %>%
  tidyr::pivot_longer(
    cols = -c(Season, Rep),
    names_to = "Model",
    values_to = "Value"
  ) %>%
  mutate(
    Metric = "R2_random (R2c - R2m)",
    Model = factor(Model, levels = c("storm","ag","site","full")),
    Season = factor(Season, levels = c("SS","GS","FS"))
  )

perf_long2 <- bind_rows(perf_long, rand_share)

# AUC
# >>> CHANGED
g_auc <- ggplot(filter(perf_long, Metric == "AUC"),
                aes(x = Model, y = Value, fill = Model)) +
  geom_boxplot(outlier.colour = NA) +
  facet_wrap(~Season, nrow = 1) +
  labs(x = "", y = "AUC") +
  my_theme2 +
  scale_fill_manual(values = my_color[c(3,2,1,4)])

# Marginal R2: variance explained by fixed effects
# >>> CHANGED
g_r2m <- ggplot(filter(perf_long, Metric == "R2m"),
                aes(x = Model, y = Value, fill = Model)) +
  geom_boxplot(outlier.colour = NA) +
  facet_wrap(~Season, nrow = 1) +
  labs(x = "", y = "Marginal R² (fixed effects)") +
  my_theme2 +
  scale_fill_manual(values = my_color[c(3,2,1,4)])

# Conditional R2: variance explained by fixed + random
# >>> CHANGED
g_r2c <- ggplot(filter(perf_long2, Metric == "R2c"),
                aes(x = Model, y = Value, fill = Model)) +
  geom_boxplot(outlier.colour = NA) +
  facet_wrap(~Season, nrow = 1) +
  labs(x = "", y = "Conditional R² (fixed + random)") +
  my_theme2 +
  scale_fill_manual(values = my_color[c(3,2,1,4)])

# Conditional R2 - Marginal R2
# >>> CHANGED
g_r2rand <- ggplot(filter(perf_long2, Metric == "R2_random (R2c - R2m)"),
                   aes(x = Model, y = Value, fill = Model)) +
  geom_boxplot(outlier.colour = NA) +
  facet_wrap(~Season, nrow = 1) +
  labs(x = "", y = "Random-effect (R²c − R²m)") +
  my_theme2 +
  scale_fill_manual(values = my_color[c(3,2,1,4)])

# Combine these 4 plots
# >>> CHANGED
g_model_perform <- plot_grid(g_auc, g_r2m, g_r2c, g_r2rand,
                             nrow = 2, align = "hv")
print_g(g_model_perform, "Model_performance", 14, 8)

# Visualize model comparisons =============
# Compare models =================
# ΔAIC = AIC_small - AIC_big
# Positive means big model improves AIC

# Storm vs Agricultural -----------
# >>> CHANGED
g_dAIC_storm_ag <- plot_dAIC(summary_df, "storm", "ag")
g_chisq_storm_ag <- plot_chisq(summary_df, "storm", "ag")

# Storm vs Site ------------
# >>> CHANGED
g_dAIC_storm_site <- plot_dAIC(summary_df, "storm", "site")
g_chisq_storm_site <- plot_chisq(summary_df, "storm", "site")

# ag vs Full ----------
# >>> CHANGED
g_dAIC_ag_full <- plot_dAIC(summary_df, "ag", "full")
g_chisq_ag_full <- plot_chisq(summary_df, "ag", "full")

# site vs Full ------
# >>> CHANGED
g_dAIC_site_full <- plot_dAIC(summary_df, "site", "full")
g_chisq_site_full <- plot_chisq(summary_df, "site", "full")

# Combine these plots
# >>> CHANGED
g_model_compare <- plot_grid(
  g_dAIC_storm_ag, g_dAIC_storm_site, g_dAIC_ag_full, g_dAIC_site_full,
  g_chisq_storm_ag, g_chisq_storm_site, g_chisq_ag_full, g_chisq_site_full,
  nrow = 2, align = "hv"
)
print_g(g_model_compare, "Model_comparison", 14, 8)

# Drop 1 variable ==========================
# ΔAIC_drop = AIC_drop - AIC_full
# Positive => dropping the variable makes AIC worse => variable is important

# >>> CHANGED
drop_summary2 <- drop_summary %>%
  mutate(
    Dropped_group = case_when(
      Dropped %in% ag_vars ~ "Agricultural",
      Dropped %in% c("MeanSlope_per","HG") ~ "Site physics",
      TRUE ~ "Other"
    ),
    Dropped_group = factor(Dropped_group, levels = c("Agricultural","Site physics","Other"))
  )

# >>> CHANGED
rank_df <- drop_summary2 %>%
  group_by(Dropped) %>%
  summarize(rank_score = mean(mean_dAIC_drop, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(rank_score))

# >>> CHANGED
drop_plot_df <- drop_summary2 %>%
  mutate(Dropped = factor(Dropped, levels = rank_df$Dropped))

# Plot delta AIC for each dropped variable
# >>> CHANGED
g_drop_dAIC_h <- ggplot(drop_plot_df,
                        aes(x = Dropped, y = mean_dAIC_drop, fill = Dropped_group)) +
  geom_col(color = "black") +
  geom_errorbar(aes(ymin = mean_dAIC_drop - sd_dAIC_drop,
                    ymax = mean_dAIC_drop + sd_dAIC_drop),
                width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~Season, nrow = 1) +
  coord_flip() +
  labs(
    x = "",
    y = expression(Delta*AIC~"(Drop - Full)"),
    fill = "Variable type"
  ) +
  my_theme2 +
  scale_fill_manual(values = my_color[c(2,1)]) +
  theme(
    axis.text.y = element_text(),
    axis.title.y = element_blank(),
    strip.background = element_blank()
  ) +
  geom_blank()

# Plot chisq for each dropped variable
# >>> CHANGED
g_drop_chi_h <- ggplot(drop_plot_df,
                       aes(x = Dropped, y = mean_chisq_drop, fill = Dropped_group)) +
  geom_col(color = "black") +
  geom_errorbar(aes(ymin = mean_chisq_drop - sd_chisq_drop,
                    ymax = mean_chisq_drop + sd_chisq_drop),
                width = 0.2) +
  facet_wrap(~Season, nrow = 1) +
  geom_text(aes(label = paste0("p<0.05: ", round(prop_sig_drop*100), "%")),
            hjust = -0.1, size = 4) +
  coord_flip() +
  labs(
    x = "",
    y = expression(chi^2~"(Drop vs Full)"),
    fill = "Variable type"
  ) +
  my_theme2 +
  scale_fill_manual(values = my_color[c(2,1)]) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  theme(
    axis.text.y = element_text(),
    axis.title.y = element_blank(),
    strip.background = element_blank()
  ) +
  geom_blank()

# Combine these two plots
# >>> CHANGED
g_drop1 <- plot_grid(g_drop_dAIC_h, g_drop_chi_h, nrow = 2)
print_g(g_drop1, "Model_drop1", 12, 8)

# Make marginal effect plots ===============
marginal_g_ls <- list()

# Only fit one model for one season
for(season in c("SS","GS","FS")){
  # >>> CHANGED
  df_fit <- P_df_clean %>%
    filter(Season == season) %>%
    select(all_of(vars_all)) %>%
    na.omit()
  
  # >>> CHANGED
  use_HG_season <- hg_okay(df_fit, min_n = 5)
  site_fx <- c("MeanSlope_per")
  if (use_HG_season) site_fx <- c(site_fx, "HG")
  
  # >>> REPLACED
  # Fit one final full model for marginal effects (no resampling)
  m_full_fit <- MELR(
    df_fit,
    vars_to_scale = c(P_vars, "PerennialFrac", "MeanSlope_per"),
    main_varls = c(P_vars, "Annual_Tillage", "PerennialFrac", site_fx),
    random_varls = "Field_Name",
    res_varname = "Q_Occurred",
    model_title = paste0(season, " Full (all data)")
  )
  m_full_season <- m_full_fit$model
  
  # Marginal effects
  # >>> CHANGED
  g_I30 <- plot(ggpredict(m_full_season, terms = "log_I30"))
  g_Dur <- plot(ggpredict(m_full_season, terms = "log_Dur"))
  g_ARF <- plot(ggpredict(m_full_season, terms = "log_ARFdays7"))
  g_PerennialF <- plot(ggpredict(m_full_season, terms = "PerennialFrac"))
  g_Tillage <- plot(ggpredict(m_full_season, terms = "Annual_Tillage"))
  g_Slope <- plot(ggpredict(m_full_season, terms = "MeanSlope_per"))
  
  # >>> CHANGED
  plot_list <- list(g_I30, g_Dur, g_ARF, g_PerennialF, g_Tillage, g_Slope)
  
  # >>> CHANGED
  if (use_HG_season) {
    g_HG <- plot(ggpredict(m_full_season, terms = "HG"))
    plot_list <- c(plot_list, list(g_HG))
  }
  
  # Add season title
  g_row <- plot_grid(
    ggdraw() + draw_label(season, fontface = "bold", x = 0, hjust = 0),
    plot_grid(plotlist = plot_list, nrow = 1, align = "hv"),
    ncol = 1, rel_heights = c(0.15, 1)
  )
  
  marginal_g_ls[[season]] <- g_row
}

# Combine these plots
# >>> CHANGED
g_marginal <- plot_grid(plotlist = marginal_g_ls, nrow = 3)
print_g(g_marginal, "Marginal_effect_full_model", 28, 12)