# Author: Zhaozhe Chen
# Date: 2026.3.2

# This code is to model Q event runoff coefficient (RC) using mixed linear regression models
# RC is converted to log scale for normalily

# Seasons to consider
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
ag_vars <- c("Annual_Tillage","DOY","PerennialFrac")

# Site-level (time-invariant) variables to test
site_vars <- c("MeanSlope_per","HG")

# Precipitation-related variables
P_vars <- c("log_I30","log_Dur","log_ARFdays7")

# Output path
Output_path <- "D:/OneDrive - UW-Madison/Research/Discovery Farms/DF Runoff Generation/Results/RC_modeling_log/"

# Uncertainty/season comparability settings
set.seed(123)
n_rep <- 200

# -------- Functions -----------
rmse <- function(obs, pred) sqrt(mean((obs - pred)^2)) 

# LRT wrapper (in case of fitting issues)
safe_anova <- function(m_small, m_big) {              
  out <- tryCatch(anova(m_small, m_big), error = function(e) NULL)
  out
}

# Extract random intercept variance (how much site-to-site baseline remains)
get_re_var <- function(m) {                                
  vc <- as.data.frame(VarCorr(m))
  # random intercept variance for Field_Name
  v <- vc %>% filter(grp == "Field_Name", var1 == "(Intercept)") %>% pull(vcov)
  if (length(v) == 0) NA_real_ else v
}

# helper to pull chisq/p from anova output
get_chi_p <- function(LRT){
  if (is.null(LRT)) return(list(chisq = NA_real_, p = NA_real_))
  list(chisq = LRT$Chisq[2], p = LRT$`Pr(>Chisq)`[2])
}

# helper: decide whether HG is estimable in this sampled dataset
hg_okay <- function(df, min_n = 5){
  tab <- table(df$HG)
  # require at least 2 levels present + each present level has >= min_n
  if (length(tab) < 2) return(FALSE)
  all(tab >= min_n)
}

# ----- Main ----------
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
         Monitoring = factor(Monitoring),
         Tile = factor(Tile),
         Tillage = factor(Tillage),
         Annual_Tillage = factor(Annual_Tillage,levels=c("Conventional","Reduced","No-Till","Pasture")),
         SoilType = factor(SoilType),
         Crop = factor(Crop),
         HydrologicGroup = factor(HydrologicGroup),
         MeanSlope_per = as.numeric(MeanSlope_per),
         log_P = log(rain_in),
         log_Dur = log(duration),
         log_I30 = log(I30),
         log_ARFdays7 = log(ARFdays7+0.1)) %>%
  # Get DOY 
  mutate(DOY = yday(Q_start)) %>%
  # Group Hydrologic Group into fewer levels so that each group has sufficient samples
  mutate(HG = case_when(HydrologicGroup %in% c("A","B") ~ "High-infiltration",
                        HydrologicGroup %in% c("B/D","C/D","D") ~ "Slow-infiltration",
                        HydrologicGroup %in% c("C") ~ "Moderate-infiltration"),
         HG = factor(HG,levels=c("Slow-infiltration","Moderate-infiltration","High-infiltration"))) %>%
  mutate(Season = case_when(month(Q_start) %in% 5:9 ~"GS",
                            month(Q_start) %in% 1:4 ~ "SS",
                            month(Q_start) %in% 10:12 ~ "FS"),
         Season = factor(Season,levels = c("SS","GS","FS")))

# Balance sample size across seasons ========
# All variables in the full model
vars_all <- c("log_RC","Field_Name",P_vars,ag_vars,site_vars)
# Extract all required variables
Q_df_clean <- Q_df %>%
  select(all_of(c(vars_all, "Season"))) %>%
  na.omit() 
# Check sample size in each season
season_counts <- Q_df_clean %>%
  count(Season) %>%
  arrange(Season)
# Get the minimum sample size in any of the season
n_min = min(season_counts$n)
n_sample <- n_min
message("Balanced bootstrap n_sample per season =",n_sample)

# Fit Mixed-effect linear regression model =====================
# Initialize a list to store model performance output for the 4 models
res_ls <- list()
# Initialize a list to store model performance output for dropped models
drop_ls <- list()

# Loop over the three seasons
for(season in c("SS","GS","FS")){
  Q_df_season <- Q_df_clean %>%
    filter(Season == season)
  
  # Balanced Bootstrapping for uncertainty + season comparability
  for(r in 1:n_rep){
    # Balanced bootstrap sample
    Q_df_common <- Q_df_season %>%
      slice_sample(n=n_sample,replace = TRUE)
    # Decide whether to include HG in the replicate
    use_HG <- hg_okay(Q_df_common,min_n=5)
    
    # Site fixed effects
    site_fx <- c("MeanSlope_per")
    if(use_HG) site_fx <- c(site_fx,"HG")
    
    # Storm-only baseline model ---------
    m_storm <- MER(Q_df_common,
                   vars_to_scale = c(P_vars),
                   main_varls = c(P_vars),
                   random_varls = c("Field_Name"),
                   res_varname = "log_RC",
                   model_title = paste(season,"Storm"),
                   REML = FALSE)$model
    
    # Storm + agricultural model ------------
    m_ag <- MER(Q_df_common,
                vars_to_scale = c(P_vars,"DOY","PerennialFrac"),
                main_varls = c(P_vars,ag_vars),
                random_varls = c("Field_Name"),
                res_varname = "log_RC",
                model_title = paste(season,"Storm + ag"),
                REML = FALSE)$model
    
    # Storm + site physics ----------
    m_site <- MER(Q_df_common,
                  vars_to_scale = c(P_vars,"MeanSlope_per"),
                  main_varls = c(P_vars,site_fx),
                  random_varls = c("Field_Name"),
                  res_varname = "log_RC",
                  model_title = paste(season,"Storm + Site"),
                  REML = FALSE)$model
    
    # Full model: Storm + ag + Site physics ----------
    m_full <- MER(Q_df_common,
                  vars_to_scale = c(P_vars,"DOY","PerennialFrac","MeanSlope_per"),
                  main_varls = c(P_vars,ag_vars,site_fx),
                  random_varls = c("Field_Name"),
                  res_varname = "log_RC",
                  model_title = paste(season,"Full"),
                  REML = FALSE)$model
    
    # Model comparison ========
    AIC_storm <- AIC(m_storm)
    AIC_ag    <- AIC(m_ag)
    AIC_site  <- AIC(m_site)
    AIC_full  <- AIC(m_full)
    
    # LRTs (nested when HG inclusion is consistent)
    LRT_storm_vs_ag   <- safe_anova(m_storm, m_ag)
    LRT_storm_vs_site <- safe_anova(m_storm, m_site)
    LRT_ag_vs_full    <- safe_anova(m_ag, m_full)
    LRT_site_vs_full  <- safe_anova(m_site, m_full)
    
    cp1 <- get_chi_p(LRT_storm_vs_ag)
    cp2 <- get_chi_p(LRT_storm_vs_site)
    cp3 <- get_chi_p(LRT_ag_vs_full)
    cp4 <- get_chi_p(LRT_site_vs_full)
    
    # R2 (Nakagawa)
    r2_storm <- performance::r2_nakagawa(m_storm)
    r2_ag    <- performance::r2_nakagawa(m_ag)
    r2_site  <- performance::r2_nakagawa(m_site)
    r2_full  <- performance::r2_nakagawa(m_full)
    
    # Random intercept variance (site-to-site baseline)
    re_storm <- get_re_var(m_storm)
    re_full  <- get_re_var(m_full)
    
    # RMSE (in-sample; comparable because n_sample is equal across seasons)
    RMSE_storm <- rmse(Q_df_common$log_RC, predict(m_storm))
    RMSE_ag    <- rmse(Q_df_common$log_RC, predict(m_ag))
    RMSE_site  <- rmse(Q_df_common$log_RC, predict(m_site))
    RMSE_full  <- rmse(Q_df_common$log_RC, predict(m_full))
    
    res_ls[[length(res_ls)+1]] <- data.frame(
      Season = season,
      Rep = r,
      n = nrow(Q_df_common),
      use_HG = use_HG,
      
      AIC_storm = AIC_storm,
      AIC_ag = AIC_ag,
      AIC_site = AIC_site,
      AIC_full = AIC_full,
      
      dAIC_storm_ag = AIC_storm - AIC_ag,
      dAIC_storm_site = AIC_storm - AIC_site,
      dAIC_ag_full = AIC_ag - AIC_full,
      dAIC_site_full = AIC_site - AIC_full,
      
      chisq_storm_ag = cp1$chisq, p_storm_ag = cp1$p,
      chisq_storm_site = cp2$chisq, p_storm_site = cp2$p,
      chisq_ag_full = cp3$chisq, p_ag_full = cp3$p,
      chisq_site_full = cp4$chisq, p_site_full = cp4$p,
      
      R2m_storm = r2_storm$R2_marginal, R2c_storm = r2_storm$R2_conditional,
      R2m_ag    = r2_ag$R2_marginal,    R2c_ag    = r2_ag$R2_conditional,
      R2m_site  = r2_site$R2_marginal,  R2c_site  = r2_site$R2_conditional,
      R2m_full  = r2_full$R2_marginal,  R2c_full  = r2_full$R2_conditional,
      
      REvar_storm = re_storm,
      REvar_full  = re_full,
      
      RMSE_storm = RMSE_storm,
      RMSE_ag = RMSE_ag,
      RMSE_site = RMSE_site,
      RMSE_full = RMSE_full
    )
    
    # Drop 1 variable and quantify contribution =============
    full_terms <- c(P_vars,ag_vars,site_fx)
    # List of variables to test (always include slope, also include HG if used)
    drop_vars_ls <- c(ag_vars,"MeanSlope_per")
    if (use_HG) drop_vars_ls <- c(drop_vars_ls,"HG")
    
    for(v_drop in drop_vars_ls){
      # Remaining main effect
      main_drop <- setdiff(full_terms,v_drop)
      # scale vars present in dropped model (continuous only)
      scale_drop <- intersect(c(P_vars, "DOY","PerennialFrac","MeanSlope_per"), main_drop)
      # Dropped model 
      m_drop <- MER(
        Q_df_common,
        vars_to_scale = scale_drop,
        main_varls = main_drop,
        random_varls = "Field_Name",
        res_varname = "log_RC",
        model_title = paste0(season, " Drop ", v_drop),
        REML = FALSE
      )$model
      
      # Compare dropped model with full model
      LRT_drop <- safe_anova(m_drop, m_full)
      cpD <- get_chi_p(LRT_drop)
      
      AIC_drop <- AIC(m_drop)
      dAIC_drop <- AIC_drop - AIC_full
      
      # Store this result
      drop_ls[[length(drop_ls)+1]] <- data.frame(
        Season = season,
        Rep = r,
        n = nrow(Q_df_common),
        use_HG = use_HG,
        Dropped = v_drop,
        AIC_drop = AIC_drop,
        dAIC_drop = dAIC_drop,
        chisq_drop = cpD$chisq,
        p_drop = cpD$p
      )
    }
    if (r %% 25 == 0) message("Complete ", season, " rep ", r)
  }
  message("Complete",season)
}

# Summarize Output model results ========================
res_df <- bind_rows(res_ls)
drop_df <- bind_rows(drop_ls)

write.csv(res_df,paste0(Output_path,"RC_model_compare_rep.csv"),row.names = FALSE)
write.csv(drop_df,paste0(Output_path,"RC_drop1_rep.csv"),row.names = FALSE)

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
    
    mean_chisq_storm_ag = mean(chisq_storm_ag),sd_chisq_storm_ag = sd(chisq_storm_ag),
    mean_chisq_storm_site = mean(chisq_storm_site),sd_chisq_storm_site = sd(chisq_storm_site),
    mean_chisq_ag_full = mean(chisq_ag_full),sd_chisq_ag_full = sd(chisq_ag_full),
    mean_chisq_site_full = mean(chisq_site_full),sd_chisq_site_full = sd(chisq_site_full),
    
    prop_sig_storm_ag = mean(p_storm_ag < 0.05, na.rm=TRUE),
    prop_sig_storm_site = mean(p_storm_site < 0.05, na.rm=TRUE),
    prop_sig_ag_full = mean(p_ag_full < 0.05, na.rm=TRUE),
    prop_sig_site_full = mean(p_site_full < 0.05, na.rm=TRUE),
    
    mean_R2m_storm = mean(R2m_storm), sd_R2m_storm = sd(R2m_storm),
    mean_R2c_storm = mean(R2c_storm), sd_R2c_storm = sd(R2c_storm),
    
    mean_R2m_ag = mean(R2m_ag), sd_R2m_ag = sd(R2m_ag),
    mean_R2c_ag = mean(R2c_ag), sd_R2c_ag = sd(R2c_ag),
    
    mean_R2m_site = mean(R2m_site), sd_R2m_site = sd(R2m_site),
    mean_R2c_site = mean(R2c_site), sd_R2c_site = sd(R2c_site),
    
    mean_R2m_full = mean(R2m_full), sd_R2m_full = sd(R2m_full),
    mean_R2c_full = mean(R2c_full), sd_R2c_full = sd(R2c_full),
    
    mean_REvar_storm = mean(REvar_storm, na.rm=TRUE),
    mean_REvar_full  = mean(REvar_full, na.rm=TRUE),
    
    mean_RMSE_storm = mean(RMSE_storm), sd_RMSE_storm = sd(RMSE_storm),
    mean_RMSE_full  = mean(RMSE_full),  sd_RMSE_full  = sd(RMSE_full),
    
    .groups = "drop"
  ) %>%
  mutate(Season = factor(Season, levels=c("SS","GS","FS")))

write.csv(summary_df, paste0(Output_path, "RC_model_compare_summary.csv"), row.names = FALSE)

# Summarize drop_df
drop_summary <- drop_df %>%
  group_by(Season, Dropped) %>%
  summarize(
    mean_dAIC_drop = mean(dAIC_drop),
    sd_dAIC_drop = sd(dAIC_drop),
    
    mean_chisq_drop = mean(chisq_drop, na.rm = TRUE), 
    sd_chisq_drop   = sd(chisq_drop, na.rm = TRUE),  
    
    prop_sig_drop = mean(p_drop < 0.05, na.rm=TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Season = factor(Season, levels=c("SS","GS","FS")),
    Dropped = factor(Dropped, levels=c(ag_vars, "MeanSlope_per", "HG"))
  )

write.csv(drop_summary, paste0(Output_path, "RC_drop1_summary.csv"), row.names = FALSE)

# Visualize model performance =================
perf_long <- res_df %>%
  select(Season, Rep, RMSE_storm, RMSE_ag, RMSE_site, RMSE_full,
         R2m_storm, R2m_ag, R2m_site, R2m_full,
         R2c_storm,R2c_ag,R2c_site,R2c_full) %>%
  tidyr::pivot_longer(
    cols = -c(Season, Rep),
    names_to = c("Metric", "Model"),
    names_pattern = "(RMSE|R2m|R2c)_(storm|ag|site|full)",
    values_to = "Value"
  ) %>%
  mutate(
    Model = factor(Model, levels = c("storm","ag","site","full")),
    Metric = factor(Metric, levels = c("RMSE","R2m","R2c")),
    Season = factor(Season,levels = c("SS","GS","FS"))
  )

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
# RMSE
g_rmse <- ggplot(filter(perf_long, Metric == "RMSE"),
                 aes(x = Model, y = Value,fill=Model)) +
  geom_boxplot(outlier.colour = NA) +
  facet_wrap(~Season, nrow = 1) +
  labs(x = "", y = "RMSE (log_RC)") +
  my_theme2+
  scale_fill_manual(values = my_color[c(3,2,1,4)])

# Marginal R2: variance explained by fixed effects
g_r2m <- ggplot(filter(perf_long, Metric == "R2m"),
                aes(x = Model, y = Value,fill=Model)) +
  geom_boxplot(outlier.colour = NA) +
  facet_wrap(~Season, nrow = 1) +
  labs(x = "", y = "Marginal R² (fixed effects)") +
  my_theme2+
  scale_fill_manual(values = my_color[c(3,2,1,4)])

# Conditional R2: variance explained by fixed + random
g_r2c <- ggplot(filter(perf_long2, Metric == "R2c"),
                aes(x = Model, y = Value, fill = Model)) +
  geom_boxplot(outlier.colour = NA) +
  facet_wrap(~Season, nrow = 1) +
  labs(x = "", y = "Conditional R² (fixed + random)") +
  my_theme2 +
  scale_fill_manual(values = my_color[c(3,2,1,4)])

# Conditional R2 - Marginal R2
g_r2rand <- ggplot(filter(perf_long2, Metric == "R2_random (R2c - R2m)"),
                   aes(x = Model, y = Value, fill = Model)) +
  geom_boxplot(outlier.colour = NA) +
  facet_wrap(~Season, nrow = 1) +
  labs(x = "", y = "Random-effect (R²c − R²m)") +
  my_theme2 +
  scale_fill_manual(values = my_color[c(3,2,1,4)])

# Combine these 4 plots
g_model_perform <- plot_grid(g_rmse,g_r2m,g_r2c,g_r2rand,
                             nrow=2,align="hv")
print_g(g_model_perform,)

# Visualize model comparisons =============
# AIC Threshold ref: https://stats.libretexts.org/Bookshelves/Advanced_Statistics/Intermediate_Statistics_with_R_(Greenwood)/08%3A_Multiple_linear_regression/8.13%3A_AICs_for_model_selection
summary_df <- summary_df %>%
  mutate(
    # ΔAIC definition: AIC_small - AIC_big.
    # Positive => big model is better (lower AIC)
    interp_dAIC_storm_ag = case_when(
      mean_dAIC_storm_ag > 4 ~ "Strong support for adding Ag",
      mean_dAIC_storm_ag > 2  ~ "Moderate support for adding Ag",
      mean_dAIC_storm_ag > 0  ~ "Weak support for adding Ag",
      TRUE                    ~ "No support / worse with Ag"
    ),
    interp_dAIC_storm_site = case_when(
      mean_dAIC_storm_site > 4 ~ "Strong support for adding Site physics",
      mean_dAIC_storm_site > 2  ~ "Moderate support for adding Site physics",
      mean_dAIC_storm_site > 0  ~ "Weak support for adding Site physics",
      TRUE                      ~ "No support / worse with Site physics"
    )
  )

# Compare models =================
# ΔAIC = AIC_small - AIC_big
# Positive means big model improves AIC
# Storm vs Agricultural -----------
g_dAIC_storm_ag <- plot_dAIC(summary_df,"storm","ag")
# Chisq for Storm vs Agricultural
g_chisq_storm_ag <- plot_chisq(summary_df,"storm","ag")
  
# Storm vs Site ------------
g_dAIC_storm_site <- plot_dAIC(summary_df,"storm","site")
g_chisq_storm_site <- plot_chisq(summary_df,"storm","site")

# ag vs Full ----------
g_dAIC_ag_full <- plot_dAIC(summary_df,"ag","full")
g_chisq_ag_full <- plot_chisq(summary_df,"ag","full")

# site vs Full ------
g_dAIC_site_full <- plot_dAIC(summary_df,"site","full")
g_chisq_site_full <- plot_chisq(summary_df,"site","full")

# Combine these plots
g_model_compare <- plot_grid(g_dAIC_storm_ag,g_dAIC_storm_site,g_dAIC_ag_full,g_dAIC_site_full,
                   g_chisq_storm_ag,g_chisq_storm_site,g_chisq_ag_full,g_chisq_site_full,
                   nrow=2,align="hv")
print_g(g_model_compare,"Model_comparison",14,8)


# Drop 1 variable ==========================
# ΔAIC_drop = AIC_drop - AIC_full
# Positive => dropping the variable makes AIC worse => variable is important
drop_summary2 <- drop_summary %>%
  mutate(
    Dropped_group = case_when(
      Dropped %in% ag_vars ~ "Agricultural",
      Dropped %in% c("MeanSlope_per","HG") ~ "Site physics",
      TRUE ~ "Other"
    ),
    Dropped_group = factor(Dropped_group, levels = c("Agricultural","Site physics","Other"))
  )
# Rank the order of these variables
rank_df <- drop_summary2 %>%
  group_by(Dropped) %>%
  summarize(rank_score = mean(mean_dAIC_drop, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(rank_score))

drop_plot_df <- drop_summary2 %>%
  mutate(Dropped = factor(Dropped, levels = rank_df$Dropped))
# Plot delta AIC for each dropped variable
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
  # show Dropped names only on the left facet (SS)
  theme(
    axis.text.y = element_text(),
    axis.title.y = element_blank(),
    strip.background = element_blank()
  ) +
  geom_blank()

# Plot chisq for each dropped variable
g_drop_chi_h <- ggplot(drop_plot_df,
                       aes(x = Dropped, y = mean_chisq_drop, fill = Dropped_group)) +
  geom_col(color = "black") +
  geom_errorbar(aes(ymin = mean_chisq_drop - sd_chisq_drop,
                    ymax = mean_chisq_drop + sd_chisq_drop),
                width = 0.2) +
  facet_wrap(~Season, nrow = 1) +
  # annotation: with coord_flip, adjust hjust (not vjust) to move text above the bar
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
g_drop1 <- plot_grid(g_drop_dAIC_h,g_drop_chi_h,nrow=2)
print_g(g_drop1,"Model_drop1",12,8)

# Marginal effects ====================
# initiliaze a list to store plots
marginal_g_ls <- list()

for(season in c("SS","GS","FS")){
  df_fit <- Q_df_clean %>%
    filter(Season == season) %>%
    select(all_of(vars_all)) %>%
    na.omit()
  
  use_HG_season <- hg_okay(df_fit, min_n = 5)
  site_fx <- c("MeanSlope_per")
  if (use_HG_season) site_fx <- c(site_fx, "HG")
  
  # Fit one full model
  m_full_fit <- MER(
    df_fit,
    vars_to_scale = c(P_vars, "DOY","PerennialFrac","MeanSlope_per"),
    main_varls = c(P_vars,
                   "Annual_Tillage","DOY","PerennialFrac",
                   site_fx),
    random_varls = "Field_Name",
    res_varname = "log_RC",
    model_title = paste0(season, " Full (all data)"),
    REML = FALSE
  )
  m_full_season <- m_full_fit$model
  
  # Marginal effects on log scale
  g_I30 <- plot(ggpredict(m_full_season, terms = "log_I30"))
  g_Dur <- plot(ggpredict(m_full_season, terms = "log_Dur"))
  g_ARF <- plot(ggpredict(m_full_season, terms = "log_ARFdays7"))
  g_DSP <- plot(ggpredict(m_full_season, terms = "DOY"))
  g_PerennialF <- plot(ggpredict(m_full_season, terms = "PerennialFrac"))
  g_Tillage <- plot(ggpredict(m_full_season, terms = "Annual_Tillage"))
  g_Slope <- plot(ggpredict(m_full_season, terms = "MeanSlope_per"))
  
  plot_list <- list(g_I30, g_Dur, g_ARF, g_DSP, g_PerennialF, g_Tillage, g_Slope)
  
  if (use_HG_season) {
    g_HG <- plot(ggpredict(m_full_season, terms = "HG"))
    plot_list <- c(plot_list, list(g_HG))
  }
  
  g_row <- plot_grid(
    ggdraw() + draw_label(season, fontface = "bold", x = 0, hjust = 0),
    plot_grid(plotlist = plot_list, nrow = 1, align = "hv"),
    ncol = 1, rel_heights = c(0.15, 1)
  )
  
  marginal_g_ls[[season]] <- g_row
}

g_marginal <- plot_grid(plotlist = marginal_g_ls,nrow=3)
print_g(g_marginal, "Marginal_effect_full_model", 28, 12)

