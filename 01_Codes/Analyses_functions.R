# Author: Zhaozhe Chen
# Update Date: 2026.1.22

# This code includes functions to process and analyze DF dataset

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

# This function is to construct Mixed-effect logistic regression model
# Input include:
# df: input df
# vars_to_scale: a list of variable names to scale (normalize)
# main_varls: a list of variable names for main effects
# random_varls: a list of variable names for random effects
# res_varname: variable name for response variable
# model_title: title for the model, to be included in the comparison plot
MELR <- function(df,vars_to_scale = NULL,main_varls,random_varls,res_varname,model_title){
  # Only keep target variables
  vars_to_keep <- c(res_varname,main_varls,random_varls)
  df <- df %>%
    select(all_of(vars_to_keep)) %>%
    na.omit()
  
  # Normalize required variables
  if(!is.null(vars_to_scale) && length(vars_to_scale) > 0){
    df[vars_to_scale] <- scale(df[vars_to_scale])  
  }
  
  # Make the formula for modeling
  fixed_effect <- paste(main_varls,collapse = "+")
  random_effect <- paste0("(1|", random_varls, ")", collapse = " + ")
  form <- as.formula(paste0(res_varname,"~",fixed_effect,"+",random_effect))
  
  # Fit model
  model <- glmer(form,data=df,family = binomial(link = "logit"),
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
  
  # Make plots to compare modeled response vs observations
  #g_compare <- compare_model(df,model,var_res = res_varname)+
  #  ggtitle(model_title)
  
  # Output the model and the plot
  #out <- list(model = model,g = g_compare)
  return(list(model=model,data=df))
}

# This is a wrapper function to output exploratory figures of P_df dataset
# Input is the subset of P_df, which should include only the target season
# And the g_name, which is the title for figures for this season
explore_plots_wrapper <- function(P_df,g_name){
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
    # Probability of Q occurrence across continuous explanatory variable
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
  # Output this correlation matrix
  print_g(g_CM,paste0("CM_",g_name),8,8)  
}

# Compute AUC for a glmer model on a dataset (same columns used to fit)
get_auc_glmer <- function(model, df, res_var = "Q_Occurred"){
  p <- predict(model, newdata = df, type = "response", allow.new.levels = FALSE)
  roc_obj <- pROC::roc(response = df[[res_var]], predictor = p, quiet = TRUE)
  as.numeric(pROC::auc(roc_obj))
}

# This function is to conduct mixed-effect linear regression model
MER <- function(df,
                vars_to_scale = NULL,
                main_varls,
                random_varls,
                res_varname = "log_RC",
                model_title = NULL,
                REML = FALSE) {
  
  # ---- Keep only target variables and drop NA ----
  vars_to_keep <- unique(c(res_varname,
                           main_varls,
                           random_varls))
  
  df2 <- df %>%
    dplyr::select(dplyr::all_of(vars_to_keep)) %>%
    tidyr::drop_na()
  
  # ---- Scale numeric predictors if requested ----
  if (!is.null(vars_to_scale) && length(vars_to_scale) > 0) {
    # only scale columns that exist
    vars_to_scale <- intersect(vars_to_scale, names(df2))
    
    # scale numeric only (avoid accidental scaling of factors)
    num_vars <- vars_to_scale[sapply(df2[vars_to_scale], is.numeric)]
    if (length(num_vars) > 0) {
      df2[num_vars] <- scale(df2[num_vars])
    }
  }
  
  # ---- Build formula ----
  # Fixed effects
  fixed_effect <- paste(main_varls, collapse = " + ")
  if (length(main_varls) == 0) fixed_effect <- "1"
  
  random_effect <- paste0("(1|", random_varls, ")", collapse = " + ")
  
  # Full formula
  form <- as.formula(paste0(res_varname, " ~ ", fixed_effect, " + ", random_effect))
  
  # ---- Fit model ----
  model <- lme4::lmer(
    form,
    data = df2,
    REML = REML,
    control = lme4::lmerControl(
      optimizer = "bobyqa",
      optCtrl = list(maxfun = 2e5)
    )
  )
  
  return(list(model = model, data = df2, formula = form, title = model_title))
}

# =========================
# Below are functions for evaluating or comparing the performance of mixed-effect models
# =========================
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



