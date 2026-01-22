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
  g_compare <- compare_model(df,model,var_res = res_varname)+
    ggtitle(model_title)
  
  # Output the model and the plot
  out <- list(model = model,g = g_compare)
  return(out)
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
