# Author: Zhaozhe Chen
# Update Date: 2026.8.7

# This code includes shared functions for the seasonal mixed-effects models

# Scale continuous predictors within one balanced bootstrap sample
scale_model_variables <- function(df,variables){
  variables <- intersect(variables,names(df))
  variables <- variables[vapply(df[variables],is.numeric,logical(1))]
  
  for(variable in variables){
    variable_sd <- stats::sd(df[[variable]],na.rm=TRUE)
    
    if(is.finite(variable_sd) && variable_sd > 0){
      df[[variable]] <- as.numeric(scale(df[[variable]]))
    }
  }
  
  df
}

# Check whether a categorical predictor is supported in one sample
factor_has_support <- function(df,variable,min_n=5){
  if(!variable %in% names(df)){
    return(FALSE)
  }
  
  observed <- droplevels(factor(df[[variable]]))
  counts <- table(observed)
  
  length(counts) >= 2 && all(counts >= min_n)
}

# Construct one mixed-effects formula with a site random intercept
make_mixed_formula <- function(response,fixed_terms){
  fixed_text <- if(length(fixed_terms) == 0){
    "1"
  }else{
    paste(fixed_terms,collapse=" + ")
  }
  
  stats::as.formula(
    paste0(
      response,
      " ~ ",
      fixed_text,
      " + (1 | Field_Name)"
    )
  )
}

# Fit either the binomial occurrence model or linear mixed-effects model
fit_mixed_model <- function(
    df,
    response,
    fixed_terms,
    response_type=c("occurrence","continuous")){
  response_type <- match.arg(response_type)
  model_formula <- make_mixed_formula(response,fixed_terms)
  
  tryCatch(
    {
      if(response_type == "occurrence"){
        suppressWarnings(
          lme4::glmer(
            model_formula,
            data=df,
            family=stats::binomial(link="logit"),
            control=lme4::glmerControl(
              optimizer="bobyqa",
              optCtrl=list(maxfun=2e5)
            )
          )
        )
      }else{
        suppressWarnings(
          lme4::lmer(
            model_formula,
            data=df,
            REML=FALSE,
            control=lme4::lmerControl(
              optimizer="bobyqa",
              optCtrl=list(maxfun=2e5)
            )
          )
        )
      }
    },
    error=function(e) NULL
  )
}

# Extract Nakagawa marginal and conditional R-squared values
safe_nakagawa_r2 <- function(model){
  if(is.null(model)){
    return(c(R2m=NA_real_,R2c=NA_real_))
  }
  
  r2_result <- tryCatch(
    suppressWarnings(performance::r2_nakagawa(model)),
    error=function(e) NULL
  )
  
  if(is.null(r2_result)){
    return(c(R2m=NA_real_,R2c=NA_real_))
  }
  
  c(
    R2m=as.numeric(r2_result$R2_marginal),
    R2c=as.numeric(r2_result$R2_conditional)
  )
}

# Extract site random-intercept variance
safe_random_variance <- function(model){
  if(is.null(model)){
    return(NA_real_)
  }
  
  variance_table <- tryCatch(
    as.data.frame(lme4::VarCorr(model)),
    error=function(e) NULL
  )
  
  if(is.null(variance_table)){
    return(NA_real_)
  }
  
  site_variance <- variance_table %>%
    dplyr::filter(
      grp == "Field_Name",
      var1 == "(Intercept)"
    ) %>%
    dplyr::pull(vcov)
  
  if(length(site_variance) == 0){
    NA_real_
  }else{
    site_variance[1]
  }
}

# Extract likelihood-ratio-test statistics
safe_likelihood_ratio <- function(smaller_model,larger_model){
  if(is.null(smaller_model) || is.null(larger_model)){
    return(c(Chisq=NA_real_,P=NA_real_))
  }
  
  comparison <- tryCatch(
    suppressWarnings(
      suppressMessages(
        stats::anova(smaller_model,larger_model)
      )
    ),
    error=function(e) NULL
  )
  
  if(is.null(comparison) || nrow(comparison) < 2){
    return(c(Chisq=NA_real_,P=NA_real_))
  }
  
  c(
    Chisq=as.numeric(comparison$Chisq[2]),
    P=as.numeric(comparison$`Pr(>Chisq)`[2])
  )
}

# Calculate AUC for a fitted occurrence model
safe_model_auc <- function(model,df,response){
  if(is.null(model) || length(unique(df[[response]])) < 2){
    return(NA_real_)
  }
  
  tryCatch(
    {
      probability <- stats::predict(
        model,
        newdata=df,
        type="response",
        allow.new.levels=FALSE
      )
      
      roc_result <- pROC::roc(
        response=df[[response]],
        predictor=probability,
        quiet=TRUE
      )
      
      as.numeric(pROC::auc(roc_result))
    },
    error=function(e) NA_real_
  )
}

# Calculate in-sample RMSE for a fitted continuous-response model
safe_model_rmse <- function(model,df,response){
  if(is.null(model)){
    return(NA_real_)
  }
  
  tryCatch(
    {
      predicted <- stats::predict(model,newdata=df)
      sqrt(mean((df[[response]]-predicted)^2))
    },
    error=function(e) NA_real_
  )
}

# Extract model-level performance metrics
extract_model_metrics <- function(model,df,response,response_type){
  r2_values <- safe_nakagawa_r2(model)
  
  data.frame(
    AIC=if(is.null(model)) NA_real_ else stats::AIC(model),
    AUC=if(response_type == "occurrence"){
      safe_model_auc(model,df,response)
    }else{
      NA_real_
    },
    RMSE=if(response_type == "continuous"){
      safe_model_rmse(model,df,response)
    }else{
      NA_real_
    },
    R2m=r2_values[["R2m"]],
    R2c=r2_values[["R2c"]],
    REvar=safe_random_variance(model),
    Singular=if(is.null(model)){
      NA
    }else{
      lme4::isSingular(model,tol=1e-4)
    }
  )
}

# Fit the four nested model sets and the drop-one-variable models
fit_model_replication <- function(
    sampled_df,
    response,
    response_type,
    season,
    replication,
    storm_terms,
    agricultural_terms,
    site_terms,
    scale_terms,
    factor_min_n=5){
  supported_site_terms <- site_terms
  supported_storm_terms <- storm_terms

  if(
    "Frozen" %in% supported_storm_terms &&
    !factor_has_support(
      sampled_df,
      "Frozen",
      min_n=factor_min_n
    )
  ){
    supported_storm_terms <- setdiff(
      supported_storm_terms,
      "Frozen"
    )
  }
  
  for(variable in intersect(
      c("Hydrologic_Group","Tile"),
      supported_site_terms)){
    if(!factor_has_support(
        sampled_df,
        variable,
        min_n=factor_min_n
      )){
      supported_site_terms <- setdiff(
        supported_site_terms,
        variable
      )
    }
  }
  
  model_df <- scale_model_variables(
    sampled_df,
    scale_terms
  )
  
  model_terms <- list(
    Storm=supported_storm_terms,
    Agricultural=unique(c(supported_storm_terms,agricultural_terms)),
    Site=unique(c(supported_storm_terms,supported_site_terms)),
    Full=unique(
      c(
        supported_storm_terms,
        agricultural_terms,
        supported_site_terms
      )
    )
  )
  
  fitted_models <- lapply(
    model_terms,
    function(terms){
      fit_mixed_model(
        model_df,
        response=response,
        fixed_terms=terms,
        response_type=response_type
      )
    }
  )
  
  metric_rows <- lapply(
    names(fitted_models),
    function(model_name){
      metrics <- extract_model_metrics(
        fitted_models[[model_name]],
        model_df,
        response,
        response_type
      )
      
      cbind(
        data.frame(
          Season=season,
          Rep=replication,
          Model=model_name,
          n=nrow(model_df),
          Use_Frozen="Frozen" %in% supported_storm_terms,
          Use_Hydrologic_Group=
            "Hydrologic_Group" %in% supported_site_terms,
          Use_Tile="Tile" %in% supported_site_terms
        ),
        metrics
      )
    }
  )
  
  metrics_long <- dplyr::bind_rows(metric_rows)
  
  metric_wide <- metrics_long %>%
    dplyr::select(
      Season,
      Rep,
      n,
      Use_Frozen,
      Use_Hydrologic_Group,
      Use_Tile,
      Model,
      AIC,
      AUC,
      RMSE,
      R2m,
      R2c,
      REvar,
      Singular
    ) %>%
    tidyr::pivot_wider(
      names_from=Model,
      values_from=c(
        AIC,
        AUC,
        RMSE,
        R2m,
        R2c,
        REvar,
        Singular
      ),
      names_glue="{.value}_{Model}"
    )
  
  model_comparisons <- list(
    Storm_Agricultural=c("Storm","Agricultural"),
    Storm_Site=c("Storm","Site"),
    Agricultural_Full=c("Agricultural","Full"),
    Site_Full=c("Site","Full")
  )
  
  comparison_rows <- lapply(
    names(model_comparisons),
    function(comparison_name){
      model_names <- model_comparisons[[comparison_name]]
      smaller_name <- model_names[1]
      larger_name <- model_names[2]
      smaller_model <- fitted_models[[smaller_name]]
      larger_model <- fitted_models[[larger_name]]
      likelihood_ratio <- safe_likelihood_ratio(
        smaller_model,
        larger_model
      )
      
      data.frame(
        Season=season,
        Rep=replication,
        Comparison=comparison_name,
        Smaller_Model=smaller_name,
        Larger_Model=larger_name,
        Delta_AIC=
          metrics_long$AIC[metrics_long$Model == smaller_name]-
          metrics_long$AIC[metrics_long$Model == larger_name],
        Chisq=likelihood_ratio[["Chisq"]],
        P_Value=likelihood_ratio[["P"]]
      )
    }
  )
  
  drop_variables <- unique(
    c(agricultural_terms,supported_site_terms)
  )
  
  drop_rows <- lapply(
    drop_variables,
    function(dropped_variable){
      reduced_terms <- setdiff(
        model_terms$Full,
        dropped_variable
      )
      reduced_model <- fit_mixed_model(
        model_df,
        response=response,
        fixed_terms=reduced_terms,
        response_type=response_type
      )
      likelihood_ratio <- safe_likelihood_ratio(
        reduced_model,
        fitted_models$Full
      )
      
      data.frame(
        Season=season,
        Rep=replication,
        n=nrow(model_df),
        Dropped=dropped_variable,
        Variable_Group=if(
          dropped_variable %in% agricultural_terms
        ){
          "Agricultural"
        }else{
          "Site"
        },
        AIC_Dropped=if(is.null(reduced_model)){
          NA_real_
        }else{
          stats::AIC(reduced_model)
        },
        AIC_Full=metrics_long$AIC[
          metrics_long$Model == "Full"
        ],
        Delta_AIC=if(is.null(reduced_model)){
          NA_real_
        }else{
          stats::AIC(reduced_model)-
            metrics_long$AIC[
              metrics_long$Model == "Full"
            ]
        },
        Chisq=likelihood_ratio[["Chisq"]],
        P_Value=likelihood_ratio[["P"]]
      )
    }
  )
  
  list(
    Metrics=metric_wide,
    Metrics_Long=metrics_long,
    Comparisons=dplyr::bind_rows(comparison_rows),
    Drop_One=dplyr::bind_rows(drop_rows),
    Supported_Site_Terms=supported_site_terms
  )
}

# Summarize bootstrap model metrics by season and model
summarize_model_metrics <- function(metrics_long,response_type){
  metric_names <- if(response_type == "occurrence"){
    c("AUC","R2m","R2c")
  }else{
    c("RMSE","R2m","R2c")
  }
  
  metrics_long %>%
    dplyr::mutate(
      Random_R2=R2c-R2m
    ) %>%
    tidyr::pivot_longer(
      cols=dplyr::all_of(c(metric_names,"Random_R2")),
      names_to="Metric",
      values_to="Value"
    ) %>%
    dplyr::group_by(Season,Model,Metric) %>%
    dplyr::summarise(
      Replications=sum(!is.na(Value)),
      Mean=mean(Value,na.rm=TRUE),
      SD=stats::sd(Value,na.rm=TRUE),
      Median=stats::median(Value,na.rm=TRUE),
      Q25=stats::quantile(Value,0.25,na.rm=TRUE),
      Q75=stats::quantile(Value,0.75,na.rm=TRUE),
      .groups="drop"
    )
}

# Summarize nested-model comparisons by season
summarize_model_comparisons <- function(comparison_df){
  comparison_df %>%
    dplyr::group_by(
      Season,
      Comparison,
      Smaller_Model,
      Larger_Model
    ) %>%
    dplyr::summarise(
      Replications=sum(!is.na(Delta_AIC)),
      Mean_Delta_AIC=mean(Delta_AIC,na.rm=TRUE),
      SD_Delta_AIC=stats::sd(Delta_AIC,na.rm=TRUE),
      Mean_Chisq=mean(Chisq,na.rm=TRUE),
      SD_Chisq=stats::sd(Chisq,na.rm=TRUE),
      Significant_Percent=
        100*mean(P_Value < 0.05,na.rm=TRUE),
      .groups="drop"
    )
}

# Summarize drop-one-variable comparisons by season
summarize_drop_one <- function(drop_df){
  drop_df %>%
    dplyr::group_by(
      Season,
      Dropped,
      Variable_Group
    ) %>%
    dplyr::summarise(
      Replications=sum(!is.na(Delta_AIC)),
      Mean_Delta_AIC=mean(Delta_AIC,na.rm=TRUE),
      SD_Delta_AIC=stats::sd(Delta_AIC,na.rm=TRUE),
      Mean_Chisq=mean(Chisq,na.rm=TRUE),
      SD_Chisq=stats::sd(Chisq,na.rm=TRUE),
      Significant_Percent=
        100*mean(P_Value < 0.05,na.rm=TRUE),
      .groups="drop"
    )
}

# Extract the fixed-effect table from one final full model
extract_fixed_effects <- function(model,season,response_part){
  if(is.null(model)){
    return(data.frame())
  }
  
  coefficient_matrix <- summary(model)$coefficients
  
  coefficient_table <- data.frame(
    Term=rownames(coefficient_matrix),
    Estimate=as.numeric(coefficient_matrix[,1]),
    Std_Error=as.numeric(coefficient_matrix[,2]),
    Statistic=as.numeric(coefficient_matrix[,3]),
    P_Value=if(ncol(coefficient_matrix) >= 4){
      as.numeric(coefficient_matrix[,4])
    }else{
      NA_real_
    },
    row.names=NULL,
    check.names=FALSE
  )
  
  coefficient_table %>%
    dplyr::mutate(
      Analysis=response_part,
      Season=season
    ) %>%
    dplyr::select(
      Analysis,
      Season,
      Term,
      Estimate,
      Std_Error,
      Statistic,
      P_Value
    )
}
