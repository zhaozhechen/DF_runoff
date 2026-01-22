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


