# Author: Zhaozhe Chen
# Update Date: 2026.1.22

# This code is to make plots for the DF projects

library(ggplot2)
library(cowplot)
library(RColorBrewer)
library(ggrepel)
library(gghalves)
library(sf)
library(here)
library(ggpubr)
library(ggpointdensity)
library(scales)

# County-level shape file for plotting
US_bd <- st_read(here("00_Data","Msc","cb_2018_us_county_20m/cb_2018_us_county_20m.shp"))
# Only keep WI county
WI_bd <- US_bd[6][US_bd$STATEFP == 55,]
# Get the outer boundary of WI
WI_outer_bd <- st_union(WI_bd)

# Theme for maps
my_theme <- theme(
  #axis.line=element_line(color="black"),
  panel.background = element_blank(),
  #panel.border = element_rect(colour="black",fill=NA),
  legend.key = element_blank(),
  #legend.key.size = unit(6,"cm"),
  #aspect.ratio = 1/1,
  #legend.key.size = unit(0.3,'cm'),
  legend.text = element_text(size=18),
  plot.title = element_text(size=18),
  axis.text = element_blank(),
  axis.title = element_blank(),
  axis.ticks = element_blank(),
  legend.position = "right",
  legend.title = element_text(size=18)
)

# Theme for other plots
my_theme2 <- theme(
  panel.border = element_rect(colour="black",fill=NA),
  axis.text = element_text(size=18),
  legend.position = "none",
  axis.title = element_text(size=18),
  axis.line=element_line(color="black"),
  panel.background = element_blank(),
  legend.key = element_blank(),
  legend.text = element_text(size=18),
  plot.title = element_text(size=18),
  legend.title = element_text(size=18),
  legend.background = element_blank()
)

# This function is to print pdf and png figure
# Input is the figure g,title,width, and height
print_g <- function(g,title,w,h){
  pdf(paste0(Output_path,"/",title,".pdf"),
      width=w,height=h)
  print(g)
  dev.off()
  png(paste0(Output_path,"/",title,".png"),
      width=w,height=h,units = "in",
      res=600)
  print(g)
  dev.off()
}

# This function is to make bar plots (could make both stack bar plots or regular bar plots)
# total_color is the color for "Total" if there is any
plot_bar <- function(df,x_varname,y_varname,fill_name = NULL,
                     x_title,y_title,fill_title = NULL,
                     label_x = 0.1,label_y = 0.9,my_cols = NULL,bar_color = "grey60",total_color = NULL){
  if (is.null(fill_name)) {
    # If there is a specified color for "Total"
    if(!is.null(total_color)){
      df$is_total <- ifelse(df[[x_varname]] == "Total","Total","Site")
      g <- ggplot(
        data = df,
        aes(x = .data[[x_varname]],
            y = .data[[y_varname]],
            fill = is_total)
      ) +
        geom_col(color = "black") +
        labs(x = x_title, y = y_title) +
        scale_fill_manual(
          values = c("Site" = bar_color, "Total" = total_color),
          guide = "none"
        ) +
        my_theme2 +
        theme(
          axis.text.x = element_text(angle = 45, vjust = 0.5)
        )
    }else{
      # Make regular bars
      g <- ggplot(
        data = df,
        aes(x = .data[[x_varname]],
            y = .data[[y_varname]])
      ) +
        geom_col(color = "black", fill = bar_color) +
        labs(x = x_title, y = y_title) +
        my_theme2 +
        theme(
          axis.text.x = element_text(angle = 45, vjust = 0.5)
        )
    }       
  } else {
    # Make stacking bars
    g <- ggplot(
      data = df,
      aes(x = .data[[x_varname]],
          y = .data[[y_varname]],
          fill = .data[[fill_name]])
    ) +
      geom_col(color = "black") +
      labs(x = x_title, y = y_title, fill = fill_title) +
      my_theme2 +
      theme(
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = c(label_x, label_y)
      )
    
    # Apply manual colors if provided
    if (!is.null(my_cols)) {
      g <- g + scale_fill_manual(values = my_cols)
    }
  }
  return(g)
}

# This function is to make maps of DF sites
# Input include:
# df: the site info df
# var_size: variable name for size
# var_fill: variable name for fill
# var_label: variable name for labeling
# my_color: palette for coloring
# size_name and fill_name are the titles for legends
DF_map <- function(df,var_size,var_fill,var_label,my_color,
                   size_name,fill_name){
  # Turn the filled value to be factor
  fill_factor <- factor(df[[var_fill]])
  # Get # of levels
  n_level <- length(levels(fill_factor))
  # Choose the first n_level colors from my_color
  pal <- my_color[1:n_level]
  
  g_map <- ggplot()+
    geom_sf(data=WI_bd,fill="#aec8df",alpha=0.8,color="grey")+
    geom_sf(data=WI_outer_bd,fill=NA,color="black")+
    geom_point(data=df,
               aes(x=LONG_approx,y=LAT_approx,
                   size = .data[[var_size]],
                   fill = .data[[var_fill]]),
               shape = 21,
               color="black",
               alpha=0.8)+
    scale_fill_manual(values = pal)+
    my_theme+
    geom_label_repel(data=df,
                     aes(x=LONG_approx,y=LAT_approx,label=.data[[var_label]]),
                     point.padding = 0,
                     label.padding = 0.25,
                     box.padding = 0.25,
                     min.segment.length = 0,
                     max.overlaps = 30,
                     segment.color="black")+
    guides(fill = guide_legend(override.aes = list(size = 6,shape=21)),
           size = guide_legend(override.aes = list(shape=21)))+
    labs(fill=fill_name,size=size_name)
  
  return(g_map)
}

# This function is to make box plots, violin plots, and jittered points
plot_box <- function(df,x_varname,y_varname,fill_name=NULL,
                     x_title,y_title,fill_title = NULL,
                     label_x = 0.1,label_y = 0.9,
                     jitter_offset = 0.2,jitter_width = 0.1,box_width = 0.1,y_limits = NULL,my_cols=NULL,white_box = NULL){
  
  # This is a function to compare across groups
  get_group_p_label <- function(df, x_varname, y_varname) {
    d2 <- df %>% dplyr::select(dplyr::all_of(c(x_varname, y_varname))) %>% tidyr::drop_na()
    x <- as.factor(d2[[x_varname]])
    
    # If 2 groups -> Wilcoxon; if >2 -> Kruskal-Wallis (robust default)
    if (nlevels(x) == 2) {
      p <- wilcox.test(d2[[y_varname]] ~ x)$p.value
      test_name <- "Wilcoxon"
    } else {
      p <- kruskal.test(d2[[y_varname]] ~ x)$p.value
      test_name <- "Kruskal"
    }
    
    paste0(test_name, " p=", format.pval(p, digits = 3, eps = 1e-3))
  }
  
  # Trunk values to fit in y_limits
  if(!is.null(y_limits)){
    df[[y_varname]][df[[y_varname]] > y_limits[2]] <- y_limits[2]
    df[[y_varname]][df[[y_varname]] < y_limits[1]] <- y_limits[1]
  }
  
  p_lab <- get_group_p_label(df,x_varname,y_varname)
  
  g <- ggplot(data = df,
              aes(x = .data[[x_varname]],y = .data[[y_varname]],fill = .data[[fill_name]])) +
    geom_half_violin(alpha = 0.5, color=NA)+
    # Jittered points, nudged a bit to the right
    geom_jitter(
      aes(x = as.numeric(as.factor(.data[[x_varname]])) + jitter_offset,
          color=.data[[fill_name]]),
      position = position_jitter(width = jitter_width),
      size = 2,
      alpha = 0.7
    ) +
    labs(x = x_title, y = y_title, fill = fill_title,color=fill_title) +
    my_theme2+
    theme(
      axis.text.x = element_text(angle = 45, vjust = 0.5),
      legend.position = c(label_x, label_y)
    )+
    guides(color="none")+
    annotate("text",x=Inf,y=-Inf,label=p_lab,
             hjust=1.05,vjust=-0.6,size=5)
  
  if(is.null(white_box)){
    # Boxplot
    g <- g + geom_boxplot(width = box_width,color = "black",outlier.color = NA)
  }else{
    g <- g + geom_boxplot(width = box_width,color = "black",fill="white",outlier.color = NA)
  }
  
  # Change labels if y_limits are provided
  if(!is.null(y_limits)){
    g <- g + coord_cartesian(ylim = y_limits) +
      scale_y_continuous(
        breaks = scales::pretty_breaks(),
        labels = function(brks) {
          labs <- as.character(brks)
          i_max <- which.max(brks)
          labs[i_max] <- paste0("\u2265 ", y_limits[2])  # "≥ upper_lim"
          labs
        }
      )
  }
  
  if(!is.null(my_cols)){
    g <- g+ 
      scale_fill_manual(values = my_cols)+
      scale_color_manual(values = my_cols)
  }
  
  return(g)
}


# This function is to make pdp plot
# rf is the RF model
# df_train is training dataset
# var_re is the variable name for response variable
# IP_var_ls is the list of response variables
# i is the idx for response variable to be plotted against
make_pdp_plot <- function(rf,df_train,var_re,IP_var_ls,i){
  # Get pdp using package function
  pp <- partialPlot(rf, pred.data = df_train, x.var = IP_var_ls[i], plot = FALSE)
  # Make it a df
  test <- data.frame(x=pp$x,y=pp$y)
  # Get predictor data
  var_name <- IP_var_ls[i]
  xcol <- df_train[[var_name]]
  
  # For numerical data
  if(is.numeric(xcol)){
    # Get density of the data
    test$density <- density(xcol,n=nrow(test))$y
    
    g <- ggplot(test,aes(x,y))+
      geom_line(aes(color=density),size=2)+
      my_theme2+
      labs(x=var_name,y=var_re)+
      theme(aspect.ratio = 1/1.5,
            legend.position = "none")+
      scale_color_distiller(palette = "YlGnBu",direction = "1")
  }
  
  # For categorical data
  if(is.character(xcol) | is.factor(xcol)){
    test$x <- factor(test$x,levels = levels(xcol))
    # Get frequency table
    freq_df <- as.data.frame(table(xcol))
    freq_df$x <- factor(freq_df$x,levels=levels(xcol))
    test <- dplyr::left_join(test, freq_df, by = "x")
    
    g <- ggplot(test, aes(x = y, y = x)) +
      geom_col(aes(fill = Freq), color = "black", width = 0.75) +
      my_theme2 +
      labs(x = var_re, y = var_name) +
      theme(aspect.ratio = 1/1.5, legend.position = "none") +
      scale_fill_distiller(palette = "YlGnBu",direction = "1")
  }
  return(g)
}

# This function is to make scatter plot, color coded by variable of interest
# var_re is response variable name
# var_name1 is the predictor name
# var_group is the variable name to be color coded with
plot_scatter <- function(df,var_name1,var_re,var_group,
                         y_limits=NULL,x_limits=NULL,mycolor){
  # Only keep the target variables
  df_tmp <- data.frame(x=df[[var_name1]],
                       y=df[[var_re]],
                       group = df[[var_group]])
  df_tmp <- na.omit(df_tmp)
  
  # Trunk values to fit in y_limits
  if(!is.null(y_limits)){
    df_tmp$y[df_tmp$y > y_limits[2]] <- y_limits[2]
    df_tmp$y[df_tmp$y < y_limits[1]] <- y_limits[1]
  }
  
  # Trunk values to fit in x_limits
  if(!is.null(x_limits)){
    df_tmp$x[df_tmp$x > x_limits[2]] <- x_limits[2]
    df_tmp$x[df_tmp$x < x_limits[1]] <- x_limits[1]
  }
  
  g <- ggplot(df_tmp,aes(x= x,y = y,color=group)) +
    geom_point(size=2,alpha=0.6)+
    my_theme2+
    theme(legend.position = "right")+
    labs(x = var_name1,y=var_re,color=var_group)+
    scale_color_manual(values = mycolor)
  
  g <- g + coord_cartesian(
    xlim = x_limits,
    ylim = y_limits
  )
  
  # Revise truncated labels
  if (!is.null(y_limits)) {
    g <- g + scale_y_continuous(
      breaks = scales::pretty_breaks(),
      labels = function(brks) {
        labs <- as.character(brks)
        i_min <- which.min(brks)
        i_max <- which.max(brks)
        #labs[i_min] <- paste0("\u2264 ", y_limits[1])  # "≤ lower"
        labs[i_max] <- paste0("\u2265 ", y_limits[2])  # "≥ upper"
        labs
      }
    )
  }
  
  if (!is.null(x_limits)) {
    g <- g + scale_x_continuous(
      breaks = scales::pretty_breaks(),
      labels = function(brks) {
        labs <- as.character(brks)
        i_min <- which.min(brks)
        i_max <- which.max(brks)
        #abs[i_min] <- paste0("\u2264 ", x_limits[1])  # "≤ lower"
        labs[i_max] <- paste0("\u2265 ", x_limits[2])  # "≥ upper"
        labs
      }
    )
  }
  return(g)
}

# This function is to plot distributions of the continuous data
Dist_bar <- function(df,varname,xtitle){
  g <- ggplot(data = df,aes(x=.data[[varname]]))+
    geom_histogram(fill="grey",color="black")+
    labs(x=xtitle)+
    my_theme2
  return(g)
}

# This function is to plot mean and sd of target var across groups, color coded by var_group
# Error bar represents bootstrapped 95% CI
# df is the input df
# varname1 is the variable name on the x axis
# varname2 is the variable name on the y axis
# var_group is the variable name to be grouped by
# ytitle
# xtitle
# grouptitle
# mycolor is the colors
plot_Qprob <- function(df,varname1,varname2,vargroup,xtitle,ytitle,grouptitle,mycolor){
  # Get a df of only target variables
  df_tmp <- df[,c(varname1,varname2,vargroup)]
  # Only keep complete obs
  df_tmp <- na.omit(df_tmp)
  names(df_tmp) <- c("x","y","group")
  
  # Make plot
  g <- ggplot(data=df_tmp,aes(x=x,y=y,color=group,fill=group))+
    stat_summary(fun.data=mean_cl_boot,geom="errorbar",width=0.2,
                 position = position_dodge(width=0.4))+
    stat_summary(fun=mean,geom="point",shape=21,size=2.5,color="black",
                 position = position_dodge(width=0.4))+
    scale_color_manual(values = mycolor)+
    scale_fill_manual(values = mycolor)+
    my_theme2+
    theme(legend.position = c(0.2,0.8))+
    labs(x = xtitle,y=ytitle,color=grouptitle,fill=grouptitle)
  return(g)
}

# This function is to plot # of P events per bin
plot_Pcount_by_bin <- function(df, bin_var, xtitle = bin_var) {
  
  df_cnt <- df %>%
    filter(!is.na(.data[[bin_var]])) %>%
    count(.data[[bin_var]], name = "n_P")
  
  g <- ggplot(df_cnt, aes(x = .data[[bin_var]], y = n_P)) +
    geom_col(fill = "grey70", color = "black") +
    geom_text(aes(label = n_P), vjust = -0.4, size = 5) +
    labs(x = xtitle, y = "n(P events)") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    my_theme2
  
  return(g)
}

# This function is to plot # of Q events per bin
plot_Qcount_by_bin <- function(df,bin_var,y_var = "Q_Occurred",xtitle = bin_var){
  df_cnt <- df %>%
    filter(!is.na(.data[[bin_var]]),!is.na(.data[[y_var]])) %>%
    group_by(.data[[bin_var]]) %>%
    summarize(n_Q = sum(.data[[y_var]]==1),.groups = "drop")
  
  g <- ggplot(df_cnt, aes(x = .data[[bin_var]], y = n_Q)) +
    geom_col(fill = "grey70", color = "black") +
    geom_text(aes(label = n_Q), vjust = -0.4, size = 5) +
    labs(x = xtitle, y = "n(Q Occurrence)") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    my_theme2
  
  return(g)
}

# This function is to plot total Q depth per bin
plot_Qdepth_by_bin <- function(df,bin_var,xtitle = bin_var){
  df_sum <- df %>%
    filter(!is.na(.data[[bin_var]]),!is.na(Q_total_in),!is.na(Associated_Q)) %>%
    # Sum Q depth only for events where Q occurred
    filter(Associated_Q == TRUE) %>%
    group_by(.data[[bin_var]]) %>%
    summarise(total_Q_in = sum(Q_total_in))
  
  # Total Q
  Q_total <- sum(df_sum$total_Q_in)
  
  g <- ggplot(df_sum, aes(x = .data[[bin_var]], y = total_Q_in)) +
    geom_col(fill = "grey70", color = "black") +
    geom_text(aes(label = round(total_Q_in, 2)), vjust = -0.4, size = 5) +
    geom_text(aes(y = total_Q_in/2, label = paste0(round(total_Q_in/Q_total,2)*100," %")),size=5) +
    labs(x = xtitle, y = "Total Q (in)") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    my_theme2
  
  return(g)
}

# This function is to plot Q occurrence across continuous x variable
plot_Qoccurence_x <- function(df,x_varname,xtitle,linecolor){
  g <- ggplot(df,aes(x = .data[[x_varname]],y=Q_Occurred))+
    geom_point(alpha=0.08)+
    stat_summary_bin(fun = mean, bins = 25, geom = "line",color = linecolor,linewidth = 1) +
    labs(y = "P(Q Occurrence)", x = xtitle)+
    my_theme2
  
  return(g)
}

# This function is to plot Q occurrence response surface to two P characteristics
# Plot No Q first, then Q occured, for visualization
scatter_Q_occurence <- function(df,x_varname,y_varname,mycolor,x_title,y_title){
  g <- ggplot()+
    geom_point(data = df %>%
                 filter(Associated_Q == FALSE),
               aes(x=.data[[x_varname]],y=.data[[y_varname]],color=Associated_Q),
               size=2,alpha=0.6)+
    geom_point(data = df %>%
                 filter(Associated_Q == TRUE),
               aes(x=.data[[x_varname]],y=.data[[y_varname]],color=Associated_Q),
               size=2,alpha=0.6)+
    my_theme2+
    scale_color_manual(values = mycolor)+
    labs(x = x_title,y=y_title,color="Q Occurred?")
  
  return(g)
}

# This function is to visualize the marginal effect of each variable in the mixed-effect logistic regression model
# varname is the variable name to be plotted against (which should be in the model)
# var_res is the variable name for the response
marginal_plot <- function(df,model,varname,var_res,x_title,y_title){
  # predict marginal changes with respect to this predictor
  pred_tmp <- ggeffect(model,terms = varname)
  g <- ggplot()+
    # raw observations (jittered)
    geom_jitter(data = df,aes(x = .data[[varname]], y = .data[[var_res]]),
                height = 0.05,width = 0,alpha = 0.2) +
    # Plot model prediction
    geom_line(data=pred_tmp,aes(x=x,y=predicted),
              linewidth = 1.2,color="black")+
    # Ribbon
    geom_ribbon(data=pred_tmp,aes(x=x,ymin=conf.low,ymax=conf.high),alpha=0.25)+
    labs(x = x_title,y=y_title)+
    my_theme2
  return(g)
}

# This function is to compare model performance vs observations
compare_model <- function(df,model,var_res){
  df$pred_prob <- predict(model,type= "response")
  df_cal <- df %>%
    mutate(bin = ntile(pred_prob,20)) %>%
    group_by(bin) %>%
    summarize(obs = mean(.data[[var_res]]),
              pred = mean(pred_prob))
  # Get R2
  R2 <- cor(df_cal$pred,df_cal$obs)^2
  g <- ggplot(df_cal,aes(x=pred,y=obs))+
    geom_point(size=3) +
    geom_abline(slope=1,intercept=0,linetype = "dashed")+
    labs(x = "Modeled P(Q Occurrence)",y="Observed P(Q Occurrence)")+
    annotate("text",x=Inf,y=-Inf,
             label = paste0("R2=",round(R2,3)),
             hjust=1.1,vjust=-0.5,size=5)+
    my_theme2
  return(g)
}




plot_marginal_gge <- function(model, term, x_title = term, y_title = "P(Q Occurrence)", color = "black") {
  df_eff <- as.data.frame(ggeffects::ggeffect(model, terms = term))
  # ggeffect returns columns: x, predicted, conf.low, conf.high (and group if interaction)
  
  g <- ggplot(df_eff, aes(x = x, y = predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
    geom_line(linewidth = 1) +
    labs(x = x_title, y = y_title) +
    my_theme2
  
  return(g)
}

plot_marginal_factor_gge <- function(model, term, x_title = term, y_title = "P(Q Occurrence)") {
  df_eff <- as.data.frame(ggeffects::ggeffect(model, terms = term))
  
  g <- ggplot(df_eff, aes(x = x, y = predicted)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15) +
    labs(x = x_title, y = y_title) +
    my_theme2
  
  return(g)
}

# This function is to make scatter plots with annotated linear regression lines
plot_scatter_lm <- function(df, x, y, group = NULL,
                            point_alpha = 0.8, point_size = 2,
                            line_size = 0.9, se = FALSE,
                            p_digits = 3, r2_digits = 2,
                            label_x = 0.02, label_y = 0.98,
                            my_colors = my_colors) {
  
  # keep only needed cols + drop NA
  cols <- c(x, y, group)
  d <- df %>%
    dplyr::select(dplyr::all_of(cols)) %>%
    tidyr::drop_na()
  
  # helper to compute lm stats (p for slope, R2)
  lm_stats <- function(dat) {
    fit <- lm(dat[[y]] ~ dat[[x]], data = dat)
    sm  <- summary(fit)
    p   <- sm$coefficients[2, 4]
    r2  <- sm$r.squared
    tibble::tibble(
      p = p,
      r2 = r2,
      label = paste0(
        "p=", formatC(p, format = "g", digits = p_digits),
        ", R²=", formatC(r2, format = "f", digits = r2_digits)
      )
    )
  }
  
  if (is.null(group)) {
    stats <- lm_stats(d) %>%
      dplyr::mutate(.grp = "All")
    
    p <- ggplot(d, aes(x = .data[[x]], y = .data[[y]])) +
      geom_point(alpha = point_alpha, size = point_size,color=my_colors) +
      geom_smooth(method = "lm", se = se, linewidth = line_size)
    
    # one label, placed in plot corner
    p <- p +
      annotate(
        "text",
        x = -Inf, y = Inf,
        hjust = label_x, vjust = label_y,
        label = stats$label[1]
      )+
      my_theme2
  } else {
    
    # per-group stats
    stats <- d %>%
      dplyr::group_by(.data[[group]]) %>%
      dplyr::group_modify(~ lm_stats(.x)) %>%
      dplyr::ungroup()
    
    # compute dynamic spacing in data units
    y_range <- range(d[[y]], na.rm = TRUE)
    y_span  <- diff(y_range)
    
    stats <- stats %>%
      dplyr::mutate(
        .row = dplyr::row_number(),
        x_pos = min(d[[x]], na.rm = TRUE),
        y_pos = max(d[[y]], na.rm = TRUE) - (.row - 1) * 0.08 * y_span
      )
    
    p <- ggplot(d, aes(x = .data[[x]], y = .data[[y]], color = .data[[group]])) +
      geom_point(alpha = point_alpha, size = point_size) +
      geom_smooth(aes(fill=.data[[group]]),method = "lm", se = se, linewidth = line_size) +
      geom_text(
        data = stats,
        aes(x = x_pos, y = y_pos, label = label, color = .data[[group]]),
        hjust = 0,
        show.legend = FALSE
      )+
      scale_color_manual(values = my_colors)+
      scale_fill_manual(values = my_colors)+
      guides(color=guide_legend(nrow=4))+
      my_theme2+
      theme(legend.position = "bottom",
            legend.title.position = "top")
  }
  return(p)
}

# This function plots density of variable across groups
Density_group <- function(df,varname,group,xtitle,my_colors){
  g <- ggplot(data=df,aes(x=.data[[varname]],color=.data[[group]],fill=.data[[group]]))+
    geom_density(alpha=0.3)+
    labs(x=xtitle)+
    scale_color_manual(values = my_colors)+
    scale_fill_manual(values = my_colors)+
    guides(color=guide_legend(nrow=4))+
    my_theme2+
    theme(legend.position = "bottom",
          legend.title.position = "top")
  return(g)
}
