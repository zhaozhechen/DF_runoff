# Author: Zhaozhe Chen
# Update Date: 2025.7.22

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
  legend.title = element_text(size=18)
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

# This function makes bar plot of target variable
# Input include:
# df: the site info df
# var_fill: variable name for fill
# fill_label: title for the legends
DF_bar <- function(df,var_fill,my_color,fill_name){
  # Turn the filled value to be factor
  fill_factor <- factor(df[[var_fill]])
  # Get # of levels
  n_level <- length(levels(fill_factor))
  # Choose the first n_level colors from my_color
  pal <- my_color[1:n_level]
  g_bar <- ggplot(data=df,
                  aes(y=.data[[var_fill]],
                      fill = .data[[var_fill]]))+
    geom_bar(color="black")+
    scale_fill_manual(values = pal)+
    my_theme2+
    labs(x = "# of Sites",y = "")+
    ggtitle(fill_name)
  return(g_bar)  
}

# This function combines map of the target variable and distribution of the target variable
# Input include:
# df: the site info df
# var_size: variable name for size
# var_fill: variable name for fill
# var_label: variable name for labeling
# my_color: palette for coloring
# size_name and fill_name are the titles for legends
# w and h: width and height of output figure
Site_plot <- function(df,var_size,var_fill,var_label,my_color,
                      size_name,fill_name,w,h){
  g_map <- DF_map(df,var_size,var_fill,var_label,my_color,
                  size_name,fill_name)
  g_bar <- DF_bar(df,var_fill,my_color,fill_name)  
  g <- plot_grid(g_map,g_bar,nrow=2,
                 rel_heights = c(1.5,1))
  print_g(g,paste0("DF_Site_",var_fill),w,h)
}

# This function is to plot bar plots for field-year data
Year_plot <- function(varname){
  g_bar <- DF_bar(varname,DF_meta_Year)
  print_g(g_bar,paste0("DF_Year_",varname),6,5)
}

# This function is to make boxplots for target variables grouped by storm vs non-storm
var_storm_compare <- function(varname,df,my_title){
  # Summarize eof_df based on the grouping criteria
  eof_summary <- df %>%
    group_by(Field_Name,storm) %>%
    summarise(var_sum = sum(.data[[varname]],na.rm=TRUE)) %>%
    group_by(Field_Name) %>%
    mutate(
      total = sum(var_sum),
      contribution = var_sum/total*100)
  
  # Make boxplots for comparison
  g <- ggplot(data=eof_summary,aes(x=storm,y=contribution,color=storm,fill=storm))+
    geom_half_violin(alpha = 0.5, color=NA)+
    geom_boxplot(width = 0.1,color="black",outlier.color = NA)+
    geom_jitter(aes(x=as.numeric(as.factor(storm))+0.2),
                position = position_jitter(width=0.1))+
    my_theme2+
    labs(y = "Contribution (%)",x="")+
    ggtitle(my_title)
  
  return(g)
}

# This function is to plot variables across different groups
var_compare_group <- function(varname,group_var,df,x_title,y_title){
  # log transformation of the target value
  df[[varname]] <- log(df[[varname]])

  g <- ggplot(df,aes(x=factor(.data[[group_var]]),y=.data[[varname]],color=frozen,fill=frozen))+
   # geom_half_violin(alpha = 0.5,color=NA)+
    geom_boxplot(width = 0.1,color="black",outlier.color = NA)+
    geom_jitter(aes(x=as.numeric(as.factor(.data[[group_var]]))+0.2),
                position = position_jitter(width=0.1),
                alpha=0.7)+
    my_theme2+
    labs(x=x_title,y=y_title,color="",fill="")
  return(g)  
}

# This function is to make plots for all target variables across the same group
var_compare_group_all <- function(varname_ls,group_var,df,x_title,y_title_ls,w,h,g_title){
  g_all <- list()
  for(i in 1:length(varname_ls)){
    g <- var_compare_group(varname_ls[i],group_var,df,x_title,y_title_ls[i])
    g_all[[i]] <- g
  }
  g_all[[length(varname_ls)]] <- g_all[[length(varname_ls)]] + theme(legend.position = "bottom")
  # Put them together
  g_all <- plot_grid(plotlist = g_all,ncol=1)
  print_g(g_all,g_title,w,h)
  return(g_all)
}

# This function plots distribution of target variable in eof_df
# Input include
# df: eof_df
# varname: the target variable name
# x_title: title on the x axis
Hist_plot <- function(df,varname,x_title){
  # Wilcoxon test between frozen vs non-frozen
  p_value <- wilcox.test(df[[varname]]~df$frozen)$p.value
  # Format p-value
  p_label <- paste0("p = ",signif(p_value,3))
  
  g <- ggplot(data=df,aes(x=.data[[varname]],fill=frozen))+
    geom_histogram(position = "identity",color="black",alpha=0.8)+
    scale_fill_manual(labels=c("Frozen","Non-Frozen"),
                      values = my_color[c(1,4)])+
    annotate("text",
             x=Inf,y=Inf,label=p_label,
             hjust=1.1,vjust=1.5,size=6)+
    my_theme2+
    labs(fill="",x=x_title,y="count")
  return(g)
}

# This function is to make scatter plots of response variable vs predictor
# With pieacewise fitted model
# Input include:
# df: the eof_df
# var_res: response variable name
# var_pre: predictor variable name
BP_plot <- function(df,var_res,var_pre){
  # Fit linear model
  lm_formula <- as.formula(paste(var_res,"~",var_pre))
  lm_fit <- lm(formula = lm_formula,data=df)
  # Fit segmented model, 1 breakpoint to be estimated
  seg_formula <- as.formula(paste("~",var_pre))
  seg_fit <- segmented(lm_fit,seg.Z = seg_formula,npsi = 1)
  
  # Get breakpoint
  bp <- round(summary(seg_fit)$psi[1,"Est."],2)
  
  # Make a df for plotting
  x_seq <- seq(min(df[[var_pre]],na.rm=TRUE),
               max(df[[var_pre]],na.rm=TRUE),
               length.out = 200)
  
  new_data <- setNames(data.frame(x_seq),var_pre)
  
  # Predict fitted values for plotting, for both models
  pred_lm <- data.frame(x=x_seq,
                        y=predict(lm_fit,newdata = new_data),
                        model = "Linear")
  pred_seg <- data.frame(x=x_seq,
                         y=predict(seg_fit,newdata = new_data),
                         model = "Segmented")
  pred_df <- rbind(pred_lm,pred_seg)
  
  # Get R2 for the two models
  r2_lm <- round(summary(lm_fit)$r.squared,2)
  r2_seg <- round(summary(seg_fit)$r.squared,2)
  
  g <- ggplot(data = df,aes(x = .data[[var_pre]],
                            y = .data[[var_res]]))+
    geom_point(size=2)+
    # Add LM fitted line
    geom_line(data=pred_df %>% filter(model == "Linear"),
              aes(x=x,y=y),
              color=my_color[1],size=1)+
    # Add Seg fitted line
    geom_line(data=pred_df %>% filter(model == "Segmented"),
              aes(x=x,y=y),
              color=my_color[2],size=1)+
    geom_vline(xintercept = bp,color=my_color[2],linetype="dashed",size=1)+
    # R2 labels
    annotate("text",x=-Inf,y=Inf,hjust=-0.1,vjust=2,
             label = paste0("R2(LM) = ",r2_lm),
             size=5,color=my_color[1])+
    annotate("text",x=-Inf,y=Inf,hjust=-0.1,vjust=3.5,
             label = paste0("R2(Seg) = ",r2_seg),
             size=5,color=my_color[2])+
    # BP label
    annotate("text",x=Inf,y=-Inf,hjust=1.1,vjust=-0.5,
             label = paste0("BP = ",bp),
             color=my_color[2],size=6)+
    my_theme2
  
  out <- list(g,bp)
  return(out)
}

