# Author: Zhaozhe Chen
# Update Date: 2025.11.15

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

# This function is to make box plots
plot_box <- function(df,x_varname,y_varname,fill_name=NULL,
                     x_title,y_title,fill_title = NULL,
                     label_x = 0.1,label_y = 0.9,
                     jitter_offset = 0.2,jitter_width = 0.1,box_width = 0.1,y_limits = NULL,my_cols=NULL,white_box = NULL){
  
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
    labs(x = x_title, y = y_title, fill = fill_name,color=fill_name) +
    my_theme2+
    theme(
      axis.text.x = element_text(angle = 45, vjust = 0.5),
      legend.position = c(label_x, label_y)
    )+
    guides(color="none")
  
  if(is.null(white_box)){
    # Boxplot
    g <- g + geom_boxplot(width = box_width,color = "black",outlier.color = NA)
  }else{
    g <- g + geom_boxplot(width = box_width,color = "black",fill="white",outlier.color = NA)
  }
  
  if(!is.null(y_limits)){
    g <- g + ylim(y_limits)
  }
  
  if(!is.null(my_cols)){
    g <- g+ 
      scale_fill_manual(values = my_cols)+
      scale_color_manual(values = my_cols)
  }
  
  return(g)
}
