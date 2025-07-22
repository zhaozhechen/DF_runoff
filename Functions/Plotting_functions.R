# Author: Zhaozhe Chen
# Update Date: 2025.7.22

# This code is to make plots for the DF projects

library(ggplot2)
library(cowplot)
library(RColorBrewer)
library(ggrepel)
library(gghalves)

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
my_theme2 <- my_theme+
  theme(panel.border = element_rect(colour="black",fill=NA),
        axis.text = element_text(size=18),
        legend.position = "none",
        axis.title = element_text(size=18))

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
# varname: variable name to color
DF_map <- function(varname){
  g <- ggplot()+
    geom_sf(data=WI_bd,fill=my_color[3],alpha=0.3,color="grey")+
    geom_sf(data=WI_outer_bd,fill=NA,color="black")+
    geom_point(data=DF_meta_Site,
               aes(x=LONG_approx,y=LAT_approx,
                   size = UseableYears,
                   fill = .data[[varname]]),
               shape = 21,
               color="black",
               alpha=0.7)+
    scale_fill_brewer(palette = "Set3")+
    my_theme+
    geom_label_repel(data=DF_meta_Site,
                     aes(x=LONG_approx,y=LAT_approx,label=SiteID),
                     point.padding = 0,
                     label.padding = 0.25,
                     box.padding = 0.25,
                     min.segment.length = 0,
                     max.overlaps = 30,
                     segment.color="black")+
    guides(fill = guide_legend(override.aes = list(size = 6,shape=21)),
           size=guide_legend(override.aes = list(shape=21)))
  return(g)
}

# This function makes bar plot of number of sites for each variable
# Input include:
# varname: The target varname
# df: The data frame
DF_bar <- function(varname,df){
  g <- ggplot(data=df,
              aes(y=.data[[varname]],
                  fill = .data[[varname]]))+
    geom_bar(color="black")+
    scale_fill_brewer(palette = "Set3")+
    my_theme2+
    ggtitle(varname)
  return(g)
}

# This function combines map of the target variable and distribution of the target variable
# Input include:
# varname: The target variable name in the DF_meta_Site df
Site_plot <- function(varname){
  g_map <- DF_map(varname)
  g_bar <- DF_bar(varname,DF_meta_Site)  
  g <- plot_grid(g_map,g_bar,nrow=1,
                 rel_widths = c(1.8,1))
  print_g(g,paste0("DF_Site_",varname),16,6)
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

