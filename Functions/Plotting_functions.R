# Author: Zhaozhe Chen
# Date: 2025.7.8

# This code is to make plots for the DF projects

library(ggplot2)
library(cowplot)
library(RColorBrewer)
library(ggrepel)

# Theme for all plots
my_theme <- theme(
  #axis.line=element_line(color="black"),
  panel.background = element_blank(),
  #panel.border = element_rect(colour="black",fill=NA),
  legend.key = element_blank(),
  #legend.key.size = unit(6,"cm"),
  #aspect.ratio = 1/1,
  #legend.key.size = unit(0.3,'cm'),
  legend.text = element_text(size=14),
  plot.title = element_text(size=14),
  axis.text = element_blank(),
  axis.title = element_blank(),
  axis.ticks = element_blank(),
  legend.position = "right",
  legend.title = element_text(size=14)
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
    scale_fill_brewer(palette = "Set2")+my_theme+
    geom_label_repel(data=DF_meta_Site,
                     aes(x=LONG_approx,y=LAT_approx,label=SiteID),
                     point.padding = 0,
                     label.padding = 0.25,
                     box.padding = 0.25,
                     min.segment.length = 0,
                     max.overlaps = 30,
                     segment.color="black")
  return(g)
}

# This function makes bar plot of number of sites for each variable
DF_bar <- function(varname){
  g <- ggplot(data=DF_meta_Site,
              aes(y=.data[[varname]],
                  fill = .data[[varname]]))+
    geom_bar(color="black")+
    scale_fill_brewer(palette = "Set2")+
    my_theme+
    theme(panel.border = element_rect(colour="black",fill=NA),
          axis.text = element_text(size=14),
          legend.position = "none")+
    ggtitle(varname)
  return(g)
}
