# Author: Zhaozhe Chen
# Update Date: 2026.1.15

# This code is to explore non-frozen Q events

# ---------- Global -----------
library(stringr)
library(dplyr)
library(lubridate)
library(cowplot)
library(randomForest)

# Data path =======
# Joint non-frozen Q events
Q_df <- read.csv("00_Data/Processed_data_v2/Non-Frozen_Q_joint_df.csv")
# Source functions
source("01_Codes/Plotting_functions.R")

# Colors for plotting 
my_color <- RColorBrewer::brewer.pal(7,"Set2")

# Set random seed
seed <- 111

# Variables to be included in RF =====
# Response variable
var_re <- "Q_response_time_hr"

# Predictors to be included
var_pr <- c("duration","I30","ARFdays7","rain_in",
            "Tillage","Tile","MeanSlope_per","SoilType",
            "PerennialFrac","DSP")
var_ls_all <- c(var_re,var_pr)

# Threshold for low P events
P_in_th <- 0.2

# Output path
Output_path <- "D:/OneDrive - UW-Madison/Research/Discovery Farms/DF Runoff Generation/Results/RF_results/"

# ------- Main ---------
# Processing Q event df =========
Q_df <- Q_df %>%
  filter(rain_in > 0) %>%
  # Calculate Runoff coefficient (RC)
  mutate(RC = runoff_in/rain_in) %>%
  # DSP <0 should be 0
  mutate(DSP = ifelse(DSP < 0,0,DSP)) %>%
  # Convert classes
  mutate(Monitoring = as.factor(Monitoring),
         FarmEnterprise = as.factor(FarmEnterprise),
         Tillage = as.factor(Tillage),
         Tile = as.factor(Tile),
         SoilType = as.factor(SoilType),
         Field_Name = as.factor(Field_Name),
         Crop = as.factor(Crop),
         PreviousCrop = as.factor(PreviousCrop)) %>%
  # Filter out tile monitoring sites
  filter(Monitoring == "Surface")

# Random Forest ==========
# Only keep required variables
df_all <- Q_df[,var_ls_all]
# Only keep complete observations
df_all <- na.omit(df_all)

# Filter out low P events
df_all <- df_all %>%
  filter(rain_in > P_in_th)

# Split dataset into Training set 70% and Testing set 30%
set.seed(seed)
train_index <- sample(1:nrow(df_all),size=nrow(df_all)*0.7)
df_train  <- df_all[train_index,]
df_test   <- df_all[-train_index,]

print('Fitting RF model ...')
# # of trees: 500
# Get formula
set.seed(seed)
f <- as.formula(paste(var_re,"~.",sep=""))
system.time(rf <- randomForest(f,
                               data = df_train,
                               importance=TRUE,
                               type = "regression",
                               ntree=100))
print("Complete RF model")

# Record r for testing and training set
pred_train <- predict(rf,df_train)
pred_test <- predict(rf,df_test)
r_train <- cor(pred_train,df_train[var_re],use="pairwise.complete.obs")
r_test  <- cor(pred_test,df_test[var_re],use="pairwise.complete.obs")

rf_importance <- as.data.frame(importance(rf))
rf_importance$Feature <- rownames(rf_importance)
rf_importance <- rf_importance %>% 
  rename("Importance" = "%IncMSE") %>%
  # Add groups for coloring
  mutate(
    Group = case_when(
      Feature %in% c("rain_in","I30","duration","ARFdays7","ARFdays1","ARFdays14") ~ "Rainfall",
      Feature %in% c("MeanSlope_per","SoilType") ~ "Landscape",
      TRUE ~ "Management"
    )
  )

# Define colors for the three types
my_fill <- c(
  "Rainfall" = my_color[3],
  "Landscape" = my_color[1],
  "Management" = my_color[2]
)
  
# Rescale importance to sum = 1
rf_importance$Importance <- rf_importance$Importance/sum(rf_importance$Importance)
# Plot of relative importance
g_imp <- ggplot(data=rf_importance,aes(x=Importance,y=reorder(Feature,Importance),fill=Group))+
  geom_bar(stat="identity",color="black")+
  annotate("text",x=0.8*max(rf_importance$Importance),y=3,label=paste("R2_train =",round(r_train^2,2)))+
  annotate("text",x=0.8*max(rf_importance$Importance),y=2,label=paste("R2_test =",round(r_test^2,2)))+
  scale_fill_manual(values = my_fill)+
  labs(y="")+
  my_theme2

# Make PDP
# Initialize a list to store pdp plots
pdp_ls <- list()
IP_var_ls <- rownames(rf_importance[order(rf_importance$Importance,decreasing = TRUE),])
# Remove some categorical variables
IP_var_ls <- setdiff(IP_var_ls,
                     c("SoilType","Tillage","Tile"))

for(i in 1:length(IP_var_ls)){
  g <- make_pdp_plot(rf,df_train,var_re,IP_var_ls,i)
  pdp_ls[[i]] <- g
  print(i)
}

# Combine all plots
g_pdp_all <- plot_grid(plotlist = pdp_ls)
g_all <- plot_grid(g_imp,g_pdp_all,
                   nrow = 1,rel_widths = c(0.6,1))

# Output this figure
print_g(g_all,paste0("RF_",var_re,"_Pth",P_in_th),16,8)

# Explore Response variable vs most important variables scatter plots ============================
# Plot against rain_in, color coded by Tillage
g_scatter1 <- plot_scatter(Q_df,var_name1 = "rain_in",var_re=var_re,var_group = "Tillage",y_limits = c(0,24),mycolor=my_color[c(1,2,3,4)])
g_scatter2 <- plot_scatter(Q_df,var_name1 = "duration",var_re=var_re,var_group = "Tillage",
                           y_limits = c(0,24),x_limits = c(0,24),mycolor=my_color[c(1,2,3,4)])
g_scatter3 <- plot_scatter(Q_df,var_name1 = "MeanSlope_per",var_re=var_re,var_group = "Tillage",
                           y_limits = c(0,24),mycolor=my_color[c(1,2,3,4)])
g_scatter4 <- plot_scatter(Q_df,var_name1 = "DSP",var_re=var_re,var_group = "Tillage",
                           y_limits = c(0,24),mycolor=my_color[c(1,2,3,4)])
g_scatter5 <- plot_scatter(Q_df,var_name1 = "I30",var_re=var_re,var_group = "Tillage",
                           y_limits = c(0,24),mycolor=my_color[c(1,2,3,4)])
g_scatter6 <- plot_scatter(Q_df,var_name1 = "ARFdays7",var_re=var_re,var_group = "Tillage",
                           y_limits = c(0,24),mycolor=my_color[c(1,2,3,4)])
# Combine these
g_scatter <- plot_grid(g_scatter1,g_scatter2,g_scatter3,g_scatter4,g_scatter5,g_scatter6,nrow=2)
# Output this figure
print_g(g_scatter,paste0("Scatter_",var_re,"_Tillage"),16,8)






# Explore RC across groups ==========
if(FALSE){
  
# Across Sites
RC_Site <- plot_box(df = Q_df,x_varname = "Field_Name",y_varname = "RC",fill_name = "Monitoring",
         x_title = "",y_title = "Runoff Coefficient",fill_title = "",box_width = 0.4,
         jitter_offset = 0.4,label_y = 0.8,y_limits = c(0,2),
         my_cols = c("Surface" = my_color[1],"Tile" = my_color[4]))
# Across Soil types
RC_Soil <- plot_box(df = Q_df,x_varname = "SoilType",y_varname = "RC",fill_name = "storm",
                    x_title = "",y_title = "Runoff Coefficient",box_width = 0.4,fill_title = NULL,
                    jitter_offset = 0.4,label_y = 0.8,y_limits = c(0,2))
# Across Drainage class
RC_Drainage <- plot_box(df = Q_df,x_varname = "DrainageClass",y_varname = "RC",fill_name = "storm",
                    x_title = "",y_title = "Runoff Coefficient",box_width = 0.4,fill_title = NULL,
                    jitter_offset = 0.4,label_y = 0.8,y_limits = c(0,2))
# Across Monitoring
RC_Monitoring <- plot_box(df = Q_df,x_varname = "Monitoring",y_varname = "RC",fill_name = "storm",
                        x_title = "",y_title = "Runoff Coefficient",box_width = 0.4,fill_title = NULL,
                        jitter_offset = 0.4,label_y = 0.8,y_limits = c(0,2))
# Across Tillage
# Should also check Current Crop type perennial or annual for cover crop with No-Till!!!!!!!!!!!!

RC_Tillage <- plot_box(df = Q_df,x_varname = "Tillage",y_varname = "RC",fill_name = "storm",
                          x_title = "",y_title = "Runoff Coefficient",box_width = 0.4,fill_title = NULL,
                          jitter_offset = 0.4,label_y = 0.8,y_limits = c(0,2))

}






