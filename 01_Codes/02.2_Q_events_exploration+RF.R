# Author: Zhaozhe Chen
# Update Date: 2026.1.8

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
seed <- 1

# Response variable
var_re <- "RC"
# var_re <- "Q_response_time_hr
# Predictors to be included
var_pr <- c("duration","I30","ARFdays1","ARFdays14","Field_Name",
            #"Monitoring","FarmEnterprise","Tillage","Tile","SoilType","MeanSlope_per","Clay_Fraction",
            "PerennialFrac","DSP")
var_ls_all <- c(var_re,var_pr)

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
         Field_Name = as.factor(Field_Name)) %>%
  # Filter out tile monitoring sites
  filter(Monitoring == "Surface")

# Random Forest ==========
# Only keep required variables
df_all <- Q_df[,var_ls_all]
# Only keep complete observations
df_all <- na.omit(df_all)

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
                               ntree=500))
print("Complete RF model")

# Record r for testing and training set
r_train <- cor(rf$predicted,df_train[var_re],use="pairwise.complete.obs")
rf.test.pred <- predict(rf,df_test)
r_test  <- cor(rf.test.pred,df_test[var_re],use="pairwise.complete.obs")
rf_importance <- as.data.frame(importance(rf))
rf_importance$Feature <- rownames(rf_importance)
rf_importance <- rf_importance %>% rename("Importance" = "%IncMSE")
# Rescale importance to sum = 1
rf_importance$Importance <- rf_importance$Importance/sum(rf_importance$Importance)
# Plot of relative importance
g_imp <- ggplot(data=rf_importance,aes(x=Importance,y=reorder(Feature,Importance)))+
  geom_bar(stat="identity",fill="skyblue",color="black")+
  annotate("text",x=0.8*max(rf_importance$Importance),y=3,label=paste("R2_train =",round(r_train^2,2)))+
  annotate("text",x=0.8*max(rf_importance$Importance),y=2,label=paste("R2_test =",round(r_test^2,2)))+
  labs(y="")


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

}






