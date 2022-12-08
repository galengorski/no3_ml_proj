#===================================================================================#
# NOTES: this script looks at the hyperparameter analysis for the single site models
# so that we can finalize and run 
#-----------------------------------------------------------------------------------#
# Galen Gorski                                                                      #
# galengorski@berkeley.edu                                                          #
# 2022-08-07                                                                        #  
#-----------------------------------------------------------------------------------#
#===================================================================================#

#===================================================================================#
#####INSTALL PACKAGES#####
# install.packages('tidyverse')
library(tidyverse)
#####
#===================================================================================#



hp_runs <- list.files('03_model/out/single_site/hyper_param_tune/')

hp_vals <- readxl::read_xlsx('03_model/single_site_hyperparameter.xlsx')

full_hp <- data.frame()

for(i in 1:length(hp_runs)){
  temp_hp <- read_csv(paste0('03_model/out/single_site/hyper_param_tune/',hp_runs[i],'/Rep_00/AllSitesModelResults.csv'))
  temp_hp$learning_rate <- hp_vals$`Learning Rate`[i]
  temp_hp$layers <- hp_vals$Layers[i]
  temp_hp$seq_length <- hp_vals$`Sequence Length`[i]
  
  full_hp <- rbind(full_hp, temp_hp)
}

full_hp %>%
  mutate(learning_rate = factor(learning_rate), layers = factor(layers), seq_length = factor(seq_length)) %>%
  ggplot(aes(y = RMSE_validation, x = layers, fill = seq_length))+
  geom_boxplot()+
  facet_wrap(~learning_rate) +
  theme_bw()+
  ylab('RMSE')

full_hp %>%
  group_by(Run) %>%
  summarize(med_RMSE_val = median(RMSE_validation), med_RMSE_tr = median(RMSE_training), 
            med_NRMSE_val = median(NRMSE_validation), med_NRMSE_tr = median(NRMSE_training),
            med_NSE_val = median(NSE_validation), med_NSE_tr = median(NSE_training),
            med_PBIAS_val = median(PBIAS_validation), med_PBIAS_tr = median(PBIAS_training),
            `Sequence Length` = first(seq_length), `Learning Rate` = first(`learning_rate`), Layers = first(layers)) %>%
  write_csv('04_analysis/out/single_site_hyperparameter_tuning.csv')
  
# full_hp %>%
#   filter(learning_rate == 0.005 & seq_length == 60) %>%
#   mutate(learning_rate = factor(learning_rate), layers = factor(layers), seq_length = factor(seq_length)) %>%
#   ggplot(aes(y = NSE_validation, x = layers, fill = layers))+
#   ylim(-1,1)+
#   geom_boxplot()
  
