#===================================================================================#
# NOTES: script for SI figure                                                                            #
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

#hyperparameter tuning for single site model
full_hp <- read_csv('04_analysis/out/single_site_hyperparameter_tuning.csv')

lr.labs <- c('Learning rate = 0.001','Learning rate = 0.005')
names(lr.labs) <- c('0.001','0.005')

#Saved as 792x538
full_hp %>%
  mutate(`Learning Rate` = factor(`Learning Rate`), Layers = factor(`Layers`), `Sequence Length` = factor(`Sequence Length`)) %>%
  ggplot(aes(y = med_RMSE_val, x = Layers, fill = `Sequence Length`))+
  geom_point(shape = 21, size = 2.5, alpha = 0.7)+
  scale_fill_manual(values = c('#073b4c','#ffc300','#c1121f'))+
  facet_wrap(~`Learning Rate`, labeller = labeller(`Learning Rate` = lr.labs)) +
  theme_bw()+
  ylab('RMSE (mg/L)')+
  xlab('Number of layers')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text.x = element_text(size = 12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12))+
  ggtitle('Median RMSE single-site models')