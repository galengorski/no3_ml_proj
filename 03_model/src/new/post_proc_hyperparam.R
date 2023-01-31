#===================================================================================#
# NOTES: a script for reading the results from the single site hyperparameter tuning
#
#-----------------------------------------------------------------------------------#
# Galen Gorski                                                                      #
# galengorski@berkeley.edu                                                          #
# 2023-01-30                                                                        #  
#-----------------------------------------------------------------------------------#
#===================================================================================#

#===================================================================================#
#####INSTALL PACKAGES#####
# install.packages('tidyverse')
library(tidyverse)
# install.packages('yaml')

#####
#===================================================================================#


n_reps <- 6
n_hp_combos <- 54
reps_dir <- '03_model/out/single_site/Run_00_HP'

rep_hp_performance <- data.frame()
for (rep in 1:n_reps){
  print(paste0('Reading replicate ',rep,' of ',n_reps))
  rep_str <- paste('Rep',str_pad(rep-1,2,pad = '0'), sep = '_')
  for (hp in 1:n_hp_combos){
    
    hp_combo <- paste('HP',str_pad(hp-1,2,pad = '0'), sep = '_')
    
    if(file.exists(file.path(reps_dir, rep_str, hp_combo))){
    
      hp_performance <- read_csv(file.path(reps_dir, rep_str, hp_combo, 'AllSitesModelResults.csv'), show_col_types = FALSE)
    
      hp_performance$hp_run <- hp_combo
    
      hp_performance$replicate <- rep_str
    
      rep_hp_performance <- rbind(rep_hp_performance, hp_performance)
    }
    
    }
  }
  
#recreate hyperparameter values, this should be replaced so to read these in from the file
sl <- c(20,60,180)
lr <- c(0.005, 0.001)
nl <- c(1,2,4)
nc <- c(10,20,40)

hp_grid <- expand.grid(seq_len = sl, learning_rate = lr, num_layers = nl, num_cells = nc) %>%
  arrange(seq_len,desc(learning_rate), num_layers, num_cells) %>%
  mutate(across(seq_len:num_cells, as.factor))

hp_grid$row <- seq(1:nrow(hp_grid)) 

hp_grid <- hp_grid %>%
  mutate(hp_run = paste('HP',str_pad(row-1,2,pad = '0'), sep = '_')) %>%
  dplyr::select(!row) 

hp_perf_grid <- merge(rep_hp_performance, hp_grid, by = 'hp_run')



hp_perf_grid %>%
  #rename_with(.fn = paste('Number of Cells'), ncells)
  ggplot(aes(x = num_cells, y = NSE_validation, color = seq_len)) +
  geom_boxplot()+
  facet_wrap(~num_layers+learning_rate, ncol = 3)+
  ylim(-1,1)


hp_perf_grid %>%
  group_by(hp_run) %>%
  summarize(median_NSE = median(NSE_validation), median_NRMSE = median(NRMSE_validation), median_PBIAS = median(abs(PBIAS_validation)),
            seq_len = first(seq_len), learning_rate = first(learning_rate), num_layers = first(num_layers),
            num_cells = first(num_cells)) %>%
  filter(learning_rate == 0.005, seq_len == 180, num_layers == 2) %>%
  #arrange(desc(median_NSE))
  arrange(median_NRMSE)
  arrange(median_PBIAS)

hp_perf_grid %>%
  arrange(median_NRMSE)
  
  
unique_site <- hp_perf_grid$Site_name %>% 
  unique()

for(i in 1:length(unique_site)){
  site_hp <- hp_perf_grid %>%
    filter(Site_name == unique_site[i]) %>%
    ggplot(aes(x = num_cells, y = RMSE_validation, color = seq_len)) +
    geom_jitter()+
    facet_wrap(~num_layers+learning_rate, ncol = 2)+
    #ylim(-1,1)+
    ggtitle(unique_site[i])
  print(site_hp)
}
  
  
  
  
  
