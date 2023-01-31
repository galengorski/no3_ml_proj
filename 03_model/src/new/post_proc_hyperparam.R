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
  rename_with(.fn = paste('Number of Cells'), ncells)
  ggplot(aes(x = seq_len, y = NRMSE_validation, color = replicate)) +
  geom_boxplot()+
  facet_wrap(~learning_rate+num_layers+num_cells, ncol = 6)
  #ylim(-1,1)



runs <- c('00','01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16')
runs_perf <- list()
for (j in 1:length(runs)){
sites <- list.files(paste0('~/Google Drive/My Drive/ESDL Postdoc/02_Projects/no3_ml_proj/03_model/out/multi_site/hyper_param_tune/HP_Run_',runs[j]), full.names = TRUE)

runs_perf[[j]] <- data.frame()

for(i in 1:(length(sites)-2)){
a <- read.table(paste0(sites[i],'/model_param_output.txt'), header = FALSE, sep = ':', fill = TRUE)
b <- data.frame(t(a[,2])) 
colnames(b) <- a[,1]
runs_perf[[j]] <- rbind(runs_perf[[j]], b)
}

#summary(as.numeric(perf$NSE_Training))
#summary(as.numeric(perf$NSE_Validation))

}



runs_perf_df <- as.data.frame(do.call(rbind, runs_perf)) %>%
  as_tibble()

runs_perf_df %>%
  group_by(`Learning rate`, `Batch Size`) %>%
  mutate(`Learning rate` = as.numeric(`Learning rate`)) %>%
  filter(`Learning rate` > 5e-04) %>%
  dplyr::summarise(NSE_Valid = median(as.numeric(NSE_Validation)),
                   NSE_Training = median(as.numeric(NSE_Training)),
                   RMSE_Valid = median(as.numeric(RMSE_Validation)),
                   RMSE_Training = median(as.numeric(RMSE_Training))) %>%
  arrange(RMSE_Valid)

#NSE
runs_perf_df %>%
  dplyr::select(`Station Name`, `Site Number`, `Learning rate`, `Batch Size`, RMSE_Training, RMSE_Validation, NSE_Training, NSE_Validation) %>%
  pivot_longer(cols = c(NSE_Training, NSE_Validation)) %>%
  mutate(NSE = as.numeric(value)) %>%
  mutate(`Batch Size` = as.numeric(`Batch Size`)) %>%
  mutate(`Batch Size` = factor(`Batch Size`)) %>%
  mutate(`Learning rate` = as.numeric(`Learning rate`)) %>%
  filter(`Learning rate` > 5e-04) %>%
  ggplot(aes(x = `Batch Size`, y = NSE, fill = name))+
  geom_boxplot()+
  facet_wrap(.~`Learning rate`, nrow = 1) +
  ylim(-10,1)

#RMSE
runs_perf_df %>%
  dplyr::select(`Station Name`, `Site Number`, `Learning rate`, `Batch Size`, RMSE_Training, RMSE_Validation, NSE_Training, NSE_Validation) %>%
  pivot_longer(cols = c(RMSE_Training, RMSE_Validation)) %>%
  mutate(RMSE = as.numeric(value)) %>%
  mutate(`Batch Size` = as.numeric(`Batch Size`)) %>%
  mutate(`Batch Size` = factor(`Batch Size`)) %>%
  mutate(`Learning rate` = as.numeric(`Learning rate`)) %>%
  filter(`Learning rate` > 5e-04) %>%
  ggplot(aes(x = `Batch Size`, y = RMSE, fill = name))+
  geom_boxplot()+
  facet_wrap(.~`Learning rate`, nrow = 1) +
  ylim(0,10)