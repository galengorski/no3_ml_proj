library(dplyr)
library(ggplot2)
library(stringr)

runs <- seq(0,74)
runs <- str_pad(runs, 2, pad = "0")

runs_perf <- list()
for (j in 1:length(runs)){
  if (j !=63){
sites <- list.files(paste0('~/Google Drive/My Drive/ESDL Postdoc/02_Projects/no3_ml_proj/03_model/out/multi_site/hyper_param_tune/HP_Run_',runs[j]), full.names = TRUE)
}
runs_perf[[j]] <- data.frame()

for(i in 1:(length(sites))){
  if(dir.exists(sites[i])){
    a <- read.table(paste0(sites[i],'/model_param_output.txt'), header = FALSE, sep = ':', fill = TRUE)
    b <- data.frame(t(a[,2])) 
    colnames(b) <- a[,1]
    b$run <- j-1
    runs_perf[[j]] <- rbind(runs_perf[[j]], b)
        
  }
  
}
print(j)
print(runs_perf[[j]][1,c(7,8,9,12,13,14,15)])
}

runs_perf_df <- as.data.frame(do.call(rbind, runs_perf)) %>%
  as_tibble()

convert_cols = c('Epochs','Learning rate','Batch Size','Training Fraction','Validation Fraction','Sequence Length','Cells','Dropout','Optimizer weight decay',
                 'RMSE_Training','RMSE_Validation','NSE_Training','NSE_Validation')
runs_perf_df[,convert_cols] <- lapply(runs_perf_df[,convert_cols], as.numeric)


runs_perf_df %>%
  group_by(run, `Learning rate`, `Batch Size`,`Optimizer weight decay`, `Dropout`,`Sequence Length`) %>%
  mutate(`Learning rate` = as.numeric(`Learning rate`)) %>%
  #filter(`Learning rate` > 5e-04) %>%
  dplyr::summarise(NSE_Valid = median(as.numeric(NSE_Validation)),
                   NSE_Training = median(as.numeric(NSE_Training)),
                   RMSE_Valid = median(as.numeric(RMSE_Validation)),
                   RMSE_Training = median(as.numeric(RMSE_Training))) %>%
  arrange(NSE_Valid) %>%
  View()

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
  ylim(0,5)

#RMSE
runs_perf_df %>%
  dplyr::select(`Station Name`, `Site Number`, `Learning rate`, `Batch Size`, RMSE_Training, RMSE_Validation, NSE_Training, NSE_Validation,`Optimizer weight decay`) %>%
  pivot_longer(cols = c(RMSE_Training, RMSE_Validation)) %>%
  mutate(RMSE = as.numeric(value)) %>%
  mutate(`Optimizer weight decay` = as.numeric(`Optimizer weight decay`)) %>%
  mutate(`Optimizer weight decay` = factor(`Optimizer weight decay`)) %>%
  mutate(`Learning rate` = as.numeric(`Learning rate`)) %>%
  filter(`Learning rate` > 5e-04) %>%
  ggplot(aes(x = `Optimizer weight decay`, y = RMSE, fill = name))+
  geom_boxplot()+
  facet_wrap(.~`Learning rate`, nrow = 1) +
  ylim(0,5)


#look at individual sites for effect of batch size with weight and learning rate fixed
runs_perf_df %>%
  filter(`Optimizer weight decay` == 0.001) %>%
  filter(`Learning rate` == 0.009) %>%
  dplyr::select(`Station Name`, `Batch Size`, RMSE_Training, RMSE_Validation, NSE_Training, NSE_Validation,`Optimizer weight decay`) %>%
  pivot_longer(cols = c(RMSE_Training, RMSE_Validation)) %>%
  mutate(RMSE = as.numeric(value)) %>%
  mutate(`Batch Size` = as.factor(`Batch Size`)) %>%
  ggplot(aes(x = `Batch Size`, y = RMSE, fill = name))+
  geom_bar(stat = 'identity',position = 'dodge')+
  facet_wrap(.~`Station Name`) +
  ggtitle('Weight decay = 0.001 Learning Rate = 0.009')

#with weight decay at 0.001 0.009 learning rate appears the best
runs_perf_df %>%
  filter(`Optimizer weight decay` == 0.001) %>%
  #filter(`Learning rate` == 0.009) %>%
  dplyr::select(`Station Name`, `Batch Size`, RMSE_Training, RMSE_Validation, NSE_Training, NSE_Validation,`Learning rate`) %>%
  pivot_longer(cols = c(RMSE_Training, RMSE_Validation)) %>%
  mutate(RMSE = as.numeric(value)) %>%
  mutate(`Learning rate` = as.factor(`Learning rate`)) %>%
  ggplot(aes(x = `Learning rate`, y = RMSE, fill = name))+
  geom_bar(stat = 'identity',position = 'dodge')+
  facet_wrap(.~`Station Name`) +
  ggtitle('Weight decay = 0.001')

#with learning rate set to 0.009 the effect of optimizer weight decay is not strong so set at 0.001
runs_perf_df %>%
  #filter(`Optimizer weight decay` == 0.0001) %>%
  filter(`Learning rate` == 0.005) %>%
  dplyr::select(`Station Name`, `Optimizer weight decay`, RMSE_Training, RMSE_Validation, NSE_Training, NSE_Validation,`Learning rate`) %>%
  pivot_longer(cols = c(RMSE_Training, RMSE_Validation)) %>%
  mutate(RMSE = as.numeric(value)) %>%
  mutate(`Optimizer weight decay` = as.factor(`Optimizer weight decay`)) %>%
  ggplot(aes(x = `Optimizer weight decay`, y = RMSE, fill = name))+
  geom_bar(stat = 'identity',position = 'dodge')+
  facet_wrap(.~`Station Name`) +
  ggtitle('Learning rate = 0.009')


i = 66
runs_perf[[i]][,17:20] <- lapply(runs_perf[[i]][,17:20], as.numeric)
runs_perf[[i]][,17:20] %>% summary()


#look at all sites validation period for seqlen = 365 and batch size = 256
runs_perf_df %>%
  filter(run > 61) %>%
  #filter(`Optimizer weight decay` == 0.0001) %>%
  #filter(`Learning rate` == 0.005) %>%
  filter(`Batch Size` == 256) %>%
  dplyr::select(`Station Name`, `Optimizer weight decay`, RMSE_Training, RMSE_Validation, NSE_Training, NSE_Validation,`Learning rate`,run, `Batch Size`) %>%
  #pivot_longer(cols = `Optimizer weight decay`) %>%
  mutate(RMSE = as.numeric(RMSE_Validation)) %>%
  mutate(`Learning rate` = as.factor(`Learning rate`)) %>%
  mutate(`Optimizer weight decay` = as.factor(`Optimizer weight decay`)) %>%
  ggplot(aes(x = `Learning rate`, y = RMSE, fill = `Optimizer weight decay`))+
  geom_bar(stat = 'identity',position = 'dodge')+
  facet_wrap(.~`Station Name`) +
  ggtitle('365 sequence length')

#THESE TWO ARE INTERESTING
runs_perf_df %>%
  filter(run > 61) %>%
  dplyr::select(`Station Name`, `Optimizer weight decay`, RMSE_Training, RMSE_Validation, NSE_Training, NSE_Validation,`Learning rate`,run, `Batch Size`) %>%
  pivot_longer(cols = c(RMSE_Training, RMSE_Validation)) %>%
  mutate(RMSE = as.numeric(value)) %>%
  mutate(`Learning rate` = as.factor(`Learning rate`)) %>%
  ggplot(aes(x = `Learning rate`, y = RMSE, fill = name))+
  geom_boxplot()+
  facet_wrap(.~`Optimizer weight decay`) +
  ggtitle('sl=365; bs=256;do=0')

runs_perf_df %>%
  filter(run > 61) %>%
  dplyr::select(`Station Name`, `Optimizer weight decay`, RMSE_Training, RMSE_Validation, NSE_Training, NSE_Validation,`Learning rate`,run, `Batch Size`) %>%
  pivot_longer(cols = c(RMSE_Training, RMSE_Validation)) %>%
  mutate(RMSE = as.numeric(value)) %>%
  mutate(`Optimizer weight decay` = as.factor(`Optimizer weight decay`)) %>%
  ggplot(aes(x = `Optimizer weight decay`, y = RMSE, fill = name))+
  geom_boxplot()+
  facet_wrap(.~`Learning rate`) +
  ggtitle('sl=365; bs=256;do=0')

runs_perf_df %>%
  filter(run > 61) %>%
  dplyr::select(`Station Name`, `Optimizer weight decay`, RMSE_Training, RMSE_Validation, NSE_Training, NSE_Validation,`Learning rate`,run, `Batch Size`) %>%
  #pivot_longer(cols = c(RMSE_Training, RMSE_Validation)) %>%
  #mutate(RMSE = as.numeric(value)) %>%
  mutate(`Learning rate` = as.factor(`Learning rate`)) %>%
  mutate(`Optimizer weight decay` = as.factor(`Optimizer weight decay`)) %>%
  ggplot(aes(x = `Learning rate`, y = RMSE_Validation, fill = `Optimizer weight decay`))+
  geom_boxplot()+
  #facet_wrap(.~`Optimizer weight decay`) +
  ggtitle('sl=365; bs=256;do=0;validation only')

runs_perf_df %>%
  filter(run > 61) %>%
  dplyr::select(`Station Name`, `Optimizer weight decay`, RMSE_Training, RMSE_Validation, NSE_Training, NSE_Validation,`Learning rate`,run, `Batch Size`) %>%
  #pivot_longer(cols = c(RMSE_Training, RMSE_Validation)) %>%
  #mutate(RMSE = as.numeric(value)) %>%
  mutate(`Learning rate` = as.factor(`Learning rate`)) %>%
  mutate(`Optimizer weight decay` = as.factor(`Optimizer weight decay`)) %>%
  ggplot(aes(x = `Learning rate`, y = NSE_Validation, fill = `Optimizer weight decay`))+
  geom_boxplot()+
  #facet_wrap(.~`Optimizer weight decay`) +
  ylim(-1,1)+
  ggtitle('365 Sequence Length')

runs_perf_df %>%
  group_by(run, `Learning rate`, `Batch Size`,`Optimizer weight decay`, `Dropout`,`Sequence Length`) %>%
  filter(run > 61) %>%
  dplyr::summarise(NSE_Valid = median(as.numeric(NSE_Validation)),
                   NSE_Training = median(as.numeric(NSE_Training)),
                   RMSE_Valid = median(as.numeric(RMSE_Validation)),
                   RMSE_Training = median(as.numeric(RMSE_Training))) %>%
  view()
  
