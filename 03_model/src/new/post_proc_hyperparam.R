library(dplyr)
library(ggplot2)

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