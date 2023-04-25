#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jan 26 14:47:54 2023

@author: galengorski
"""
#%%
import sys
sys.path.append("/Users/galengorski/Documents/GitHub/no3_ml_proj/04_analysis/src")
#print(sys.path)
import lstm_modeling_functions as lmf
import expected_gradients_functions as egf
#%%
#run multi_site models
#run_config_loc = '03_model/multi_site_run_config.yaml'
#lmf.wrapper_run_multi_site_model_c(run_config_loc)

#%%
#run_single_site_models
#run_config_loc = '03_model/single_site_run_config.yaml'
#lmf.wrapper_run_single_site_model_c(run_config_loc)
#%%
#run_config_loc = '03_model/single_site_run_hyperparameter_config.yaml'
#lmf.wrapper_single_site_model_hyperparameter_tuning(run_config_loc)
#%%
run_config_loc = '03_model/multi_site_run_config.yaml'
lmf.wrapper_run_cluster_model(run_config_loc)
#%%
#run_config_loc = '03_model/multi_site_run_config.yaml'
#lmf.wrapper_run_hydroterrane_model(run_config_loc)
#%% Train out of sample models, multi-site model must be trained first and the 
#train_oos_exp: parameter must be set to True in 'multi_site_run_config.yaml'
#run_config_loc = '03_model/multi_site_run_config.yaml'
#lmf.wrapper_run_multi_site_model_c(run_config_loc)
#lmf.wrapper_run_cluster_model(run_config_loc)
#lmf.wrapper_run_hydroterrane_model(run_config_loc)
#%% Run expected gradient calcs for global model
#run_config_loc = '03_model/multi_site_run_config.yaml'
#egf.calc_expected_gradients_all_sites(run_config_loc)
#%%
#run_config_loc = '03_model/multi_site_run_config.yaml'
#lmf.wrapper_fine_tune_multi_site_models(run_config_loc)







