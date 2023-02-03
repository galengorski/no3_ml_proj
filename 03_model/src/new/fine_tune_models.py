#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jan 26 14:47:54 2023

@author: galengorski
"""
#%%
import lstm_modeling_functions as lmf
#%%
#run multi_site models
run_config_loc = '03_model/multi_site_run_config.yaml'
lmf.wrapper_run_multi_site_model_c(run_config_loc)

#%%
#run_single_site_models
#run_config_loc = '03_model/single_site_run_config.yaml'
#lmf.wrapper_run_single_site_model_c(run_config_loc)
#%%
#run_config_loc = '03_model/single_site_run_hyperparameter_config.yaml'
#lmf.wrapper_single_site_model_hyperparameter_tuning(run_config_loc)
#%%
#run_config_loc = '03_model/multi_site_run_config.yaml'
#lmf.wrapper_run_cluster_model(run_config_loc)
#%%
#run_config_loc = '03_model/multi_site_run_config.yaml'
#lmf.wrapper_run_hydroterrane_model(run_config_loc)






