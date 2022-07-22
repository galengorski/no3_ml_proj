#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jul 21 11:36:07 2022

@author: galengorski
"""
import itertools
import lstm_modeling_functions as lmf
import pandas as pd
import os

#%% ATTRIBUTES NO BASEFLOW SEP
netcdf_loc = '02_munge/out/model_input_rolling.nc'
config_loc = '03_model/single_site_model_config.yaml'
#site_info = pd.read_csv('03_model/out/multi_site/Run_25/Rep_00/AllSitesModelResults.csv')
site_info = pd.read_csv('01_fetch/out/site_list_220507.csv', dtype = {'site_no':str})

#%%%
lr = [0.005,0.001]
sl = [20,60,180]
l = [1,2,4]
hy_param = list(itertools.product(lr,sl,l))

#slim the list down to the hyper parameters that have not already been tested
#hy_param_non_redundant = [hp_set for hp_set in hy_param if hp_set not in prev_hy_param]
#%%

for i,hp in enumerate(hy_param):
    if i<4: 
        continue
    else:
        print(hp)
        HP_Run_pad = str(i).zfill(2)
        model_run_id = 'HP_'+HP_Run_pad
        model_run_dir = os.path.join('03_model/out/single_site/hyper_param_tune',model_run_id)
        hp_tune_vals = {'learning_rate':hy_param[i][0], 'seq_len':hy_param[i][1], 
                        'num_layers':hy_param[i][2]}
        
        print('Create input data')
        read_input_data_from_file = False
        input_file_loc = os.path.join(model_run_dir,'Rep_00')

        train_model = True
        save_results_csv = True
        hp_tune = True
        multi_site = False
        
        #input file location
        
        run_id = os.path.join(model_run_id,  'Rep_00')
        
        all_sites_results_list = [] 
        
        for j,site in enumerate(site_info.site_no): 
            site_no_list = site
            station_nm_list = list(site_info[site_info.site_no == site].station_nm)
            out_dir = os.path.join(model_run_dir,  'Rep_00', site)
        
            site_perf = lmf.run_single_site_model_c(netcdf_loc, config_loc, site_no_list, station_nm_list, 
                                   read_input_data_from_file, input_file_loc, out_dir, run_id,
                                   train_model, multi_site, hp_tune = True, hp_tune_vals = hp_tune_vals, save_results_csv = True)
        
            all_sites_results_list.append(site_perf)
            
        all_sites_results_df = pd.DataFrame(all_sites_results_list)
        all_sites_results_df.to_csv(os.path.join(input_file_loc,"AllSitesModelResults.csv"))
