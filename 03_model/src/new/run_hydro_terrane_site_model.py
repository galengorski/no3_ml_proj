#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Apr 20 16:08:57 2022

@author: galengorski
"""

import lstm_modeling_functions as lmf
import numpy as np
import os
import pandas as pd
#%% ATTRIBUTES NO BASEFLOW SEP
netcdf_loc = '02_munge/out/model_input_rolling.nc'
config_loc = '03_model/multi_site_model_config.yaml'

site_info = pd.read_csv('04_analysis/out/basin_char_w_clusters_hydroterranes_221005.csv',  dtype = {'site_no':str, 'cluster_01':str, 'cluster_02':str})
#input file location
read_input_data_from_file = False
input_file_loc = None
model_run_id = 'Run_08_HT'
model_run_dir = os.path.join('03_model/out/multi_site',model_run_id)
train_model = True
save_results_csv = False
hp_tune = False
hp_tune_vals = {}
multi_site = True

for j in range(3,5):
    
    rep = 'Rep_0'+str(j)

    for group in np.sort(site_info.hydro_terrane.unique()):
        print(group,'of 8 terranes')
    
        site_no_list = site_info[site_info['hydro_terrane'] == group].site_no
        station_nm_list = site_info[site_info['hydro_terrane'] == group].station_nm
        
        
        run_id = os.path.join(model_run_id, rep,  'Terrane_'+str(group))
        out_dir = os.path.join(model_run_dir, rep,  'Terrane_'+str(group))
        
        read_input_data_from_file = False
        input_file_loc = os.path.join(model_run_dir,'Terrane_'+str(group))
        
        weights_dir = os.path.join('03_model/out/multi_site/Run_08_HT/',rep,'Terrane_'+str(group))
        
        lmf.run_multi_site_model_c(netcdf_loc, config_loc, site_no_list, station_nm_list, 
                               read_input_data_from_file, input_file_loc, out_dir, run_id,
                               train_model, multi_site, weights_dir)
