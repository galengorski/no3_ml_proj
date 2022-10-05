#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Apr 20 16:08:57 2022

@author: galengorski
"""

import lstm_modeling_functions as lmf
import pandas as pd
import os
 #%% ATTRIBUTES NO BASEFLOW SEP
netcdf_loc = '02_munge/out/model_input_rolling.nc'
config_loc = '03_model/multi_site_model_config.yaml'
#site_info = pd.read_csv('01_fetch/out/site_list_220507.csv', dtype = {'site_no':str})

site_info = pd.read_csv('01_fetch/out/site_list_220507.csv',  dtype = {'site_no':str})
site_no_list = site_info.site_no
station_nm_list = site_info.station_nm



#input file location
read_input_data_from_file = False
input_file_loc = None
model_run_id = 'Run_04_DAM'
model_run_dir = os.path.join('03_model/out/multi_site',model_run_id)
train_model = True
save_results_csv = False
hp_tune = False
hp_tune_vals = {}
multi_site = True

# for group in site_info.cluster.unique():
#     print(group)


for j in range(5):

    run_id = os.path.join(model_run_id,  ('Rep_0'+str(j)))
    out_dir = os.path.join(model_run_dir,  ('Rep_0'+str(j)))
    
    read_input_data_from_file = False
    input_file_loc = os.path.join(model_run_dir,('Rep_0'+str(j)))
    
    lmf.run_multi_site_model_c(netcdf_loc, config_loc, site_no_list, station_nm_list, 
                           read_input_data_from_file, input_file_loc, out_dir, run_id,
                           train_model, multi_site)

