#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Feb 18 12:21:40 2022

@author: galengorski
"""

import lstm_modeling_functions as lmf
import pandas as pd
import os

#%% ATTRIBUTES NO BASEFLOW SEP
netcdf_loc = '02_munge/out/model_input_rolling.nc'
config_loc = '03_model/single_site_model_config.yaml'
#site_info = pd.read_csv('03_model/out/multi_site/Run_25/Rep_00/AllSitesModelResults.csv')
site_info = pd.read_csv('01_fetch/out/site_list_220507.csv', dtype = {'site_no':str})

model_run_id = 'Run_40'
model_run_dir = os.path.join('03_model/out/single_site',model_run_id)
train_model = True
save_results_csv = False
hp_tune = False
hp_tune_vals = {}
multi_site = False

#input file location

run_id = os.path.join(model_run_id,  'Rep_00')


read_input_data_from_file = False
input_file_loc = os.path.join(model_run_dir,'Rep_00')

all_sites_results_list = [] 

for i,site in enumerate(site_info.site_no): 
    site_no_list = site
    station_nm_list = list(site_info[site_info.site_no == site].station_nm)
    out_dir = os.path.join(model_run_dir,  'Rep_00', site)

    site_perf = lmf.run_single_site_model_c(netcdf_loc, config_loc, site_no_list, station_nm_list, 
                           read_input_data_from_file, input_file_loc, out_dir, run_id,
                           train_model, multi_site)

    all_sites_results_list.append(site_perf)
    
all_sites_results_df = pd.DataFrame(all_sites_results_list)
all_sites_results_df.to_csv(os.path.join(input_file_loc,"AllSitesModelResults.csv"))
