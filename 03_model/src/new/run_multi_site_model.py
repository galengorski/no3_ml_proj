#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Apr 20 16:08:57 2022

@author: galengorski
"""

import itertools
import lstm_modeling_functions as lmf
import pandas as pd
import os
#%% ATTRIBUTES NO BASEFLOW SEP
netcdf_loc = '02_munge/out/model_input.nc'
config_loc = '03_model/multi_site_model_config.yaml'
site_info = pd.read_csv('01_fetch/out/site_list_220128.csv', dtype = {'site_no':str})
site_info = site_info.drop(index = [8,47])
#input file location
site_no_list = site_info.site_no
station_nm_list = site_info.station_nm
read_input_data_from_file = False
input_file_loc = None
model_run_id = 'Run_09'
model_run_dir = os.path.join('03_model/out/multi_site',model_run_id)
train_model = True
save_results_csv = False
hp_tune = False
hp_tune_vals = {}



run_id = os.path.join(model_run_id,  'Rep_3')
out_dir = os.path.join(model_run_dir,  'Rep_3')

read_input_data_from_file = True
input_file_loc = os.path.join(model_run_dir,'Rep_0')

lmf.run_multi_site_model_c(netcdf_loc, config_loc, site_no_list, station_nm_list, 
                       read_input_data_from_file, input_file_loc, out_dir, run_id,
                       train_model)
#%% ATTRIBUTES WITH BASEFLOW SEP
netcdf_loc = '02_munge/out/model_input.nc'
config_loc = '03_model/multi_site_model_config_baseflowsep.yaml'
#site_info = pd.read_csv('01_fetch/out/site_list_220128.csv', dtype = {'site_no':str})
#input file location
#input_file_loc= '03_model/out/multi_site/Run_00'
#site_no_list = site_info.site_no
#station_nm_list = site_info.station_nm
read_input_data_from_file = False
input_file_loc = None
model_run_id = 'Run_10'
model_run_dir = os.path.join('03_model/out/multi_site',model_run_id)
train_model = True
save_results_csv = False
hp_tune = False


run_id = os.path.join(model_run_id, 'Rep_3')
out_dir = os.path.join(model_run_dir, 'Rep_3')
read_input_data_from_file = True
input_file_loc = os.path.join(model_run_dir,'Rep_0')


lmf.run_multi_site_model_c(netcdf_loc, config_loc, site_no_list, station_nm_list, 
                   read_input_data_from_file, input_file_loc, out_dir, run_id,
                   train_model)
#%% NO ATTRIBUTES NO BASEFLOW SEP
netcdf_loc = '02_munge/out/model_input.nc'
config_loc = '03_model/multi_site_model_config_noattr.yaml'
#site_info = pd.read_csv('01_fetch/out/site_list_220128.csv', dtype = {'site_no':str})
#input file location
#input_file_loc= '03_model/out/multi_site/Run_00'
#site_no_list = site_info.site_no
#station_nm_list = site_info.station_nm
read_input_data_from_file = False
input_file_loc = None
model_run_id = 'Run_11'
model_run_dir = os.path.join('03_model/out/multi_site',model_run_id)
train_model = True
save_results_csv = False
hp_tune = False


run_id = os.path.join(model_run_id, 'Rep_3')
out_dir = os.path.join(model_run_dir, 'Rep_3')
read_input_data_from_file = True
input_file_loc = os.path.join(model_run_dir,'Rep_0')

lmf.run_multi_site_model_c(netcdf_loc, config_loc, site_no_list, station_nm_list, 
                   read_input_data_from_file, input_file_loc, out_dir, run_id,
                   train_model)
#%% NO ATTRIBUTES AND BASEFLOW SEP
netcdf_loc = '02_munge/out/model_input.nc'
config_loc = '03_model/multi_site_model_config_baseflowsep_noattr.yaml'
#site_info = pd.read_csv('01_fetch/out/site_list_220128.csv', dtype = {'site_no':str})
#input file location
#input_file_loc= '03_model/out/multi_site/Run_00'
#site_no_list = site_info.site_no
#station_nm_list = site_info.station_nm
read_input_data_from_file = False
input_file_loc = None
model_run_id = 'Run_12'
model_run_dir = os.path.join('03_model/out/multi_site',model_run_id)
train_model = True
save_results_csv = False
hp_tune = False

run_id = os.path.join(model_run_id, 'Rep_3')
out_dir = os.path.join(model_run_dir, 'Rep_3')
read_input_data_from_file = True
input_file_loc = os.path.join(model_run_dir,'Rep_0')

lmf.run_multi_site_model_c(netcdf_loc, config_loc, site_no_list, station_nm_list, 
                   read_input_data_from_file, input_file_loc, out_dir, run_id,
                   train_model)