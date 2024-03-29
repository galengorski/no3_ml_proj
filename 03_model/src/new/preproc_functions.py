#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Feb  4 18:57:47 2022

@author: galengorski
"""
#import netCDF4
import datetime
import math
import matplotlib.pyplot as plt
import numpy as np
import os
import pandas as pd
import pickle
import torch
import xarray
import yaml

if torch.cuda.is_available():
    device = "cuda:0"
else:
    device = "cpu"

def xarray_to_df(netcdf_location, site_no, feat_list):
    '''reads in a single site from the netcdf and '''
    site_data = xarray.open_dataset(netcdf_location, group = site_no)
    site_data_df = site_data.to_dataframe()
    date_time = site_data['Date'].to_index()
    site_data_df = site_data_df.set_index(date_time[:])
    site_data_df.loc[site_data_df.Nitrate < 0 , 'Nitrate'] = np.nan 
    site_data_df = site_data_df[feat_list]
    site_data.close()
    print(site_no,' data read to dataframe')
    return site_data_df

def xarray_to_df_mod_feat(netcdf_location, site_no, feat_list):
    '''reads in a single site from the netcdf and '''
    add_feat = ['NLCD_11', 'NLCD_21', 'NLCD_22', 'NLCD_23', 'NLCD_24', 'NLCD_31', 'NLCD_41', 
                 'NLCD_42', 'NLCD_43', 'NLCD_52', 'NLCD_71', 'NLCD_81', 'NLCD_82', 'NLCD_90', 'NLCD_95']
    feat_list_c = feat_list.copy()
    feat_list_c.extend(add_feat)

    site_data = xarray.open_dataset(netcdf_location, group = site_no)
    site_data_df = site_data.to_dataframe()
    date_time = site_data['Date'].to_index()
    site_data_df = site_data_df.set_index(date_time[:])
    site_data_df.loc[site_data_df.Nitrate < 0 , 'Nitrate'] = np.nan 
    
    site_data_df['NLCD_DEV'] = site_data_df['NLCD_21']+site_data_df['NLCD_22']+site_data_df['NLCD_23']+site_data_df['NLCD_24']
    site_data_df['NLCD_FOR'] = site_data_df['NLCD_41']+site_data_df['NLCD_42']+site_data_df['NLCD_43']
    site_data_df['NLCD_AG'] = site_data_df['NLCD_81']+site_data_df['NLCD_82']
    site_data_df['NLCD_WTLND'] = site_data_df['NLCD_90']+site_data_df['NLCD_95']
    
    
    feat_list_c = [e for e in feat_list_c if e not in add_feat]
    
    site_data_df = site_data_df[feat_list_c]
    
    site_data.close()
    print(site_no,' data read to dataframe')
    return site_data_df

def split_norm_combine(data, seq_len, trn_frac, val_frac, test_frac, config_loc, run_config_loc, weights_dir):
    
    with open(config_loc) as stream:
        config = yaml.safe_load(stream)
        
    with open(run_config_loc) as stream:
        run_config = yaml.safe_load(stream)    
        
    predict_period = config['predict_period']
    fine_tune = run_config['fine_tune']
    data_resolution = run_config['data_resolution']
    
    if fine_tune:
        df = data.copy()
        print('Resampling data at '+data_resolution+' resolution for fine tuning')
        if data_resolution == 'weekly':
            df['week'] = df.index.strftime('%Y-%U')
            retain = df.groupby('week').sample(n = 1).index
            df['drop'] = np.where(df.index.isin(retain),False,True)
            df.loc[df['drop'],'Nitrate'] = np.nan
            df = df.drop(columns = ['week','drop'])
        elif data_resolution == 'monthly':
            df['month'] = df.index.strftime('%Y-%m')
            retain = df.groupby('month').sample(n = 1).index
            df['drop'] = np.where(df.index.isin(retain),False,True)
            df.loc[df['drop'],'Nitrate'] = np.nan
            df = df.drop(columns = ['month','drop'])
        elif data_resolution == 'quarterly':
            df['year'] = df.index.year
            df['quarter'] = df.index.quarter
            df['yq'] = df['year'].astype(str)+'-'+df['quarter'].astype(str)
            retain = df.groupby('yq').sample(n = 1).index
            df['drop'] = np.where(df.index.isin(retain),False,True)
            df.loc[df['drop'],'Nitrate'] = np.nan
            df = df.drop(columns = ['year','quarter','yq','drop'])
        del data
        data = df
            
    
    #get rid of nitrate na values
    data_no_nans = data['Nitrate'].dropna()
    #this gets rid of any data before the first available nitrate observation with
    #seq len predictors before it
    data_no_nans_seq_len = data_no_nans[data_no_nans.index > data.index[seq_len]]
    
    #get the split points based on the availability of nitrate data
    start_train_date = data_no_nans_seq_len.index[0]
    end_train_date = data_no_nans_seq_len.index[math.floor(trn_frac*data_no_nans_seq_len.shape[0])-1]
    #the validation set start date is calculated by counting backward from the end of the dataset
    #for the test fraction and the validation fraction
    start_val_date = data_no_nans_seq_len.index[-(math.floor((test_frac)*data_no_nans_seq_len.shape[0])+math.floor((val_frac)*data_no_nans_seq_len.shape[0]))-1]
    end_val_date = data_no_nans_seq_len.index[-math.floor((test_frac)*data_no_nans_seq_len.shape[0])-2]
    start_test_date = data_no_nans_seq_len.index[-math.floor((test_frac)*data_no_nans_seq_len.shape[0])-1]
    end_test_date = data_no_nans_seq_len.index[data_no_nans_seq_len.shape[0]-1] 
    
    set_dates = {
    'start_train_date': start_train_date,
    'end_train_date': end_train_date,
    'start_val_date': start_val_date,
    'end_val_date': end_val_date,
    'start_test_date': start_test_date,
    'end_test_date': end_test_date
        }
    
    #split train validate and test splits
    train = data[:end_train_date]
    n_obs_train = train.Nitrate.dropna().shape[0]
    val = data[start_val_date:end_val_date]
    n_obs_val = val.Nitrate.dropna().shape[0]
    train_val = data[:end_val_date]
    n_obs_train_val = train_val.Nitrate.dropna().shape[0]
    test = data[start_test_date:end_test_date]
    n_obs_test = test.Nitrate.dropna().shape[0]    
    
    if fine_tune:
        with open(os.path.join(weights_dir,'all_variables_means_stds'), 'rb') as all_variables_means_stds:
            all_variables_means_stds_full_dataset = pickle.load(all_variables_means_stds)
        #normalize with entire dataset        
        train_norm = (train-all_variables_means_stds_full_dataset['train_mean'])/all_variables_means_stds_full_dataset['train_std']
        val_norm = (val-all_variables_means_stds_full_dataset['train_mean'])/all_variables_means_stds_full_dataset['train_std']
        train_val_norm = (train_val-all_variables_means_stds_full_dataset['train_mean'])/all_variables_means_stds_full_dataset['train_std']
        test_norm = (test-all_variables_means_stds_full_dataset['train_mean'])/all_variables_means_stds_full_dataset['train_std']    
        print('Normalizing with multi_site datasest')
    else:
    
        #normalize sets separately
        train_norm = (train - train.mean(axis=0)) / train.std(axis=0)
        val_norm = (val - train.mean(axis=0)) / train.std(axis=0)
        train_val_norm = (train_val - train.mean(axis=0)) / train.std(axis=0)
        test_norm = (test - train.mean(axis=0)) / train.std(axis=0)
        print('Normalizing with single-site dataset')
    
    if predict_period == 'full':
        full_norm = pd.concat([train_norm, test_norm])
    else:
        #recombine into single dataframe
        full_norm = pd.concat([train_norm, val_norm, test_norm])

    #dictionary of normalized datasets
    norm_sets = {
    'train_norm': train_norm,
    'val_norm': val_norm,
    'train_val_norm': train_val_norm,
    'test_norm': test_norm,
    'full_norm': full_norm
        }
    
    #dictionary of nitrate means and stds
    n_means_stds_n_obs = {
    'train_mean' : train.Nitrate.mean(),
    'train_std' : train.Nitrate.std(),
    'train_val_mean': train_val.Nitrate.mean(),
    'train_val_std': train_val.Nitrate.std(),
    'val_mean' : val.Nitrate.mean(),
    'val_std' : val.Nitrate.std(),
    'n_obs_train': n_obs_train,
    'n_obs_val': n_obs_val,
    'n_obs_train_val': n_obs_train_val,
    'n_obs_test': n_obs_test
    }
    
    print('data split and normalized')
    
    return norm_sets, set_dates, n_means_stds_n_obs
    

def split_multi_site_data(data, seq_len, trn_frac, val_frac, test_frac, config_loc):
    
    with open(config_loc) as stream:
        config = yaml.safe_load(stream)
        
    predict_period = config['predict_period']
    
    #get rid of nitrate na values
    data_no_nans = data['Nitrate'].dropna()
    #this gets rid of any data before the first available nitrate observation with
    #seq len predictors before it
    data_no_nans_seq_len = data_no_nans[data_no_nans.index > data.index[seq_len]]
    
    #get the split points based on the availability of nitrate data
    start_train_date = data_no_nans_seq_len.index[0]
    end_train_date = data_no_nans_seq_len.index[math.floor(trn_frac*data_no_nans_seq_len.shape[0])-1]
    #the validation set start date is calculated by counting backward from the end of the dataset
    #for the test fraction and the validation fraction
    start_val_date = data_no_nans_seq_len.index[-(math.floor((test_frac)*data_no_nans_seq_len.shape[0])+math.floor((val_frac)*data_no_nans_seq_len.shape[0]))-1]
    end_val_date = data_no_nans_seq_len.index[-math.floor((test_frac)*data_no_nans_seq_len.shape[0])-2]
    start_test_date = data_no_nans_seq_len.index[-math.floor((test_frac)*data_no_nans_seq_len.shape[0])-1]
    end_test_date = data_no_nans_seq_len.index[data_no_nans_seq_len.shape[0]-1] 
    
    set_dates = {
    'start_train_date': start_train_date,
    'end_train_date': end_train_date,
    'start_val_date': start_val_date,
    'end_val_date': end_val_date,
    'start_test_date': start_test_date,
    'end_test_date': end_test_date
        }
    
    #split train validate and test splits
    train = data[:end_train_date]
    val = data[start_val_date:end_val_date]
    train_val = data[:end_val_date]
    test = data[start_test_date:end_test_date]
    
      
    if predict_period == 'full':
        full = pd.concat([train, test])
    else:
        #recombine into single dataframe
        full = pd.concat([train, val, test])

    #dictionary of non-normalized datasets
    sets = {
    'train': train,
    'val': val,
    'train_val': train_val,
    'test': test,
    'full': full
        }
    
    #data returned from this function will have NAs because the data is 
    #subset by date from the original data object, which is continously dated
    return sets, set_dates

def normalize_multi_site_data(data, train, val, train_val, test):
    train_site_v = train['site_no']
    val_site_v = val['site_no']
    train_val_site_v = train_val['site_no']
    test_site_v = test['site_no']
    
    #remove site numbers
    train = train.drop(columns = ['site_no'])
    val = val.drop(columns = ['site_no'])
    train_val = train_val.drop(columns = ['site_no'])
    test = test.drop(columns = ['site_no'])

    #normalize sets separately
    train_norm = (train - train.mean(axis=0)) / train.std(axis=0)
    val_norm = (val - train.mean(axis=0)) / train.std(axis=0)
    train_val_norm = (train_val - train.mean(axis=0)) / train.std(axis=0)
    test_norm = (test - train.mean(axis=0)) / train.std(axis=0)
    
    #add site no back in
    train_norm['site_no'] = train_site_v
    val_norm['site_no'] = val_site_v
    train_val_norm['site_no'] = train_val_site_v
    test_norm['site_no'] = test_site_v
    
    #dictionary of nitrate means and stds
    n_means_stds = {'full_mean' : data.Nitrate.mean(),
    'full_std' : data.Nitrate.std(),
    'train_mean' : train.Nitrate.mean(),
    'train_std' : train.Nitrate.std(),
    'val_mean' : val.Nitrate.mean(),
    'val_std' : val.Nitrate.std(),
    'train_val_mean': train_val.Nitrate.mean(),
    'train_val_std': train_val.Nitrate.std(),
    'test_mean' : test.Nitrate.mean(),
    'test_std' : test.Nitrate.std()}
    
    all_variables_means_stds = {'train_mean' : train.mean(axis=0),
    'train_std' : train.std(axis=0),
    'val_mean' : val.Nitrate.mean(),
    'val_std' : val.Nitrate.std(),
    'train_val_mean': train_val.mean(axis=0),
    'train_val_std': train_val.std(axis=0),
    'test_mean' : test.mean(axis=0),
    'test_std' : test.std(axis=0)}
    
    return train_norm, val_norm, train_val_norm, test_norm, n_means_stds, all_variables_means_stds



def prepare_data(data, seq_len, set_dates):
    '''returns an array with dim [data length, seq_len, num features]'''
    
    start_train_date = set_dates['start_train_date']
    end_train_date = set_dates['end_train_date']
    start_val_date = set_dates['start_val_date']
    end_val_date = set_dates['end_val_date']
    start_test_date = set_dates['start_test_date']
    end_test_date = set_dates['end_test_date']
    
    #define a function which that finds the row with nan
    which = lambda lst:list(np.where(lst)[0])
        
   
    #get rid of nitrate na values
    data_no_nans = data['Nitrate'].dropna()
    #seq len predictors before it
    data_no_nans_seq_len = data_no_nans[data_no_nans.index > data.index[seq_len]]

    train_y = data_no_nans_seq_len[start_train_date:end_train_date]
    train_dates = data_no_nans_seq_len[start_train_date:end_train_date].index
    nobs_train = data_no_nans_seq_len[start_train_date:end_train_date].shape[0]
    
    val_y = data_no_nans_seq_len[start_val_date:end_val_date]
    val_dates = data_no_nans_seq_len[start_val_date:end_val_date].index
    nobs_val = data_no_nans_seq_len[start_val_date:end_val_date].shape[0]
    
    test_y = data_no_nans_seq_len[start_test_date:end_test_date]
    test_dates = data_no_nans_seq_len[start_test_date:end_test_date].index
    nobs_test = data_no_nans_seq_len[start_test_date:end_test_date].shape[0]
    
    #full data from seq_len through to the end of the dataset including na dates
    full_y = data.Nitrate[seq_len:]
    full_dates = data[seq_len:].index
    
    full_y_no_nans = data_no_nans_seq_len[start_train_date:end_test_date]
    
    #this is the number of nitrate observations that have seq_len days of 
    #predictors before them in the dataset, for example if we have a nitrate observation
    #on the first day, we don't have any predictor data for the previous seq_len days for that 
    #observation
    nobs = data_no_nans[data_no_nans.index > data.index[seq_len]].shape[0] 
    
    nobs_train_val = nobs - nobs_test
    trainval_dates = data_no_nans_seq_len[:nobs_train_val].index
    
    #get rid of nitrate in the predictors
    data = data.drop(columns = 'Nitrate')

    #this will only prepare the data on days with nitrate data available
    #create empty array 
    combined_x = np.empty([nobs, seq_len, data.shape[1]])

    for i, curr_idx in enumerate(data_no_nans_seq_len.index):
        date_lst = list(data.index == curr_idx)
        data_date_idx = int(which(date_lst)[0])
        if data_date_idx < seq_len:
            continue
        temp_split = data.iloc[(data_date_idx-seq_len):data_date_idx,:]
        combined_x[i,:,:] = temp_split
     
    #this will prepare the data for all days when drivers are available
    #prepare the full data set
    full_combined_x = np.empty([len(data)-seq_len, seq_len, data.shape[1]])
    
    for i, curr_idx in enumerate(data.iloc[seq_len:,].index):
        date_lst = list(data.index == curr_idx)
        data_date_idx = int(which(date_lst)[0])
        if data_date_idx < seq_len:
            continue
        temp_split = data.iloc[(data_date_idx-seq_len):data_date_idx,:]
        full_combined_x[i,:,:] = temp_split
        
    train_x = combined_x[0:nobs_train,:,:]
    val_x = combined_x[nobs_train:(nobs_train+nobs_val),:,:]
    trainval_x = combined_x[0:nobs_train_val,:,:]
    #test_x = combined_x[(nobs_train+nobs_val):(nobs_train+nobs_val+nobs_test),:,:]
    test_x = combined_x[(nobs-nobs_test):(nobs),:,:]
    
    train_x_t, train_y_t = torch.from_numpy(np.array(train_x)).float(), torch.from_numpy(np.array(train_y)).float()
    val_x_t, val_y_t = torch.from_numpy(np.array(val_x)).float(), torch.from_numpy(np.array(val_y)).float()
    trainval_x_t, trainval_y_t = torch.from_numpy(np.array(trainval_x)).float(), torch.from_numpy(np.array(full_y_no_nans[0:-nobs_test])).float()
    test_x_t, test_y_t = torch.from_numpy(np.array(test_x)).float(), torch.from_numpy(np.array(test_y)).float()
    full_x_t, full_y_t = torch.from_numpy(np.array(full_combined_x)).float(), torch.from_numpy(np.array(full_y)).float()    
   
    print('data prepped')
    
    prepped_drivers_data = {
        'train_x': train_x_t,
        'train_y': train_y_t,
        'train_dates': train_dates,
        'val_x': val_x_t,
        'val_y': val_y_t,
        'val_dates': val_dates,
        'train_val_x': trainval_x_t,
        'train_val_y': trainval_y_t,
        'train_val_dates': trainval_dates,
        'test_x': test_x_t,
        'test_y': test_y_t,
        'test_dates': test_dates,
        'full_x': full_x_t,
        'full_y': full_y_t,
        'full_dates': full_dates}
    
    return prepped_drivers_data

def full_prepare_multi_site_data(netcdf_loc, config_loc, site_no_list, station_nm_list, out_dir, fine_tune, weights_dir):

    with open(config_loc) as stream:
        config = yaml.safe_load(stream)
    
    
    seq_len = config['seq_len']
    trn_frac = config['trn_frac']
    val_frac = config['val_frac']
    test_frac = config['test_frac']
    
    feat_list = config['feat_list']
    if config['static_features_used']:
        static_features = config['static_features']
        feat_list.extend(static_features)
    
    #if config predcit period is "full" meaning you are not using a validation
    #set, then the training fraction is trn_frac + val_frac
    if config['predict_period'] == 'full':
        trn_frac = config['trn_frac']+config['val_frac']
    else:    
        trn_frac = config['trn_frac']
    
    train_data_all_sites = pd.DataFrame()
    val_data_all_sites = pd.DataFrame()
    train_val_data_all_sites = pd.DataFrame()
    test_data_all_sites = pd.DataFrame()
    full_data_all_sites = pd.DataFrame()
    
    train_range = {}
    val_range = {}
    train_val_range = {}
    test_range = {}
    n_obs_full = {}
    n_obs_train_val = {}
    
    for site_no in site_no_list:
        #convert single site from xarray group to dataframe
        df = xarray_to_df_mod_feat(netcdf_loc, site_no, feat_list)
        if len(df.columns[df.isna().any()].tolist()) > 0:
            print(site_no+ " nans found:",df.columns[df.isna().any()].tolist())
        #add individual site no
        df['site_no'] = site_no
        #split data into train, val, test splits individually by site
        sets, set_dates = split_multi_site_data(df, seq_len, trn_frac, val_frac, test_frac, config_loc)
        #recombine into single dataframe
        train_data_all_sites = pd.concat([train_data_all_sites, sets['train']])
        val_data_all_sites = pd.concat([val_data_all_sites, sets['val']])
        train_val_data_all_sites = pd.concat([train_val_data_all_sites, sets['train_val']])
        test_data_all_sites = pd.concat([test_data_all_sites, sets['test']])
        full_data_all_sites = pd.concat([full_data_all_sites, sets['full']])
        
        train_range[site_no] = {'From':0, 'To':0}
        train_range[site_no]['From'] = set_dates['start_train_date']
        train_range[site_no]['To'] = set_dates['end_train_date']
        
        val_range[site_no] = {'From':0, 'To':0}
        val_range[site_no]['From'] = set_dates['start_val_date']
        val_range[site_no]['To'] = set_dates['end_val_date']
        
        train_val_range[site_no] = {'From':0, 'To':0}
        train_val_range[site_no]['From'] = set_dates['start_train_date']
        train_val_range[site_no]['To'] = set_dates['end_val_date']
        
        test_range[site_no] = {'From':0, 'To':0}
        test_range[site_no]['From'] = set_dates['start_test_date']
        test_range[site_no]['To'] = set_dates['end_test_date']
        
        n_obs_full[site_no] = sets['full'].shape[0]
        n_obs_train_val[site_no] = sets['train_val'].shape[0]
        
     
    if fine_tune:
        with open(os.path.join(weights_dir,'all_variables_means_stds'), 'rb') as all_variables_means_stds:
            all_variables_means_stds_full_dataset = pickle.load(all_variables_means_stds)
        #normalize with entire dataset        
        train_norm = (train_data_all_sites.iloc[:,:-1]-all_variables_means_stds_full_dataset['train_mean'])/all_variables_means_stds_full_dataset['train_std']
        val_norm = (val_data_all_sites.iloc[:,:-1]-all_variables_means_stds_full_dataset['train_mean'])/all_variables_means_stds_full_dataset['train_std']
        train_val_norm = (train_val_data_all_sites.iloc[:,:-1]-all_variables_means_stds_full_dataset['train_mean'])/all_variables_means_stds_full_dataset['train_std']
        test_norm = (test_data_all_sites.iloc[:,:-1]-all_variables_means_stds_full_dataset['train_mean'])/all_variables_means_stds_full_dataset['train_std']
        
    else:

        train_norm, val_norm, train_val_norm, test_norm, n_means_stds, all_variables_means_stds = normalize_multi_site_data(full_data_all_sites, train_data_all_sites, val_data_all_sites, train_val_data_all_sites, test_data_all_sites)
    
    #initiate empty tensors for the input data
    train_x = torch.empty((0,seq_len,len(feat_list)-1), dtype=torch.float, device = device)
    train_y = torch.empty((0), dtype=torch.float, device = device)
    train_dates = np.empty([0], dtype='datetime64[ns]')
    
    val_x = torch.empty((0,seq_len,len(feat_list)-1), dtype=torch.float, device = device)
    val_y = torch.empty((0), dtype=torch.float, device = device)
    val_dates = np.empty([0], dtype='datetime64[ns]')
    
    train_val_x = torch.empty((0,seq_len,len(feat_list)-1), dtype=torch.float, device = device)
    train_val_y = torch.empty((0), dtype=torch.float, device = device)
    train_val_dates = np.empty([0], dtype='datetime64[ns]')

    test_x = torch.empty((0,seq_len,len(feat_list)-1), dtype=torch.float, device = device)
    test_y = torch.empty((0), dtype=torch.float, device = device)
    test_dates = np.empty([0], dtype='datetime64[ns]')
    
    full_x = torch.empty((0,seq_len,len(feat_list)-1), dtype=torch.float, device = device)
    full_y = torch.empty((0), dtype=torch.float, device = device)
    full_dates = np.empty([0], dtype='datetime64[ns]')
    
    solute_mean = {}
    solute_std = {}
    
    #the indices dictionaries are for storing the indices for each watershed's data
    #so that the data can be split correctly after modelings
    train_indices = {}
    val_indices = {}
    train_val_indices = {}
    test_indices = {}
    full_indices = {}

    full_data_indices_w_nans = {}
    total_data = 0

    train_val_data_indices_w_nans = {}
    train_val_data = 0

    #split data back up for creating the input sequences
    for site_no in site_no_list:
        print(site_no)
        train_single_site = train_norm[train_norm['site_no'] == site_no]
        val_single_site = val_norm[val_norm['site_no'] == site_no]
        train_val_single_site = train_val_norm[train_val_norm['site_no'] == site_no]
        test_single_site = test_norm[test_norm['site_no'] == site_no]
        
        #create a dirctory for the site
        out_path = os.path.join(out_dir,site_no)
        os.makedirs(out_path, exist_ok = True)
        #save a plot of the time series showing the splits
        save_nitrate_plot(train_single_site, val_single_site, test_single_site, out_dir, site_no)
        
        if config['predict_period'] == 'full':
            full_data_single_site = pd.concat([train_single_site, test_single_site])
        else:
            #recombine into single dataframe
            full_data_single_site = pd.concat([train_single_site, val_single_site, test_single_site])

        #get the full number of rows for each site including nan values
        full_data_indices_w_nans[site_no] = {'From':0,'To':0}
        full_data_indices_w_nans[site_no]['From'] = total_data
        total_data = total_data+n_obs_full[site_no]
        full_data_indices_w_nans[site_no]['To'] = total_data
        
        #get the full number of rows for each site including nan values
        train_val_data_indices_w_nans[site_no] = {'From':0,'To':0}
        train_val_data_indices_w_nans[site_no]['From'] = train_val_data
        train_val_data = train_val_data+n_obs_train_val[site_no]
        train_val_data_indices_w_nans[site_no]['To'] = train_val_data
        
        #some static attributes might have nans if all the sites  have the same value
        cols_with_nan = full_data_single_site.columns[full_data_single_site.isna().any()].tolist()
        cols = [s for s in cols_with_nan if s != 'Nitrate']
        
        if len(cols) >0:
            full_data_single_site[cols] = full_data_single_site[cols].fillna(0)
            print('nans found and filled with zeroes in '+site_no+':',cols)
        
        full_data_single_site = full_data_single_site.drop(columns = 'site_no')
        
        
        site_set_dates = {
        'start_train_date' : train_range[site_no]['From'],
        'end_train_date' : train_range[site_no]['To'],
        'start_val_date' : val_range[site_no]['From'],
        'end_val_date' : val_range[site_no]['To'],
        'start_test_date' : test_range[site_no]['From'],
        'end_test_date' : test_range[site_no]['To']
            }
                
        prepped_drivers_data_site = prepare_data(full_data_single_site, seq_len, site_set_dates)
        
        #iteratively fill all the tensors by concatenation with their constituent parts
        train_indices[site_no] = {"From":0,"To":0}
        train_indices[site_no]["From"] = train_x.shape[0]
        train_x = torch.cat((train_x, prepped_drivers_data_site['train_x']), dim = 0)
        train_indices[site_no]["To"] = train_x.shape[0]
        train_y = torch.cat((train_y, prepped_drivers_data_site['train_y']), dim = 0)
        
        train_dates = np.concatenate((train_dates, prepped_drivers_data_site['train_dates']), axis = 0)

        #validation
        val_indices[site_no] = {"From":0,"To":0}
        val_indices[site_no]["From"] = val_x.shape[0]
        val_x = torch.cat((val_x, prepped_drivers_data_site['val_x']), dim = 0)
        val_indices[site_no]["To"] = val_x.shape[0]
        val_y = torch.cat((val_y, prepped_drivers_data_site['val_y']), dim = 0)
        
        val_dates = np.concatenate((val_dates, prepped_drivers_data_site['val_dates']), axis = 0)
        
        #train validation together
        train_val_indices[site_no] = {"From":0,"To":0}
        train_val_indices[site_no]["From"] = train_val_x.shape[0]
        train_val_x = torch.cat((train_val_x, prepped_drivers_data_site['train_val_x']), dim = 0)
        train_val_indices[site_no]["To"] = train_val_x.shape[0]
        train_val_y = torch.cat((train_val_y, prepped_drivers_data_site['train_val_y']), dim = 0)
        
        train_val_dates = np.concatenate((train_val_dates, prepped_drivers_data_site['train_val_dates']), axis = 0)

        #testing
        test_indices[site_no] = {"From":0,"To":0}
        test_indices[site_no]["From"] = test_x.shape[0]
        test_x = torch.cat((test_x, prepped_drivers_data_site['test_x']), dim = 0)
        test_indices[site_no]["To"] = test_x.shape[0]
        test_y = torch.cat((test_y, prepped_drivers_data_site['test_y']), dim = 0)
        
        test_dates = np.concatenate((test_dates, prepped_drivers_data_site['test_dates']), axis = 0)
        
        #full
        full_indices[site_no] = {"From":0,"To":0}
        full_indices[site_no]["From"] = full_x.shape[0]
        full_x = torch.cat((full_x, prepped_drivers_data_site['full_x']), dim = 0)
        full_indices[site_no]["To"] = full_x.shape[0]
        full_y = torch.cat((full_y, prepped_drivers_data_site['full_y']), dim = 0)
        
        full_dates = np.concatenate((full_dates, prepped_drivers_data_site['full_dates']), axis = 0)
        
    concat_data = {}
    concat_data['train_x'] = train_x
    concat_data['train_y'] = train_y
    concat_data['train_dates'] = train_dates
    
    concat_data['val_x'] = val_x
    concat_data['val_y'] = val_y
    concat_data['val_dates'] = val_dates
    
    concat_data['train_val_x'] = train_val_x
    concat_data['train_val_y'] = train_val_y
    concat_data['train_val_dates'] = train_val_dates
    
    concat_data['train_val_data_indices_w_nans'] = train_val_data_indices_w_nans

    concat_data['test_x'] = test_x
    concat_data['test_y'] = test_y
    concat_data['test_dates'] = test_dates
    
    concat_data['full_x'] = full_x
    concat_data['full_y'] = full_y
    concat_data['full_dates'] = full_dates

    concat_data['solute_mean'] = solute_mean
    concat_data['solute_std'] = solute_std
    concat_data['train_indices'] = train_indices
    concat_data['val_indices'] = val_indices
    concat_data['train_val_indices'] = train_val_indices
    concat_data['test_indices'] = test_indices
    concat_data['full_indices'] = full_indices
    
    concat_data['full_data_indices_w_nans'] = full_data_indices_w_nans
    
    #write prepped data to file
    concat_data_file = open(os.path.join(out_dir,'prepped_data'),'wb')
    pickle.dump(concat_data, concat_data_file)
    concat_data_file.close()
    
    #write n_means_stds_to_file
    n_means_stds_file = open(os.path.join(out_dir,'n_means_stds'),'wb')
    pickle.dump(n_means_stds, n_means_stds_file)
    n_means_stds_file.close()
    
    #write all variables means stds to file
    all_variables_means_stds_file = open(os.path.join(out_dir,'all_variables_means_stds'),'wb')
    pickle.dump(all_variables_means_stds, all_variables_means_stds_file)
    all_variables_means_stds_file.close()
    

    return concat_data, n_means_stds

def full_prepare_single_site_data(netcdf_loc, run_config_loc, site_no, station_nm, out_dir, hp_tune, hp_tune_vals, weights_dir):
    
    with open(run_config_loc) as stream:
            run_config = yaml.safe_load(stream)
            
    config_loc = run_config['config_loc']
    
    with open(config_loc) as stream:
            config = yaml.safe_load(stream)

    
    if hp_tune:
        seq_len = hp_tune_vals['seq_len']
    else:
        seq_len = config['seq_len']

    if config['predict_period'] == 'full':
        trn_frac = config['trn_frac']+config['val_frac']
    else:    
        trn_frac = config['trn_frac']

    val_frac = config['val_frac']
    test_frac = config['test_frac']
    
    feat_list = config['feat_list']
    if config['static_features_used']:
        static_features = config['static_features']
        feat_list.extend(static_features)
    
    df = xarray_to_df_mod_feat(netcdf_loc, site_no, feat_list)
    if len(df.columns[df.isna().any()].tolist()) > 0:
        print(site_no+ " nans found:",df.columns[df.isna().any()].tolist())
    

    #add individual site no
    #split and normalize input data
    norm_sets, set_dates, n_means_stds_n_obs = split_norm_combine(df, seq_len, trn_frac, val_frac, test_frac, config_loc, run_config_loc, weights_dir)
    #arrange input data into arrays of [data_len, seq_len, num_feat]
    prepped_drivers_data = prepare_data(norm_sets['full_norm'], seq_len, set_dates)
    
    site_data_file = open(os.path.join(out_dir,'prepped_data'),'wb')
    pickle.dump(prepped_drivers_data, site_data_file)
    site_data_file.close()

    n_means_stds_n_obs_file = open(os.path.join(out_dir,'n_means_stds_n_obs'),'wb')
    pickle.dump(n_means_stds_n_obs, n_means_stds_n_obs_file)
    n_means_stds_n_obs_file.close()

    return prepped_drivers_data, n_means_stds_n_obs

def save_nitrate_plot(train_single_site, val_single_site, test_single_site, out_dir, site_no):
    plt.plot(train_single_site.Nitrate, 'b', label = 'Training')
    plt.plot(val_single_site.Nitrate, 'g', label = 'Validation')
    plt.plot(test_single_site.Nitrate, 'r', label = 'Testing')
    plt.ylabel('Nitrate (mg/L)')
    plt.title(train_single_site.site_no[0])
    plt.legend()
    plt.savefig(os.path.join(out_dir,site_no,'splits.png'))
    plt.close()