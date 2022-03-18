#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Feb  4 18:57:47 2022

@author: galengorski
"""
#import netCDF4
import datetime
import math
import numpy as np
import pandas as pd
import torch
import xarray

if torch.cuda.is_available():
    dev = "cuda:0"
else:
    dev = "cpu"

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


def split_norm_combine(data, seq_len, trn_frac, val_frac, test_frac):
    
    #get rid of nitrate na values
    data_no_nans = data['Nitrate'].dropna()
    #this gets rid of any data before the first available nitrate observation with
    #seq len predictors before it
    data_no_nans_seq_len = data_no_nans[data_no_nans.index > data.index[seq_len]]
    
    #get the split points based on the availability of nitrate data
    end_train_date = data_no_nans_seq_len.index[math.floor(trn_frac*data_no_nans_seq_len.shape[0])-1]
    start_val_date = end_train_date+datetime.timedelta(days=1)
    end_val_date = data_no_nans_seq_len.index[math.floor((1-test_frac)*data_no_nans_seq_len.shape[0])-1]
    start_test_date = end_val_date+datetime.timedelta(days=1)
    end_test_date = data_no_nans_seq_len.index[data_no_nans_seq_len.shape[0]-1] 
        
    #split train validate and test splits
    train = data[:end_train_date]
    nobs_train = train.Nitrate.dropna().shape[0]
    val = data[start_val_date:end_val_date]
    nobs_val = val.Nitrate.dropna().shape[0]
    test = data[start_test_date:end_test_date]
    nobs_test = test.Nitrate.dropna().shape[0]
    
    #normalize sets separately
    train_norm = (train - train.mean(axis=0)) / train.std(axis=0)
    val_norm = (val - val.mean(axis=0)) / val.std(axis=0)
    test_norm = (test - test.mean(axis=0)) / test.std(axis=0)
    
    #dictionary of nitrate means and stds
    n_means_stds = {'full_mean' : data.Nitrate.mean(),
    'full_std' : data.Nitrate.std(),
    'train_mean' : train.Nitrate.mean(),
    'train_std' : train.Nitrate.std(),
    'val_mean' : val.Nitrate.mean(),
    'val_std' : val.Nitrate.std(),
    'test_mean' : test.Nitrate.mean(),
    'test_std' : test.Nitrate.std()}
    


    #recombine into single dataframe
    full_data = pd.concat([train_norm, val_norm, test_norm])
    
    print('data split and normalized')
    
    return full_data, nobs_train, nobs_val, nobs_test, n_means_stds

def split_multi_site_data(data, seq_len, trn_frac, val_frac, test_frac):
    
    #get rid of nitrate na values
    data_no_nans = data['Nitrate'].dropna()
    #this gets rid of any data before the first available nitrate observation with
    #seq len predictors before it
    data_no_nans_seq_len = data_no_nans[data_no_nans.index > data.index[seq_len]]
    
    #get the split points based on the availability of nitrate data
    end_train_date = data_no_nans_seq_len.index[math.floor(trn_frac*data_no_nans_seq_len.shape[0])-1]
    start_val_date = end_train_date+datetime.timedelta(days=1)
    end_val_date = data_no_nans_seq_len.index[math.floor((1-test_frac)*data_no_nans_seq_len.shape[0])-1]
    start_test_date = end_val_date+datetime.timedelta(days=1)
    end_test_date = data_no_nans_seq_len.index[data_no_nans_seq_len.shape[0]-1] 
        
    #split train validate and test splits
    train = data[:end_train_date]
    nobs_train = train.Nitrate.dropna().shape[0]
    val = data[start_val_date:end_val_date]
    nobs_val = val.Nitrate.dropna().shape[0]
    test = data[start_test_date:end_test_date]
    nobs_test = test.Nitrate.dropna().shape[0]
    
    return data_no_nans, train, nobs_train, val, nobs_val, test, nobs_test

def normalize_multi_site_data(data, train, val, test):
    train_site_v = train['site_no']
    val_site_v = val['site_no']
    test_site_v = test['site_no']
    
    #remove site numbers
    train = train.drop(columns = ['site_no'])
    val = val.drop(columns = ['site_no'])
    test = test.drop(columns = ['site_no'])

    #normalize sets separately
    train_norm = (train - train.mean(axis=0)) / train.std(axis=0)
    val_norm = (val - val.mean(axis=0)) / val.std(axis=0)
    test_norm = (test - test.mean(axis=0)) / test.std(axis=0)
    
    #add site no back in
    train_norm['site_no'] = train_site_v
    val_norm['site_no'] = val_site_v
    test_norm['site_no'] = test_site_v
    
    #dictionary of nitrate means and stds
    n_means_stds = {'full_mean' : data.Nitrate.mean(),
    'full_std' : data.Nitrate.std(),
    'train_mean' : train.Nitrate.mean(),
    'train_std' : train.Nitrate.std(),
    'val_mean' : val.Nitrate.mean(),
    'val_std' : val.Nitrate.std(),
    'test_mean' : test.Nitrate.mean(),
    'test_std' : test.Nitrate.std()}
    
    return train_norm, val_norm, test_norm, n_means_stds



def prepare_data(data, seq_len, trn_frac, val_frac, test_frac):
    '''returns an array with dim [data length, seq_len, num features]'''
    
    #define a function which that finds the row with nan
    which = lambda lst:list(np.where(lst)[0])
        
    #split data into train/val/test 
    data_no_nans, train, nobs_train, val, nobs_val, test, nobs_test = split_multi_site_data(data, seq_len, trn_frac, val_frac, test_frac)
    
    
    #seq len predictors before it
    data_no_nans_seq_len = data_no_nans[data_no_nans.index > data.index[seq_len]]

    #combined_y = data_no_nans_seq_len
    train_y = train['Nitrate'].dropna()
    train_dates = train['Nitrate'].dropna().index
    val_y = val['Nitrate'].dropna()
    val_dates = val['Nitrate'].dropna().index
    test_y = test['Nitrate'].dropna()
    test_dates = test['Nitrate'].dropna().index
    full_y = data.Nitrate
    
    
    #this is the number of nitrate observations that have seq_len days of 
    #predictors before them in the dataset, for example if we have a nitrate observation
    #on the first day, we don't have any predictor data for the previous seq_len days for that 
    #observation
    nobs = data_no_nans[data_no_nans.index > data.index[seq_len]].shape[0]   
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
    trainval_x = combined_x[0:(nobs_train+nobs_val),:,:]
    test_x = combined_x[(nobs_train+nobs_val):(nobs_train+nobs_val+nobs_test),:,:]
    
    
    train_x_t, train_y_t = torch.from_numpy(np.array(train_x)).float(), torch.from_numpy(np.array(train_y)).float()
    val_x_t, val_y_t = torch.from_numpy(np.array(val_x)).float(), torch.from_numpy(np.array(val_y)).float()
    trainval_x_t, trainval_y_t = torch.from_numpy(np.array(trainval_x)).float(), torch.from_numpy(np.array(train_y.append(val_y))).float()
    test_x_t, test_y_t = torch.from_numpy(np.array(test_x)).float(), torch.from_numpy(np.array(test_y)).float()
    full_x_t, full_y_t = torch.from_numpy(np.array(full_combined_x)).float(), torch.from_numpy(np.array(full_y)).float()    

    
    print('data prepped')
    
    return full_x_t, train_x_t, val_x_t, trainval_x_t, test_x_t, full_y_t, train_y_t, val_y_t, trainval_y_t, test_y_t, train_dates, val_dates, test_dates

    