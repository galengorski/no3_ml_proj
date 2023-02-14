#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Dec  1 12:25:41 2022

@author: galengorski
"""
import lstm_modeling_functions as lmf
import numpy as np
import os
import pandas as pd
import pickle
import yaml
import torch
from torch.utils.data import DataLoader

     

def expected_gradients_lstm(x_data_in, x_set, model, n_samples, temporal_focus=None):
    '''
    A general function for generating expected gradients from a pytorch model
    Parameters
    ----------
    x_data_in : torch.Tensor
        x variables prepared for model
    x_set : torch.Tensor
        x_variables prepared for model for the entire dataset, used to generate the baseline
    model : pytorch model
        pre trained pytorch model
    n_samples : int
        number of samples to draw for calculating expcted gradient
    temporal_focus : None (default) or int
        If the expected gradient should be calculated with respect to a single 
        day of year then that day of year should be input, otherwise none for every day. 
        The default is None.

    Returns
    -------
    numpy.ndarray of the same shape as x_data_in

    '''
    device = 'cpu'
    n_series = x_set.shape[0]
    #num_vars = x_set.shape[2]
    #seq_len = x_set.shape[1]
    
    for k in range(n_samples):
        # SAMPLE A SERIES FROM OUR DATA
        rand_seq = np.random.choice(n_series) # rand_time may be more accurate
        baseline_x = x_set[rand_seq].to(device)

        # SAMPLE A RANDOM SCALE ALONG THE DIFFERENCE
        scale = np.random.uniform()

        # SAME IG CALCULATION
        x_diff = x_data_in - baseline_x
        curr_x = baseline_x + scale*x_diff
        if curr_x.requires_grad == False:
            curr_x.requires_grad = True
        model.zero_grad()
        y,_ = model(curr_x)

        # GET GRADIENT
        if temporal_focus == None:
            gradients = torch.autograd.grad(y, curr_x, torch.ones_like(y))
        else:
            gradients = torch.autograd.grad(y[temporal_focus], curr_x, torch.ones_like(y[temporal_focus]))

        if k == 0:
            expected_gradients = x_diff*gradients[0] * 1/n_samples
        else:
            expected_gradients = expected_gradients + ((x_diff*gradients[0]) * 1/n_samples)

    return(expected_gradients.detach().cpu().numpy())

def initialize_model(config_loc, weights_loc):
    '''

    Parameters
    ----------
    config_loc : str
        file path; location of the model config yaml file
    weights_loc : str
        file paht; location of the trained weights file
 
    Returns
    -------
    feat_list : list
        list of features used in the model
    model : lstm_modeling_functions.LSTM_layer
        initialized model

    '''
 #read in model configs
    with open(config_loc) as stream:
        config = yaml.safe_load(stream)  
    
    learning_rate = config['learning_rate']
    seq_len = config['seq_len']
    num_layers = config['num_layers']
        
    feat_list = config['feat_list']
    
    static_features = config['static_features']    
    feat_list.extend(static_features)
    num_features = config['num_features']
    
    #batch_size = config['batch_size']
    dropout = config['dropout']
    weight_decay = config['weight_decay']
    hidden_size = config['hidden_size']
    #shuffle = config['shuffle']
    
    #initialize the model
    model = lmf.LSTM_layer(num_features, hidden_size, seq_len, num_layers, dropout, learning_rate, weight_decay)
    #model = model.to(device)
    model.load_state_dict(torch.load(weights_loc))

    return feat_list, model

def select_site(site, run_id, batch_size = 512, device = 'cpu', shuffle = True):
    '''
    Parameters
    ----------
    site : str
        site_no of the site of interest.
    batch_size : int, optional
        needed to load the data into DataLoader. The default is 512.
    device : str, optional
        The default is 'cpu'.
    shuffle : boolean, optional
        shuffled input data for the model. The default is True.

    Returns
    -------
    concat_model_data : dictionary
        concatenated model data read in from file.
    site_dates : numpy.array
        dates of observations
    train_loader_site : torch.DataLoader
        loaded data for single site
    train_loader_site_full : torch.DataLoader
        loaded data for all sites (used to generate baseline values)

    '''
    with open(os.path.join('03_model/out/multi_site',run_id,'Rep_00/prepped_data'), 'rb') as input_data:
            concat_model_data = pickle.load(input_data)

    site_X = concat_model_data['train_val_x'][concat_model_data['train_val_indices'][site]["From"]:concat_model_data['train_val_indices'][site]["To"]]
    site_y = concat_model_data['train_val_y'][concat_model_data['train_val_indices'][site]["From"]:concat_model_data['train_val_indices'][site]["To"]]
    site_dates = concat_model_data['train_val_dates'][concat_model_data['train_val_indices'][site]["From"]:concat_model_data['train_val_indices'][site]["To"]]

    device = 'cpu'

    #prepare the train val dataset
    train_val_dataset = lmf.CatchmentDataset(site_X, site_y)
    train_val_dataset.device = device
    train_loader_site = DataLoader(dataset = train_val_dataset, batch_size = batch_size, shuffle=shuffle,drop_last=False, pin_memory=True)
    
    #prepare the full dataset, all sites (needed for baseline)
    train_val_dataset_full = lmf.CatchmentDataset(concat_model_data['train_val_x'], concat_model_data['train_val_y'])
    train_val_dataset_full.device = device
    train_loader_site_full = DataLoader(dataset = train_val_dataset_full, batch_size = batch_size, shuffle=shuffle,drop_last=False, pin_memory=True)
    
    return concat_model_data, site_dates, train_loader_site, train_loader_site_full

def calc_expected_gradients(model_config_loc, run_id, site, n_reps):
    '''
    Parameters
    ----------
    model_config_loc : str
        file path; location of the model config yaml file
    run_id: str
        model run directory
    site : str
        site_no of the site of interest

    Returns
    -------
    inputs_df : data frame
        model inputs returned for plotting
    eg_df : data frame
        expected gradients

    '''
    
    
    n_samples = 200
    
    
    
    egs_all_reps_list = list()
    for j in range(n_reps):
        rep = 'Rep_0'+str(j)
        weights_loc = os.path.join('03_model/out/multi_site',run_id,rep,'model_weights.pt')
        if j == 0:
            print("Preparing data: "+rep)
            feat_list, model = initialize_model(model_config_loc, weights_loc)
            concat_model_data, site_dates, site_data, full_data = select_site(site, run_id)

            #site_data.dataset.X has dimension [n_obs, seq_len, n_features]
            x_data_in = site_data.dataset.X
            x_set = full_data.dataset.X
            print("Calculating expected gradients for "+site)
            egs = expected_gradients_lstm(x_data_in, x_set, model, n_samples, temporal_focus=None)

        else:
            print("Using prepared data: "+ rep)
            print("Calculating expected gradients for "+site)
            egs = expected_gradients_lstm(x_data_in, x_set, model, n_samples, temporal_focus=None)
            
        egs_all_reps_list.append(egs) 
        
        #stack the resulting list into a numpy array
    egs_all_reps_np = np.stack(egs_all_reps_list, axis = 0)
    #take the mean of all reps
    egs_np = np.mean(egs_all_reps_np, axis = 0)
    eg_df = pd.DataFrame(egs_np[:,0,:], columns = feat_list[1:], index = site_dates)
        
    #inputs_np = x_data_in.detach().numpy()
    #inputs_df = pd.DataFrame(inputs_np[:,0,:], columns = feat_list[1:], index = site_dates)
    
    #eg_df = pd.DataFrame(egs[:,0,:], columns = feat_list[1:], index = site_dates)
    return eg_df

def fetch_predictions(site, reps):
    ss_rep_all = pd.read_csv('03_model/out/single_site/Run_00_Full/Rep_00/'+site+'/ModelResults.csv', parse_dates = True, index_col = 'DateTime')
    for rep in range(1,reps):
        ss_rep_temp = pd.read_csv('03_model/out/single_site/Run_00_Full/Rep_0'+str(rep)+'/'+site+'/ModelResults.csv', parse_dates = True, index_col = 'DateTime').drop( columns = ['Labeled','Train/Val/Test'])
        ss_rep_all = ss_rep_all.join(ss_rep_temp, rsuffix = '_rep_'+str(rep))
    
    model_preds_mean_ss = ss_rep_all.filter(like = 'Predicted').mean(axis = 1)
    model_preds_mean_ss['Labeled'] = ss_rep_all['Labeled']
    model_preds_mean_ss['Train/Val/Test'] = ss_rep_all['Train/Val/Test']
    
    ms_rep_all = pd.read_csv('03_model/out/multi_site/Run_06_MS/Rep_00/'+site+'/ModelResults.csv', parse_dates = True, index_col = 'DateTime')
    for rep in range(1,reps):
        ms_rep_temp = pd.read_csv('03_model/out/single_site/Run_06_MS/Rep_0'+str(rep)+'/'+site+'/ModelResults.csv', parse_dates = True, index_col = 'DateTime').drop( columns = ['Labeled','Train/Val/Test'])
        ms_rep_all = ss_rep_all.join(ms_rep_temp, rsuffix = '_rep_'+str(rep))
    
    model_preds_mean_ms = ms_rep_all.filter(like = 'Predicted').mean(axis = 1)
    model_preds_mean_ms['Labeled'] = ms_rep_all['Labeled']
    model_preds_mean_ms['Train/Val/Test'] = ms_rep_all['Train/Val/Test']
    
    return model_preds_mean_ss, model_preds_mean_ms

def calc_expected_gradients_all_sites(run_config_loc):
    '''
    wrapper function that calculates expected gradients across n replicates and saves them to a csv for each site

    Parameters
    ----------
    run_config_loc : str
        location of run config yaml file

    Returns
    -------
    None.

    '''
    with open(run_config_loc) as stream:
        run_config = yaml.safe_load(stream)  

    model_config_loc = run_config['config_loc']
    run_id = run_config['model_run_id']    
    site_info = pd.read_csv(run_config['site_info_loc'],  dtype = {'site_no':str}) 
    n_reps = run_config['n_reps']
    
    for site in site_info.site_no.unique():
        eg_df = calc_expected_gradients(model_config_loc, run_id, site, n_reps)
        eg_df.to_csv('04_analysis/out/EG_sites/multi_site_01_single_rep'+site+'.csv')
