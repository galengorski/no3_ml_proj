#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Feb 18 11:47:03 2022

@author: galengorski
"""
from datetime import date
import hydroeval as he
import numpy as np
import pandas as pd
import preproc_functions as ppf
import math
import matplotlib.pyplot as plt
import os
import seaborn as sns
import time
import torch
import torch.nn as nn
from torch.utils.data import Dataset, DataLoader
import tqdm
import yaml


if torch.cuda.is_available():
    dev = "cuda:0"
else:
    dev = "cpu"
    
device = torch.device(dev)

######## Dataset helper functions #######
class CatchmentDataset(Dataset):
    def __init__(self, X, y):
        self.len = X.shape[0]
        self.X = X
        self.y = y
        #self.w = w
        
    def __getitem__(self, index):
        return self.X[index], self.y[index]#, self.w[index]
    
    def __len__(self):
        return self.len


class LSTM_layer(nn.Module):
    def __init__(self, input_size, hidden_size, seq_len, num_layers, dropout, learning_rate):
        super().__init__() 
        self.input_size = input_size # Number of features
        self.hidden_size = hidden_size # Cell/Hidden state length
        self.learning_rate = learning_rate #learning rate
        self.num_layers = num_layers
        self.seq_len = seq_len
      

        self.lstm = nn.LSTM(input_size, hidden_size, num_layers, batch_first=True, dropout=dropout)
        self.linear = nn.Linear(self.hidden_size, 1)

        self.loss_fn = nn.MSELoss()
        self.optimizer = torch.optim.Adam(self.parameters(), lr=learning_rate) 

    def forward(self, input):
        if input.device != dev:
            input = input.to(device)
        # input has 3 dimensions (batch_n, seq_len(365), input_size)
        assert (input.shape[1] == self.seq_len),"Seq length is not {}!".format(self.seq_len)
        assert (input.shape[2] == self.input_size),"Input size is not {}!".format(self.input_size)
        batch_n = input.shape[0]

        h0 = torch.randn(self.num_layers, batch_n, self.hidden_size).to(device)
        c0 = torch.randn(self.num_layers, batch_n, self.hidden_size).to(device)
        hidden_all_ts, (hn, cn) = self.lstm(input, (h0, c0)) 
        # hidden_all_ts (batch_n, seq_len, hidden_size), 
        # hn (num_layers, batch_n, hidden_size)
        # hidden_all_ts stores the hidden states at each timesteps at the last layer, which means output[:,-1,:] = hn
        
        hidden_last_ts = hidden_all_ts[:,-1,:] # or hn[-1,:,:]
        output = self.linear(hidden_last_ts) # output dimension:(batch_n,1)
        return output, (hn, cn)


    def loss(self, input, target):  
        if input.device != dev:
            input = input.to(device)
        if target.device != dev:
            target = target.to(device)
        if len(target.shape)<=1:
            target = target.reshape(target.shape[0],1)

        output = self(input)[0]
        loss = self.loss_fn(output, target)
        
        return loss # tensor
    
    
    def update(self, input, target):  
        self.optimizer.zero_grad()
        loss = self.loss(input, target)
        loss.backward()
        self.optimizer.step()

    def report_mse(self, dataset):
        # make prediction   
        sequence, label = dataset.X, dataset.y
        if label.device != dev:
            label = label.to(device)
        pred = self(sequence)[0].reshape(-1)
        return to_numpy(pred), to_numpy(label), to_numpy(self.loss_fn(pred, label)) 

####### tensor helper functions #######
def to_numpy(tensor):
    if tensor.device !='cpu':
        tensor = tensor.cpu()
    return tensor.detach().numpy()


def run_model(netcdf_loc, config_loc, site_no, station_nm, out_dir):

    with open(config_loc) as stream:
        config = yaml.safe_load(stream)['run_single_site_model.py']
        
    out_path = os.path.join(out_dir,site_no)
    
    os.makedirs(out_path, exist_ok = True)
    
    seq_len = config['seq_len']
    trn_frac = config['trn_frac']
    val_frac = config['val_frac']
    test_frac = config['test_frac']
    
    feat_list = config['feat_list']
    num_features = config['num_features']
    #static_features = config['static_features']
    learning_rate = config['learning_rate']
    num_epochs = config['num_epochs']
    batch_size = config['batch_size']
    num_layers = config['num_layers']
    dropout = config['dropout']
    hidden_size = config['hidden_size']
    shuffle = config['shuffle']
    
    #prep data
    df = ppf.xarray_to_df(netcdf_loc, site_no, feat_list)
    data_split, nobs_train, nobs_val, nobs_test, n_means_stds = ppf.split_norm_combine(df, seq_len, trn_frac, val_frac, test_frac)
    full_x, train_x, val_x, trainval_x, test_x, full_y, train_y, val_y, trainval_y, test_y, train_dates, val_dates, test_dates = ppf.prepare_data(data_split, seq_len, nobs_train, nobs_val, nobs_test)

    # set up data splits
    train_dataset = CatchmentDataset(train_x, train_y)
    val_dataset = CatchmentDataset(val_x, val_y)
    full_dataset = CatchmentDataset(full_x, full_y)
    
    # Load data in batch
    train_loader = DataLoader(dataset = train_dataset, batch_size = batch_size, shuffle=shuffle,drop_last=False, pin_memory=True)
    #valid_loader = LSTM_helper.DataLoader(dataset = valid_dataset, batch_size = valid_dataset.len, drop_last=True, pin_memory=True)
    #full_loader = DataLoader(dataset = full_dataset, batch_size = full_dataset.len, drop_last=True, pin_memory=True)
    
    # initialize the model
    model = LSTM_layer(num_features, hidden_size, seq_len, num_layers, dropout, learning_rate)
    model = model.to(device)
    
    #run the model
    running_loss = []
    valid_loss = []
    t = time.time()
    print("Training in progress...")
    for epoch in np.arange(num_epochs):
        print("\nEpoch {}/{}".format(epoch+1, num_epochs))
        with tqdm.tqdm(total=np.floor(train_dataset.len / batch_size), position=0, leave=True) as progress_bar:
            for i, data in enumerate(train_loader):
                sequence, label = data
                model.update(sequence, label)         
                progress_bar.update(1)
        with torch.no_grad():
            train_loss = model.loss(train_x, train_y)
            validation_loss = model.loss(val_x, val_y)
            print(train_loss.item())
            running_loss.append(train_loss.item())
            valid_loss.append(validation_loss.item())
    #torch.save(model.state_dict(), par+".pt")
    print("Training done!")
    elapsed = time.time() - t
    print('The running time is:', elapsed)
    #Performance Evaluation
    train_pred, train_label, train_mse = model.report_mse(train_dataset)
    val_pred, val_label, val_mse = model.report_mse(val_dataset)
    full_pred, full_label, full_mse = model.report_mse(full_dataset)
    print('The calibration error is', round(np.sqrt(train_mse),4))
    print('The validation error is', round(np.sqrt(val_mse),4))
    
    train_pred_un = unnormalize(train_pred, n_means_stds['train_mean'], n_means_stds['train_std'])
    train_label_un = unnormalize(train_label, n_means_stds['train_mean'], n_means_stds['train_std'])
    val_pred_un = unnormalize(val_pred, n_means_stds['val_mean'], n_means_stds['val_std'])
    val_label_un = unnormalize(val_label, n_means_stds['val_mean'], n_means_stds['val_std'])
    full_pred_un = unnormalize(full_pred, n_means_stds['full_mean'], n_means_stds['full_std'])
    full_label_un = unnormalize(full_label, n_means_stds['full_mean'], n_means_stds['full_std'])

    #save the predictions
    #calibing data
    train_d = {"DateTime": train_dates,
               "Predicted": train_pred_un,
               "Labeled": train_label_un,
               "Training/Validation":np.repeat("Training", len(train_dates)).tolist()}
    train_df = pd.DataFrame(train_d)
    
    #validation data
    val_d = {"DateTime": val_dates,
              "Predicted": val_pred_un,
              "Labeled": val_label_un,
              "Training/Validation":np.repeat("Validation", len(val_dates)).tolist()}
    val_df = pd.DataFrame(val_d)
    
    full_df = pd.concat(objs = [train_df, val_df])
    
    full_df.to_csv(out_path+"/ModelResults.csv")

    #Calculate mutual info between obs and predicted
    p_val = np.array(val_d['Predicted'])
    l_val = np.array(val_d['Labeled'])
    
    # pred_lab_val = np.column_stack((p_val,l_val))
    
    # MI_val = MI_helper.mutinfo_newRel(pred_lab_val, nbins = [11,11,11])
    
    p_train = np.array(train_d['Predicted'])
    l_train = np.array(train_d['Labeled'])
    
    # pred_lab_train = np.column_stack((p_train,l_train))
    
    # MI_train = MI_helper.mutinfo_newRel(pred_lab_train, nbins = [11,11,11])
    
    # print("The Calibration MI is ", round(MI_train,4))
    # print( "The Validation MI is ", round(MI_val,4))
    #
    #save the running losses
    losses_d = {"Training":running_loss,
                "Validation": valid_loss}
    losses_df = pd.DataFrame(losses_d)
    # losses_df.to_csv(wkdir+"/model_output/EpochLosses.csv")
    #save model
    torch.save(model.state_dict(), out_path+"/model.pt")
    #save model parameters
    f= open(out_path+"/model_param_output.txt","w+")
    f.write("Date: %s\r\n" % date.today().strftime("%b-%d-%Y"))
    f.write("Station Name: %s\r\n" % station_nm)
    f.write("Site Number: %s\r\n" % site_no)
    f.write("Feature List: %s\r\n" % feat_list)
    f.write("Epochs: %d\r\n" % num_epochs)
    f.write("Learning rate: %f\r\n" % learning_rate)
    f.write("Batch Size: %d\r\n" % batch_size)
    f.write("Training Fraction: %f\r\n" % trn_frac)
    f.write("Validation Fraction: %f\r\n" % val_frac)
    f.write("Sequence Length: %d\r\n" % seq_len)
    f.write("Cells: %d\r\n" % hidden_size)
    f.write("---------RESULTS-------------\n")
    f.write("RMSE_Calib: %f\r\n" % np.sqrt(train_mse))
    f.write("RMSE_Valid: %f\r\n" % np.sqrt(val_mse))
    f.write("NSE_Calib: %f\r\n" % he.nse(p_train, l_train))
    f.write("NSE_Valid: %f\r\n" % he.nse(p_val, l_val))
    #f.write("MI_Calib: %f\r\n" % MI_calib)
    #f.write("MI_Valid: %f\r\n" % MI_valid)
    f.close() 
    #Save plot of results
    full_df = full_df.assign(DateTime = pd.to_datetime(full_df.DateTime))
    sns.set(rc={'figure.figsize':(30,20)})
    sns.set_context("poster", font_scale=1.2)
    sns.set_style(style = 'white')
    fig, axes = plt.subplots(2,1)
    
    sns.lineplot(ax = axes[0], data = full_df, x = "DateTime", y = "Labeled", label="Observed", color = 'black')
    #ptq
    sns.lineplot(ax = axes[0], data = full_df[full_df["Training/Validation"] == "Training"], x = "DateTime", y = "Predicted", color = 'dodgerblue', label = 'Training')
    sns.lineplot(ax = axes[0], data = full_df[full_df['Training/Validation'] == "Validation"], x = "DateTime", y = "Predicted", color = 'blue', label = 'Validation')
    # Set title and labels for axes
    axes[0].set(xlabel="Date",
           ylabel='Nitrate mg/L [NO3+NO2]',
           title="Predicting Nitrate at "+station_nm)
    
    sns.lineplot(ax = axes[1], data = losses_df, x = losses_df.index.values, y = "Training", color = 'dodgerblue', label = 'Training')
    sns.lineplot(ax = axes[1], data = losses_df, x = losses_df.index.values, y = "Validation", color = 'blue', label = 'Validation')
    # Set title and labels for axes
    axes[1].set(xlabel="Epoch number",
           ylabel="RMSE")
    plt.savefig(out_path+"/model_summary.png")

def unnormalize(pred, mean, std):
    return mean + std * pred


#MULTISITE MODEL
#getting there, need to figure out site indices and nobs per site
def run_multi_site_model(netcdf_loc, config_loc, site_no_list, station_nm_list, out_dir):

    with open(config_loc) as stream:
        config = yaml.safe_load(stream)['run_multi_site_model.py']
        
    out_path = os.path.join(out_dir,site_no)
    
    os.makedirs(out_path, exist_ok = True)
    
    seq_len = config['seq_len']
    trn_frac = config['trn_frac']
    val_frac = config['val_frac']
    test_frac = config['test_frac']
    
    feat_list = config['feat_list']
    static_features = config['static_features']
    feat_list.extend(static_features)
    num_features = config['num_features']
    static_features_used = config['static_features_used']
    learning_rate = config['learning_rate']
    num_epochs = config['num_epochs']
    batch_size = config['batch_size']
    num_layers = config['num_layers']
    dropout = config['dropout']
    hidden_size = config['hidden_size']
    shuffle = config['shuffle']
    
    train_data_all_sites = pd.DataFrame()
    val_data_all_sites = pd.DataFrame()
    test_data_all_sites = pd.DataFrame()
    full_data_all_sites = pd.concat([train_data_all_sites,val_data_all_sites,test_data_all_sites])
    
    for site_no in site_no_list_short:
        #convert single site from xarray group to dataframe
        df = ppf.xarray_to_df(netcdf_loc, site_no, feat_list)
        #add individual site no
        df['site_no'] = site_no
        #split data into train, val, test splits individually by site
        data_no_nans, train, nobs_train, val, nobs_val, test, nobs_test = ppf.split_multi_site_data(df, seq_len, trn_frac, val_frac, test_frac)
        
        #recombine into single dataframe
        train_data_all_sites = pd.concat([train_data_all_sites, train])
        val_data_all_sites = pd.concat([val_data_all_sites, val])
        test_data_all_sites = pd.concat([test_data_all_sites, train])
        full_data_no_nans_all_sites = pd.concat([full_data_no_nans_all_sites, data_no_nans])
    
    #normalize data as a full data set wtih all sites
    full_data_all_sites = pd.concat([train_data_all_sites,val_data_all_sites,test_data_all_sites])
    train_norm, val_norm, test_norm, n_means_stds = ppf.normalize_multi_site_data(full_data_all_sites, train_data_all_sites, val_data_all_sites, test_data_all_sites)
    
    #split data back up for creating the input sequences
    for site_no in site_no_list_short:
        train_single_site = train_norm[train_norm['site_no'] == site_no]
        val_single_site = val_norm[val_norm['site_no'] == site_no]
        test_single_site = test_norm[test_norm['site_no'] == site_no]
        full_data_single_site = pd.concat([train_single_site, val_single_site, test_single_site])
    
        full_x, train_x, val_x, test_x, full_y, train_y, val_y, test_y, train_dates, val_dates, test_dates = ppf.prepare_data(full_data_single_site, seq_len, nobs_train, nobs_val, nobs_test)
