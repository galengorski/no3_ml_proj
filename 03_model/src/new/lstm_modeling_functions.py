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
import pickle
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
    def __init__(self, input_size, hidden_size, seq_len, num_layers, dropout, learning_rate, weight_decay):
        super().__init__() 
        self.input_size = input_size # Number of features
        self.hidden_size = hidden_size # Cell/Hidden state length
        self.learning_rate = learning_rate #learning rate
        self.num_layers = num_layers
        self.seq_len = seq_len
      

        self.lstm = nn.LSTM(input_size, hidden_size, num_layers, batch_first=True, dropout=dropout)
        self.linear = nn.Linear(self.hidden_size, 1)
        self.dropout = nn.Dropout(dropout)

        self.loss_fn = nn.MSELoss()
        #self.loss_fn = nn.L1Loss()
        self.optimizer = torch.optim.Adam(self.parameters(), lr=learning_rate, weight_decay=weight_decay) 

    def forward(self, input):
        if input.device != dev:
            input = input.to(device)
        
        # input has 3 dimensions (batch_n, seq_len(365), input_size)
        assert (input.shape[1] == self.seq_len),"Seq length is not {}!".format(self.seq_len)
        assert (input.shape[2] == self.input_size),"Input size is not {}!".format(self.input_size)
        batch_n = input.shape[0]
    
        h0 = torch.randn(self.num_layers, batch_n, self.hidden_size).to(device)
        c0 = torch.randn(self.num_layers, batch_n, self.hidden_size).to(device)
        
        input = self.dropout(input)
        
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

def train_lstm(config_loc, concat_model_data, out_dir, hp_tune, hp_tune_vals):
    
    with open(config_loc) as stream:
      config = yaml.safe_load(stream)
    
    if hp_tune:
        learning_rate = hp_tune_vals['learning_rate']
        seq_len = hp_tune_vals['seq_len']
        num_layers = hp_tune_vals['num_layers']
    else:
        learning_rate = config['learning_rate']
        seq_len = config['seq_len']
        num_layers = config['num_layers']
        
    feat_list = config['feat_list']
    if config['static_features_used']:
        static_features = config['static_features']    
        feat_list.extend(static_features)
    num_features = config['num_features']
    #static_features_used = config['static_features_used']
    #learning_rate = config['learning_rate']
    num_epochs = config['num_epochs']
    batch_size = config['batch_size']
    dropout = config['dropout']
    weight_decay = config['weight_decay']
    hidden_size = config['hidden_size']
    shuffle = config['shuffle']

    # set up data splits
    train_dataset = CatchmentDataset(concat_model_data['train_x'], concat_model_data['train_y'])
    val_dataset = CatchmentDataset(concat_model_data['val_x'], concat_model_data['val_y'])
    #full_dataset = CatchmentDataset(concat_model_data['full_x'], concat_model_data['full_y'])

    # Load data in batch
    train_loader = DataLoader(dataset = train_dataset, batch_size = batch_size, shuffle=shuffle,drop_last=False, pin_memory=True)
    #valid_loader = LSTM_helper.DataLoader(dataset = valid_dataset, batch_size = valid_dataset.len, drop_last=True, pin_memory=True)
    #full_loader = DataLoader(dataset = full_dataset, batch_size = full_dataset.len, drop_last=True, pin_memory=True)

    # initialize the model
    model = LSTM_layer(num_features, hidden_size, seq_len, num_layers, dropout, learning_rate, weight_decay)
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
            train_loss = model.loss(concat_model_data['train_x'], concat_model_data['train_y'])
            validation_loss = model.loss(concat_model_data['val_x'], concat_model_data['val_y'])
            print(train_loss.item())
            running_loss.append(train_loss.item())
            valid_loss.append(validation_loss.item())
    torch.save(model.state_dict(), os.path.join(out_dir,"model_weights.pt"))
    print("Training done!")
    elapsed = time.time() - t
    print('The running time is:', elapsed)
    
    #plot losses
    plt.plot(range(num_epochs),running_loss, color = 'dodgerblue', label = 'Training')
    plt.plot(range(num_epochs), valid_loss, color = 'blue', label = 'Validation')
    # Set title and labels for axes
    plt.xlabel('Epoch number')
    plt.ylabel('RMSE')
    plt.legend()
    plt.savefig(os.path.join(out_dir,"losses.png"))
    plt.close()

    
    #return model, running_loss, valid_loss

def make_predictions_lstm(config_loc, out_dir, concat_model_data, hp_tune, hp_tune_vals):
    
    with open(config_loc) as stream:
      config = yaml.safe_load(stream)
      
    if hp_tune:
        learning_rate = hp_tune_vals['learning_rate']
        seq_len = hp_tune_vals['seq_len']
        num_layers = hp_tune_vals['num_layers']
    else:
        learning_rate = config['learning_rate']
        seq_len = config['seq_len']
        num_layers = config['num_layers']

    
        
    feat_list = config['feat_list']
    if config['static_features_used']:
        static_features = config['static_features']    
        feat_list.extend(static_features)
    num_features = config['num_features']
    #static_features_used = config['static_features_used']
    #num_epochs = config['num_epochs']
    dropout = config['dropout']
    weight_decay = config['weight_decay']
    
    
    hidden_size = config['hidden_size']
    #shuffle = config['shuffle']
    
    #model = torch.load(weights_dict+"model_weights.pt")
    
    model = LSTM_layer(num_features, hidden_size, seq_len, num_layers, dropout, learning_rate, weight_decay)
    model = model.to(device)
    model.load_state_dict(torch.load(os.path.join(out_dir,"model_weights.pt")))
    # set up data splits
    train_dataset = CatchmentDataset(concat_model_data['train_x'], concat_model_data['train_y'])
    val_dataset = CatchmentDataset(concat_model_data['val_x'], concat_model_data['val_y'])
    train_val_dataset = CatchmentDataset(concat_model_data['train_val_x'], concat_model_data['train_val_y'])
    #full_dataset = CatchmentDataset(concat_model_data['full_x'], concat_model_data['full_y'])

    train_pred, train_label, train_mse = model.report_mse(train_dataset)
    val_pred, val_label, val_mse = model.report_mse(val_dataset)
    train_val_pred, train_val_label, train_val_mse = model.report_mse(train_val_dataset)
    
    lstm_predictions = {}
    
    lstm_predictions['train_pred'] = train_pred
    lstm_predictions['train_label'] = train_label
    lstm_predictions['val_pred'] = val_pred
    lstm_predictions['val_label'] = val_label
    lstm_predictions['train_val_pred'] = train_val_pred
    lstm_predictions['train_val_label'] = train_val_label
    
    return lstm_predictions




def save_results(train_pred_un, train_label_un, train_dates, val_pred_un, val_label_un, val_dates, train_val_pred_un, train_val_label_un, train_val_dates,config_loc, out_dir, station_nm, site_no, hp_tune, hp_tune_vals, save_results_csv, run_id, multi_site):

    
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
    
    #validation data
    train_val_d = {"DateTime": train_val_dates,
              "Train_Val_Predicted": train_val_pred_un}
    train_val_df = pd.DataFrame(train_val_d)
    
    full_df = pd.concat(objs = [train_df, val_df])
    
   
    p_val = np.array(val_d['Predicted'])
    l_val = np.array(val_d['Labeled'])
    
    
    p_train = np.array(train_d['Predicted'])
    l_train = np.array(train_d['Labeled'])
    
    p_train_val = np.array(train_val_d['Train_Val_Predicted'])
    l_train_val = np.array(train_val_label_un)
    
    site_dict = save_config(out_dir, config_loc, station_nm, site_no, p_train, l_train, p_val, l_val, p_train_val, l_train_val, hp_tune, hp_tune_vals, run_id, multi_site)

    #Save plot of results
    full_df = full_df.assign(DateTime = pd.to_datetime(full_df.DateTime))
    #train_val_df = train_val_df.assign(DateTime = pd.to_datetime(train_val_df.DateTime))
    #train_val_df = train_val_df.drop(columns = 'DateTime')
    #full_df = full_df.join(train_val_df)
    
    if save_results_csv:
        if multi_site:
            full_df.to_csv(os.path.join(out_dir,site_no,"ModelResults.csv"))
        else:
            full_df.to_csv(os.path.join(out_dir,"ModelResults.csv"))

    sns.set(rc={'figure.figsize':(30,20)})
    sns.set_context("poster", font_scale=1.2)
    sns.set_style(style = 'white')
    
    p=sns.lineplot(data = full_df, x = "DateTime", y = "Labeled", label="Observed", color = 'black')
    #ptq
    sns.lineplot(data = full_df[full_df["Training/Validation"] == "Training"], x = "DateTime", y = "Predicted", color = 'dodgerblue', label = 'Training')
    sns.lineplot(data = full_df[full_df['Training/Validation'] == "Validation"], x = "DateTime", y = "Predicted", color = 'blue', label = 'Validation')
    #sns.lineplot(data = full_df, x = 'DateTime', y = 'Train_Val_Predicted', color = 'red', label = 'Training and Validation')
    # Set title and labels for axes
    p.set(xlabel="Date",
           ylabel='Nitrate mg/L [NO3+NO2]',
           title="Predicting Nitrate at "+str(station_nm))
    if multi_site:
        plt.savefig(os.path.join(out_dir, site_no,"model_summary.png"))
    else:
        plt.savefig(os.path.join(out_dir,"model_summary.png"))
    plt.close()
    
    return site_dict
    

def save_config(out_path, config_loc, station_nm, site_no, p_train, l_train, p_val, l_val, p_train_val, l_train_val, hp_tune, hp_tune_vals, run_id, multi_site):
    
    with open(config_loc) as stream:
        config = yaml.safe_load(stream)
        
    if hp_tune:
        learning_rate = hp_tune_vals['learning_rate']
        seq_len = hp_tune_vals['seq_len']
        num_layers = hp_tune_vals['num_layers']
    else:
        learning_rate = config['learning_rate']
        seq_len = config['seq_len']
        num_layers = config['num_layers']
    
    rmse_training = he.evaluator(he.rmse, p_train, l_train)
    rmse_validation = he.evaluator(he.rmse, p_val, l_val)
    nrmse_training = he.evaluator(he.rmse, p_train, l_train)/np.mean(l_train)
    nrmse_validation = he.evaluator(he.rmse, p_val, l_val)/np.mean(l_val)
    nse_training = he.evaluator(he.nse, p_train, l_train)
    nse_validation = he.evaluator(he.nse, p_val, l_val)
    pbias_training = he.evaluator(he.pbias, p_train, l_train)
    pbais_validation = he.evaluator(he.pbias, p_val, l_val)
    
    #save model parameters
    if multi_site:
        f= open(os.path.join(out_path, site_no,"model_param_output.txt"),"w+")
    else:
        f= open(os.path.join(out_path,"model_param_output.txt"),"w+")
    f.write("Date: %s\r\n" % date.today().strftime("%b-%d-%Y"))
    f.write("Station Name: %s\r\n" % station_nm)
    f.write("Site Number: %s\r\n" % site_no)
    f.write("Feature List: %s\r\n" % config['feat_list'])
    f.write("Static Features Used?: %s\r\n" % config['static_features_used'])
    if config['static_features_used']:
        f.write("Static Feature List: %s\r\n" % config['static_features'])
    f.write("Epochs: %d\r\n" % config['num_epochs'])
    f.write("Learning rate: %f\r\n" % learning_rate)
    f.write("Batch Size: %d\r\n" % config['batch_size'])
    f.write("Training Fraction: %f\r\n" % config['trn_frac'])
    f.write("Validation Fraction: %f\r\n" % config['val_frac'])
    f.write("Sequence Length: %d\r\n" % seq_len)
    f.write("Cells: %d\r\n" % config['hidden_size'])
    f.write("Layers: %d\r\n" % num_layers)
    f.write("Dropout: %f\r\n" % config['dropout'])
    f.write("Optimizer weight decay: %f\r\n" % config['weight_decay'])
    f.write("---------RESULTS-------------\n")
    f.write("RMSE_Training: %f\r\n" % rmse_training)
    f.write("RMSE_Validation: %f\r\n" % rmse_validation)
    f.write("NRMSE_Training: %f\r\n" % nrmse_training)
    f.write("NRMSE_Validation: %f\r\n" % nrmse_validation)
    f.write("NSE_Training: %f\r\n" % nse_training)
    f.write("NSE_Validation: %f\r\n" % nse_validation)
    f.write("PBIAS_Training: %f\r\n" % pbias_training)
    f.write("PBIAS_Validation: %f\r\n" % pbais_validation)

    f.close() 
    
    site_dict = {"Run":run_id,"Site_name":station_nm,"Site_number":site_no,
                 "RMSE_training":rmse_training[0], "RMSE_validation":rmse_validation[0],
                 "NRMSE_training":nrmse_training[0], "NRMSE_validation":nrmse_validation[0],
                 "NSE_training":nse_training[0], "NSE_validation":nse_validation[0],
                 "PBIAS_training":pbias_training[0], "PBIAS_validation":pbais_validation[0]}
    
    return site_dict



def unnormalize(pred, mean, std):
    return mean + std * pred




def run_multi_site_model_c(netcdf_loc, config_loc, site_no_list, station_nm_list, read_input_data_from_file, input_file_loc, out_dir, run_id, train_model, multi_site, hp_tune = False, hp_tune_vals = {}, save_results_csv = True):
    
    os.makedirs(out_dir, exist_ok = True)
       
    
    if read_input_data_from_file:
        with open(os.path.join(input_file_loc,'prepped_data'), 'rb') as input_data:
            concat_model_data = pickle.load(input_data)
        with open(os.path.join(input_file_loc,'n_means_stds'), 'rb') as means_stds:
            n_means_stds = pickle.load(means_stds)
        print('Reading input data from '+ out_dir)
    else:         
        #prepare the data
        concat_model_data, n_means_stds = ppf.full_prepare_multi_site_data(netcdf_loc, config_loc, site_no_list, station_nm_list, out_dir)
    
    if train_model:  
        train_lstm(config_loc, concat_model_data, out_dir, hp_tune, hp_tune_vals)
    
    lstm_predictions = make_predictions_lstm(config_loc, out_dir, concat_model_data, hp_tune, hp_tune_vals)
    
    train_pred_un = unnormalize(lstm_predictions['train_pred'], n_means_stds['train_mean'], n_means_stds['train_std'])
    train_label_un = unnormalize(lstm_predictions['train_label'], n_means_stds['train_mean'], n_means_stds['train_std'])
    
    val_pred_un = unnormalize(lstm_predictions['val_pred'], n_means_stds['train_mean'], n_means_stds['train_std'])
    val_label_un = unnormalize(lstm_predictions['val_label'], n_means_stds['train_mean'], n_means_stds['train_std'])
    
    train_val_pred_un = unnormalize(lstm_predictions['train_val_pred'], n_means_stds['train_val_mean'], n_means_stds['train_val_std'])
    train_val_label_un = unnormalize(lstm_predictions['train_val_label'], n_means_stds['train_val_mean'], n_means_stds['train_val_std'])
 
    all_sites_results_list = [] 
 
    for i,site_no in enumerate(site_no_list):
        station_nm = list(station_nm_list)[i]
        print("Saving "+station_nm)
        os.makedirs(os.path.join(out_dir, site_no), exist_ok = True)
        #data from watershed are stored in model data starting at index from and going to index to
        site_from_train = concat_model_data['train_indices'][site_no]["From"]
        site_to_train = concat_model_data['train_indices'][site_no]["To"]
        site_from_val = concat_model_data['val_indices'][site_no]["From"]
        site_to_val = concat_model_data['val_indices'][site_no]["To"]
        site_from_train_val = concat_model_data['train_val_indices'][site_no]["From"]
        site_to_train_val = concat_model_data['train_val_indices'][site_no]["To"]
        #site_from_full = concat_model_data['full_indices'][site]["From"]
        #site_to_full = concat_model_data['full_indices'][site]["To"]
        
        train_pred_site = train_pred_un[site_from_train:site_to_train]
        train_label_site = train_label_un[site_from_train:site_to_train]
        val_pred_site = val_pred_un[site_from_val:site_to_val]
        val_label_site = val_label_un[site_from_val:site_to_val]
        train_val_pred_site = train_val_pred_un[site_from_train_val:site_to_train_val]
        train_val_label_site = train_val_label_un[site_from_train_val:site_to_train_val]
        
        
        train_dates = concat_model_data['train_dates'][site_from_train:site_to_train]
        val_dates = concat_model_data['val_dates'][site_from_val:site_to_val]
        train_val_dates = concat_model_data['train_val_dates'][site_from_train_val:site_to_train_val]

        site_dict = save_results(train_pred_site, train_label_site, train_dates, val_pred_site, val_label_site, val_dates, train_val_pred_site, train_val_label_site, train_val_dates,config_loc, out_dir, station_nm, site_no, hp_tune, hp_tune_vals, save_results_csv, run_id, multi_site)
        
        all_sites_results_list.append(site_dict)
    
    all_sites_results_df = pd.DataFrame(all_sites_results_list)
    all_sites_results_df.to_csv(os.path.join(out_dir,"AllSitesModelResults.csv"))
    
def run_single_site_model_c(netcdf_loc, config_loc, site_no, station_nm, read_input_data_from_file, input_file_loc, out_dir, run_id, train_model, multi_site, hp_tune, hp_tune_vals, save_results_csv = True):
    os.makedirs(out_dir, exist_ok = True)
    
    if read_input_data_from_file:
        with open(os.path.join(input_file_loc,'prepped_data'), 'rb') as input_data:
            site_data = pickle.load(input_data)
        with open(os.path.join(input_file_loc,'n_means_stds'), 'rb') as means_stds:
            n_means_stds = pickle.load(means_stds)
            print('Reading input data from '+ out_dir)
    else:         
        #prepare the data
        site_data, n_means_stds = ppf.full_prepare_single_site_data(netcdf_loc, config_loc, site_no, station_nm, out_dir, hp_tune, hp_tune_vals)

    if train_model:  
        train_lstm(config_loc, site_data, out_dir, hp_tune, hp_tune_vals)
    
    lstm_predictions = make_predictions_lstm(config_loc, out_dir, site_data, hp_tune, hp_tune_vals)
    
    train_pred_un = unnormalize(lstm_predictions['train_pred'], n_means_stds['train_mean'], n_means_stds['train_std'])
    train_label_un = unnormalize(lstm_predictions['train_label'], n_means_stds['train_mean'], n_means_stds['train_std'])
    val_pred_un = unnormalize(lstm_predictions['val_pred'], n_means_stds['train_mean'], n_means_stds['train_std'])
    val_label_un = unnormalize(lstm_predictions['val_label'], n_means_stds['train_mean'], n_means_stds['train_std'])
    train_val_pred_un = unnormalize(lstm_predictions['train_val_pred'], n_means_stds['train_val_mean'], n_means_stds['train_val_std'])
    train_val_label_un = unnormalize(lstm_predictions['train_val_label'], n_means_stds['train_val_mean'], n_means_stds['train_val_std'])

    
    
    train_dates = site_data['train_dates']
    val_dates = site_data['val_dates']
    train_val_dates = site_data['train_val_dates']
    
    site_dict = save_results(train_pred_un, train_label_un, train_dates, val_pred_un, val_label_un, val_dates, train_val_pred_un, train_val_label_un, train_val_dates, config_loc, out_dir, station_nm, site_no, hp_tune, hp_tune_vals, save_results_csv, run_id, multi_site)
    
    return site_dict
#%%
