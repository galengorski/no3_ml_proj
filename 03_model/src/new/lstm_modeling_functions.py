#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Feb 18 11:47:03 2022

@author: galengorski
"""
from datetime import date, timedelta
import hydroeval as he
import itertools
import numpy as np
import pandas as pd
import preproc_functions as ppf
import math
import matplotlib.pyplot as plt
import os
import pickle
import scipy
import seaborn as sns
import time
import torch
import torch.nn as nn
from torch.utils.data import Dataset, DataLoader
import tqdm
import yaml
#%%

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

def train_lstm(run_config_loc, concat_model_data, out_dir, hp_tune, hp_tune_vals, fine_tune, weights_dir):
    
    with open(run_config_loc) as stream:
        run_config = yaml.safe_load(stream)
            
    config_loc = run_config['config_loc']
    
    with open(config_loc) as stream:
            config = yaml.safe_load(stream)
    
    if hp_tune:
        learning_rate = hp_tune_vals['learning_rate']
        seq_len = hp_tune_vals['seq_len']
        num_layers = hp_tune_vals['num_layers']
        hidden_size = hp_tune_vals['num_cells']
    else:
        learning_rate = config['learning_rate']
        seq_len = config['seq_len']
        num_layers = config['num_layers']
        hidden_size = config['hidden_size']
        
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
    fine_tune = run_config['fine_tune']
    #weights_dir = run_config['weights_dir']

    # set up data splits
    # if config['predict_period'] == 'full':
    #     train_dataset = CatchmentDataset(concat_model_data['train_val_x'], concat_model_data['train_val_y'])
    # else:
    #     train_dataset = CatchmentDataset(concat_model_data['train_x'], concat_model_data['train_y'])
    
    train_dataset = CatchmentDataset(concat_model_data['train_x'], concat_model_data['train_y'])
    # Load data in batch
    train_loader = DataLoader(dataset = train_dataset, batch_size = batch_size, shuffle=shuffle,drop_last=False, pin_memory=True)
 
    # initialize the model
    model = LSTM_layer(num_features, hidden_size, seq_len, num_layers, dropout, learning_rate, weight_decay)
    model = model.to(device)
    
    #if fine_tune = True, the read the weights in from the file,
    #train the model using the new data, and save the new weights in the out_dir
    if fine_tune:
        model.load_state_dict(torch.load(os.path.join(weights_dir,"model_weights.pt")))
        print("Loading pre-trained weights for fine tuning")
    else:
        print("Not loading pre-trained weights")
        
    #run the model
    running_loss = []
    valid_loss = []
    t = time.time()
    print("Training in progress...")
    for epoch in np.arange(num_epochs):
        if(int(epoch+1) % 16) == 0:
            print("\nEpoch {}/{}".format(epoch+1, num_epochs))
        #with tqdm.tqdm(total=np.floor(train_dataset.len / batch_size), position=0, leave=True) as progress_bar:
        for i, data in enumerate(train_loader):
            sequence, label = data
            model.update(sequence, label)         
            #progress_bar.update(1)
        with torch.no_grad():
            # if config['predict_period'] == 'full':
            #     train_loss = model.loss(concat_model_data['train_val_x'], concat_model_data['train_val_y'])
            # else:
            #     train_loss = model.loss(concat_model_data['train_x'], concat_model_data['train_y'])
            train_loss = model.loss(concat_model_data['train_x'], concat_model_data['train_y'])
            if config['predict_period'] == 'full':
                validation_loss = model.loss(concat_model_data['test_x'], concat_model_data['test_y'])
            else:
                validation_loss = model.loss(concat_model_data['val_x'], concat_model_data['val_y'])
            if(int(epoch+1) % 16) == 0:
                print("Training loss: "+ str(round(train_loss.item(), ndigits = 4)))
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

def make_predictions_lstm(run_config_loc, out_dir, concat_model_data, hp_tune, hp_tune_vals, train_model, weights_dir):
    
    with open(run_config_loc) as stream:
        run_config = yaml.safe_load(stream)
            
    config_loc = run_config['config_loc']
    
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
    predict_period = config['predict_period']
    fine_tune = run_config['fine_tune']
    weights_dir = run_config['weights_dir']
    
    model = LSTM_layer(num_features, hidden_size, seq_len, num_layers, dropout, learning_rate, weight_decay)
    model = model.to(device)
    
    if train_model or fine_tune:
        model.load_state_dict(torch.load(os.path.join(out_dir,"model_weights.pt")))
    else:
        model.load_state_dict(torch.load(os.path.join(weights_dir,"model_weights.pt")))
    
    train_val_dataset = CatchmentDataset(concat_model_data['train_val_x'], concat_model_data['train_val_y'])
    full_dataset = CatchmentDataset(concat_model_data['full_x'], concat_model_data['full_y'])

    lstm_predictions = {}
    if predict_period == 'full':
        full_pred, full_label, full_mse = model.report_mse(full_dataset)
        lstm_predictions['full_pred'] = full_pred
        lstm_predictions['full_label'] = full_label
    else:
        train_val_pred, train_val_label, train_val_mse = model.report_mse(train_val_dataset)
        lstm_predictions['train_val_pred'] = train_val_pred
        lstm_predictions['train_val_label'] = train_val_label

    return lstm_predictions


def unnormalize_lstm_prediction(config_loc, lstm_predictions, n_means_stds):
    with open(config_loc) as stream:
        config = yaml.safe_load(stream)
        
    predict_period = config['predict_period']
    
    if predict_period == 'full':
        pred_un = unnormalize(lstm_predictions['full_pred'], n_means_stds['train_mean'], n_means_stds['train_std'])
        label_un = unnormalize(lstm_predictions['full_label'], n_means_stds['train_mean'], n_means_stds['train_std'])
    else:
        pred_un = unnormalize(lstm_predictions['train_val_pred'], n_means_stds['train_mean'], n_means_stds['train_std'])
        label_un = unnormalize(lstm_predictions['train_val_label'], n_means_stds['train_mean'], n_means_stds['train_std'])

    preds_unnorm = {'pred':pred_un,
                    'label':label_un}
    
    return preds_unnorm



def save_results(config_loc, preds_unnorm, site_data, out_dir, station_nm, site_no, hp_tune, hp_tune_vals, save_results_csv, run_id, multi_site):
    with open(config_loc) as stream:
        config = yaml.safe_load(stream)
        
    predict_period = config['predict_period']
    print(site_data['val_dates'], site_data['val_dates'].shape)

    if predict_period == 'full':
        plot_label_set = 'Testing'
        full_dict = {
            'DateTime': site_data['full_dates'],
            "Predicted": preds_unnorm['pred'],
            "Labeled": preds_unnorm['label'],
            "Train/Val/Test": np.repeat(np.nan, len(preds_unnorm['pred']))
            }
        full_df_long = pd.DataFrame(full_dict)
        full_df_long = full_df_long.set_index('DateTime')
        train_end = site_data['train_dates'][-1]
        full_df_long["Train/Val/Test"] = np.repeat('Training',len(full_df_long[full_df_long.index <= train_end])).tolist()+np.repeat('Testing',len(full_df_long[full_df_long.index > train_end ])).tolist()
    else:
        plot_label_set = 'Validation'
        full_dict = {
            "DateTime": site_data['train_dates'].union(site_data['val_dates']),
            "Predicted": preds_unnorm['pred'],
            "Labeled": preds_unnorm['label'],
            "Train/Val/Test": np.repeat('Training', len(site_data['train_dates'])).tolist()+np.repeat('Validation', len(site_data['val_dates'])).tolist()
            }
        full_df_long = pd.DataFrame(full_dict)
        full_df_long = full_df_long.set_index('DateTime')
        
    #only keep the predictions starting at seq_len number of days before the first nitrate observation
    full_df = full_df_long[full_df_long.index >= (full_df_long['Labeled'].dropna().index[0] - timedelta(days = config['seq_len']))]
    
    p_val = np.array(full_df[full_df['Train/Val/Test'] != 'Training'].Predicted)
    l_val = np.array(full_df[full_df['Train/Val/Test'] != 'Training'].Labeled)
    
    p_train = np.array(full_df[full_df['Train/Val/Test'] == 'Training'].Predicted)
    l_train = np.array(full_df[full_df['Train/Val/Test'] == 'Training'].Labeled)
    
    #if there are not any observations in the val set then don't save the run summary
    #this only happens during fine tuning with quarterly down sampling
    if len(site_data['val_dates'] != 0):    
        site_dict = save_config(out_dir, config_loc, station_nm, site_no, p_train, l_train, p_val, l_val, hp_tune, hp_tune_vals, run_id, multi_site)
    else:
        site_dict = {}
     
    if save_results_csv:
        if multi_site:
            full_df.to_csv(os.path.join(out_dir,site_no,"ModelResults.csv"))
        else:
            full_df.to_csv(os.path.join(out_dir,"ModelResults.csv"))

    #plot the results
    plt.plot(full_df.Labeled, c = 'black', label = 'Observed')
    plt.plot(full_df[full_df['Train/Val/Test'] == 'Training'].Predicted, c = 'dodgerblue', label = 'Training')
    plt.plot(full_df[full_df['Train/Val/Test'] != 'Training'].Predicted, c = 'blue', label = plot_label_set)
    plt.legend()
    plt.xlabel('Date')
    plt.xticks(rotation=45, ha='right')
    plt.ylabel('Nitrate mg/L [NO3+NO2]')
    plt.title("Predicting Nitrate at "+str(station_nm)) 
    plt.tight_layout()
    
    if multi_site:
        plt.savefig(os.path.join(out_dir, site_no,"model_summary.png"))
    else:
        plt.savefig(os.path.join(out_dir,"model_summary.png"))
    plt.close()
    
    return site_dict
    

def save_config(out_path, config_loc, station_nm, site_no, p_train, l_train, p_val, l_val, hp_tune, hp_tune_vals, run_id, multi_site):
    
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
    nrmse_training = he.evaluator(he.rmse, p_train, l_train)/np.nanmean(l_train)
    nrmse_validation = he.evaluator(he.rmse, p_val, l_val)/np.nanmean(l_val)
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
    f.write("Prediction Period: %s\r\n" % config['predict_period'])
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


def run_multi_site_model_c(netcdf_loc, run_config_loc, site_no_list, station_nm_list, read_input_data_from_file, input_file_loc, out_dir, run_id, train_model, multi_site, fine_tune, weights_dir, hp_tune, hp_tune_vals, save_results_csv):
    
    os.makedirs(out_dir, exist_ok = True)
    
    with open(run_config_loc) as stream:
            run_config = yaml.safe_load(stream)
            
    config_loc = run_config['config_loc']
    
    with open(config_loc) as stream:
            config = yaml.safe_load(stream)
            
    if read_input_data_from_file:
        with open(os.path.join(input_file_loc,'prepped_data'), 'rb') as input_data:
            concat_model_data = pickle.load(input_data)
        with open(os.path.join(input_file_loc,'n_means_stds'), 'rb') as means_stds:
            n_means_stds = pickle.load(means_stds)
        print('Reading input data from '+ input_file_loc)
    else:         
        #prepare the data
        concat_model_data, n_means_stds = ppf.full_prepare_multi_site_data(netcdf_loc, config_loc, site_no_list, station_nm_list, out_dir, fine_tune, weights_dir)
    
    if train_model:  
        train_lstm(run_config_loc, concat_model_data, out_dir, hp_tune, hp_tune_vals, fine_tune, weights_dir)
    
    lstm_predictions = make_predictions_lstm(run_config_loc, out_dir, concat_model_data, hp_tune, hp_tune_vals, train_model, weights_dir)

    preds_unnorm = unnormalize_lstm_prediction(config_loc, lstm_predictions, n_means_stds)
    
    all_sites_results_list = [] 
    
    site_index = 0
    for i,site_no in enumerate(site_no_list):
        station_nm = list(station_nm_list)[i]
        print("Saving "+station_nm)
        os.makedirs(os.path.join(out_dir, site_no), exist_ok = True)
        #data from watershed are stored in model data starting at index from and going to index to
        site_from_train = concat_model_data['train_indices'][site_no]["From"]
        site_to_train = concat_model_data['train_indices'][site_no]["To"]
        site_from_val = concat_model_data['val_indices'][site_no]["From"]
        site_to_val = concat_model_data['val_indices'][site_no]["To"]
        n_train = site_to_train - site_from_train
        n_val = site_to_val - site_from_val
        site_from_train_val = concat_model_data['train_val_indices'][site_no]["From"]
        site_to_train_val = concat_model_data['train_val_indices'][site_no]["To"]
        
        site_from_full = concat_model_data['full_indices'][site_no]["From"]
        site_to_full = concat_model_data['full_indices'][site_no]["To"]

            
        if config['predict_period'] == 'full':
            site_preds_unnorm = {
                'pred':preds_unnorm['pred'][site_from_full:site_to_full],
                'label':preds_unnorm['label'][site_from_full:site_to_full]
            }
        else:
            site_preds_unnorm = {
                'pred':preds_unnorm['pred'][site_index:(site_index+n_train+n_val)],
                'label':preds_unnorm['label'][site_index:(site_index+n_train+n_val)]
            }
        
        site_data = {
            'train_dates':pd.to_datetime(concat_model_data['train_dates'][site_from_train:site_to_train]),
            'val_dates':pd.to_datetime(concat_model_data['val_dates'][site_from_val:site_to_val]),
            'val_train_dates':pd.to_datetime(concat_model_data['train_val_dates'][site_from_train_val:site_to_train_val]),
            'full_dates':pd.to_datetime(concat_model_data['full_dates'][site_from_full:site_to_full])
            }

        site_dict = save_results(config_loc, site_preds_unnorm, site_data, out_dir, station_nm, site_no, hp_tune, hp_tune_vals, save_results_csv, run_id, multi_site)
        
        all_sites_results_list.append(site_dict)
        
        site_index = site_index+n_train+n_val
    
    all_sites_results_df = pd.DataFrame(all_sites_results_list)
    all_sites_results_df.to_csv(os.path.join(out_dir,"AllSitesModelResults.csv"))

def wrapper_run_multi_site_model_c(run_config_loc):
    with open(run_config_loc) as stream:
        run_config = yaml.safe_load(stream) 
        
    site_info = pd.read_csv(run_config['site_info_loc'],  dtype = {'site_no':str})
    site_no_list = site_info.site_no
    station_nm_list = site_info[site_info.site_no.isin(site_no_list)].station_nm
    
    
    model_run_id = run_config['model_run_id']
    
     
    netcdf_loc = run_config['netcdf_loc']
    config_loc = run_config['config_loc']
    read_input_data_from_file = run_config['read_input_data_from_file']
    n_reps = run_config['n_reps']
    train_model = run_config['train_model']
    save_results_csv = run_config['save_results_csv']
    hp_tune = run_config['hp_tune']
    hp_tune_vals = run_config['hp_tune_vals']
    fine_tune = run_config['fine_tune']
    weights_dir = run_config['weights_dir']
    #oos_sites = run_config['oos_sites']
    multi_site = True
    
    if run_config['train_oos_exp']:
        model_run_id = os.path.join(run_config['model_run_id'], 'global')

    input_file_loc = os.path.join('03_model/out/multi_site',model_run_id)
    
    for j in range(n_reps):
        
        rep = 'Rep_0'+str(j)
        run_id = os.path.join('03_model/out/multi_site',model_run_id,rep)
        out_dir = os.path.join('03_model/out/multi_site',model_run_id,rep)
        weights_dir = os.path.join('03_model/out/multi_site',model_run_id,rep)
        
        #if you're training for out of sample experiments
        if run_config['train_oos_exp']:
            #read site info back in fresh so you have all the sites
            del site_info
            site_info = pd.read_csv(run_config['site_info_loc'],  dtype = {'site_no':str})   

            #generate out of sample sites
            oos_sites = site_info.groupby('cluster', group_keys = False).apply(lambda x: x.sample(1))[['site_no','cluster','hydro_terrane']]
            #write the out of sample sites to file
            os.makedirs(out_dir, exist_ok=True)
            oos_sites.to_csv(os.path.join(out_dir,'oos_sites.csv'))
            #drop those sites from the site_info dataframe
            site_info.drop(oos_sites.index, inplace = True)
            site_no_list = site_info.site_no
            station_nm_list = site_info[site_info.site_no.isin(site_no_list)].station_nm

        #if it's the first rep or if we are training out of sample models
        if j == 0 or run_config['train_oos_exp']:
            print(j)
            read_input_data_from_file = False
        else:
            read_input_data_from_file = True
            input_file_loc = os.path.join('03_model/out/multi_site',model_run_id,'Rep_00')
        
        run_multi_site_model_c(netcdf_loc, run_config_loc, site_no_list, station_nm_list, 
                   read_input_data_from_file, input_file_loc, out_dir, run_id,
                   train_model, multi_site, fine_tune, weights_dir, hp_tune, hp_tune_vals, save_results_csv)

def run_single_site_model_c(netcdf_loc, run_config_loc, site_no, station_nm, read_input_data_from_file, input_file_loc, out_dir, run_id, train_model, multi_site, hp_tune, hp_tune_vals, weights_dir, save_results_csv):
    
    os.makedirs(out_dir, exist_ok = True)
    
    with open(run_config_loc) as stream:
        run_config = yaml.safe_load(stream)
            
    config_loc = run_config['config_loc']
    fine_tune = run_config['fine_tune']
    
    if read_input_data_from_file:
        with open(os.path.join(input_file_loc,'prepped_data'), 'rb') as input_data:
            site_data = pickle.load(input_data)
        with open(os.path.join(input_file_loc,'n_means_stds_n_obs'), 'rb') as means_stds:
            n_means_stds = pickle.load(means_stds)
            print('Reading input data from '+ input_file_loc)
            print('Writing results to '+ out_dir)
    else:         
        #prepare the data
        site_data, n_means_stds = ppf.full_prepare_single_site_data(netcdf_loc, run_config_loc, site_no, station_nm, out_dir, hp_tune, hp_tune_vals, weights_dir)

    if train_model:  
        train_lstm(run_config_loc, site_data, out_dir, hp_tune, hp_tune_vals, fine_tune, weights_dir)
    
    lstm_predictions = make_predictions_lstm(run_config_loc, out_dir, site_data, hp_tune, hp_tune_vals, train_model, weights_dir)    
        
    preds_unnorm = unnormalize_lstm_prediction(config_loc, lstm_predictions, n_means_stds)
        
    site_dict = save_results(config_loc, preds_unnorm, site_data, out_dir, station_nm, site_no, hp_tune, hp_tune_vals, save_results_csv, run_id, multi_site)
    return site_dict

def wrapper_fine_tune_multi_site_models(run_config_loc):
    with open(run_config_loc) as stream:
        run_config = yaml.safe_load(stream) 
        
    site_info = pd.read_csv(run_config['site_info_loc'],  dtype = {'site_no':str})
    site_no_list = site_info.site_no
    station_nm_list = site_info[site_info.site_no.isin(site_no_list)].station_nm
    
    
    model_run_id = run_config['model_run_id']
    model_run_dir = os.path.join('03_model/out/multi_site', model_run_id)
    netcdf_loc = run_config['netcdf_loc']
    config_loc = run_config['config_loc']
    read_input_data_from_file = run_config['read_input_data_from_file']
    n_reps = run_config['n_reps']
    train_model = run_config['train_model']
    save_results_csv = run_config['save_results_csv']
    hp_tune = run_config['hp_tune']
    hp_tune_vals = run_config['hp_tune_vals']
    weights_top_dir = run_config['weights_dir']
    #fine_tune = run_config['fine_tune']
    #oos_sites = run_config['oos_sites']
    multi_site = False

    for j in range(n_reps):
        
        rep = ('Rep_0'+str(j))
        oos_sites = pd.read_csv(os.path.join(weights_top_dir,'global',rep,'oos_sites.csv'),  dtype = {'site_no':str})
        
        for i, oos_site in enumerate(oos_sites.site_no):
            site = oos_site
            cl = '0'+str(oos_sites['cluster'].iloc[i])
            ht = oos_sites['hydro_terrane'].iloc[i]
            
            #for global model
            weights_dir = os.path.join(weights_top_dir, 'global',rep)
            out_dir = os.path.join(model_run_dir,'global',rep, site)
            station_nm_list = list(site_info[site_info.site_no == site].station_nm)
            run_id = os.path.join(model_run_id,'global', rep, site)

            read_input_data_from_file = False
            input_file_loc = os.path.join(model_run_id,'global', rep, site)
            
            print('-----------------------------------------------------------------------')
            print('Global Model')
            print('Training site: ', site, ' # ', str(i+1), ' of 5 out sample sites')
            print('Reading model weights from : ', weights_dir)
            print('Writing results to: ', out_dir)
            print('-----------------------------------------------------------------------')
        
            site_perf = run_single_site_model_c(netcdf_loc, run_config_loc, site, station_nm_list, 
                                   read_input_data_from_file, input_file_loc, out_dir, run_id,
                                   train_model, multi_site, hp_tune, hp_tune_vals, weights_dir, save_results_csv)
            
            
            #for clustered model
            del site_perf
            weights_dir = os.path.join(weights_top_dir, 'cluster',rep,'Cluster_'+cl)
            out_dir = os.path.join(model_run_dir,'cluster',rep, site)
            run_id = os.path.join(model_run_id,'cluster', rep, site)

            read_input_data_from_file = True
            input_file_loc = os.path.join(model_run_dir,'global', rep, site)
            
            print('-----------------------------------------------------------------------')
            print('Cluster Model')
            print('Training site: ', site, ' # ', str(i+1), ' of 5 out sample sites')
            #print('Reading model weights from : ', weights_dir)
            #print('Writing results to: ', out_dir)
            print('-----------------------------------------------------------------------')
        
            site_perf = run_single_site_model_c(netcdf_loc, run_config_loc, site, station_nm_list, 
                                   read_input_data_from_file, input_file_loc, out_dir, run_id,
                                   train_model, multi_site, hp_tune, hp_tune_vals, weights_dir, save_results_csv)
            
            #for hydroterrane model
            del site_perf
            weights_dir = os.path.join(weights_top_dir, 'hydroterrane',rep,'Terrane_'+ht)
            out_dir = os.path.join(model_run_dir,'hydroterrane',rep, site)
            run_id = os.path.join(model_run_id,'hydroterrane', rep, site)

            read_input_data_from_file = True
            input_file_loc = os.path.join(model_run_dir,'global', rep, site)
            
            print('-----------------------------------------------------------------------')
            print('Hydroterrane Model')
            print('Training site: ', site, ' # ', str(i+1), ' of 5 out sample sites')
            #print('Reading model weights from : ', weights_dir)
            #print('Writing results to: ', out_dir)
            print('-----------------------------------------------------------------------')
        
            site_perf = run_single_site_model_c(netcdf_loc, run_config_loc, site, station_nm_list, 
                                   read_input_data_from_file, input_file_loc, out_dir, run_id,
                                   train_model, multi_site, hp_tune, hp_tune_vals, weights_dir, save_results_csv)
        
        


def wrapper_run_single_site_model_c(run_config_loc):
    with open(run_config_loc) as stream:
        run_config = yaml.safe_load(stream) 
        
    site_info = pd.read_csv(run_config['site_info_loc'],  dtype = {'site_no':str})
    site_no_list = site_info.site_no
    station_nm_list = site_info[site_info.site_no.isin(site_no_list)].station_nm
    
    
    model_run_id = run_config['model_run_id']
    model_run_dir = os.path.join('03_model/out/single_site', model_run_id)
    netcdf_loc = run_config['netcdf_loc']
    config_loc = run_config['config_loc']
    read_input_data_from_file = run_config['read_input_data_from_file']
    n_reps = run_config['n_reps']
    train_model = run_config['train_model']
    save_results_csv = run_config['save_results_csv']
    hp_tune = run_config['hp_tune']
    hp_tune_vals = run_config['hp_tune_vals']
    weights_dir = run_config['weights_dir']
    #fine_tune = run_config['fine_tune']
    #oos_sites = run_config['oos_sites']
    multi_site = False
    
    
    #run n replicates
    for j in range(n_reps):
        run_id = os.path.join(model_run_id,  ('Rep_0'+str(j)))
        
        all_sites_results_list = [] 
        
        for i,site in enumerate(site_info.site_no): 
            site_no_list = site
            
            #if it isn't the first rep use the data that has been processed for rep 0
            if j == 0:
                read_input_data_from_file = False
                input_file_loc = os.path.join(model_run_dir,  ('Rep_0'+str(j)))
            else:
                read_input_data_from_file = True
                input_file_loc = os.path.join(model_run_dir, 'Rep_00', site)
                
            station_nm_list = list(site_info[site_info.site_no == site].station_nm)
            out_dir = os.path.join(model_run_dir,  ('Rep_0'+str(j)), site)
            
            print('-----------------------------------------------------------------------')
            print('Training site: ', site, ' # ', str(i+1), ' of 46 sites')
            print('-----------------------------------------------------------------------')
        
            site_perf = run_single_site_model_c(netcdf_loc, run_config_loc, site_no_list, station_nm_list, 
                                   read_input_data_from_file, input_file_loc, out_dir, run_id,
                                   train_model, multi_site, hp_tune, hp_tune_vals, weights_dir, save_results_csv)
        
            all_sites_results_list.append(site_perf)
            
        all_sites_results_df = pd.DataFrame(all_sites_results_list)
        all_sites_results_df.to_csv(os.path.join(model_run_dir,  ('Rep_0'+str(j)),"AllSitesModelResults.csv"))
    
def wrapper_single_site_model_hyperparameter_tuning(run_config_loc):
    
    with open(run_config_loc) as stream:
            run_config = yaml.safe_load(stream) 
            
    model_run_id = run_config['model_run_id']
    #model_run_dir = os.path.join('03_model/out/single_site', model_run_id)
    netcdf_loc = run_config['netcdf_loc']
    config_loc = run_config['config_loc']
    read_input_data_from_file = run_config['read_input_data_from_file']
    n_reps = run_config['n_reps']
    train_model = run_config['train_model']
    save_results_csv = run_config['save_results_csv']

    hp_tune = run_config['hp_tune']
    sl = run_config['sl']
    lr = run_config['lr']
    l = run_config['l']
    ncl = run_config['ncl']
    hp_tune_vals = list(itertools.product(sl,lr,l,ncl))
    
    #these are the indices of the first hp set with a new sequence length
    #we'll call back to these to reuse the data
    first_seq_len = []
    hp_tune_vals_df = pd.DataFrame(hp_tune_vals)
    for seq_len_hp in sl:
        first_seq_len.append(hp_tune_vals_df[hp_tune_vals_df[0] == seq_len_hp].index[0])
    
    weights_dir = run_config['weights_dir']
    multi_site = False
    
    for rep in range(n_reps):
        
        all_sites_results_list = [] 
            
        #randomly select n sites for hyperparameter tuning
        site_info = pd.read_csv(run_config['site_info_loc'],  dtype = {'site_no':str}).sample(15)
        site_no_list = site_info.site_no
        station_nm_list = site_info[site_info.site_no.isin(site_no_list)].station_nm
        
        model_run_rep_id = os.path.join('03_model/out/single_site',model_run_id, 'Rep_'+str(rep).zfill(2))
        
        for i,hp in enumerate(hp_tune_vals):
            HP_Run_pad = str(i).zfill(2)
            model_HP_run_id = 'HP_'+HP_Run_pad
            model_rep_HP_dir = os.path.join(model_run_rep_id,model_HP_run_id)
            #print(model_HP_run_dir)
            hp_tune_vals_run = {'seq_len':hp_tune_vals[i][0],'learning_rate':hp_tune_vals[i][1], 
                            'num_layers':hp_tune_vals[i][2], 'num_cells':hp_tune_vals[i][3]}
            #print(hp_tune_vals_run)
            
            input_file_dir = model_run_rep_id
            #if the sequence length is the same as a previous run then don't reproduce the data
            if i == 0:
                data_source = 'FIRST SET OF HYPERPARAMETERS'
                read_input_data_from_file = False
            elif hp_tune_vals_run['seq_len'] == hp_tune_vals[i-1][0]:
                data_source = "READING HYPERPARAMETER VALUES FROM FILE"
                read_input_data_from_file = True
                #find the previous directory that had the same seqence length
                HP_Run_pad_read = str(int(math.floor((i)/(len(hp_tune_vals)/len(sl)))*(len(hp_tune_vals)/len(sl)))).zfill(2)
                print(HP_Run_pad_read)
                model_HP_run_id_read = 'HP_'+HP_Run_pad_read
                input_file_dir = os.path.join(model_run_rep_id,model_HP_run_id_read)
            else:
                data_source = "FIRST INSTANCE OF THIS SEQUENCE LENGTH -- PREPARING DATA"
                read_input_data_from_file = False
                              
            print('-----------------------------------------------------------------------')
            print('HP Set '+str(i+1)+' of '+str(len(hp_tune_vals)))
            print(hp_tune_vals_run)
            print('Rep '+str(rep+1)+' of '+ str(n_reps))
            print(data_source)
            if data_source == "READING HYPERPARAMETER VALUES FROM FILE":
                print(input_file_dir)
            print('-----------------------------------------------------------------------')
            
            all_sites_results_list = [] 
            
            for j,site in enumerate(site_info.site_no): 
                site_no_list = site
                station_nm_list = list(site_info[site_info.site_no == site].station_nm)
                run_id = model_rep_HP_dir
                out_dir = os.path.join(model_rep_HP_dir, site)
                
                #this is the location to read the data in from if it has already been created
                input_file_loc = os.path.join(input_file_dir, site)
            
                site_perf = run_single_site_model_c(netcdf_loc, run_config_loc, site_no_list, station_nm_list, 
                                       read_input_data_from_file, input_file_loc, out_dir, run_id,
                                       train_model, multi_site, hp_tune = hp_tune, hp_tune_vals = hp_tune_vals_run, weights_dir = weights_dir, save_results_csv = save_results_csv)
            
                all_sites_results_list.append(site_perf)
                
            all_sites_results_df = pd.DataFrame(all_sites_results_list)
            all_sites_results_df.to_csv(os.path.join(model_rep_HP_dir,"AllSitesModelResults.csv"))
            
def wrapper_run_cluster_model(run_config_loc):
    with open(run_config_loc) as stream:
        run_config = yaml.safe_load(stream) 
        
    site_info = pd.read_csv(run_config['site_info_loc'],  dtype = {'site_no':str})   
        
    model_run_id = run_config['model_run_id']
    netcdf_loc = run_config['netcdf_loc']
    config_loc = run_config['config_loc']
    read_input_data_from_file = run_config['read_input_data_from_file']
    n_reps = run_config['n_reps']
    train_model = run_config['train_model']
    save_results_csv = run_config['save_results_csv']
    hp_tune = run_config['hp_tune']
    hp_tune_vals = run_config['hp_tune_vals']
    fine_tune = run_config['fine_tune']
    weights_dir = run_config['weights_dir']
    #oos_sites = run_config['oos_sites']
    multi_site = True
    
    if run_config['train_oos_exp']:
        model_run_id = os.path.join(run_config['model_run_id'], 'cluster')
    
    input_file_loc = os.path.join('03_model/out/multi_site',model_run_id)
    
    for j in range(n_reps):
        rep = 'Rep_0'+str(j)
        #if you're training for out of sample experiments
        if run_config['train_oos_exp']:
            
            #read site info back in fresh so you have all the sites
            del site_info
            site_info = pd.read_csv(run_config['site_info_loc'],  dtype = {'site_no':str})   

            oos_sites = pd.read_csv(os.path.join('03_model/out/multi_site',run_config['model_run_id'],'global',rep,'oos_sites.csv'), index_col = 0)
            #drop those sites from the site_info dataframe
            site_info.drop(oos_sites.index, inplace = True)
            site_no_list = site_info.site_no
            station_nm_list = site_info[site_info.site_no.isin(site_no_list)].station_nm

         
        for group in np.sort(site_info.cluster.unique()):
            print('-----------------------------------------------------------------------')
            print('Training group ', group, ' of 5 | replicate #', str(j+1), 'of ', str(n_reps))
            print('-----------------------------------------------------------------------')
            site_no_list = site_info[site_info['cluster'] == group].site_no
            station_nm_list = site_info[site_info['cluster'] == group].station_nm
            run_id = os.path.join(model_run_id, rep,  'Cluster_0'+str(group))
            out_dir = os.path.join('03_model/out/multi_site', model_run_id, rep,  'Cluster_0'+str(group))
            
            #if it's the first replicate or we are training out of sample models
            if j == 0 or run_config['train_oos_exp']:
                read_input_data_from_file = False
            else:
                read_input_data_from_file = True
                input_file_loc = os.path.join('03_model/out/multi_site',model_run_id,'Rep_00','Cluster_0'+str(group))
            
            run_multi_site_model_c(netcdf_loc, run_config_loc, site_no_list, station_nm_list, 
                       read_input_data_from_file, input_file_loc, out_dir, run_id,
                       train_model, multi_site, fine_tune, weights_dir, hp_tune, hp_tune_vals, save_results_csv)


def wrapper_run_hydroterrane_model(run_config_loc):
    with open(run_config_loc) as stream:
        run_config = yaml.safe_load(stream) 
        
    site_info = pd.read_csv(run_config['site_info_loc'],  dtype = {'site_no':str})    
    
    model_run_id = run_config['model_run_id']
    netcdf_loc = run_config['netcdf_loc']
    config_loc = run_config['config_loc']
    read_input_data_from_file = run_config['read_input_data_from_file']
    n_reps = run_config['n_reps']
    train_model = run_config['train_model']
    save_results_csv = run_config['save_results_csv']
    hp_tune = run_config['hp_tune']
    hp_tune_vals = run_config['hp_tune_vals']
    fine_tune = run_config['fine_tune']
    weights_dir = run_config['weights_dir']
    multi_site = True
    
    if run_config['train_oos_exp']:
        model_run_id = os.path.join(run_config['model_run_id'], 'hydroterrane')

    
    input_file_loc = os.path.join('03_model/out/multi_site',model_run_id)
    
    for j in range(n_reps):
        rep = 'Rep_0'+str(j)
        #if you're training for out of sample experiments
        if run_config['train_oos_exp']:
            #read site info back in fresh so you have all the sites
            del site_info
            site_info = pd.read_csv(run_config['site_info_loc'],  dtype = {'site_no':str})   

            oos_sites = pd.read_csv(os.path.join('03_model/out/multi_site',run_config['model_run_id'],'global',rep,'oos_sites.csv'), index_col = 0)
            #drop those sites from the site_info dataframe
            site_info.drop(oos_sites.index, inplace = True)
            site_no_list = site_info.site_no
            station_nm_list = site_info[site_info.site_no.isin(site_no_list)].station_nm
         
        for k, group in enumerate(np.sort(site_info.hydro_terrane.unique())):
            print('-----------------------------------------------------------------------')
            print('Training ', group, ' | group', str(k+1), ' of 8 hydroterranes | replicate #', str(j+1), 'of ', str(n_reps))
            print('-----------------------------------------------------------------------')
            site_no_list = site_info[site_info['hydro_terrane'] == group].site_no
            station_nm_list = site_info[site_info['hydro_terrane'] == group].station_nm
            run_id = os.path.join(model_run_id, rep,  'hydro_terrane'+str(group))
            out_dir = os.path.join('03_model/out/multi_site', model_run_id, rep,  'Terrane_'+str(group))
            
            #if it's the first replicate or we are training out of sample models
            if j == 0 or run_config['train_oos_exp']:
                read_input_data_from_file = False
            else:
                read_input_data_from_file = True
                input_file_loc = os.path.join('03_model/out/multi_site',model_run_id,'Rep_00','Terrane_'+str(group))
            
            run_multi_site_model_c(netcdf_loc, run_config_loc, site_no_list, station_nm_list, 
                       read_input_data_from_file, input_file_loc, out_dir, run_id,
                       train_model, multi_site, fine_tune, weights_dir, hp_tune, hp_tune_vals, save_results_csv)


