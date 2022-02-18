#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Feb 18 11:47:03 2022

@author: galengorski
"""

import numpy as np
import pandas as pd
import math
import torch
import torch.nn as nn
from torch.utils.data import Dataset, DataLoader
import time

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
