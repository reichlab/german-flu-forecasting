#!/usr/bin/env python
# encoding: utf-8

from __future__ import print_function
import argparse
import math
import time
from torch.autograd import Variable

import torch
import torch.nn as nn
from models import AR, VAR, GAR, RNN
from models import CNNRNN, CNNRNN_Res
import numpy as np
import sys
import os
from utils import *
import Optim


def evaluate(loader, data, model, evaluateL2, evaluateL1, batch_size):
    model.eval();
    total_loss = 0;
    total_loss_l1 = 0;
    n_samples = 0;
    predict = None;
    test = None;

    for inputs in loader.get_batches(data, batch_size, False):
        X, Y = inputs[0], inputs[1]
        output = model(X);

        if predict is None:
            predict = output.cpu();
            test = Y.cpu();
        else:
            predict = torch.cat((predict,output.cpu()));
            test = torch.cat((test, Y.cpu()));

        scale = loader.scale.expand(output.size(0), loader.m)
        total_loss += evaluateL2(output * scale , Y * scale ).data[0]
        total_loss_l1 += evaluateL1(output * scale , Y * scale ).data[0]
        n_samples += (output.size(0) * loader.m);

    rse = math.sqrt(total_loss / n_samples)/loader.rse
    rae = (total_loss_l1/n_samples)/loader.rae
    correlation = 0;

    predict = predict.data.numpy();
    Ytest = test.data.numpy();
    sigma_p = (predict).std(axis = 0);
    sigma_g = (Ytest).std(axis = 0);
    mean_p = predict.mean(axis = 0)
    mean_g = Ytest.mean(axis = 0)
    index = (sigma_g!=0);

    correlation = ((predict - mean_p) * (Ytest - mean_g)).mean(axis = 0)/(sigma_p * sigma_g);
    correlation = (correlation[index]).mean();
    # root-mean-square error, absolute error, correlation
    return rse, rae, correlation;


def train(loader, data, model, criterion, optim, batch_size):
    model.train();
    total_loss = 0;
    n_samples = 0;
    counter = 0
    for inputs in loader.get_batches(data, batch_size, True):
        counter += 1
        X, Y = inputs[0], inputs[1]
        model.zero_grad();
        output = model(X);
        scale = loader.scale.expand(output.size(0), loader.m)
        loss = criterion(output * scale, Y * scale);
        loss.backward();
        optim.step();
        total_loss += loss.data[0];
        n_samples += (output.size(0) * loader.m);
    return total_loss / n_samples


def train_dl(data,adjacency_matrix,model_name,save_name,step_ahead,epochs):
    
  parser = argparse.ArgumentParser(description='Epidemiology Forecasting')
  # --- Data option
  parser.add_argument('--data', type=str, default="./training_data.txt",help='location of the data file')
  #parser.add_argument('--train', type=float, default=0.8,help='how much data used for training')
  #parser.add_argument('--valid', type=float, default=0.2,help='how much data used for validation')
  parser.add_argument('--model', type=str, default='AR',help='model to select')
  # --- CNNRNN option
  parser.add_argument('--sim_mat', type=str,help='file of similarity measurement (Required for CNNRNN, CNN)')
  parser.add_argument('--hidRNN', type=int, default=50, help='number of RNN hidden units')
  parser.add_argument('--residual_window', type=int, default=4,help='The window size of the residual component')
  parser.add_argument('--ratio', type=float, default=1.,help='The ratio between CNNRNN and residual')
  parser.add_argument('--output_fun', type=str, default=None, help='the output function of neural net')
  # --- Logging option
  parser.add_argument('--save_dir', type=str,  default='./save',help='dir path to save the final model')
  parser.add_argument('--save_name', type=str,  default='tmp', help='filename to save the final model')
  # --- Optimization option
  parser.add_argument('--optim', type=str, default='adam', help='optimization method')
  parser.add_argument('--dropout', type=float, default=0.2, help='dropout applied to layers (0 = no dropout)')
  parser.add_argument('--epochs', type=int, default=100,help='upper epoch limit')
  parser.add_argument('--clip', type=float, default=1.,help='gradient clipping')
  parser.add_argument('--lr', type=float, default=0.001, help='learning rate')
  parser.add_argument('--weight_decay', type=float, default=0, help='weight decay (L2 regularization)')
  parser.add_argument('--batch_size', type=int, default=128, metavar='N',help='batch size')
  # --- Misc prediction option
  parser.add_argument('--horizon', type=int, default=12, help='predict horizon')
  parser.add_argument('--window', type=int, default=10,help='window size')
  parser.add_argument('--metric', type=int, default=1, help='whether (1) or not (0) normalize rse and rae with global variance/deviation ')
  parser.add_argument('--normalize', type=int, default=0, help='the normalized method used, detail in the utils.py')
  
  parser.add_argument('--seed', type=int, default=54321,help='random seed')
  parser.add_argument('--gpu', type=int, default=None, help='GPU number to use')
  parser.add_argument('--cuda', type=str, default=False, help='use gpu or not')
  
  tmp = np.reshape(data,(-1,16))
  np.savetxt("tmp.txt",tmp,delimiter=",", fmt='%f')
  #np.savetxt("adjacency_matrix.txt",adjacency_matrix,delimiter=",", fmt='%f')
  args = parser.parse_args()
  args.model = model_name
  args.data = "./tmp.txt"
  args.sim_mat = adjacency_matrix
  args.save_name = save_name
  args.horizon = int(step_ahead)
  args.epochs = int(epochs)
  
  #print(args);
  if not os.path.exists(args.save_dir):
      os.makedirs(args.save_dir)
  if args.model in ['CNNRNN', 'CNN'] and args.sim_mat is None:
      print('CNNRNN/CNN requires "sim_mat" option')
      sys.exit(0)
  
  args.cuda = args.gpu is not None
  if args.cuda:
      torch.cuda.set_device(args.gpu)
  # Set the random seed manually for reproducibility.
  torch.manual_seed(args.seed)
  if torch.cuda.is_available():
      if not args.cuda:
          print("WARNING: You have a CUDA device, so you should probably run with --cuda")
      else:
          torch.cuda.manual_seed(args.seed)
  
  
  print ("here")
  Data = Data_utility(args);
 
  print('shape of training data is:')
  print(np.shape(Data.train[0]))
  print(np.shape(Data.train[1]))
  model = eval(args.model).Model(args, Data);
  print('model:', model)
  if args.cuda:
      model.cuda()
  
  nParams = sum([p.nelement() for p in model.parameters()])
  print('* number of parameters: %d' % nParams)
  
  criterion = nn.MSELoss(size_average=False);
  evaluateL2 = nn.MSELoss(size_average=False);
  evaluateL1 = nn.L1Loss(size_average=False)
  if args.cuda:
      criterion = criterion.cuda()
      evaluateL1 = evaluateL1.cuda();
      evaluateL2 = evaluateL2.cuda();
  
  
  best_val = 10000000;
  optim = Optim.Optim(
      model.parameters(), args.optim, args.lr, args.clip, weight_decay = args.weight_decay,
  )
  
  # At any point you can hit Ctrl + C to break out of training early.
  #model_path = "base"
 # try:
  print('begin training');
  for epoch in range(1, args.epochs+1):
    epoch_start_time = time.time()
    train_loss = train(Data, Data.train, model, criterion, optim, args.batch_size)
    val_loss, val_rae, val_corr = evaluate(Data, Data.train, model, evaluateL2, evaluateL1, args.batch_size);
    print('| end of epoch {:3d} | time: {:5.2f}s | train_loss {:5.8f} | valid rse {:5.4f} | valid rae {:5.4f} | valid corr  {:5.4f}'.format(epoch, (time.time() - epoch_start_time), train_loss, val_loss, val_rae, val_corr))
    # Save the model if the validation loss is the best we've seen so far.
    if val_loss < best_val:
      best_val = val_loss
      #model_path = '%s/%s.pt' % (args.save_dir, args.save_name)
      model_path = 'best_train_model_%dahead.pt'%step_ahead
      #print (model_path)
      with open(model_path, 'wb') as f:
        torch.save(model.state_dict(), f)
        # test_acc, test_rae, test_corr  = evaluate(Data, Data.test, model, evaluateL2, evaluateL1, args.batch_size);
        # print ("test rse {:5.4f} | test rae {:5.4f} | test corr {:5.4f}".format(test_acc, test_rae, test_corr))
         
  return (train_loss)
  

def mytest(data,adjacency_matrix,model_name,save_name,horizon, test_dat=None ):
  parser = argparse.ArgumentParser(description='Epidemiology Forecasting')
  # --- Data option
  parser.add_argument('--data', type=str, default="./training_data.txt",help='location of the data file')
  # parser.add_argument('--train', type=float, default=0.8,help='how much data used for training')
  # parser.add_argument('--valid', type=float, default=0.2,help='how much data used for validation')
  parser.add_argument('--model', type=str, default='AR',help='model to select')
  # --- CNNRNN option
  parser.add_argument('--sim_mat', type=str,help='file of similarity measurement (Required for CNNRNN, CNN)')
  parser.add_argument('--hidRNN', type=int, default=50, help='number of RNN hidden units')
  parser.add_argument('--residual_window', type=int, default=4,help='The window size of the residual component')
  parser.add_argument('--ratio', type=float, default=1.,help='The ratio between CNNRNN and residual')
  parser.add_argument('--output_fun', type=str, default=None, help='the output function of neural net')
  # --- Logging option
  parser.add_argument('--save_dir', type=str,  default='./save',help='dir path to save the final model')
  parser.add_argument('--save_name', type=str,  default='tmp', help='filename to save the final model')
  # --- Optimization option
  parser.add_argument('--optim', type=str, default='adam', help='optimization method')
  parser.add_argument('--dropout', type=float, default=0.2, help='dropout applied to layers (0 = no dropout)')
  parser.add_argument('--epochs', type=int, default=100,help='upper epoch limit')
  parser.add_argument('--clip', type=float, default=1.,help='gradient clipping')
  parser.add_argument('--lr', type=float, default=0.001, help='learning rate')
  parser.add_argument('--weight_decay', type=float, default=0, help='weight decay (L2 regularization)')
  parser.add_argument('--batch_size', type=int, default=128, metavar='N',help='batch size')
  # --- Misc prediction option
  parser.add_argument('--horizon', type=int, default=1, help='predict horizon')
  parser.add_argument('--window', type=int, default=10 ,help='window size')
  parser.add_argument('--metric', type=int, default=1, help='whether (1) or not (0) normalize rse and rae with global variance/deviation ')
  parser.add_argument('--normalize', type=int, default=0, help='the normalized method used, detail in the utils.py')
  
  parser.add_argument('--seed', type=int, default=54321,help='random seed')
  parser.add_argument('--gpu', type=int, default=None, help='GPU number to use')
  parser.add_argument('--cuda', type=str, default=False, help='use gpu or not')
  
  # np.savetxt("adjacency_matrix",adjacency_matrix,delimiter=",")

  args = parser.parse_args()
  args.horizon = int(horizon)
  args.model = model_name
  args.data = "./tmp.txt"
  args.sim_mat = adjacency_matrix
  args.save_name = save_name
  
  
  print(args);
  if not os.path.exists(args.save_dir):
      os.makedirs(args.save_dir)
  if args.model in ['CNNRNN', 'CNN'] and args.sim_mat is None:
      print('CNNRNN/CNN requires "sim_mat" option')
      sys.exit(0)
  
  args.cuda = args.gpu is not None
  if args.cuda:
      torch.cuda.set_device(args.gpu)
  # Set the random seed manually for reproducibility.
  torch.manual_seed(args.seed)
  if torch.cuda.is_available():
      if not args.cuda:
          print("WARNING: You have a CUDA device, so you should probably run with --cuda")
      else:
          torch.cuda.manual_seed(args.seed)
  
  
  
  Data = Data_utility(args);
  
  
  model = eval(args.model).Model(args, Data);
  print('model:', model)
  if args.cuda:
      model.cuda()
  
  nParams = sum([p.nelement() for p in model.parameters()])
  print('* number of parameters: %d' % nParams)
  
  criterion = nn.MSELoss(size_average=False);
  evaluateL2 = nn.MSELoss(size_average=False);
  evaluateL1 = nn.L1Loss(size_average=False)
  if args.cuda:
      criterion = criterion.cuda()
      evaluateL1 = evaluateL1.cuda();
      evaluateL2 = evaluateL2.cuda();
  
  
  best_val = 10000000;
  optim = Optim.Optim(
      model.parameters(), args.optim, args.lr, args.clip, weight_decay = args.weight_decay,
  )
  
  if (test_dat is None):
      print('Testing requires a test set, exiting ...')
      sys.exit(0)
  else:
    # append test data to train data and batchify according to specified horizon and window
    test_data = Data.test_batchify(test_dat)
    
  # Load the best saved model.
  #model_path = '%s/%s.pt' % (args.save_dir, args.save_name)
  model_path = "best_train_model_%dahead.pt"%horizon
  with open(model_path, 'rb') as f:
      model.load_state_dict(torch.load(f));
  test_acc, test_rae, test_corr  = evaluate(Data, test_data, model, evaluateL2, evaluateL1, args.batch_size);
  print ("test rse {:5.4f} | test rae {:5.4f} | test corr {:5.4f}".format(test_acc, test_rae, test_corr))
  
  forecast =model(Variable(test_data[0]))
  print (np.shape(forecast))
  print (forecast[-1,])
  return forecast[-1,].data.numpy()
