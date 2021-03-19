import torch
import numpy as np 
import torch.nn as nn
from torch.autograd import Variable
import pandas as pd 
from sklearn.model_selection import train_test_split
import matplotlib.pyplot as plt

train = pd.read_csv("/home/fern/Downloads/ANNData.csv", dtype=np.float32)

x = train.drop(['Response'], axis=1)
y = train['Response']

x_train, x_test, y_train, y_test = train_test_split(x, y, test_size=0.2, random_state=2021)

x_train = torch.from_numpy(x_train.values)
y_train = torch.from_numpy(y_train.values).type(torch.LongTensor)

x_test = torch.from_numpy(x_test.values)
y_test = torch.from_numpy(y_test.values).type(torch.LongTensor)

batch_size = 2
iteration_num = 100

class ANNNet(torch.nn.Module):
    def __init__(self, n_input, n_hidden, n_output):
        super(ANNNet, self).__init__()
        self.hidden = torch.nn.Linear(n_input, n_hidden)
        self.out = torch.nn.Linear(n_hidden, n_output)

    def forward(self, x):
        x = nn.functional.relu(self.hidden(x))
        x = self.out(x)
        return x

net = ANNNet(n_input=3, n_hidden=6, n_output=1)
print(net)
optimizer = torch.optim.Adam(net.parameters(),lr=0.8)
loss_func = torch.nn.MSELoss()

for i in range(1000):
    prediction_train = net(x_train)
    prediction_train = prediction_train.to(torch.float32)
    loss_train = loss_func(prediction_train, y_train)
    
    prediction_test = net(x_test)
    prediction_test = prediction_test.to(torch.float32)
    loss_train = loss_func(prediction_test, y_test)
    
    optimizer.zero_grad()
    loss.backward()
    optimizer.step()
    
    if i % 5 == 0:
        print('Epoch %d, Training loss %.4f, Varidation loss %.4f' % (i, float(loss_train), float(loss_test))
prediction_train = net(x_train)
prediction_test = net(x_test)
