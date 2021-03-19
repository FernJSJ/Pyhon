import numpy as np 
import torch
from collections import Counter
from sklearn import datasets
import torch.nn.functional as Fun 

import pandas as pd
df_data = pd.read_csv("/home/fern/Downloads/ANNData.csv")

X = df_data.drop(['Response'], axis=1)
y = df_data['Response']
input=torch.FloatTensor(X)
label=torch.LongTensor(y)


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

train = torch.utils.data.TensorDataset(x_train, y_train)
test = torch.utils.data.TensorDataset(x_test, y_test)

train_loader = torch.utils.data.DataLoader(train, batch_size=batch_size, shuffle=True)
test_loader = torch.utils.data.DataLoader(test, batch_size=batch_size, shuffle=True)

# 1
class ANNModel(nn.Module):
    def __init__(self, input_dim, hidden_dim, output_dim):
        super(ANNModel, self).__init__()
        self.fc1 = nn.Linear(input_dim, hidden_dim)
        self.relu1 = nn.ReLU()

        self.fc2 = nn.Linear(hidden_dim, hidden_dim)
        self.relu2 = nn.ReLU()

        self.fc3 = nn.Linear(hidden_dim, output_dim)

    def forward(self, x):
        out = self.fc1(x)
        out = self.relu1(out)
        out = self.fc2(out)
        out = self.relu2(out)
        out = self.fc3(out)
        return out

input_dim = 3
hidden_dim = 6
output_dim = 1

model = ANNModel(input_dim, hidden_dim, output_dim)
CrossEntropyLoss = nn.CrossEntropyLoss()

lost_list = []

learning_rate = 0.001
optimizer = torch.optim.Adam(model.parameters(), lr = learning_rate)

# 2
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
optimizer = torch.optim.SGD(net.parameters(), lr=0.5)
loss_func = torch.nn.MSELoss()

loss_func = torch.nn.CrossEntropyLoss()

for i in range(100000):
    out = net(x_train)
    loss = loss_func(out, y_train)
    optimizer.zero_grad()
    loss.backward()
    optimizer.step()

# 3
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
pred_train = prediction_train.data.numpy()
pred_train