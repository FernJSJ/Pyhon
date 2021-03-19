import matplotlib.pyplot as plt 
import seaborn as sns 
sns.set(style="darkgrid") # 风格

vent_time = df_data.vent_time
vent_pressure = df_data.vent_pressure
purge_time = df_data.purge_time
Response = df_data.Response

ax = sns.scatterplot(x="vent_time", y="Response", data=df_data)

X = df_data.drop(['Response'], axis=1)
y = df_data['Response']
X.head()


from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=0)

from sklearn.linear_model import LogisticRegression
lr = LogisticRegression()
history = lr.fit(X_train, y_train)
print("逻辑回归预测准确率 {:.2f}%".format(lr.score(X_test, y_test)*100))

import keras
from keras.models import Sequential
from keras.layers import Dense
ann = Sequential()
ann.add(Dense(units=3, input_dim=2, activation='relu'))
ann.add(Dense(units=6, activation='relu'))
ann.add(Dense(units=1, activation='sigmoid'))
ann.summary()

from IPython.display import SVG
from keras.utils.vis_utils import model_to_dot
SVG(model_to_dot(ann,show_shapes = True ).create(prog='dot', format='svg'))


ann.compile(optimizer='adam', loss='binary_crossentory', metrics=['acc'])

history = ann.fit(X_train, y_train,epoch=30, batch_size=4, validation_data=(X_test, y_test))

history = ann.fit(X_train, y_train, # 指定训练集
                  epochs=30,        # 指定训练的轮次
                  batch_size=4,    # 指定数据批量
                  validation_data=(X_test, y_test))