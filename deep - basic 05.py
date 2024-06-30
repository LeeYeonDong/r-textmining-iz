# forward
import numpy as np

def sigmoid(x):
    return 1 / (1 + np.exp(-x))

def identity_function(x):
    return x

def init_network():
    network = {}
    network["W1"] = np.array([[0.1, 0.3, 0.5], [0.2, 0.4, 0.6]])
    network["b1"] = np.array([0.1, 0.2, 0.3])
    network["W2"] = np.array([[0.1, 0.4], [0.2, 0.5], [0.3, 0.6]])
    network["b2"] = np.array([0.1, 0.2])
    network["W3"] = np.array([[0.1, 0.3], [0.2, 0.4]])
    network["b3"] = np.array([0.1, 0.2])
    
    return network

def forward(network, x):
    w1, w2, w3 = network["W1"], network["W2"], network["W3"]
    b1, b2, b3 = network["b1"], network["b2"], network["b3"]
    
    a1 = np.dot(x, w1) + b1 # np.dot : dot product
    z1 = sigmoid(a1)
    a2 = np.dot(z1, w2) + b2
    z2 = sigmoid(a2)
    a3 = np.dot(z2, w3) + b3
    y = identity_function(a3)
    
    return y

network = init_network()
x = np.array([1.0, 0.5])
y = forward(network, x)
print(y)
    
    
# neuralnet_mnist
import sys, os
sys.path.append("D:\\Not_Pretence\\vs_workspace\\deep-learning-from-scratch-master")
import numpy as np
import pickle # text 데이터를 효율적으로 저장하기 위한 library
from dataset.mnist import load_mnist
from common.functions import sigmoid, softmax

def get_data():
    (x_train, y_train) , (x_test, y_test) = load_mnist(normalize = True, flatten = True, 
                                                       one_hot_label = False)
    return x_test, y_test

def init_network():
    with open("D:\\Not_Pretence\\vs_workspace\\deep-learning-from-scratch-master\\ch03\\sample_weight.pkl", "rb") as f:
        network = pickle.load(f)
    return network

def predict(network, x):
    w1, w2, w3 = network["W1"], network["W2"], network["W3"] # 대소문자 주의 sample_weight의 변수는 대문자(W1, W2,...)
    b1, b2, b3 = network["b1"], network["b2"], network["b3"]
    
    a1 = np.dot(x, w1) + b1 # np.dot : dot product
    z1 = sigmoid(a1)
    a2 = np.dot(z1, w2) + b2
    z2 = sigmoid(a2)
    a3 = np.dot(z2, w3) + b3
    y = softmax(a3)
    
    return y

x, y = get_data() # test data : 10000장
network = init_network()
accuracy_cnt = 0
for i in range(len(x)):
    pred = predict(network, x[i]) # ith 이미지 가져오기
    p = np.argmax(pred) # 확률이 가장 높은 원소의 인덱스를 얻는다
    if p == y[i]: # nn으로 예측한 숫자와 실제 labeling한 값이 일치하는지 확인
        accuracy_cnt += 1 # 10000장 이미지를 얼마나 맞추었나

np.argmax(np.array([1,3,2])) # 최대값 3의 index를 반환

float(accuracy_cnt) / len(x)


# pickle nnet structure
import sys, os
sys.path.append("D:\\Not_Pretence\\vs_workspace\\deep-learning-from-scratch-master")
import numpy as np
import pickle # text 데이터를 효율적으로 저장하기 위한 library
from dataset.mnist import load_mnist
np.set_printoptions(linewidth=1000, threshold=100000) # linewidth 터미널 출력 행 길이 개수 설정 / threshold 열 길이

with open("D:\\Not_Pretence\\vs_workspace\\deep-learning-from-scratch-master\\ch03\\sample_weight.pkl", "rb") as f:
        network = pickle.load(f)
        
w1, w2, w3 = network["W1"], network["W2"], network["W3"] # 대소문자 주의 sample_weight의 변수는 대문자(W1, W2,...)
b1, b2, b3 = network["b1"], network["b2"], network["b3"]

print(w1)
print(type(network))
print(network.keys())
str(w1.shape)
str(w2.shape)
str(w3.shape)
str(b1.shape)
str(b2.shape)
str(b3.shape)

# nnet_mnist_batch
# neuralnet_mnist와 앞부분 동일
batch_size = 100
accuracy_cnt = 0

for i in np.arange(start = 0, stop = len(x), step = batch_size):
    pred_batch = predict(network, x[i:i+batch_size]) # ith 이미지 가져오기
    p = np.argmax(pred_batch, axis = 1) # 확률이 가장 높은 원소의 인덱스를 얻는다
    accuracy_cnt += np.sum(p == y[i:i+batch_size])

float(accuracy_cnt) / len(x)

# nnet_mnist_error
# neuralnet_mnist와 앞부분 동일
error = []
answer = []
for i in np.arange(start = 0, stop = len(x)):
    pred = predict(network, x[i])
    p = np.argmax(pred)
    if p != y[i]:
        error.append(i)
        answer.append(p)
        
len(error) / len(x)

import matplotlib.pyplot as plt

plt.figure(figsize = (10,40)) # 가로 세로 길이
for i in np.arange(stop = 100):
    plt.subplot(20, 5, i+1) # grid nrows, ncols, index
    plt.xticks([]) # x축 눈금 없애기
    plt.yticks([])
    plt.imshow(x[error[i]].reshape(28,28), cmap = plt.cm.binary) # plt.cm.binary : 흑백
    plt.xlabel(str(y[error[i]]) + "==>" + str(answer[i])) # 가로 라벨
plt.show()

# prob > 0.8 이상 예상할때만 
accuracy_cnt = 0
for i in np.arange(stop = len(x)):
    pred = predict(network, x[i])
    p = np.argmax(pred)
    if (p == y[i]) & (pred[p] > 0.8):
        accuracy_cnt += 1 
                
accuracy_cnt / len(x)


## 60000장 중 랜덤하게 25장 뽑기
# def get_data():
#    (x_train, y_train) , (x_test, y_test) = load_mnist(normalize = True, flatten = True, 
#                                                       one_hot_label = False)
#    return x_test, y_test // plt.figure가 안됨

(x_train, y_train),(x_test, y_test) = load_mnist(flatten = False, normalize = False)

plt.figure()
plt.imshow(x_train[0][0])
plt.colorbar()
plt.show()

I = np.random.choice(60000,25)

plt.figure(figsize = (10,10)) # 가로 세로 길이
for i in np.arange(stop = 25):
    plt.subplot(5, 5, i+1) # grid nrows, ncols, index
    plt.xticks([]) # x축 눈금 없애기
    plt.yticks([])
    plt.imshow(x_train[I[i]][0], cmap = plt.cm.binary) # plt.cm.binary : 흑백
    plt.xlabel(y_train[I[i]]) # 가로 라벨
plt.show()

# confusion matrix (10 X 10)
def get_data():
    (x_train, y_train) , (x_test, y_test) = load_mnist(normalize = True, flatten = True, one_hot_label = False)
    return x_test, y_test 

x, y = get_data() # test data : 10000장
network = init_network()

confusion = np.zeros((10,10), dtype=int)

for i in np.arange(len(x)):
    j = int(y[i])
    pred = predict(network, x[i])
    k = np.argmax(pred)
    confusion[j][k] += 1
print(confusion)
# [row(예측값), col(실제값)] ---> confusion matrix
    