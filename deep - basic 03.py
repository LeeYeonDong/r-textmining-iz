# 

import numpy as np
import matplotlib.pylab as plt
import inspect

# 
def identity_function(x):
    return x

def step_function(x):
    return np.array(x > 0, dtype = np.int) # T/F를 np.int(정수)로 반환하라

def step_function(x): # vector를 input했을 때 에러 난다
    if x > 0:
        return 1
    else:
        return 0 

x = np.array([-1, 1, 2])
step_function(x)
x>0

np.arange(5) # 파이썬은 언제나 미만
np.arange(-5,5)
np.arange(-5, 5, 0.1)

help(np.arange)
x = np.arange(start = -5, stop = 5, step = 0.1)
y = step_function(x)
plt.plot(x,y)
plt.ylim(-0.1, 1.1)
plt.show()


def sigmoid(x):
    return 1/(1+np.exp(-x))

x = np.arange(start = -5, stop = 5, step = 0.1)
y1 = sigmoid(x)
y2 = step_function(x)
help(plt.plot)
plt.plot(x,y1)
plt.plot(x,y2, 'k--')
plt.ylim(-0.1, 1.1)
plt.show()

x = np.array([1, -1, -1, 1, 1])
y = np.array([1, 1, -1, -1, 1])
plt.plot(x,y, 'k--')
plt.show()

plt.plot(x,y, 'ro')
plt.show()


def relu(x):
    return np.maximum(0, x)

x = np.arange(start = -5, stop = 5, step = 0.1) 
y = relu(x)
plt.plot(x, y)
plt.ylim(-1.0, 5.5)
plt.show()