import sys, os
sys.path.append("D:\\Not_Pretence\\vs_workspace\\deep-learning-from-scratch-master")
import numpy as np

np.set_printoptions(linewidth = 200, threshold = 1000) # linewidth 터미널 출력 행 길이 개수 설정 / threshold 열 길이

from dataset.mnist import load_mnist

(x_train, y_train),(x_test, y_test) = load_mnist(flatten = False, normalize = False)

print(x_train[0]) # 28 * 28 matrix => image matrix
print(y_train[0])

x_train.ndim

x_train.shape

from PIL import Image

def img_show(img):
    pil_img  = Image.fromarray(np.uint8(img))
    pil_img.show()
    
np.uint8(255)
np.uint8(256)

np.int8(127)
np.int8(128)

(x_train, y_train),(x_test, y_test) = load_mnist(flatten = True, normalize = False)

img = x_train[0]
label = y_train[0]

x = np.arange(12)
x.reshape(3,4)
x.reshape(2,2,3)
    
img = img.reshape(28, 28) # flatten한 행렬을 다시 행렬 모영으로
print(img.shape)
img_show(img)

x = np.array([1,2]) # list
x = np.array([[1,2]]) # 1 * 2 행렬 
x[0] # 행렬 안 list 선택

x = np.array([[1,2],[3,4]])
x-1 
img_show(255-img)


import matplotlib.pyplot as plt

(x_train, y_train),(x_test, y_test) = load_mnist(flatten = False, normalize = False)

plt.figure()
plt.imshow(x_train[0][0])
plt.colorbar()
plt.show()

plt.figure(figsize=(10,10))
for i in range(25):
    plt.subplot(5,5,i+1) # 5x5 이미지 콜렉션에서 1번째
   # plt.xticks([]) # 눈금 / ([]) <- 없애기
   # plt.yticks([])
    plt.imshow(x_train[i][0], cmap = plt.cm.binary) # plt.cm.binary는 흑백(이진) 이미지를 나타내기 위한 색상 맵으로, 0과 1 사이의 값을 흑백으로 표현
    plt.xlabel(y_train[i])
plt.show()
