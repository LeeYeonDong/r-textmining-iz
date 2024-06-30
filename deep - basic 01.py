# 밑바닥 딥러닝 01
1+2

type(10)
type(2.718) # float는 실수
type("hello")

x = 10
y = 3.14

x*y
type(x*y)

# list
a = [1,2,3,4,5]
len(a)

a[0]
a[4] = 99
a

a[0:2]
a[0:]
a[1:]
a[:3]
a[:-1]
a[:-2]

# dictionary: 여러 값을 저장해두고 필요한 값을 꺼내 쓰는 기능 / <이름표>를 이용하여 값을 꺼내 사용
me = {"height": 180}
me["height"]

me["weight"] = 70
me

# bool: True False 두 값 중 하나를 취한다
hungry = True
sleepy = False
not hungry
hungry and sleepy
hungry or sleepy

# if 문
if hungry:
    print("i'm hungry")
    
hungry = False
if hungry:
    print("i'm hungry")
else:
    print("i'm not hungry")
    print("i'm sleepy")    
    
# for 문
for i in [1,2,3]:
    print(i)
    
# 함수
def hello():
    print("Hello World!")

hello()

def hello(object):
    print("Hello"+ " " + object + "!")
    
hello("cat")

# numpy 
import numpy as np

x = np.array([1.0, 2.0, 3.0])
type(x)

y = np.array([2.0, 4.0, 6.0])

x + y
x * y
x - y

x / 2

# n dimension

A = np.array([[1,2], [3,4]])
print(A)

A.shape
A.dtype

B = np.array([[3,0], [0,6]])

A + B
A * B

# broadcast
A = np.array([[1,2], [3,4]])
B = np.array([10, 20])
A * B

X = np.array([[51, 55], [14,19], [0,4]])
X[0]
X[0][1]
X[0][0]

for row in X:
    print(row)

X = X.flatten()
X 
X[np.array([0,2,4])]

# matplotlib
import matplotlib.pyplot as plt
x = np.arange(0, 6, 0.1) # 0에서 6까지 0.1 간격으로 실수 생성
y = np.sin(x)

plt.plot(x, y)
plt.show()

sin = np.sin(x)
cos = np.cos(x)

plt.plot(x, sin)
plt.plot(x, cos)
plt.legend()
plt.show()

plt.plot(x, sin, label = "sin")
plt.plot(x, cos, label = "cos", linestyle = "--")
plt.legend()
plt.show()