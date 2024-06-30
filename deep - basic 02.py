x = [1,2]
y = [3,4]

x+y

import numpy as np

np.array(x) + np.array(y)

def AND(x1, x2):
    x = np.array([x1, x2])
    w = np.array([0.5, 0.5])
    b = -0.7 # w1x1 + w2x2 - theta = 0
    tmp = np.sum(w*x) + b # b는 theta : threshold
    if tmp <= 0:
        return 0
    else:
        return 1

if __name__ == "__main__": # if __name__ == "__main__": 라인은 모듈이 직접 실행될 때 AND을 실행. 그러나 모듈이 다른 모듈에서 import 되어 사용될 때는 AND가 실행되지 않는다.
    for xs in [(0,0), (1,0), (0,1), (1,1)]:
        y = AND(xs[0], xs[1]) # xs가 (1,0)일때 xs[0] = 1, xs[1] = 0
        print(str(xs) + "->" + str(y)) # str : 정수나 실수를 문자열 형태로 바꿔주는 함수


def NAND(x1, x2):
    x = np.array([x1, x2])
    w = np.array([-0.5, -0.5])
    b = 0.7
    tmp = np.sum(w*x) + b # b는 theta : threshold
    if tmp <= 0:
        return 0
    else:
        return 1

if __name__ == "__main__":
    for xs in [(0,0), (1,0), (0,1), (1,1)]:
        y = NAND(xs[0], xs[1]) # xs가 (1,0)일때 xs[0] = 1, xs[1] = 0
        print(str(xs) + "->" + str(y)) # str : 정수나 실수를 문자열 형태로 바꿔주는 함수


def OR(x1, x2):
    x = np.array([x1, x2])
    w = np.array([0.5, 0.5])
    b = -0.2
    tmp = np.sum(w*x) + b # b는 theta : threshold
    if tmp <= 0:
        return 0
    else:
        return 1

if __name__ == "__main__":
    for xs in [(0,0), (1,0), (0,1), (1,1)]:
        y = OR(xs[0], xs[1]) # xs가 (1,0)일때 xs[0] = 1, xs[1] = 0
        print(str(xs) + "->" + str(y)) # str : 정수나 실수를 문자열 형태로 바꿔주는 함수
        

def XOR(x1, x2):
    s1 = NAND(x1, x2)
    s2 = OR(x1, x2)
    y = AND(s1, s2)
    return y

if __name__ == "__main__":
    for xs in [(0,0), (1,0), (0,1), (1,1)]:
        y = XOR(xs[0], xs[1]) # xs가 (1,0)일때 xs[0] = 1, xs[1] = 0
        print(str(xs) + "->" + str(y)) # str : 정수나 실수를 문자열 형태로 바꿔주는 함수