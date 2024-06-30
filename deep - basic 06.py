def softmax(x):
    if x.dim == 2: # batch 처리 : 묶음
        x = x.T # T = transpose
        x = x - np.max(x, axis = 0) # axis = 0 열(0)에 대한 최대값
        y = np.exp(x) / np.sum(np.exp(x), axis = 0)
        return y.T
    
    x = x - np.max(X) # 오버플로 대책
    return np.exp(x) / np.sum(np.exp(x))

def softmax(x):
    if x.ndim == 2: # batch 처리 : 묶음
        x = x - np.max(x, axis = 1) 
        y = np.exp(x) / np.sum(np.exp(x), axis = 0)
        return y
    
    x = x - np.max(X) # 오버플로 대책
    return np.exp(x) / np.sum(np.exp(x))

def mean_squared_error(y, t):
    return 0.5 * np.sum((y-t**2)) # ** squared  

def cross_entropy_error(y, t):
    if y.ndim == 1: # if가 없다면 y.dim = 10, y.ndim = 1만 처리하겠다 => batch 처리를 안하겠다 # dim -> ndim으로 대체
        t = t.reshape(1, t.size) # 1 X t.size matrix로 reshape하라. mnist -> t.size = 10
        y = y.reshape(1, y.size)
        
    if t.size == y.size: # lable(t)이 one-hot encoding이 되었다면
        t = t.argmax(axis = 1)
    
    batch_size = y.shape[0]
    return -np.sum(np.log(y[np.arange(batch_size), t])) / batch_size
    
    
# slicing
x = np.arange(12)
x = x.reshape(3,4)
x
x[[1, 2, 0], [3, 2, 0]] # 행렬에서 1행 3열 = 7, 2행 2열 = 10
len(x)
x.ndim == 1


t = [0,0,1,0,0,0,0,0,0,0]
y = [0.1, 0.05, 0.6, 0, 0.05, 0.1, 0.0, 0.1, 0, 0]
np.array(y).ndim
cross_entropy_error(np.array(y), np.array(t))
