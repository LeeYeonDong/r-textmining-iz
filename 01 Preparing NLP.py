## package
# pip install gensim
# pip install keras
# pip install tensorflow-gpu
# pip install scikit-learn
# pip install nltk
# pip install dplython
# pip install pandas-profiling
# vs에서 실행은 alt + enter

import nltk
nltk.download('treebank')
# pip install konlpy

# dplython
import pandas as pd
from dplython import (DplyFrame, X, diamonds, select, sift, sample_n, sample_frac, 
                      head, arrange, mutate, group_by,summarize, DelayFunction)
values = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
index = ['one', 'two', 'three']
columns = ['A', 'B', 'C']
df = DplyFrame(values, index=index, columns=columns)
df
df >> select(X.A)

# pip install numpy
import numpy as np
# pip install pandas
import pandas as pd

# youtube data loading
df_ytb1 = pd.read_csv("D:/대학원/논문/커뮤니케이션학과/유튜브.csv", encoding = "utf-8")
print(df_ytb1.index)
# pip install pandas-profiling
import pandas_profiling
df_ytb1.profile_report()
