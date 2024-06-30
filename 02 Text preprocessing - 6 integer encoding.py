# pip install numpy
import numpy as np
# pip install pandas
import pandas as pd

from nltk import sent_tokenize
from nltk import word_tokenize
from nltk.corpus import stopwords

## loading data
df_ytb1 = pd.read_csv("D:/대학원/논문/커뮤니케이션학과/유튜브.csv", encoding = "utf-8")

# lower character + 특수문자 지우기
df_ytb1["댓글_ytb"] = df_ytb1["댓글_ytb"].apply(str.lower).str.replace(pat = r"[^\w]", repl = " ", regex = True)

# loading packages
from nltk import word_tokenize
from nltk import WordPunctTokenizer
from tensorflow.keras.preprocessing.text import text_to_word_sequence

# 케라스(Keras)의 텍스트 전처리
from tensorflow.keras.preprocessing.text import Tokenizer

def sen_word_tokenize(text):
    sents = sent_tokenize(text)
    words = [word_tokenize(sent_1) for sent_1 in sents]
    words = [[word for word in sent if len(word) > 2] for sent in words]
    return words

df_ytb1["sen_word_ytb"] = df_ytb1["댓글_ytb"].apply(sen_word_tokenize)

tokenizer = Tokenizer()
tokenizer.fit_on_texts(df_ytb1["댓글_ytb"])

# fit_on_texts()안에 코퍼스를 입력으로 하면 빈도수를 기준으로 단어 집합을 생성.
df_ytb1["encoded_ytb"] = df_ytb1["sen_word_ytb"].apply(lambda word: tokenizer.texts_to_sequences(word))

df_ytb1["encoded_ytb"] = df_ytb1["encoded_ytb"].apply(lambda encoded: [x for x in encoded if len(x) >= 5])
# remove value if length of index is less than 5

df_ytb1 = df_ytb1.dropna(axis = 0) # To remove rows that contain NA values