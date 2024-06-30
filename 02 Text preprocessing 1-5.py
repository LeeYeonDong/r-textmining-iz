# pip install numpy # ctrl + k + c : 주석처리 ctrl + k + u : 주석해제
import numpy as np
# pip install pandas
import pandas as pd
# dplython
from dplython import (DplyFrame, X, diamonds, select, sift,
  sample_n, sample_frac, head, arrange, mutate, group_by,
  summarize, DelayFunction)

## loading data
df_ytb1 = pd.read_csv("D:/대학원/논문/커뮤니케이션학과/유튜브.csv", encoding = "utf-8")

# lower character + 특수문자 지우기
df_ytb1["댓글_ytb"] = df_ytb1["댓글_ytb"].apply(str.lower).str.replace(pat = r'[^\w]', repl = ' ', regex = True)

print(df_ytb1.columns)
print(df_ytb1.values)
print(df_ytb1.shape)
print(df_ytb1)
df_ytb1.describe()
df_ytb1.info() # 데이터프레임 여부 확인


# loading packages
from nltk import word_tokenize
from nltk import WordPunctTokenizer
from tensorflow.keras.preprocessing.text import text_to_word_sequence


# 토큰화
# word_tokenize
df_ytb1["댓글_ytb"] # 열 선택
df_ytb1["댓글_ytb"].info()

df_ytb1["token_ytb"] = df_ytb1.apply(lambda row: word_tokenize(row['댓글_ytb']), axis = 1) # bad

df_ytb1["token_ytb"] = df_ytb1["댓글_ytb"].apply(word_tokenize)
df_ytb1.describe()
df_ytb1.info()


# wordPunctTokenizer
df_ytb1.apply(lambda row: WordPunctTokenizer().tokenize(row['댓글_ytb']), axis = 1)

df_ytb1["댓글_ytb"].apply(WordPunctTokenizer().tokenize)


# 케라스의 text_to_word_sequence는 기본적으로 모든 알파벳을 소문자로 바꾸면서 마침표나 컴마, 느낌표 등의 구두점을 제거
df_ytb1.apply(lambda row: text_to_word_sequence(row['댓글_ytb']), axis = 1)

df_ytb1["댓글_ytb"].apply(text_to_word_sequence)


# Penn Treebank Tokenization - 표준으로 쓰이는 방법
from nltk import TreebankWordTokenizer
df_ytb1.apply(lambda row: TreebankWordTokenizer.tokenize(row['댓글_ytb']), axis = 1)

df_ytb1["댓글_ytb"].apply(TreebankWordTokenizer().tokenize)


# 문장 토큰화(Sentence Tokenization)
from nltk.tokenize import sent_tokenize

df_ytb1["댓글_ytb"].info()
df_ytb1["댓글_ytb"].apply(sent_tokenize) # data frame에서 sent_tokenize

" ".join(df_ytb1["댓글_ytb"]) # series 한꺼번에 합치기
sent_tokenize(" ".join(df_ytb1["댓글_ytb"]))


# 문장 토큰화(Sentence Tokenization) - 한국어
# pip install kss
import kss
text = '딥 러닝 자연어 처리가 재미있기는 합니다. 그런데 문제는 영어보다 한국어로 할 때 너무 어렵습니다. 이제 해보면 알걸요?'
kss.split_sentences(text)


# 품사 태깅(Part-of-speech tagging)
# NLTK와 KoNLPy를 이용한 영어, 한국어 토큰화 실습
from nltk import word_tokenize
from nltk import pos_tag

df_ytb1["댓글_ytb_tag"] = df_ytb1["댓글_ytb"].apply(word_tokenize).apply(pos_tag)
댓글_ytb_tag.info()


# 한국어 NLP에서 형태소 분석기를 사용하여 단어 토큰화. 더 정확히는 형태소 토큰화(morpheme tokenization)
from konlpy.tag import Okt
from konlpy.tag import Kkma

Okt().morphs("열심히 코딩한 당신, 연휴에는 여행을 가봐요")
Okt().pos("열심히 코딩한 당신, 연휴에는 여행을 가봐요")
Okt().nouns("열심히 코딩한 당신, 연휴에는 여행을 가봐요")


# 표제어 추출(Lemmatization)
from nltk.stem import WordNetLemmatizer

df_ytb1["token_ytb"] = df_ytb1["댓글_ytb"].apply(word_tokenize)
df_ytb1["token_ytb"].info()
df_ytb1.info()

def lemmatizer(text):
    return [WordNetLemmatizer().lemmatize(word) for word in text]

df_ytb1["lemma_ytb"] = df_ytb1["token_ytb"].apply(lemmatizer)

# 어간 추출(Stemming)
from nltk import PorterStemmer
from nltk import word_tokenize

def stemmer(text):
    return [PorterStemmer().stem(word) for word in text]
  
df_ytb1["stemm_ytb"] = df_ytb1["token_ytb"].apply(stemmer)

from nltk.stem import LancasterStemmer

def lac_stemmer(text):
  return [LancasterStemmer().stem(word) for word in text]

df_ytb1["token_ytb"].apply(lac_stemmer)


# 불용어(Stopword)
from nltk.corpus import stopwords
from nltk import word_tokenize 
from konlpy.tag import Okt

stop_words = stopwords.words("english")
stop_words = set(stop_words)
len(stop_words)
stop_words
stop_words.info()
stop_words.__class__ # class -> set 확인
df_ytb1["token_ytb"].info()

df_ytb1["token_ytb"] = df_ytb1["댓글_ytb"].apply(word_tokenize)

df_ytb1["token_ytb"] = df_ytb1["token_ytb"].apply(lambda i: [word for word in i if word not in stop_words])
df_ytb1 = df_ytb1.drop(columns = ["댓글_ytb_tag"], axis = 1)






