# pip install numpy # ctrl + k + c : 주석처리 ctrl + k + u : 주석해제
import numpy as np
# pip install pandas
import pandas as pd
from bertopic import BERTopic
# 필요한 것들을 임포트
from tqdm import tqdm
from sklearn.feature_extraction.text import CountVectorizer
# pip install eunjeon
# pip install mecab
# pip install konlpy
from konlpy.tag import Mecab
from eunjeon import Mecab
m = Mecab()
out = m.morphs(u"Mecab 설치를 확인합니다.")
print(out)

## dynamic topic modeling
import re

# pip install bertopic
# pip install mecab-python3

# 데이터
# 학습을 위한 데이터가 필요합니다. 여기서는 하나의 라인(line)에 하나의 문서로 구성된 파일이 필요한데요. 우선, 여러분들의 데이터가 없다면 여기서 준비한 파일로 실습을 해봅시다.
from sklearn.datasets import fetch_20newsgroups
docs = fetch_20newsgroups(subset='all',  remove=('headers', 'footers', 'quotes'))['data']
docs[:5]