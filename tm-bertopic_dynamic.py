import numpy as np
print(np.__version__)

# pip install bertopic
# pip install mecab-python3

# 데이터
# 학습을 위한 데이터가 필요합니다. 여기서는 하나의 라인(line)에 하나의 문서로 구성된 파일이 필요한데요. 우선, 여러분들의 데이터가 없다면 여기서 준비한 파일로 실습을 해봅시다.
from sklearn.datasets import fetch_20newsgroups
docs = fetch_20newsgroups(subset='all',  remove=('headers', 'footers', 'quotes'))['data']
docs[:5]

# 필요한 것들을 임포트
from tqdm import tqdm
from sklearn.feature_extraction.text import CountVectorizer
# pip install konlpy
from konlpy.tag import Mecab
from eunjeon import Mecab
m = Mecab()
out = m.morphs(u"Mecab 설치를 확인합니다.")
print(out)
from bertopic import BERTopic

# rawdata
with open(r'D:\대학원\논문\bert\2016-10-20.txt', 'r', encoding="utf-8") as file:
    text_file = file.read()
    
# 전처리
documents = [line.strip() for line in text_file.split('\n')]
preprocessed_documents = []
for line in tqdm(documents):
  # 빈 문자열이거나 숫자로만 이루어진 줄은 제외
  if line and not line.replace(' ', '').isdecimal():
    preprocessed_documents.append(line)
    
preprocessed_documents[:5]

# Mecab과 SBERT를 이용한 Bertopic
class CustomTokenizer:
    def __init__(self, tagger):
        self.tagger = tagger
    def __call__(self, sent):
        sent = sent[:1000000]
        word_tokens = self.tagger.morphs(sent)
        result = [word for word in word_tokens if len(word) > 1]
        return result

custom_tokenizer = CustomTokenizer(Mecab())
vectorizer = CountVectorizer(tokenizer=custom_tokenizer, max_features=3000)
model = BERTopic(embedding_model="sentence-transformers/xlm-r-100langs-bert-base-nli-stsb-mean-tokens", \
                 vectorizer_model=vectorizer,
                 nr_topics=50,
                 top_n_words=10,
                 calculate_probabilities=True)
topics, probs = model.fit_transform(preprocessed_documents)
model.visualize_topics()