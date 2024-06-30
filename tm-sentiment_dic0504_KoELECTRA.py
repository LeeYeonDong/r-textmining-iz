# library import
import tensorflow as tf
from tensorflow.keras.preprocessing.text import Tokenizer
from tensorflow.keras.preprocessing.sequence import pad_sequences
import numpy as np
from collections import Counter
import pandas as pd
from sklearn.model_selection import GridSearchCV
from tensorflow.keras.wrappers.scikit_learn import KerasClassifier
from tensorflow.keras.callbacks import EarlyStopping
from sklearn.model_selection import train_test_split
from tensorflow.keras.layers import Dense, Dropout, Embedding, LSTM, Bidirectional
from tensorflow.keras.regularizers import l1, l2
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Embedding, Bidirectional, LSTM, Dropout, Dense
from tensorflow.keras.optimizers import Adam
from scipy.stats import beta
from konlpy.tag import Okt
import pandas as pd
from transformers import AutoTokenizer, AutoModelForSequenceClassification, pipeline
import re


# example
df_12 = pd.read_excel('D:/대학원/논문/소논문/부동산_감정사전/부동산_본문_추가_2012_df.xlsx', engine='openpyxl')
df_13 = pd.read_excel('D:/대학원/논문/소논문/부동산_감정사전/부동산_본문_추가_2013_df.xlsx', engine='openpyxl')
df_14 = pd.read_excel('D:/대학원/논문/소논문/부동산_감정사전/부동산_본문_추가_2014_df.xlsx', engine='openpyxl')
df_15 = pd.read_excel('D:/대학원/논문/소논문/부동산_감정사전/부동산_본문_추가_2015_df.xlsx', engine='openpyxl')
df_16 = pd.read_excel('D:/대학원/논문/소논문/부동산_감정사전/부동산_본문_추가_2016_df.xlsx', engine='openpyxl')
df_17 = pd.read_excel('D:/대학원/논문/소논문/부동산_감정사전/부동산_본문_추가_2017_df.xlsx', engine='openpyxl')
df_18 = pd.read_excel('D:/대학원/논문/소논문/부동산_감정사전/부동산_본문_추가_2018_df.xlsx', engine='openpyxl')
df_19 = pd.read_excel('D:/대학원/논문/소논문/부동산_감정사전/부동산_본문_추가_2019_df.xlsx', engine='openpyxl')
df_20 = pd.read_excel('D:/대학원/논문/소논문/부동산_감정사전/부동산_본문_추가_2020_df.xlsx', engine='openpyxl')
df_21 = pd.read_excel('D:/대학원/논문/소논문/부동산_감정사전/부동산_본문_추가_2021_df.xlsx', engine='openpyxl')
df_22 = pd.read_excel('D:/대학원/논문/소논문/부동산_감정사전/부동산_본문_추가_2022_df.xlsx', engine='openpyxl')
df_23 = pd.read_excel('D:/대학원/논문/소논문/부동산_감정사전/부동산_본문_추가_2023_df.xlsx', engine='openpyxl')
df = pd.concat([df_12, df_13, df_14, df_15, df_16, df_17, df_18, df_19, df_20, df_21, df_22, df_23], ignore_index=True)
# df = pd.read_excel('D:/대학원/논문/소논문/부동산_감정사전/부동산_본문_추가_2023_df.xlsx', engine='openpyxl')
df = df.drop(columns=['링크.1'])
# df = df.sample(frac=0.01)

# 모델과 토크나이저 로드
tokenizer = AutoTokenizer.from_pretrained("monologg/koelectra-small-v3-discriminator")
model = AutoModelForSequenceClassification.from_pretrained("monologg/koelectra-small-v3-discriminator")
nlp = pipeline('sentiment-analysis', model=model, tokenizer=tokenizer)

# 전처리 함수 정의
def preprocess_text(text):
    # 특수기호로 둘러싸인 텍스트 제거
    text = re.sub(r'\[.*?\]', '', text)  # 대괄호 안의 텍스트 제거
    text = re.sub(r'\(.*?\)', '', text)  # 괄호 안의 텍스트 제거
    text = re.sub(r'\{.*?\}', '', text)  # 중괄호 안의 텍스트 제거
    text = re.sub(r'<.*?>', '', text)    # <> 태그 안의 텍스트 제거
    # 이메일, URL, 숫자 등 제거
    text = re.sub(r'\S*@\S*\s?', '', text)  # 이메일 주소 제거
    text = re.sub(r'http\S+', '', text)     # URL 제거
    text = re.sub(r'\d+', '', text)         # 숫자 제거
    # 공백 정리
    text = re.sub(r'\s+', ' ', text).strip()  # 공백 정리
    return text

# Okt 형태소 분석기 초기화
okt = Okt()
def tokenize(text):
    # 전처리된 텍스트를 토큰화
    return okt.morphs(text)

def analyze_sentiment(text):
    # 텍스트 전처리
    processed_text = preprocess_text(text)
    if not processed_text:  # 전처리 후 텍스트가 비어 있는 경우
        print("전처리 후 텍스트가 비어 있습니다. 중립과 점수 0.5를 반환합니다.")
        return 'Neutral', 0.5, 0.5, 0.5  # 중립으로 처리하고 모든 점수를 0.5로 설정

    # 텍스트를 512 토큰 단위로 분할
    parts = [processed_text[i:i+512] for i in range(0, len(processed_text), 512)]
    print(f"분석을 위해 텍스트를 {len(parts)} 부분으로 나누었습니다.")

    # 각 부분에 대한 감정 분석 수행 및 결과 수집
    sentiments = []
    scores = []
    for part in parts:
        result = nlp(part)[0]
        sentiments.append(result['label'])
        scores.append(result['score'])
        print(f"부분 감정: {result['label']}, 점수: {result['score']}")

    # 가장 흔한 감정 결정 및 최소, 최대 점수 계산
    if sentiments:
        sentiment_counts = Counter(sentiments)
        most_common_sentiment = max((s for s in sentiments if s != 'Neutral'), key=lambda x: sentiment_counts[x], default='Neutral')
        min_score = min(scores)
        max_score = max(scores)
        average_score = sum(scores) / len(scores)  # 평균 점수 계산
        print(f"가장 흔한 감정: {most_common_sentiment}, 최소 점수: {min_score}, 최대 점수: {max_score}, 평균 점수: {average_score}")
        sentiment = "Positive" if most_common_sentiment == "LABEL_1" else "Negative" if most_common_sentiment == "LABEL_0" else "Neutral"
        return sentiment, min_score, max_score, average_score
    else:
        return 'Neutral', 0.5, 0.5, 0.5  # 텍스트가 비어 있으면 중립으로 처리하고 모든 점수를 0.5로 설정


# 텍스트를 전처리, 토큰화하고 감정 분석을 수행하는 함수
def process_text_and_analyze_sentiment(text):
    # 텍스트 전처리
    processed_text = preprocess_text(text)
    # 텍스트 토큰화
    tokens = tokenize(processed_text)
    # 토큰 다시 조합하여 분석 준비
    prepared_text = ' '.join(tokens)
    # 감정 분석
    sentiment, score = analyze_sentiment(prepared_text)
    return sentiment, score

# DataFrame의 '본문' 열에 함수 적용
df['sentiment'], df['min_score'], df['max_score'], df['average_score'] = zip(*df['본문'].apply(analyze_sentiment))

df_filtered = df[['제목', '날짜', 'sentiment', 'min_score', 'max_score', 'average_score']]

file_path = "D:\\대학원\\논문\\소논문\\부동산_감정사전\\KoELECTRA0504.csv"
df_filtered.to_csv(file_path, index=False, encoding='utf-8-sig')