# library import
import tensorflow as tf
from tensorflow.keras.preprocessing.text import Tokenizer
from tensorflow.keras.preprocessing.sequence import pad_sequences
import numpy as np
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


# example
df_19 = pd.read_excel('D:/대학원/논문/소논문/부동산_감정사전/부동산_본문_추가_2019_df.xlsx', engine='openpyxl')
df_20 = pd.read_excel('D:/대학원/논문/소논문/부동산_감정사전/부동산_본문_추가_2020_df.xlsx', engine='openpyxl')
df_21 = pd.read_excel('D:/대학원/논문/소논문/부동산_감정사전/부동산_본문_추가_2021_df.xlsx', engine='openpyxl')
df_22 = pd.read_excel('D:/대학원/논문/소논문/부동산_감정사전/부동산_본문_추가_2022_df.xlsx', engine='openpyxl')
df_23 = pd.read_excel('D:/대학원/논문/소논문/부동산_감정사전/부동산_본문_추가_2023_df.xlsx', engine='openpyxl')
df = pd.concat([df_19, df_20, df_21, df_22, df_23], ignore_index=True)
df = df.drop(columns=['링크.1'])

df = df.sample(frac=0.001)

# Hugging Face의 transformers 라이브러리와 pipeline 기능을 사용하여 한국어 감정 분석을 수행하려면, 우선적으로 한국어를 지원하는 사전 훈련된 모델을 선택해야 합니다. 
from transformers import pipeline, AutoTokenizer, AutoModelForSequenceClassification

# KoELECTRA 모델과 토크나이저를 불러옵니다.
tokenizer = AutoTokenizer.from_pretrained("monologg/koelectra-small-v3-discriminator")
model = AutoModelForSequenceClassification.from_pretrained("monologg/koelectra-small-v3-discriminator")

# 사전 훈련된 감정 분석 파이프라인을 초기화합니다.
nlp = pipeline('sentiment-analysis', model=model, tokenizer=tokenizer)

# 특수문자로 시작하는 단어 제거 ([, {, <, @)
df['본문'] = df['본문'].str.replace(r'\[[^\]]*\]', '', regex=True)  # 대괄호 안의 내용 삭제
df['본문'] = df['본문'].str.replace(r'\{[^\}]*\}', '', regex=True)  # 중괄호 안의 내용 삭제
df['본문'] = df['본문'].str.replace(r'<[^>]*>', '', regex=True)    # 꺽쇠 괄호 안의 내용 삭제
df['본문'] = df['본문'].str.replace(r'@[^\s]+', '', regex=True)    # @로 시작하는 단어 삭제

# 특수 문자 제거 (문장부호 제외)
df['본문'] = df['본문'].str.replace(r'[^\w\s,.?!\'"]', '', regex=True)

# 한 글자 단어 제거
df['본문'] = df['본문'].str.replace(r'\b\w\b', '', regex=True)

# 불필요한 공백 제거
df['본문'] = df['본문'].str.replace(r'\s+', ' ', regex=True).str.strip()

#  긴 텍스트 처리 전략: 긴 텍스트를 512 토큰씩 나누고 각 부분에 대해 감정 분석을 수행하여 가장 빈번한 감정을 결과로 선택합니다. 이 방법은 텍스트의 전체적인 감정을 평가하는 데 유용할 수 있습니다.
def analyze_sentiment(text):
    if text.strip() == "":
        print("Empty text provided, returning Neutral with score 0.")
        return 'Neutral', 0.0  # 빈 텍스트는 'Neutral'과 점수 0으로 처리
    
    # 텍스트를 512 토큰 단위로 분할
    parts = [text[i:i+512] for i in range(0, len(text), 512)]
    print(f"Text divided into {len(parts)} parts.")  # 분할된 파트 수 출력

    # 각 파트에 대한 감정 분석 수행
    results = [nlp(part) for part in parts]
    
    # 결과 출력 및 평균 score 계산
    scores = [result[0]['score'] for result in results if result]
    average_score = np.mean(scores) if scores else 0.0  # scores가 비어있지 않다면 평균 계산, 그렇지 않으면 0
    print(f"Average sentiment score: {average_score}")  # 평균 점수 출력

    # 가장 흔한 감정과 평균 점수 반환
    most_common_sentiment = max(set(result[0]['label'] for result in results if result), key=lambda x: [result[0]['label'] for result in results if result].count(x))
    return most_common_sentiment, average_score

# 데이터 프레임의 '본문' 열에 감정 분석 함수 적용
df['sentiment'], df['average_score'] = zip(*df['본문'].apply(analyze_sentiment))

# 스케일 조정: 평균을 0으로, 표준편차를 1로 조정
df['scaled_score'] = (df['average_score'] - df['average_score'].mean()) / df['average_score'].std()
# 로지스틱 함수 적용
df['logistic_score'] = 1 / (1 + np.exp(-df['scaled_score']))

# # 베타 분포 매개변수
# a, b = 0.5, 0.5
# # 데이터를 베타 분포 CDF로 변환
# df['beta_normalized'] = df['average_score'].apply(lambda x: beta.cdf(x, a, b))

# 결과 확인
print(df[['sentiment', 'logistic_score']])

file_path = "D:\\대학원\\논문\\소논문\\부동산_감정사전\\sentiment_scores0424.csv"
df.to_csv(file_path, index=False, encoding='utf-8-sig') # cp949 utf-8 깨짐

# 'logistic_score'로 Pos Neg Neu 나누기

# 
selected_df = df[df["sen_pol"] != "N"]

series = pd.Series(selected_df["sen_pol"])
table = series.value_counts()
table

title =  selected_df["제목"].str.replace("[^가-힣\s]", "", regex=True)
polarity = selected_df["sen_pol"] # 양수 음수 이진법으로 분류해야함
