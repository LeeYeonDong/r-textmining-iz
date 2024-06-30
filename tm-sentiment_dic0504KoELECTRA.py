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
from konlpy.tag import Okt
import pandas as pd
from transformers import AutoTokenizer, AutoModelForSequenceClassification, pipeline


df = pd.read_excel('D:/대학원/논문/소논문/부동산_감정사전/부동산_수정_df.xlsx', engine='openpyxl')
df = df.sample(frac=0.001)

# 모델과 토크나이저 로드
tokenizer = AutoTokenizer.from_pretrained("monologg/koelectra-small-v3-discriminator")
model = AutoModelForSequenceClassification.from_pretrained("monologg/koelectra-small-v3-discriminator")
nlp = pipeline('sentiment-analysis', model=model, tokenizer=tokenizer)

# 데이터 전처리 함수 정의
def preprocess_text(text):
    # 특수 문자 제거 (따옴표 제외)
    text = re.sub(r"[^가-힣A-Za-z0-9'\s]", '', text)
    # 연속된 공백은 하나의 공백으로
    text = re.sub(r'\s+', ' ', text).strip()
    return text

# 감성 분석 및 결과 저장
# '제목' 컬럼 전처리
df['제목'] = df['제목'].apply(preprocess_text)

# 감성 분석 수행 및 결과 저장
df['sentiment'] = df['제목'].apply(lambda x: nlp(x)[0] if x.strip() != "" else {'label': 'Neutral', 'score': None})


# 결과 확인
print(df[['sentiment']])

file_path = "D:\\대학원\\논문\\소논문\\부동산_감정사전\\KoELECTRA0504.csv"
df.to_csv(file_path, index=False, encoding='utf-8-sig')