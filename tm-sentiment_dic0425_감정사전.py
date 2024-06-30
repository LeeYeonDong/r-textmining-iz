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


# example
df = pd.read_csv("D:/대학원/논문/소논문/부동산_감정사전/re_df.csv", encoding='cp949')
df.columns
df.head

# 특수문자로 시작하는 단어 제거 ([, {, <, @)
df['제목'] = df['제목'].str.replace(r'\[[^\]]*\]', '', regex=True)  # 대괄호 안의 내용 삭제
df['제목'] = df['제목'].str.replace(r'<[^>]*>', '', regex=True)    # 꺽쇠 괄호 안의 내용 삭제
df['제목'] = df['제목'].str.replace(r'\{[^\}]*\}', '', regex=True)  # 중괄호 안의 내용 삭제
df['제목'] = df['제목'].str.replace(r'@[^\s]+', '', regex=True)    # @로 시작하는 단어 삭제
# 특수 문자 제거 (문장부호 제외)
df['제목'] = df['제목'].str.replace(r'[^\w\s,.?!\'"]', '', regex=True)
# 한 글자 단어 제거
df['제목'] = df['제목'].str.replace(r'\b\w\b', '', regex=True)
# 불필요한 공백 제거
df['제목'] = df['제목'].str.replace(r'\s+', ' ', regex=True).str.strip()

df = df.sample(frac=0.3)

selected_df = df[df["sen_pol"] != 0]

series = pd.Series(selected_df["sen_pol"])
table = series.value_counts()
table

# 텍스트를 토큰화하
def tokenize(text):
    tokens = okt.morphs(text)  # 텍스트를 형태소 단위로 토큰화
    return tokens

# '제목' 컬럼에 대해 토큰화 및 불용어 제거 수행
selected_df["제목"] = selected_df["제목"].apply(tokenize)

title =  selected_df["제목"].str.replace("[^가-힣\s]", "", regex=True)
polarity = selected_df["sen_pol"] # 양수 음수 이진법으로 분류해야함

# text data tokenize and to sequence
tokenizer = Tokenizer()
tokenizer.fit_on_texts(title)
sequences = tokenizer.texts_to_sequences(title)


# sequence pading
max_len = max(len(seq) for seq in sequences)
padded_sq = pad_sequences(sequences, maxlen = max_len, padding = "post")


# 데이터 분리 (80% 훈련 데이터, 20% 검증 데이터)
X = np.array(padded_sq)
y = np.array(polarity)

X_train, X_val, y_train, y_val = train_test_split(X, y, test_size=0.2, random_state=42)
print("훈련 데이터 크기:", X_train.shape, y_train.shape)
print("검증 데이터 크기:", X_val.shape, y_val.shape)

# 조기 종료 설정 (성능 향상이 멈출 경우 훈련 중지)
early_stopping = EarlyStopping(monitor='val_loss', patience=3, restore_best_weights=True)

# Learning Rate 조절
def create_regularized_model(dropout_rate=0.1, learning_rate=0.001, lamda=0.01):
    print(f"dropout_rate={dropout_rate}, learning_rate={learning_rate}, lamda={lamda}")  # 현재 설정 출력
    model = Sequential([
        Embedding(input_dim=len(tokenizer.word_index) + 1, output_dim=64, input_length=max_len),
        Bidirectional(LSTM(64, return_sequences=True, kernel_regularizer=l2(lamda))),
        Dropout(dropout_rate),
        Bidirectional(LSTM(64)),
        Dense(1, activation='sigmoid', kernel_regularizer=l1(lamda))
    ])
    optimizer = Adam(learning_rate=learning_rate)
    model.compile(optimizer=optimizer, loss='binary_crossentropy', metrics=['accuracy'])
    return model

model = create_regularized_model()
model = KerasClassifier(build_fn=create_regularized_model, verbose=2)

# 그리드 서치 파라미터 설정 (learning rate 추가)
param_grid = {
    'dropout_rate': [0.1, 0.3, 0.5],
    'learning_rate': [0.001, 0.0005, 0.0001],
    'epochs': [5, 10, 15],
    'batch_size': [16, 32, 64],  # 배치 크기가 너무 작으면 불안정할 수 있으므로 1은 제외
    'lamda': [0.01, 0.05, 0.1]  # lamda 값의 범위를 조정하여 더 강한 정규화 효과를 탐색
}

# GridSearchCV 수행
grid = GridSearchCV(estimator=model, param_grid=param_grid, cv=3, verbose=2)
grid_result = grid.fit(X_train, y_train, validation_data=(X_val, y_val), callbacks=[early_stopping], verbose=2)

# # 최적 파라미터 및 결과 출력
print("최적 파라미터: ", grid_result.best_params_)
print("최적 점수: ", grid_result.best_score_)
# >>> print("최적 파라미터: ", grid_result.best_params_)
# 최적 파라미터:  {'batch_size': 16, 'dropout_rate': 0.1, 'epochs': 15, 'lamda': 0.01, 'learning_rate': 0.001}
# >>> print("최적 점수: ", grid_result.best_score_)
# 최적 점수:  0.9147986769676208

# GridSearchCV 결과를 DataFrame으로 변환
results = pd.DataFrame(grid_result.cv_results_)

# 지정된 경로에 DataFrame을 CSV 파일로 저장
file_path = "D:\\대학원\\논문\\소논문\\부동산_감정사전\\GridSearchCV0425-1.csv"
results.to_csv(file_path, index=False, encoding='cp949')


# 시각화
import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd

# Heatmap을 사용하여 2개의 하이퍼파라미터 ('batch_size'와 'epochs')에 따른 성능 시각화
# 'mean_test_score'를 이용하여 평균 테스트 점수를 기준으로 시각화 진행
# if 'param_batch_size' in results.columns and 'param_epochs' in results.columns:
#     # pivot_table 메소드를 사용하여 적절한 형태의 테이블 생성
#     pivot_table = results.pivot(index='param_batch_size', columns='param_epochs', values='mean_test_score')
#     plt.figure(figsize=(10, 6))
#     # sns.heatmap 함수를 사용하여 Heatmap 생성
#     sns.heatmap(pivot_table, annot=True, cmap='YlGnBu', fmt='.4f')
#     plt.title('GridSearchCV Mean Test Scores (Heatmap)')
#     plt.xlabel('Epochs')
#     plt.ylabel('Batch Size')
#     plt.show()

# # 선 그래프를 사용하여 'epochs'에 따른 성능 시각화
# # 'epochs' 하이퍼파라미터를 x축, 'mean_test_score'를 y축으로 사용
# if 'param_epochs' in results.columns:
#     plt.figure(figsize=(10, 6))
#     # sns.lineplot 함수를 사용하여 선 그래프 생성
#     sns.lineplot(x=results['param_epochs'].astype(int), y='mean_test_score', data=results, marker='o')
#     plt.title('Mean Test Scores vs. Epochs (Line Plot)')
#     plt.xlabel('Epochs')
#     plt.ylabel('Mean Test Score')
#     plt.grid(True)
#     plt.show()

# 전체 데이터로 학습 시작
df = pd.read_csv("D:/대학원/논문/소논문/부동산_감정사전/re_df.csv", encoding='cp949')
df.columns
df.head

# 특수문자로 시작하는 단어 제거 ([, {, <, @)
df['제목'] = df['제목'].str.replace(r'\[[^\]]*\]', '', regex=True)  # 대괄호 안의 내용 삭제
df['제목'] = df['제목'].str.replace(r'<[^>]*>', '', regex=True)    # 꺽쇠 괄호 안의 내용 삭제
df['제목'] = df['제목'].str.replace(r'\{[^\}]*\}', '', regex=True)  # 중괄호 안의 내용 삭제
df['제목'] = df['제목'].str.replace(r'@[^\s]+', '', regex=True)    # @로 시작하는 단어 삭제
# 특수 문자 제거 (문장부호 제외)
df['제목'] = df['제목'].str.replace(r'[^\w\s,.?!\'"]', '', regex=True)
# 한 글자 단어 제거
df['제목'] = df['제목'].str.replace(r'\b\w\b', '', regex=True)
# 불필요한 공백 제거
df['제목'] = df['제목'].str.replace(r'\s+', ' ', regex=True).str.strip()

selected_df = df[df["sen_pol"] != 0]

series = pd.Series(selected_df["sen_pol"])
table = series.value_counts()
table

# 텍스트를 토큰화하
def tokenize(text):
    tokens = okt.morphs(text)  # 텍스트를 형태소 단위로 토큰화
    return tokens

# '제목' 컬럼에 대해 토큰화
selected_df["제목"] = selected_df["제목"].apply(tokenize)

title =  selected_df["제목"].str.replace("[^가-힣\s]", "", regex=True)
polarity = selected_df["sen_pol"] # 양수 음수 이진법으로 분류해야함

# text data tokenize and to sequence
tokenizer = Tokenizer()
tokenizer.fit_on_texts(title)
sequences = tokenizer.texts_to_sequences(title)

# sequence pading
max_len = max(len(seq) for seq in sequences)
padded_sq = pad_sequences(sequences, maxlen = max_len, padding = "post")


# 데이터 분리 (80% 훈련 데이터, 20% 검증 데이터)
X = np.array(padded_sq)
y = np.array(polarity)

X_train, X_val, y_train, y_val = train_test_split(X, y, test_size=0.2, random_state=42)
print("훈련 데이터 크기:", X_train.shape, y_train.shape)
print("검증 데이터 크기:", X_val.shape, y_val.shape)
# >>> print("훈련 데이터 크기:", X_train.shape, y_train.shape)
# 훈련 데이터 크기: (68763, 24) (68763,)
# >>> print("검증 데이터 크기:", X_val.shape, y_val.shape)
# 검증 데이터 크기: (17191, 24) (17191,)

# 조기 종료 설정 (성능 향상이 멈출 경우 훈련 중지)
early_stopping = EarlyStopping(monitor='val_loss', patience=3, restore_best_weights=True)

def create_regularized_model(dropout_rate=0.1, learning_rate=0.001, lamda=0.01):
    print(f"dropout_rate={dropout_rate}, learning_rate={learning_rate}, lamda={lamda}")  # 현재 설정 출력
    model = Sequential([
        Embedding(input_dim=len(tokenizer.word_index) + 1, output_dim=64, input_length=max_len),
        Bidirectional(LSTM(64, return_sequences=True, kernel_regularizer=l2(lamda))),
        Dropout(dropout_rate),
        Bidirectional(LSTM(64)),
        Dense(1, activation='sigmoid', kernel_regularizer=l1(lamda))
    ])
    optimizer = Adam(learning_rate=learning_rate)
    model.compile(optimizer=optimizer, loss='binary_crossentropy', metrics=['accuracy'])
    return model

model = KerasClassifier(build_fn=create_regularized_model, verbose=2)

# model training
model_fit = model.fit(X, y, epochs=15, batch_size = 16,  verbose=2, validation_split=0.2, callbacks=[early_stopping])
model_fit.history

# {'loss': [0.32203713059425354, 0.002950088819488883, -0.11151300370693207, -0.17315135896205902, -0.21725548803806305], 'accuracy': [0.8669446706771851, 0.9712941646575928, 0.9795374274253845, 0.9797313809394836, 
# 0.980313241481781], 'val_loss': [0.15413238108158112, 0.08259429037570953, 0.14712336659431458, 0.03360902518033981, 0.01848897896707058], 'val_accuracy': [0.9195112586021423, 0.918929398059845, 0.9138867259025574, 0.9158262014389038, 0.9160201549530029]}

# model training 시각화
import matplotlib.pyplot as plt

# Plot training & validation accuracy values
plt.figure(figsize=(12, 4))

plt.subplot(1, 2, 1)
plt.plot(model_fit.history['accuracy'])
plt.plot(model_fit.history['val_accuracy'])
plt.title('Model accuracy')
plt.ylabel('Accuracy')
plt.xlabel('Epoch')
plt.legend(['Train', 'Validation'], loc='upper left')

# Plot training & validation loss values
plt.subplot(1, 2, 2)
plt.plot(model_fit.history['loss'])
plt.plot(model_fit.history['val_loss'])
plt.title('Model loss')
plt.ylabel('Loss')
plt.xlabel('Epoch')
plt.legend(['Train', 'Validation'], loc='upper left')

plt.tight_layout()
plt.show()


from sklearn.metrics import precision_score, recall_score, f1_score
import numpy as np

# 모델 예측
predictions = model.predict(X)
predictions = np.round(predictions).astype(int)  # 확률을 이진 값으로 변환

# 실제 레이블과 예측 레이블 비교
precision = precision_score(y, predictions)
recall = recall_score(y, predictions)
f1 = f1_score(y, predictions)

print(f'Precision: {precision}')
print(f'Recall: {recall}')
print(f'F1 Score: {f1}')

#>>> print(f'Precision: {precision}')
#Precision: 0.9913593009420898
#>>> print(f'Recall: {recall}')
#Recall: 0.9829046606072327
#>>> print(f'F1 Score: {f1}')
#F1 Score: 0.9871138775866926

# generate sentiment dict.
sentiment_dict = {}

for word, idx in tokenizer.word_index.items():
    sequence = pad_sequences([[idx]], maxlen = max_len, padding = "post")
    prediction = model_fit.model.predict(sequence)
    sentiment_score = float(prediction[0])
    sentiment_dict[word] = sentiment_score

print(sentiment_dict)

# 딕셔너리를 pandas DataFrame으로 변환
df_sentiment = pd.DataFrame(list(sentiment_dict.items()), columns=['Word', 'Sentiment_Score'])

# 지정된 경로에 DataFrame을 CSV 파일로 저장
file_path = "D:\\대학원\\논문\\소논문\\부동산_감정사전\\sentiment_scores_문장부호 포함_0426.csv"
df_sentiment.to_csv(file_path, index=False, encoding='cp949')
