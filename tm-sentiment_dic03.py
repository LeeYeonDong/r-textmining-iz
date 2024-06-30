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
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Embedding, Bidirectional, LSTM, Dense, Dropout
from tensorflow.keras.regularizers import l1, l2
from tensorflow.keras.optimizers import Adam


# example
#df_d = pd.read_excel(f"D:/대학원/논문/소논문/부동산_감정사전/re_df.xlsx", engine='openpyxl')

#df = df_d

df_s = pd.read_csv('D:/대학원/논문/소논문/부동산_감정사전/re_df.csv', encoding='cp949')

selected_df = df_s

selected_df = selected_df.sample(frac=0.3)
df.columns
df.shape
df.head

# selected_df = df[df["sen_pol"] != 0]
selected_df.columns

series = pd.Series(selected_df["sen_pol"])
table = series.value_counts()
table

##
#word = selected_df["word"]

title = selected_df['제목'].str.replace("[^가-힣\s]", "", regex=True)

##
polarity = selected_df["sen_pol"] # 양수 음수 이진법으로 분류해야함

# # text data tokenize and to sequence
# tokenizer = Tokenizer()
# tokenizer.fit_on_texts(word)
# sequences = tokenizer.texts_to_sequences(word)

tokenizer = Tokenizer()
tokenizer.fit_on_texts(title)
sequences = tokenizer.texts_to_sequences(title)

# tokenizer = Tokenizer()
# tokenizer.fit_on_texts(selected_df['제목'])
# sequences = tokenizer.texts_to_sequences(selected_df['제목'])


# sequence pading
max_len = max(len(seq) for seq in sequences)
padded_sq = pad_sequences(sequences, maxlen = max_len, padding = "post")


# 조기 종료 설정 (성능 향상이 멈출 경우 훈련 중지)
early_stopping = EarlyStopping(monitor='val_loss', patience=3, restore_best_weights=True)

# 데이터 분리 (80% 훈련 데이터, 20% 검증 데이터)
X = np.array(padded_sq)
y = np.array(polarity)

X_train, X_val, y_train, y_val = train_test_split(X, y, test_size=0.2, random_state=42)
print("훈련 데이터 크기:", X_train.shape, y_train.shape)
print("검증 데이터 크기:", X_val.shape, y_val.shape)


# 과소적합 과적합 방지 모델
def create_regularized_model(dropout_rate=0.1, learning_rate=0.001, lamda=0.01):
    print(f"dropout_rate={dropout_rate}, learning_rate={learning_rate}, lamda={lamda}")  # 현재 설정 출력
    model = Sequential([
        Embedding(input_dim=len(tokenizer.word_index) + 1, output_dim=64, input_length=max_len),
        Bidirectional(LSTM(64, return_sequences=True, kernel_regularizer=l2(lamda))),
        Dropout(dropout_rate),
        Bidirectional(LSTM(64)),
        Dropout(dropout_rate),
        Dense(1, activation='sigmoid', kernel_regularizer=l1(lamda))
    ])
    optimizer = Adam(learning_rate=learning_rate)
    model.compile(optimizer=optimizer, loss='binary_crossentropy', metrics=['accuracy'])
    return model

# 모델 래퍼 생성
model = KerasClassifier(build_fn=create_regularized_model, verbose=2)

# 그리드 서치 파라미터 설정 (정규화 계수 lamda의 범위를 확장)
param_grid = {
    'dropout_rate': [0.1, 0.3, 0.5],
    'learning_rate': [0.001, 0.0005, 0.0001],
    'epochs': [5, 10, 15],
    'batch_size': [16, 32, 64],  # 배치 크기가 너무 작으면 불안정할 수 있으므로 1은 제외
    'lamda': [0.01, 0.05, 0.1]  # lamda 값의 범위를 조정하여 더 강한 정규화 효과를 탐색
}

# 그리드 서치 객체 생성
grid = GridSearchCV(estimator=model, param_grid=param_grid, cv=3, verbose=2, n_jobs=1)

# 그리드 서치 수행 (KerasClassifier 내부에서 EarlyStopping 적용 필요)
grid_result = grid.fit(X_train, y_train, validation_data=(X_val, y_val), callbacks=[early_stopping], verbose=2)

# accuracy 의미 확인하기

# 최적 파라미터 및 결과 출력
print("최적 파라미터: ", grid_result.best_params_)
print("최적 점수: ", grid_result.best_score_)
# 최적 파라미터:  {'batch_size': 16, 'dropout_rate': 0.5, 'epochs': 10, 'lamda': 0.01, 'learning_rate': 0.001}
# >>> print("최적 점수: ", grid_result.best_score_)
# 최적 점수:  0.9114795327186584

# GridSearchCV 시각화
import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd


# GridSearchCV 결과를 DataFrame으로 변환
results = pd.DataFrame(grid_result.cv_results_)

results.to_csv('D:/대학원/논문/소논문/부동산_감정사전/results.csv', index=False, encoding='cp949')

# 'dropout_rate'가 0.5인 경우 'dropout_rate': 0.5, 'lamda': 0.01, 'learning_rate': 0.001만 필터링
filtered_results = results[
    (results['param_dropout_rate'] == 0.5) &
    (results['param_lamda'] == 0.01) &
    (results['param_learning_rate'] == 0.001)
]

# 필터링된 결과를 사용하여 'batch_size'와 'epochs'에 따른 평균 테스트 점수의 히트맵 생성
pivot_table = filtered_results.pivot(index='param_batch_size', columns='param_epochs', values='mean_test_score')
plt.figure(figsize=(10, 6))
sns.heatmap(pivot_table, annot=True, cmap='YlGnBu', fmt=".4f")
plt.title('GridSearchCV Mean Test Scores for dropout_rate=0.5')
plt.xlabel('Epochs')
plt.ylabel('Batch Size')
plt.show()

# 선 그래프를 사용하여 'epochs'에 따른 성능 시각화
# 'epochs' 하이퍼파라미터를 x축, 'mean_test_score'를 y축으로 사용
if 'param_epochs' in results.columns:
    plt.figure(figsize=(10, 6))
    # sns.lineplot 함수를 사용하여 선 그래프 생성
    sns.lineplot(x=results['param_epochs'].astype(int), y='mean_test_score', data=results, marker='o')
    plt.title('Mean Test Scores vs. Epochs (Line Plot)')
    plt.xlabel('Epochs')
    plt.ylabel('Mean Test Score')
    plt.grid(True)
    plt.show()



# model training
df_s = pd.read_csv('D:/대학원/논문/소논문/부동산_감정사전/re_df.csv', encoding='cp949')

selected_df = df_s
#selected_df = selected_df.sample(frac=0.01)
#selected_df = df[df["sen_pol"] != 0]
selected_df.columns

series = pd.Series(selected_df["sen_pol"])
table = series.value_counts()
table

##
title = selected_df['제목'].str.replace("[^가-힣\s]", "", regex=True)

##
polarity = selected_df["sen_pol"] # 양수 음수 이진법으로 분류해야함

tokenizer = Tokenizer()
tokenizer.fit_on_texts(title)
sequences = tokenizer.texts_to_sequences(title)

# sequence pading
max_len = max(len(seq) for seq in sequences)
padded_sq = pad_sequences(sequences, maxlen = max_len, padding = "post")

# 조기 종료 설정 (성능 향상이 멈출 경우 훈련 중지)
early_stopping = EarlyStopping(monitor='val_loss', patience=3, restore_best_weights=True)

# 데이터 분리 (80% 훈련 데이터, 20% 검증 데이터)
X = np.array(padded_sq)
y = np.array(polarity)

X_train, X_val, y_train, y_val = train_test_split(X, y, test_size=0.2, random_state=42)

early_stopping = EarlyStopping(monitor='val_loss', patience=3, restore_best_weights=True)

def create_regularized_model(dropout_rate=0.5, learning_rate=0.001, lamda=0.01):
    print(f"dropout_rate={dropout_rate}, learning_rate={learning_rate}, lamda={lamda}")  # 현재 설정 출력
    model = Sequential([
        Embedding(input_dim=len(tokenizer.word_index) + 1, output_dim=64, input_length=max_len),
        Bidirectional(LSTM(64, return_sequences=True, kernel_regularizer=l2(lamda))),
        Dropout(dropout_rate),
        Bidirectional(LSTM(64)),
        Dropout(dropout_rate),
        Dense(1, activation='sigmoid', kernel_regularizer=l1(lamda))
    ])
    optimizer = Adam(learning_rate=learning_rate)
    model.compile(optimizer=optimizer, loss='binary_crossentropy', metrics=['accuracy'])
    return model

# 모델 래퍼 생성
model = KerasClassifier(build_fn=create_regularized_model, verbose=2)

model_fit = model.fit(X, y, epochs=10, batch_size = 16,  verbose=2, validation_split=0.2, callbacks=[early_stopping])
model_fit.history
# {'loss': [0.19140462577342987, -0.04486289992928505, -0.19225962460041046, -0.4330832064151764, -0.8017456531524658, -0.8958573341369629, -1.4013148546218872, -1.669137716293335, -1.9575856924057007, -2.250563144683838], 'accuracy': [0.9104751348495483, 0.9726887941360474, 0.9769352674484253, 0.9814144372940063, 0.9828832149505615, 0.9825342297554016, 0.9841484427452087, 0.9832031726837158, 0.9840320944786072, 0.9837267398834229], 'val_loss': [0.05439494177699089, -0.19503603875637054, -0.5622401833534241, -0.6545971632003784, -0.6404036283493042, -1.6937810182571411, -2.427809715270996, -2.768641710281372, -3.7661099433898926, 7.372745513916016], 'val_accuracy': [0.9144901633262634, 0.8966319561004639, 0.9109417796134949, 0.9079750776290894, 0.9096620082855225, 0.9054737687110901, 0.903786838054657, 0.8994823098182678, 0.9087894558906555, 0.9021580815315247]}

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

#
from sklearn.metrics import precision_score, recall_score, f1_score
import numpy as np

# 모델 예측
predictions = model.predict(X)
predictions = np.round(predictions).astype(int)  # 확률을 이진 값으로 변환

# NaN 값이 있는 행의 인덱스 찾기
nan_indexes = np.where(np.isnan(y))[0]

# NaN 값을 포함한 행 제거
X = np.delete(X, nan_indexes, axis=0)
y = np.delete(y, nan_indexes, axis=0)

# 모델 예측 다시 수행
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
# Precision: 0.9912506333060525
#>>> print(f'Recall: {recall}')
# Recall: 0.9818753860407659
#>>> print(f'F1 Score: {f1}')
# F1 Score: 0.9865407365746757

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

# DataFrame을 CSV 파일로 저장
import pandas as pd

# 딕셔너리를 pandas DataFrame으로 변환
df_sentiment = pd.DataFrame(list(sentiment_dict.items()), columns=['Word', 'Sentiment_Score'])

# 지정된 경로에 DataFrame을 CSV 파일로 저장
file_path = "D:\\대학원\\논문\\소논문\\부동산_감정사전\\sentiment_scores1.csv"
df_sentiment.to_csv(file_path, index=False, encoding='cp949')
