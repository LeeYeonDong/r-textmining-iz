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

from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import RandomizedSearchCV

from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Embedding, Bidirectional, LSTM, Dense
from tensorflow.keras.regularizers import l1_l2
from tensorflow.keras.optimizers import Adam
from keras.wrappers.scikit_learn import KerasClassifier



# example
df = pd.read_csv("D:/대학원/논문/소논문/부동산_감정사전/re_df.csv", encoding='cp949')
df.columns
df.head

selected_df = df[df["sen_pola"] != 0]

series = pd.Series(selected_df["sen_pola"])
table = series.value_counts()
table

title = selected_df["제목"]
polarity = selected_df["sen_pola"] # 양수 음수 이진법으로 분류해야함

# 전처리
selected_df["제목"] = selected_df["제목"].str.replace("[^ㄱ-ㅎㅏ-ㅣ가-힣 ]", "", regex=True)


# text data tokenize and to sequence
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
print("훈련 데이터 크기:", X_train.shape, y_train.shape)
print("검증 데이터 크기:", X_val.shape, y_val.shape)

# 과적합 모델
def create_regularized_model(learning_rate=0.001, l1_rate=0.01, l2_rate=0.01):
    model = Sequential([
        Embedding(input_dim=len(tokenizer.word_index) + 1, output_dim=64, input_length=max_len),
        Bidirectional(LSTM(64, return_sequences=True, kernel_regularizer=l1_l2(l1=l1_rate, l2=l2_rate))),
        Bidirectional(LSTM(64, kernel_regularizer=l1_l2(l1=l1_rate, l2=l2_rate))),
        Dense(1, activation='sigmoid', kernel_regularizer=l1_l2(l1=l1_rate, l2=l2_rate))
    ])
    optimizer = Adam(learning_rate=learning_rate)
    model.compile(optimizer=optimizer, loss='binary_crossentropy', metrics=['accuracy'])
    return model

# KerasClassifier 래퍼를 사용하여 모델 생성
model = KerasClassifier(build_fn=create_regularized_model, verbose=0)

# RandomizedSearchCV의 param_distributions에 'learning_rate', 'l1_rate', 'l2_rate' 포함
param_dist = {
    'epochs': [5, 10, 15],
    'batch_size': [1, 16, 32, 64],
    'learning_rate': [0.001, 0.01, 0.1],  # 다양한 learning rate 시도
}

# RandomizedSearchCV 수행
random_search = RandomizedSearchCV(estimator=model, param_distributions=param_dist, n_iter=10, cv=3, verbose=2)
random_search_result = random_search.fit(X_train, y_train, validation_data=(X_val, y_val), callbacks=[early_stopping], verbose=2)



# GridSearchCV 시각화
import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd

# GridSearchCV 결과를 DataFrame으로 변환
results = pd.DataFrame(grid_result.cv_results_)

# Heatmap을 사용하여 2개의 하이퍼파라미터 (예: 'batch_size'와 'epochs')에 따른 성능 시각화
if 'param_batch_size' in results.columns and 'param_epochs' in results.columns:
    pivot_table = results.pivot('param_batch_size', 'param_epochs', 'mean_test_score')
    plt.figure(figsize=(10, 6))
    sns.heatmap(pivot_table, annot=True, cmap='YlGnBu', fmt='.4f')
    plt.title('Mean Test Scores')
    plt.show()

# 선 그래프나 막대 그래프를 사용하여 하나의 하이퍼파라미터에 따른 성능 시각화
# 예: 'epochs'에 따른 성능
if 'param_epochs' in results.columns:
    plt.figure(figsize=(10, 6))
    sns.lineplot(x='param_epochs', y='mean_test_score', data=results)
    plt.title('Mean Test Scores vs. Epochs')
    plt.show()


# model training
X = np.array(padded_sq)
y = np.array(polarity)
X.shape
y.shape

early_stopping = EarlyStopping(monitor='val_loss', patience=3, restore_best_weights=True)

model_fit = model.fit(X, y, epochs=5, batch_size = 64,  verbose=1, validation_split=0.2, callbacks=[early_stopping])
model_fit.history

#>>> model_fit.history
#{'loss': [0.23138010501861572, -0.15987063944339752, 0.13936518132686615, -0.1752006560564041, -0.28332963585853577], 'accuracy': [0.9025653004646301, 0.9793060421943665, 0.9773573279380798, 0.9829126000404358, 0.9847739934921265], 'val_loss': [-0.0972813069820404, -0.3719051480293274, 0.11448339372873306, -0.2517080307006836, -0.43694907426834106], 'val_accuracy': [0.9126919507980347, 0.9089111089706421, 0.9047812819480896, 0.9088529348373413, 0.9118776321411133]}

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

# DataFrame을 CSV 파일로 저장
import pandas as pd

# 딕셔너리를 pandas DataFrame으로 변환
df_sentiment = pd.DataFrame(list(sentiment_dict.items()), columns=['Word', 'Sentiment_Score'])

# 지정된 경로에 DataFrame을 CSV 파일로 저장
file_path = "D:\\대학원\\논문\\소논문\\부동산_감정사전\\sentiment_scores.csv"
df_sentiment.to_csv(file_path, index=False, encoding='cp949')
