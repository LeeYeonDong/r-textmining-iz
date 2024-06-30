# library import
import tensorflow as tf
from tensorflow.keras.preprocessing.text import Tokenizer
from tensorflow.keras.preprocessing.sequence import pad_sequences
import numpy as np

# toy example
texts = ["행복하게 살자", "슬픈 일이 생겼어", "사랑은 아름다워", "실망하지 말고 노력해보자"]
labels = [1, 0, 1, 0]  # 1은 긍정, 0은 부정


# text data tokenize and to sequence
tokenizer = Tokenizer()
tokenizer.fit_on_texts(texts)
sequences = tokenizer.texts_to_sequences(texts)


# sequence pading
max_len = max(len(seq) for seq in sequences)
padded_sequences = pad_sequences(sequences, maxlen=max_len, padding='post')


# generate Bi-LSTM model
model = tf.keras.Sequential([
    tf.keras.layers.Embedding(input_dim=len(tokenizer.word_index) + 1, output_dim=64, input_length=max_len),
    tf.keras.layers.Bidirectional(tf.keras.layers.LSTM(64)),
    tf.keras.layers.Dense(1, activation='sigmoid')
])

model.compile(optimizer='adam', loss='binary_crossentropy', metrics=['accuracy'])

# model training
X = np.array(padded_sequences)
y = np.array(labels)

model.fit(X, y, epochs=10, batch_size=1)


# generate sentiment dict.
sentiment_dict = {}

for word, idx in tokenizer.word_index.items():
    sequence = pad_sequences([[idx]], maxlen=max_len, padding='post')
    prediction = model.predict(sequence)
    sentiment_score = float(prediction[0])
    sentiment_dict[word] = sentiment_score

print(sentiment_dict)