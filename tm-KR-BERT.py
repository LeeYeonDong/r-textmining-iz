# pip install bertopic
# pip install transformers
# pip install torch

# 라이브러리 임포트 및 데이터 로드
import pandas as pd
from bertopic import BERTopic
from transformers import BertTokenizer, BertModel
import numpy as np
import torch
import re
import time
from umap import UMAP

# 시작 시간 기록
start_time = time.time()

# 데이터 로드
df = pd.read_csv('D:/대학원/논문/소논문/부동산_토픽모델링/부동산_df.csv', encoding='cp949')
df.shape

# 숫자로 시작하지 않는 '날짜' 열을 가진 관측치 추출
new_data = df[~df['날짜'].str.match(r'^\d')]
new_data.shape

# 기존 데이터 프레임에서 해당 관측치 제거
df = df.drop(new_data.index)
df.shape

# 언론사 관측치를 링크로 옮기기
new_data['링크'] = new_data['언론사']
# 날짜 관측치를 언론사로 옮기기
new_data['언론사'] = new_data['날짜']
# 제목에서 가장 마지막의 쉼표 뒤의 문자를 날짜로 옮기기
new_data['날짜'] = new_data['제목'].str.extract(r',([^,]+)$')[0]
new_data['날짜'] = new_data['날짜'].str.strip('"')
# 제목에서 가장 마지막의 쉼표 앞의 문자만 남기기
new_data['제목'] = new_data['제목'].str.rsplit(',', n=1).str[0]

# 기존 데이터 프레임과 새 데이터 프레임 결합
df = pd.concat([df, new_data], ignore_index=True)
# 결합된 데이터 프레임을 날짜 순으로 정렬
df['날짜'] = df['날짜'].str.replace('오후', 'PM').str.replace('오전', 'AM') # '오후'와 '오전'을 'PM'과 'AM'으로 변환
df['날짜'] = pd.to_datetime(df['날짜'], format='%Y.%m.%d. %p %I:%M').dt.strftime('%Y-%m-%d')
df = df.sort_values(by='날짜')
df['날짜'] = df['날짜'].str.split(' ').str[0]
df.columns
df.head


# KR-BERT 모델 초기화
tokenizer = BertTokenizer.from_pretrained('snunlp/KR-BERT-char16424')
model = BertModel.from_pretrained('snunlp/KR-BERT-char16424')

# 문서 임베딩 함수 정의:
def embed_documents(documents):
    embeddings = []
    for doc in documents:
        inputs = tokenizer(doc, return_tensors='pt', truncation=True, max_length=512)
        outputs = model(**inputs)
        embeddings.append(outputs.last_hidden_state.mean(dim=1).detach().numpy())
    return np.vstack(embeddings)

topic_model = BERTopic(min_topic_size = 30, n_gram_range = (1,3), verbose = True, calculate_probabilities = True)


# UMAP 모델 초기화
umap_model = UMAP(n_neighbors=15, n_components=5, min_dist=0.0, metric='cosine')

# BERTopic 모델 초기화 및 UMAP 모델 적용
topic_model = BERTopic(nr_topics = "auto", n_gram_range = (1,3), verbose = True, calculate_probabilities = True, umap_model = umap_model, embedding_model="all-MiniLM-L6-v2")

# BERTopic 모델을 사용하여 주제 추출
topics, probabilities = topic_model.fit_transform(df['제목'].to_list())

# 각 문서에 대한 주제 확률 분포의 평균을 계산
df['topic_weight'] = [np.mean(prob) if prob.size > 0 else 0 for prob in probabilities]

# 종료 시간 기록
end_time = time.time()

elapsed_time = end_time - start_time
print(f"코드 실행 시간: {elapsed_time} 초")

# Visualization 
text = df.제목.to_list()
date = df.날짜.to_list()
len(text)
len(date)
topics_over_time = topic_model.topics_over_time(text, date, global_tuning=True, evolution_tuning=True, nr_bins=30)


# Visualization 1
topic_model.visualize_topics_over_time(topics_over_time, top_n_topics = 30).show()

# Visualization 2 
# 상대적 빈도(relative frequency)로 생성된 토픽에 액세스
topic_model.get_topic_freq().head()

# 두 번째로 빈번하게 생성된 주제, 즉 Topic 0. -1은 outlier
topic_model.get_topic(0)
topic_model.get_topic_info().head(20) # -1 = outlier

topic_nr = topic_model.get_topic_info().iloc[6]["Topic"] # select a frequent topic
topic_model.get_topic(topic_nr)

topic_model.visualize_topics(top_n_topics = 20).show()

# Visualization 3
topic_model.visualize_barchart(top_n_topics = 20).show()

# Visualization 4
topic_model.visualize_term_rank().show()

# Visualization 5
topic_model.visualize_hierarchy(top_n_topics = 20).show()

# Visualization 6
topic_model.visualize_heatmap(top_n_topics = 20).show()

# Visualization 7
topic_model.estimate()
topic_model.visualize_distribution(topic_model.probabilities_[0], min_probability = 0.015).show()
topic_model.visualize_distribution(probabilities[0]).show()

