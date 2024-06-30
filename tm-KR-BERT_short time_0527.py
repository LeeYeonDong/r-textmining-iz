# 라이브러리 임포트 및 데이터 로드`
import pandas as pd
from bertopic import BERTopic
from transformers import BertTokenizer, BertModel
from transformers import BertTokenizerFast, AlbertModel
import numpy as np
from sklearn.feature_extraction.text import CountVectorizer
import torch
import re
import time
from umap import UMAP
from hdbscan import HDBSCAN
import gensim
from gensim.models.coherencemodel import CoherenceModel
from gensim.corpora.dictionary import Dictionary
from gensim import corpora
from konlpy.tag import Okt
from itertools import product
from gensim import matutils
import numpy as np
from sklearn.metrics import silhouette_score
import matplotlib.pyplot as plt
import itertools
import ast


# GPU 사용 가능 여부 확인
if torch.cuda.is_available():
    print("GPU is available.")
    print("GPU Name:", torch.cuda.get_device_name(0))
else:
    print("GPU is not available.")


# 시작 시간 기록
start_time = time.time()

# 데이터 로드
df = pd.read_csv('D:/대학원/논문/소논문/부동산_토픽모델링/부동산_수정_df.csv', encoding='utf-8')
df = df.sample(frac=0.1) # test
df.columns
df.head

# 날짜 및 시간 문자열 변환 함수
def convert_korean_datetime(dt_str):
    # "오후"와 "오전"을 AM/PM으로 변환
    dt_str = dt_str.replace('오후', 'PM').replace('오전', 'AM')
    # pandas to_datetime 사용하여 날짜 및 시간 파싱
    return pd.to_datetime(dt_str, format='%Y.%m.%d. %p %I:%M')

# 날짜 칼럼 변환
df['날짜'] = df['날짜'].apply(convert_korean_datetime)

df = df[(df['날짜'].dt.year >= 2012) & (df['날짜'].dt.year <= 2022)]

# 특수문자로 시작하는 단어 제거 ([, {, <, @)
df['제목'] = df['제목'].str.replace(r'\[[^\]]*\]', '', regex=True)  # 대괄호 안의 내용 삭제
df['제목'] = df['제목'].str.replace(r'<[^>]*>', '', regex=True)    # 꺽쇠 괄호 안의 내용 삭제
df['제목'] = df['제목'].str.replace(r'\{[^\}]*\}', '', regex=True)  # 중괄호 안의 내용 삭제
df['제목'] = df['제목'].str.replace(r'@[^\s]+', '', regex=True)    # @로 시작하는 단어 삭제
# 특수 문자 제거 (문장부호 제외)
df['제목'] = df['제목'].str.replace(r'[^\w\s,.?!\'"]', '', regex=True)
# 한 글자 단어 제거
df['제목'] = df['제목'].str.replace(r'\b\w\b', '', regex=True)
# 그외 단어 제거
df['제목'] = df['제목'].str.replace(r'\d+년|\d+월|\d+일', ' ', regex=True).str.strip()
# 숫자를 제거
df['제목'] = df['제목'].str.replace(r'\d+', '', regex=True)
# 그외 단어 제거
df['제목'] = df['제목'].str.replace(r'들썩|벌써|쑥쑥|헤드라인|머니투데이', ' ', regex=True).str.strip()
# 불필요한 공백 제거
df['제목'] = df['제목'].str.replace(r'\s+', ' ', regex=True).str.strip()

# "서울" 또는 "수도권" 단어가 포함된 행을 필터링
df = df[df['제목'].str.contains("서울|수도권")]

# Okt 형태소 분석기 인스턴스 생성
okt = Okt()

# 텍스트를 토큰화하
def tokenize(text):
    tokens = okt.morphs(text)  # 텍스트를 형태소 단위로 토큰화
    return tokens

# 결측값을 빈 문자열로 대체하고, 모든 입력을 문자열로 변환
df['제목'] = df['제목'].fillna('').astype(str)

# '제목' 컬럼에 대해 토큰화
df["제목"] = df["제목"].apply(tokenize)

# 비문자열 데이터 타입 변환
df['제목'] = df['제목'].astype(str)
type(df['제목'])

# 문서 데이터 준비
documents = df['제목'].tolist()

# tokenized_docs 정의: 이는 원본 문서를 토큰화한 리스트입니다.
tokenized_docs = [ast.literal_eval(doc) for doc in documents]

# gensim 사전과 코퍼스 생성
dictionary = Dictionary(tokenized_docs)
corpus = [dictionary.doc2bow(text) for text in tokenized_docs]

# KR-BERT 모델 초기화
tokenizer = BertTokenizer.from_pretrained('snunlp/KR-BERT-char16424')
model = BertModel.from_pretrained('snunlp/KR-BERT-char16424')

# 문서 임베딩 함수
# 그리드 서치를 위한 파라미터 조합 생성
umap_params = [
    {"n_neighbors": n, "n_components": c, "min_dist": d, "metric": m}
    for n, c, d, m in product([5, 10], [2, 5], [0.0, 0.1, 0.2], ['euclidean', 'cosine'])
]
hdbscan_params = [
    {"min_cluster_size": s, "min_samples": ms, "metric": m, "prediction_data": True}
    for s, ms, m in product([200, 300, 400], [100, 200], ['euclidean', 'manhattan'])
]

# 결과를 저장할 데이터 프레임
results_df = pd.DataFrame(columns=["UMAP_Params", "HDBSCAN_Params", "Coherence_Score"])

# 결과를 저장할 리스트
results = []

# 문서 임베딩 함수 정의 (배치 처리 포함):
def embed_documents(documents, model, tokenizer, device='cuda', batch_size=16):
    model.to(device)
    embeddings = []
    for i in range(0, len(documents), batch_size):
        batch_docs = documents[i:i+batch_size]
        inputs = tokenizer(batch_docs, return_tensors='pt', padding=True, truncation=True, max_length=512).to(device)
        with torch.no_grad():
            outputs = model(**inputs)
        batch_embeddings = outputs.last_hidden_state.mean(dim=1).cpu().numpy()
        embeddings.append(batch_embeddings)
    return np.vstack(embeddings)


# 파라미터 조합에 따라 BERTopic 모델을 반복적으로 훈련하고 coherence score 계산
for umap_param in umap_params:
    for hdbscan_param in hdbscan_params:
        
        print(f"Processing UMAP: {umap_param}, HDBSCAN: {hdbscan_param}")
        try:
            # UMAP과 HDBSCAN 모델 생성
            umap_model = UMAP(**umap_param)
            hdbscan_model = HDBSCAN(**hdbscan_param)

            # BERTopic 모델 초기화 및 훈련
            topic_model = BERTopic(embedding_model=lambda docs: embed_documents(docs, model, tokenizer),
                                umap_model=umap_model,
                                hdbscan_model=hdbscan_model,
                                verbose=True,
                                calculate_probabilities=True)
        

            # BERTopic 모델을 사용하여 주제 추출
            topics, probabilities = topic_model.fit_transform(documents)

            # 모든 토픽에 대한 정보를 가져옵니다.
            topic_info = topic_model.get_topic_info()

            # 토픽의 총 수를 확인합니다.
            n_topics = len(topic_info) - 1  # -1 is because BERTopic returns an extra row for outlier detection

            # 각 토픽별로 상위 N개 단어 추출
            topic_words = []
            for topic_number in range(n_topics):
                # 각 토픽에 대한 상위 단어를 가져옵니다.
                topic = topic_model.get_topic(topic_number)
                
                # topic이 유효한 경우(즉, None이나 False가 아닌 경우)에만 처리합니다.
                if topic:
                    words = [word for word, _ in topic]
                    topic_words.append(words)

            # CoherenceModel 초기화 및 coherence score 계산
            coherence_model = CoherenceModel(topics=topic_words, texts=tokenized_docs, dictionary=dictionary, coherence='c_v')
            coherence_score = coherence_model.get_coherence()


            # 결과 저장
            results.append({
                    "UMAP_Params": umap_param,
                    "HDBSCAN_Params": hdbscan_param,
                    "Coherence_Score": coherence_score
            })
            print(f"Coherence Score: {coherence_score}\n")
        except Exception as e:
            print(f"An error occurred: {e}. Continuing with next parameters.")
            continue

# 결과를 데이터 프레임으로 변환 및 출력
results_df = pd.DataFrame(results)
print(results_df)

results_df.to_csv('D:/대학원/논문/소논문/부동산_토픽모델링/gridsearch_umap_hdbscan0524.csv', index=False, encoding='cp949')


# 전체 데이터로 학습 시작
df = pd.read_csv('D:/대학원/논문/소논문/부동산_토픽모델링/부동산_수정_df.csv', encoding='utf-8')
# df = df.sample(frac=0.01) # test
df.columns
df.head

# 날짜 및 시간 문자열 변환 함수
def convert_korean_datetime(dt_str):
    # "오후"와 "오전"을 AM/PM으로 변환
    dt_str = dt_str.replace('오후', 'PM').replace('오전', 'AM')
    # pandas to_datetime 사용하여 날짜 및 시간 파싱
    return pd.to_datetime(dt_str, format='%Y.%m.%d. %p %I:%M')

# 날짜 칼럼 변환
df['날짜'] = df['날짜'].apply(convert_korean_datetime)

df = df[(df['날짜'].dt.year >= 2012) & (df['날짜'].dt.year <= 2022)]

# 특수문자로 시작하는 단어 제거 ([, {, <, @)
df['제목'] = df['제목'].str.replace(r'\[[^\]]*\]', '', regex=True)  # 대괄호 안의 내용 삭제
df['제목'] = df['제목'].str.replace(r'<[^>]*>', '', regex=True)    # 꺽쇠 괄호 안의 내용 삭제
df['제목'] = df['제목'].str.replace(r'\{[^\}]*\}', '', regex=True)  # 중괄호 안의 내용 삭제
df['제목'] = df['제목'].str.replace(r'@[^\s]+', '', regex=True)    # @로 시작하는 단어 삭제
# 특수 문자 제거 (문장부호 제외)
df['제목'] = df['제목'].str.replace(r'[^\w\s,.?!\'"]', '', regex=True)
# 한 글자 단어 제거
df['제목'] = df['제목'].str.replace(r'\b\w\b', '', regex=True)
# 그외 단어 제거
df['제목'] = df['제목'].str.replace(r'\d+년|\d+월|\d+일', ' ', regex=True).str.strip()
# 그외 단어 제거
df['제목'] = df['제목'].str.replace(r'들썩|벌써|쑥쑥|헤드라인|머니투데이', ' ', regex=True).str.strip()
# 그외 단어 제거
df['제목'] = df['제목'].str.replace(r'뉴스|오늘|오프라인|종목', ' ', regex=True).str.strip()
# 그외 단어 제거
df['제목'] = df['제목'].str.replace(r'전하|매경|우리은행|레이더|글쎄', ' ', regex=True).str.strip()
# 불필요한 공백 제거
df['제목'] = df['제목'].str.replace(r'\s+', ' ', regex=True).str.strip()

# Okt 형태소 분석기 인스턴스 생성
okt = Okt()

# 텍스트를 토큰화하
def tokenize(text):
    tokens = okt.morphs(text)  # 텍스트를 형태소 단위로 토큰화
    return tokens

# 결측값을 빈 문자열로 대체하고, 모든 입력을 문자열로 변환
df['제목'] = df['제목'].fillna('').astype(str)

# '제목' 컬럼에 대해 토큰화
df["제목"] = df["제목"].apply(tokenize)

# 비문자열 데이터 타입 변환
df['제목'] = df['제목'].astype(str)
type(df['제목'])

# 문서 데이터 준비
documents = df['제목'].tolist()

# tokenized_docs 정의: 이는 원본 문서를 토큰화한 리스트입니다.
tokenized_docs = [ast.literal_eval(doc) for doc in documents]

# gensim 사전과 코퍼스 생성
dictionary = Dictionary(tokenized_docs)
corpus = [dictionary.doc2bow(text) for text in tokenized_docs]

# 결과를 저장할 리스트
results = []

# 문서 임베딩 함수 정의 (배치 처리 포함):
def embed_documents(documents, model, tokenizer, device='cuda', batch_size=16):
    model.to(device)
    embeddings = []
    for i in range(0, len(documents), batch_size):
        batch_docs = documents[i:i+batch_size]
        inputs = tokenizer(batch_docs, return_tensors='pt', padding=True, truncation=True, max_length=512).to(device)
        with torch.no_grad():
            outputs = model(**inputs)
        batch_embeddings = outputs.last_hidden_state.mean(dim=1).cpu().numpy()
        embeddings.append(batch_embeddings)
    return np.vstack(embeddings)

# KR-BERT 모델 초기화
tokenizer = BertTokenizer.from_pretrained('snunlp/KR-BERT-char16424')
model = BertModel.from_pretrained('snunlp/KR-BERT-char16424')


# UMAP과 HDBSCAN 객체를 생성하고 BERTopic에 전달하는 방법
umap_model = UMAP(n_neighbors=5, n_components=2, min_dist=0.0, metric='euclidean')
hdbscan_model = HDBSCAN(min_cluster_size=200, min_samples=100, metric='euclidean', prediction_data=True)

# BERTopic 모델 초기화 및 훈련
topic_model = BERTopic(embedding_model=lambda docs: embed_documents(docs, model, tokenizer),
                       umap_model=umap_model,
                       hdbscan_model=hdbscan_model,
                       verbose=True,
                       calculate_probabilities=True)

# BERTopic 모델을 사용하여 주제 추출
topics, probabilities = topic_model.fit_transform(df['제목'].tolist())

# 토픽 축소
reduced_topics = topic_model.reduce_topics(df['제목'].tolist(), nr_topics = 20)

# 각 문서에 대한 주제 확률 분포의 평균을 계산
df['topic_weight'] = [np.mean(prob) if prob.size > 0 else 0 for prob in probabilities]
df = df[['제목','날짜','topic_weight']]

# 모든 토픽의 키워드 확인
all_topics = topic_model.get_topics()

for topic_num, topic_keywords in all_topics.items():
    print(f"토픽 {topic_num}:")
    for keyword, weight in topic_keywords:
        print(f"  {keyword}: {weight}")
    print("\n")

topic_data = []

# 모든 토픽의 키워드를 DataFrame으로 변환
topic_data = []
for topic_num, topic_keywords in all_topics.items():
    for keyword, weight in topic_keywords:
        topic_data.append({"토픽 번호": topic_num, "키워드": keyword, "가중치": weight})

df_topics = pd.DataFrame(topic_data)
df_topics.to_csv('D:/대학원/논문/소논문/부동산_토픽모델링/df_topics_words0525.csv', index=False, encoding='cp949')


# CSV 파일 불러오기
df_topics = pd.read_csv('D:/대학원/논문/소논문/부동산_토픽모델링/df_topics_words0525.csv', encoding='cp949')

# Visualization 
text = df['제목'].to_list()
date = df['날짜'].to_list()
len(text)
len(date)


# BERTopic 함수 적용
topics_over_time = topic_model.topics_over_time(text, date, global_tuning=True, evolution_tuning=True)

# frequency 추출
topics_over_time['Words']
topics_over_time['Frequency']

topics_over_time.to_csv('D:/대학원/논문/소논문/부동산_토픽모델링/topics_over_time0525.csv', index=False, encoding='utf-8')

topics_over_time = pd.read_csv('D:/대학원/논문/소논문/부동산_토픽모델링/topics_over_time0525.csv', encoding='utf-8')

# Topic의 빈도를 확인
topic_frequencies = topics_over_time['Topic'].value_counts()
print(topic_frequencies)

# Topic이 -1이 아닌 행만 선택
filtered_11_topics = topics_over_time[topics_over_time['Topic'] != -1]


# Visualization 1
topic_model.visualize_topics_over_time(filtered_11_topics).show()

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

# 종료 시간 기록
end_time = time.time()

elapsed_time = end_time - start_time
print(f"코드 실행 시간: {elapsed_time} 초")


