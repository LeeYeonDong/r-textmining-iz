# 라이브러리 임포트 및 데이터 로드`
import pandas as pd
from bertopic import BERTopic
from transformers import BertTokenizer, BertModel
from transformers import BertTokenizerFast, AlbertModel
import numpy as np
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

# GPU 사용 가능 여부 확인
if torch.cuda.is_available():
    print("GPU is available.")
    print("GPU Name:", torch.cuda.get_device_name(0))
else:
    print("GPU is not available.")


# 시작 시간 기록
start_time = time.time()

# 데이터 로드
df = pd.read_csv('D:/대학원/논문/소논문/부동산_토픽모델링/부동산_df.csv', encoding='cp949')
df = df.sample(n=50000) # test
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

# '제목' 열에서 한글만 남기기
df['제목'] = df['제목'].apply(lambda x: re.sub(r'[^가-힣\s]', '', str(x)) if x is not None else x)
df['제목'] = df['제목'].apply(lambda x: re.sub(r'단독', '', x))

# 전처리
stopwords_df = pd.read_csv('D:/대학원/논문/소논문/부동산_토픽모델링/stopwords_kor.csv')
stopwords_df2 = pd.read_csv('D:/대학원/논문/소논문/부동산_토픽모델링/stopwords_kor2.csv')

# 두 데이터 프레임 합치기
stopwords_df = pd.concat([stopwords_df, stopwords_df2])
# 중복 제거를 원하는 경우
stopwords_df = stopwords_df.drop_duplicates().reset_index(drop=True)

stopwords = list(stopwords_df['stopwords_kor'])

# Okt 형태소 분석기 인스턴스 생성
okt = Okt()

# 텍스트를 토큰화하고 불용어를 제거하는 함수
def tokenize_and_remove_stopwords(text):
    tokens = okt.morphs(text)  # 텍스트를 형태소 단위로 토큰화
    tokens_without_stopwords = [token for token in tokens if token not in stopwords]  # 불용어 제거
    return tokens_without_stopwords

# '제목' 컬럼에 대해 토큰화 및 불용어 제거 수행
df['제목'] = df['제목'].apply(tokenize_and_remove_stopwords)

# 결측치 제거
df = df.dropna(subset=['제목'])
df.isnull().sum()

# 비문자열 데이터 타입 변환
df['제목'] = df['제목'].astype(str)

# KR-BERT 모델 초기화
tokenizer = BertTokenizer.from_pretrained('snunlp/KR-BERT-char16424')
model = BertModel.from_pretrained('snunlp/KR-BERT-char16424')

# albert-base-kor
#tokenizer_albert = BertTokenizerFast.from_pretrained("kykim/albert-kor-base")
#model_albert = AlbertModel.from_pretrained("kykim/albert-kor-base")

## Grid Search
# 1단계: Coherence Score 계산을 위한 준비
def calculate_coherence_score(model, documents, topics):
    """
    Coherence Score 계산.

    Parameters:
    - model: BERTopic 모델 인스턴스.
    - documents: 원본 문서 리스트.
    - topics: fit_transform 결과로 얻은 주제 할당 리스트.

    Returns:
    - Coherence Score.
    """
    # Gensim 사전 생성
    texts = [doc.split() for doc in documents]
    dictionary = Dictionary(texts)
    corpus = [dictionary.doc2bow(text) for text in texts]

    # 주제별 상위 단어 추출
    topic_words = []
    for topic_number in set(topics) - {-1}:
        topic_words.append([word for word, _ in model.get_topic(topic_number)])

    # Coherence Score 계산
    coherence_model = CoherenceModel(topics=topic_words, texts=texts, dictionary=dictionary, corpus=corpus, coherence='c_v')
    coherence_score = coherence_model.get_coherence()

    return coherence_score

# 2단계: Grid Search 실행
def run_grid_search(documents, umap_params_list, hdbscan_params_list):
    best_coherence = -np.inf
    best_params = None

    for umap_params in umap_params_list:
        for hdbscan_params in hdbscan_params_list:
            # UMAP과 HDBSCAN 객체 초기화
            umap_model = UMAP(**umap_params)
            hdbscan_model = HDBSCAN(**hdbscan_params)

            # BERTopic 모델 훈련
            topic_model = BERTopic(embedding_model=lambda docs: embed_documents(docs, model, tokenizer),
                                   umap_model=umap_model,
                                   hdbscan_model=hdbscan_model,
                                   calculate_probabilities=False,
                                   verbose=False)
            topics, probabilities = topic_model.fit_transform(documents)

            # Coherence Score 계산
            coherence = calculate_coherence_score(topic_model, documents, topics)
            print(f"UMAP params: {umap_params}, HDBSCAN params: {hdbscan_params}, Coherence: {coherence}")

            # 최적 파라미터 업데이트
            if coherence > best_coherence:
                best_coherence = coherence
                best_params = {'umap': umap_params, 'hdbscan': hdbscan_params}

    print(f"Best Coherence: {best_coherence}")
    print(f"Best Parameters: {best_params}")
    
# 3단계: 실행
# 예시 UMAP과 HDBSCAN 파라미터 리스트 정의
umap_params_list = [
    {'n_neighbors': 15, 'n_components': 5, 'min_dist': 0.0, 'metric': 'cosine'},
    # 다른 UMAP 파라미터 조합
]

hdbscan_params_list = [
    {'min_cluster_size': 5, 'min_samples': None, 'metric': 'euclidean'},
    # 다른 HDBSCAN 파라미터 조합
]

# Grid Search 실행 
run_grid_search(df['제목'], umap_params_list, hdbscan_params_list)

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

# UMAP과 HDBSCAN 객체를 생성하고 BERTopic에 전달하는 방법
umap_model = UMAP(n_neighbors=15, n_components=3, min_dist=0.0, metric='cosine')
hdbscan_model = HDBSCAN(min_cluster_size=100, min_samples=None, metric='euclidean', prediction_data=True)

# BERTopic 모델 초기화 및 훈련
topic_model = BERTopic(embedding_model=lambda docs: embed_documents(docs, model, tokenizer),
                       umap_model=umap_model,
                       hdbscan_model=hdbscan_model,
                       verbose=True,
                       calculate_probabilities=True)

# embedding_model="all-MiniLM-L6-v2"은 문장 임베딩 생성에 특화되어 있으며, 다국어 환경에서의 문장 유사성 비교, 클러스터링, 정보 검색 등에 유용, 한국어는 kr-bert가 나음

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
df_topics.to_csv('D:/대학원/논문/소논문/부동산_토픽모델링/df_topics_words.csv', index=False, encoding='cp949')

# Visualization 
text = df['제목'].to_list()
date = df['날짜'].to_list()
len(text)
len(date)
topics_over_time = topic_model.topics_over_time(text, date, global_tuning=True, evolution_tuning=True)

# frequency 추출
topics_over_time['Words']
topics_over_time['Frequency']

topics_over_time.to_csv('D:/대학원/논문/소논문/부동산_토픽모델링/topics_over_time.csv', index=False, encoding='cp949')

# Visualization 1
topic_model.visualize_topics_over_time(topics_over_time).show()

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

