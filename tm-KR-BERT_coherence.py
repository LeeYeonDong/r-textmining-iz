# 문서 데이터 준비
documents = df['제목'].tolist()

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
umap_model = UMAP(n_neighbors=30, n_components=15, min_dist=0.2, metric='cosine')
hdbscan_model = HDBSCAN(min_cluster_size=20, min_samples=None, metric='euclidean', prediction_data=True)

# BERTopic 모델 초기화 및 훈련
topic_model = BERTopic(embedding_model=lambda docs: embed_documents(docs, model, tokenizer),
                       umap_model=umap_model,
                       hdbscan_model=hdbscan_model,
                       verbose=True,
                       calculate_probabilities=True)

# embedding_model="all-MiniLM-L6-v2"은 문장 임베딩 생성에 특화되어 있으며, 다국어 환경에서의 문장 유사성 비교, 클러스터링, 정보 검색 등에 유용, 한국어는 kr-bert가 나음

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

# tokenized_docs 정의: 이는 원본 문서를 토큰화한 리스트입니다.
tokenized_docs = [ast.literal_eval(doc) for doc in documents]
print(type(tokenized_docs))


# gensim 사전과 코퍼스 생성
dictionary = Dictionary(tokenized_docs)
corpus = [dictionary.doc2bow(text) for text in tokenized_docs]

# CoherenceModel 초기화 및 coherence score 계산
coherence_model = CoherenceModel(topics=topic_words, texts=tokenized_docs, dictionary=dictionary, coherence='c_v')
coherence_score = coherence_model.get_coherence()









