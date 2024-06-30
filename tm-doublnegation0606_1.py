# textRank
## BERT 임베딩 계산 -> TextRank 계산 -> 결과 출력
import torch
import torch.nn.functional as F
from transformers import BertTokenizer, BertModel
import networkx as nx
from sklearn.metrics.pairwise import cosine_similarity
import numpy as np
import pandas as pd
import re
import nltk
from nltk.tokenize import word_tokenize
from nltk.tag import pos_tag
from nltk.corpus import stopwords
from collections import Counter

# NLTK 데이터 다운로드 (최초 1회만 실행)
nltk.download('punkt')
nltk.download('averaged_perceptron_tagger')
nltk.download('stopwords')


# BERT 모델과 토크나이저 로드
tokenizer = BertTokenizer.from_pretrained('bert-base-uncased')
model = BertModel.from_pretrained('bert-base-uncased', output_attentions=True)

# 메모장 파일을 불러오는 함수
def load_text_file(file_path):
    with open(file_path, 'r', encoding='utf-8') as file:
        content = file.read()
    return content

# 파일 경로 지정
file_path = 'D:/대학원/논문/Double Negation/ihaveadream.txt'

# 파일 내용 불러오기
paragraph = load_text_file(file_path)


# 문장 분할 및 빈 문자열 제거
sentences = [sentence.strip() for sentence in paragraph.split('.') if sentence.strip()]

# BERT 임베딩 계산 함수
def get_sentence_embedding(sentence):
    inputs = tokenizer(sentence, return_tensors='pt', truncation=True, max_length=512)
    outputs = model(**inputs)
    return outputs.last_hidden_state.mean(dim=1).squeeze().detach().numpy()

# 1. BERT 임베딩 계산
sentence_embeddings = np.array([get_sentence_embedding(sentence) for sentence in sentences])

# 2. 유사도 행렬 계산
similarity_matrix = cosine_similarity(sentence_embeddings)

# 3. TextRank 계산
graph = nx.from_numpy_array(similarity_matrix)
scores = nx.pagerank(graph)

# 5. 결과 출력
ranked_sentences = sorted(((scores[i], s) for i, s in enumerate(sentences)), reverse=True)
print("중요한 문장:")
for score, sentence in ranked_sentences:
    print(f"{score:.4f}: {sentence}")

# 점수와 문장을 결합하여 새로운 리스트 생성
combined_data = [f"{score:.4f}: {sentence}" for score, sentence in ranked_sentences]

# 데이터 프레임 생성 (단일 열)
df_textrank = pd.DataFrame(combined_data, columns=['textrank'])

time.sleep(10)
      
# attention 가중치 사용
# BERT 임베딩 및 어텐션 가중치 계산 함수
def get_sentence_embedding_and_attention(sentence):
    inputs = tokenizer(sentence, return_tensors='pt', truncation=True, max_length=512)
    outputs = model(**inputs)
    
    # 어텐션 가중치가 있는지 확인
    if outputs.attentions:
        attention = outputs.attentions[-1].mean(dim=1).squeeze().detach().numpy()
    else:
        attention = None

    embedding = outputs.last_hidden_state.mean(dim=1).squeeze().detach().numpy()
    return embedding, attention

# 1. BERT 임베딩 및 어텐션 가중치 계산
sentence_embeddings = []
attention_scores = []

for sentence in sentences:
    embedding, attention = get_sentence_embedding_and_attention(sentence)
    sentence_embeddings.append(embedding)
    if attention is not None:
        attention_scores.append(attention.mean())
    else:
        attention_scores.append(0)  # 어텐션 가중치가 없는 경우 기본값 0 사용

sentence_embeddings = np.array(sentence_embeddings)

# 2. 유사도 행렬 계산
similarity_matrix = cosine_similarity(sentence_embeddings)

# 3. 가중치 계산 및 초기 점수 설정
initial_scores = np.array(attention_scores)
graph = nx.from_numpy_array(similarity_matrix)

# 4. TextRank 계산 (가중치 적용)
scores = nx.pagerank(graph, personalization=dict(enumerate(initial_scores)))

# 5. 결과 출력
ranked_sentences = sorted(((scores[i], s) for i, s in enumerate(sentences)), reverse=True)
print("중요한 문장:")
for score, sentence in ranked_sentences:
    print(f"{score:.4f}: {sentence}")

# 점수와 문장을 결합하여 새로운 리스트 생성
combined_data = [f"{score:.4f}: {sentence}" for score, sentence in ranked_sentences]

# 데이터 프레임 생성 (단일 열)
df_attention = pd.DataFrame(combined_data, columns=['attention'])

time.sleep(10)


# self attention-이중부정 추가
# 부정어 목록 정의
negations = ["not", "no", "never", "none", "nothing", "neither", "nobody", "nowhere", "hardly", "barely", "scarcely"]

mitigators = [
    "possible", "might", "could", "may", "perhaps", "seems", "potentially",
    "likely", "approximately", "around", "roughly", "almost", "nearly",
    "sort of", "kind of", "relatively", "fairly", "somewhat", "moderately",
    "usually", "generally", "often", "frequently", "typically", "probably",
    "virtually", "practically", "somehow", "arguably", "occasionally", "periodically"
]


# BERT 임베딩 및 어텐션 가중치 계산 함수
def get_sentence_embedding_and_attention(sentence):
    inputs = tokenizer(sentence, return_tensors='pt', truncation=True, max_length=512)
    outputs = model(**inputs)
    
    if outputs.attentions:
        attention = outputs.attentions[-1].mean(dim=1).squeeze().detach().numpy()
    else:
        attention = None

    embedding = outputs.last_hidden_state.mean(dim=1).squeeze().detach().numpy()
    return embedding, attention


# 문장 가중치 계산 함수
def calculate_weight(sentence, attention):
    weight = attention.mean() if attention is not None else 0
    negation_count = sum(1 for neg in negations if neg in sentence)
    if negation_count >= 2:  # 이중 부정 체크
        weight += 0.3
    for mitigator in mitigators:
        if mitigator in sentence:
            weight += 0.2
            break
    return weight

# 1. BERT 임베딩 및 어텐션 가중치 계산
sentence_embeddings = []
attention_scores = []

for sentence in sentences:
    embedding, attention = get_sentence_embedding_and_attention(sentence)
    sentence_embeddings.append(embedding)
    attention_scores.append(calculate_weight(sentence, attention))

sentence_embeddings = np.array(sentence_embeddings)

# 2. 유사도 행렬 계산
similarity_matrix = cosine_similarity(sentence_embeddings)

# 3. TextRank 계산 (가중치 적용)
initial_scores = np.array(attention_scores)
graph = nx.from_numpy_array(similarity_matrix)
scores = nx.pagerank(graph, personalization=dict(enumerate(initial_scores)))

# 4. 결과 출력
ranked_sentences = sorted(((scores[i], s) for i, s in enumerate(sentences)), reverse=True)
print("중요한 문장:")
for score, sentence in ranked_sentences:
    print(f"{score:.4f}: {sentence}")

# 점수와 문장을 결합하여 새로운 리스트 생성
combined_data = [f"{score:.4f}: {sentence}" for score, sentence in ranked_sentences]

# 데이터 프레임 생성 (단일 열)
df_DN_attention = pd.DataFrame(combined_data, columns=['DN_attention'])

time.sleep(10)


# DN_attention_etc
# 부정어 목록 정의
negations = ["not", "no", "never", "none", "nothing", "neither", "nobody", "nowhere", "hardly", "barely", "scarcely"]

mitigators = [
    "possible", "might", "could", "may", "perhaps", "seems", "potentially",
    "likely", "approximately", "around", "roughly", "almost", "nearly",
    "sort of", "kind of", "relatively", "fairly", "somewhat", "moderately",
    "usually", "generally", "often", "frequently", "typically", "probably",
    "virtually", "practically", "somehow", "arguably", "occasionally", "periodically"
]

# 단어 빈도 계산 함수
def get_most_frequent_words(text, top_n=10):
    stop_words = set(stopwords.words('english'))
    words = word_tokenize(text.lower())
    words = [word for word in words if word.isalnum() and word not in stop_words]
    word_freq = Counter(words)
    most_common_words = [word for word, freq in word_freq.most_common(top_n)]
    return most_common_words

# 텍스트에서 가장 빈번한 단어 추출
important_keywords = get_most_frequent_words(paragraph, top_n=10)
print(f"Important Keywords: {important_keywords}")

# BERT 임베딩 및 어텐션 가중치 계산 함수
def get_sentence_embedding_and_attention(sentence):
    inputs = tokenizer(sentence, return_tensors='pt', truncation=True, max_length=512)
    outputs = model(**inputs)
    
    if outputs.attentions:
        attention = outputs.attentions[-1].mean(dim=1).squeeze().detach().numpy()
    else:
        attention = None

    embedding = outputs.last_hidden_state.mean(dim=1).squeeze().detach().numpy()
    return embedding, attention

# 문장 가중치 계산 함수
def calculate_weight(sentence, attention, position, total_sentences):
    weight = attention.mean() if attention is not None else 0
    negation_count = sum(1 for neg in negations if neg in sentence)
    if negation_count >= 2:  # 이중 부정 체크
        weight += 0.3
    for mitigator in mitigators:
        if mitigator in sentence:
            weight += 0.2
            break
    for keyword in important_keywords:
        if keyword in sentence:
            weight += 0.1
            break
    if position == 0 or position == total_sentences - 1:  # 첫 번째 문장 또는 마지막 문장
        weight += 0.1
    return weight

# 1. BERT 임베딩 및 어텐션 가중치 계산
sentence_embeddings = []
attention_scores = []

for i, sentence in enumerate(sentences):
    embedding, attention = get_sentence_embedding_and_attention(sentence)
    sentence_embeddings.append(embedding)
    attention_scores.append(calculate_weight(sentence, attention, i, len(sentences)))

sentence_embeddings = np.array(sentence_embeddings)

# 2. 유사도 행렬 계산
similarity_matrix = cosine_similarity(sentence_embeddings)

# 3. TextRank 계산 (가중치 적용)
initial_scores = np.array(attention_scores)
graph = nx.from_numpy_array(similarity_matrix)
scores = nx.pagerank(graph, personalization=dict(enumerate(initial_scores)))

# 4. 결과 출력
ranked_sentences = sorted(((scores[i], s) for i, s in enumerate(sentences)), reverse=True)
print("중요한 문장:")
for score, sentence in ranked_sentences:
    print(f"{score:.4f}: {sentence}")

# 점수와 문장을 결합하여 새로운 리스트 생성
combined_data = [f"{score:.4f}: {sentence}" for score, sentence in ranked_sentences]

# 데이터 프레임 생성 (단일 열)
df_DN_attention_etc = pd.DataFrame(combined_data, columns=['DN_attention_etc'])

# 데이터 프레임을 리스트로 저장
dfs = [df_textrank, df_attention, df_DN_attention, df_DN_attention_etc]

# 데이터 프레임 행으로 결합
combined_df = pd.concat(dfs, axis=1)

# 엑셀 파일로 저장
excel_filename = 'D:/대학원/논문/Double Negation/result.xlsx'
combined_df.to_excel(excel_filename, index=False)