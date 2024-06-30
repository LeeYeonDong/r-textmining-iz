## BERT 임베딩 계산 -> TextRank 계산 -> 결과 출력
import torch
import torch.nn.functional as F
from transformers import BertTokenizer, BertModel
import networkx as nx
from sklearn.metrics.pairwise import cosine_similarity
import numpy as np

# BERT 모델과 토크나이저 로드
tokenizer = BertTokenizer.from_pretrained('bert-base-uncased')
model = BertModel.from_pretrained('bert-base-uncased')

# 예제 단락
paragraph = """
Artificial intelligence (AI) has seen rapid advancements in recent years. Natural language processing (NLP) is one of the key fields in AI. Machine learning algorithms significantly enhance the performance of NLP applications. Deep learning has revolutionized the approach to many NLP tasks. Text classification is a crucial aspect of NLP. Named entity recognition (NER) identifies important entities within text. Sentiment analysis determines the emotional tone of a document. Language models like GPT-3 can generate human-like text. Tokenization splits text into meaningful units. Part-of-speech tagging assigns grammatical categories to words. Dependency parsing shows relationships between words in a sentence. Word embeddings represent words as continuous vectors. TextRank is used for keyword extraction and text summarization. Chatbots are becoming increasingly common in customer service. Speech recognition converts spoken language into text. Machine translation systems automatically translate text from one language to another.
Despite some progress, NLP models are not always perfect and can still make mistakes. 
It's not uncommon for state-of-the-art NLP models to sometimes fail to capture the full meaning of a sentence. 
NLP models are exceptionally accurate in specific tasks. 
NLP models might sometimes appear somewhat imprecise.
NLP, more than any other field, has the potential to fundamentally change human-computer interaction.
"""

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



## BERT 임베딩 계산 -> Self-Attention 적용 -> TextRank 계산 -> 결과 출력
import torch
import torch.nn.functional as F
from transformers import BertTokenizer, BertModel
import networkx as nx
from sklearn.metrics.pairwise import cosine_similarity
import numpy as np

# 문장 분할 및 빈 문자열 제거
sentences = [sentence.strip() for sentence in paragraph.split('.') if sentence.strip()]

# BERT 임베딩 계산 함수
def get_sentence_embedding(sentence):
    inputs = tokenizer(sentence, return_tensors='pt', truncation=True, max_length=512)
    outputs = model(**inputs)
    return outputs.last_hidden_state.mean(dim=1).squeeze().detach().numpy()

# 문장 임베딩 계산
sentence_embeddings = np.array([get_sentence_embedding(sentence) for sentence in sentences])

# Self-Attention 기반 유사도 계산 함수
def distance_scaled_attention(Q, K, V, positions, emphasis_positions, mask=None):
    d_k = Q.size(-1)
    scores = torch.matmul(Q, K.transpose(-2, -1)) / torch.sqrt(torch.tensor(d_k, dtype=torch.float))

    # 강조된 단어에 더 높은 가중치 부여 (강조 가중치 증가)
    emphasis_penalty = torch.ones(scores.shape)
    for i, position_list in enumerate(emphasis_positions):
        for pos in position_list:
            emphasis_penalty[i, pos] *= 5  # 강조 단어에 더 높은 가중치 부여 (가중치를 5로 설정)

    scores = scores * emphasis_penalty

    if mask is not None:
        scores = scores.masked_fill(mask == 0, -1e9)

    weights = F.softmax(scores, dim=-1)
    output = torch.matmul(weights, V)
    return output, weights

# 예제 데이터: Self-Attention 적용을 위한 임베딩 및 위치 설정
batch_size, num_tokens, d_model = len(sentences), max(len(sentence.split()) for sentence in sentences), 768  # BERT 임베딩 크기 768
Q = torch.rand(batch_size, num_tokens, d_model)
K = torch.rand(batch_size, num_tokens, d_model)
V = torch.rand(batch_size, num_tokens, d_model)

# 문장 내 단어 위치 및 강조 위치 설정
positions = [sentence.split() for sentence in sentences]
emphasis_words = ['not', 'never', 'without', 'perhaps', 'might', 'possibly', 'seems', 'potentially', 'to some extent']
emphasis_positions = [[i for i, word in enumerate(sentence) if any(emph in word for emph in emphasis_words)] for sentence in positions]

# Self-Attention 적용
output, weights = distance_scaled_attention(Q, K, V, positions, emphasis_positions)
# Self-Attention 결과를 sentence_embeddings에 반영
adjusted_embeddings = output.mean(dim=1).detach().numpy()

# 유사도 행렬 계산
similarity_matrix = cosine_similarity(adjusted_embeddings)
graph = nx.from_numpy_array(similarity_matrix)
scores = nx.pagerank(graph)

# 결과 출력
ranked_sentences = sorted(((scores[i], s) for i, s in enumerate(sentences)), reverse=True)
print("두번째 방법 - 중요한 문장:")
for score, sentence in ranked_sentences:
    print(f"{score:.4f}: {sentence}")