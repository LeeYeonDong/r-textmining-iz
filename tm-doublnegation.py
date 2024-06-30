import torch
import torch.nn.functional as F

def scaled_dot_product_attention(Q, K, V, mask=None):
    d_k = Q.size(-1)
    scores = torch.matmul(Q, K.transpose(-2, -1)) / torch.sqrt(torch.tensor(d_k, dtype=torch.float))

    if mask is not None:
        scores = scores.masked_fill(mask == 0, -1e9)

    weights = F.softmax(scores, dim=-1)
    output = torch.matmul(weights, V)
    return output, weights

def distance_scaled_attention(Q, K, V, positions, mask=None):
    d_k = Q.size(-1)
    scores = torch.matmul(Q, K.transpose(-2, -1)) / torch.sqrt(torch.tensor(d_k, dtype=torch.float))

    # 부정어 간의 거리 계산
    distance_matrix = torch.abs(positions[:, None] - positions[None, :])
    distance_penalty = 1 / (distance_matrix.float() + 1)  # 거리가 멀수록 패널티 증가, float로 변환

    # 스코어에 거리 패널티 적용
    scores = scores * distance_penalty

    if mask is not None:
        scores = scores.masked_fill(mask == 0, -1e9)

    weights = F.softmax(scores, dim=-1)
    output = torch.matmul(weights, V)
    return output, weights

# 예제 데이터
batch_size, num_tokens, d_model = 1, 5, 64
Q = torch.rand(batch_size, num_tokens, d_model)
K = torch.rand(batch_size, num_tokens, d_model)
V = torch.rand(batch_size, num_tokens, d_model)
positions = torch.tensor([0, 2, 4, 7, 9])  # 부정어 위치 예시

output, weights = distance_scaled_attention(Q, K, V, positions)
print(output)
