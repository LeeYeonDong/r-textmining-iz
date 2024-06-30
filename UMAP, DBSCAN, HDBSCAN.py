# pip install umap-learn scikit-learn matplotlib seaborn plotly

import umap
import matplotlib.pyplot as plt
from sklearn.datasets import load_digits

# 데이터 로드
digits = load_digits()
data = digits.data
labels = digits.target

# UMAP 모델 생성 및 학습
reducer = umap.UMAP()
embedding = reducer.fit_transform(data)

# 시각화
plt.scatter(embedding[:, 0], embedding[:, 1], c=labels, cmap='Spectral', s=5)
plt.colorbar(boundaries=range(11))
plt.title('UMAP projection of the Digits dataset')
plt.show()


from sklearn.cluster import DBSCAN
from sklearn.datasets import make_moons
import seaborn as sns

# 데이터 생성
X, _ = make_moons(n_samples=300, noise=0.05, random_state=42)

# DBSCAN 모델 생성 및 학습
dbscan = DBSCAN(eps=0.3, min_samples=5)
clusters = dbscan.fit_predict(X)

# 시각화
sns.scatterplot(x=X[:, 0], y=X[:, 1], hue=clusters, palette='viridis')
plt.title('DBSCAN clustering')
plt.show()


import hdbscan
import plotly.express as px

# HDBSCAN 설치 필요
# pip install hdbscan

# 데이터 생성
X, _ = make_moons(n_samples=300, noise=0.05, random_state=42)

# HDBSCAN 모델 생성 및 학습
clusterer = hdbscan.HDBSCAN(min_cluster_size=5)
clusters = clusterer.fit_predict(X)

# 시각화
fig = px.scatter(x=X[:, 0], y=X[:, 1], color=clusters)
fig.update_layout(title='HDBSCAN clustering', xaxis_title='X', yaxis_title='Y')
fig.show()
