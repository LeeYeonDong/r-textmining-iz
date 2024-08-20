# 1. 단어 동시 출현 네트워크 (Co-occurrence Network Graph)
# 패키지 설치 및 로드
install.packages(c("tidytext", "dplyr", "tidyr", "igraph", "ggraph", "widyr"))
library(tidytext)
library(dplyr)
library(tidyr)
library(igraph)
library(widyr)
library(ggraph)

set.seed(1029)

# 예시 데이터
text_data <- tibble(text = c("I love data science",
                                 "Data science is fun",
                                 "I love learning new things",
                                 "Science is fascinating"))

# 단어 토큰화 및 문서 ID 추가
tidy_text <- text_data %>%
  mutate(document = row_number()) %>%  # 문서 ID 생성
  unnest_tokens(word, text)  # 단어 토큰화

# 단어 동시 출현 계산
word_pairs <- tidy_text %>%
  pairwise_count(document, word, sort = TRUE, upper = FALSE)

# 네트워크 그래프 생성
graph <- word_pairs %>%
  graph_from_data_frame(directed = FALSE)

# 그래프의 노드 이름 설정
V(graph)$name <- as.character(V(graph))

# 네트워크 그래프 시각화
graph %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(width = n), alpha = 0.8) +  
  geom_node_point(size = 10, color = "skyblue") + 
  geom_node_text(aes(label = name), vjust = 0, hjust = 0, size = 5) +  
  theme_void() 


# 2. 단어 간 상관 분석 (Phi Coefficient Network Graph)
# 예시 데이터
text_data <- tibble(text = c("I love data science",
                                 "Data science is fun",
                                 "I love learning new things",
                                 "Science is fascinating"))

# 단어 토큰화 및 문서 ID 추가
tidy_text <- text_data %>%
  unnest_tokens(word, text) %>%
  mutate(document = row_number())

# 단어 간 phi coefficient 계산
word_cors <- tidy_text %>%
  group_by(word) %>%
  filter(n() >= 2) %>%
  pairwise_cor(word, document, sort = TRUE)

# 네트워크 그래프 생성
graph <- word_cors %>%
  graph_from_data_frame()

graph %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation)) +
  geom_node_point(size = 10, color = "skyblue") + 
  geom_node_text(aes(label = name), vjust = 0, hjust = 0, size = 5) +  
  theme_void() 


# 3. 연이은 단어 쌍 분석 (N-gram Analysis)
# 예시 데이터
text_data <- tibble(text = c("I love data science",
                                 "Data science is fun",
                                 "I love learning new things",
                                 "Science is fascinating"))

# N-gram 생성 (2-gram 예시)
bigrams <- text_data %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

# N-gram 빈도수 계산
bigram_counts <- bigrams %>%
  count(bigram, sort = TRUE)

# bigram을 두 개의 단어로 분할
bigram_separated <- bigram_counts %>%
  separate(bigram, into = c("word1", "word2"), sep = " ")

# 네트워크 그래프 생성
bigram_graph <- bigram_separated %>%
  graph_from_data_frame()

# 네트워크 그래프 시각화
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(width = n), alpha = 0.8) +  # 빈도에 따라 선의 굵기 조절
  geom_node_point(size = 10, color = "skyblue") +  # 노드의 크기 설정
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, size = 5) +  # 텍스트 크기 조절
  theme_void()  # 배경을 깔끔하게 하기 위한 테마


# 4. 의미 연결망 (Semantic Network Analysis)
# 패키지 설치 및 로드
library(tm)
library(igraph)
library(ggraph)

# 예시 데이터 (말뭉치 생성)
docs <- Corpus(VectorSource(c("I love data science",
                              "Data science is fun",
                              "I love learning new things",
                              "Science is fascinating")))

# 단어-문서 행렬 생성
tdm <- TermDocumentMatrix(docs)

# 행렬을 데이터 프레임으로 변환
m <- as.matrix(tdm)
term_matrix <- m %*% t(m)

# 네트워크 그래프 생성
graph <- graph_from_adjacency_matrix(term_matrix, weighted = TRUE, diag = FALSE)

ggraph(graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = weight)) +
  geom_node_point(size = 10, color = "skyblue") +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, size = 5) +  # 텍스트 크기 조절
  theme_void()  # 배경을 깔끔하게 하기 위한 테마


# 5. 연관 분석 (Association Analysis)
library(arules)
library(arulesViz)

# 예시 데이터
transactions <- list(
  c("milk", "bread", "butter"),
  c("beer", "bread"),
  c("milk", "bread", "butter", "beer"),
  c("milk", "butter")
)

# 트랜잭션 데이터로 변환
trans <- as(transactions, "transactions")

# 연관 규칙 생성
rules <- apriori(trans, parameter = list(supp = 0.5, conf = 0.8))

# 연관 규칙 시각화
plot(rules, method = "graph", control = list(layout = "stress", circular = FALSE))
# plot창 크게 키우기


# 6. 사용자 이분 그래프모형(bipartite graph)
# 패키지 설치 및 로드
library(igraph)
library(ggraph)

# 예시 데이터
users_items <- data.frame(
  user = c("User1", "User1", "User2", "User2", "User3", "User4"),
  item = c("ItemA", "ItemB", "ItemA", "ItemC", "ItemB", "ItemC")
)

# 이분 그래프 생성
g <- graph_from_data_frame(users_items, directed = FALSE)
V(g)$type <- bipartite_mapping(g)$type

# 이분 그래프 시각화
ggraph(g, layout = "fr") +
  geom_edge_link() +
  geom_node_point(aes(color = V(g)$type)) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)



#### 05-1 동시 출현 단어 분석: Co-occurrence analysis
### 기본적인 전처리
library(readr)
library(dplyr)
library(stringr)
library(textclean)
library(tidytext)
library(KoNLP)

raw_news_comment <- read_csv("D:/대학원/강의/2024-2 실험계획법/Doit_textmining-main/Data/news_comment_parasite.csv")

news_comment <- raw_news_comment %>%
  select(reply) %>%
  mutate(reply = str_replace_all(reply, "[^가-힣]", " "),
         reply = str_squish(reply),
         id = row_number())

## 1. 형태소 분석기를 이용해 품사 기준으로 토큰화하기
comment_pos <- news_comment %>%
  unnest_tokens(input = reply,
                output = word,
                token = SimplePos22,
                drop = F)

comment_pos %>%
  select(word, reply)

## 2. 품사 분리하여 행 구성하기
comment_pos <- comment_pos %>%
  separate_rows(word, sep = "[+]")

comment_pos$word %>% head()

comment_pos %>%
  select(word, reply)

## 3. 품사 추출하기
# 명사 추출하기
comment_pos <- comment_pos %>%
  separate(word, into = c("word", "Pos22"), sep = "/")

noun <- comment_pos %>%
  filter(str_detect(Pos22, "n"))

# 명사 빈도 구하기
noun %>%
  count(word, sort = TRUE)

# 동사, 형용사, 명사 추출하기
pvpa <- comment_pos %>%
  filter(Pos22 %in% c("pv", "px", "pa", "nc", "nn"))


### 단어 동시 출현 빈도 구하기
library(widyr)

pair <- comment_pos %>%
  pairwise_count(item = word,
                 feature = id,
                 sort = T)
pair

pair <- pvpa %>%
  filter(nchar(word) >= 2) %>% 
  pairwise_count(item = word,
                 feature = id,
                 sort = T)
pair
