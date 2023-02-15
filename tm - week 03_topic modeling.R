library(KoNLP)
library(tidyverse)

library(ggplot2)
library(tm)
library(qdap)
library(corpus)
library(wordcloud2)
library(stringr)
library(rJava)
library(tidytext)
library(dplyr)
library(reshape) 

# DocumentTermMatrix
raw1_before_dtm <- raw1_token_df %>% 
                    group_by(id) %>% 
                    count(단어, sort = TRUE)

raw1_dtm <- cast_dtm(raw1_before_dtm, document = id, term = 단어, value = n)
class(raw1_dtm)
View(as.matrix(raw1_dtm[1:10, 1:10]))

# Topic modeling
library(topicmodels)

# the number of topics - hyper parameter
library(ldatuning)

raw1_lda_n <- FindTopicsNumber(
  raw1_dtm,
  topics = 2:10,
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 12),
)

FindTopicsNumber_plot(raw1_lda_n)

# run LDA
set.seed(1029)
raw1_lda <- LDA(raw1_dtm, k = 7, method = "Gibbs", control = list(iter = 500, alpha = 1, delta = 0.1))

# Alpha and Beta Hyperparameters – alpha represents document-topic density and Beta represents topic-word density. Higher the value of alpha, documents are composed of more topics and lower the value of alpha, documents contain fewer topics. On the other hand, higher the beta, topics are composed of a large number of words in the corpus, and with the lower value of beta, they are composed of few words.
# alpha는 문서-토픽 밀도를 나타내며, beta는 토픽-단어 밀도를 나타냅니다. alpha 값이 높아지면 문서는 더 많은 토픽으로 구성되고, alpha 값이 낮아지면 문서는 적은 토픽으로 구성됩니다. 한편, beta 값이 높아지면 토픽은 많은 단어로 구성되고, beta 값이 낮아지면 토픽은 적은 단어로 구성됩니다.
# R 언어에서 Latent Dirichlet Allocation (LDA) 모델을 적합하기 위해 사용되는 Variational Inference 알고리즘의 수렴 허용 오차를 나타내는 delta 옵션에 대해서 설명합니다. delta는 현재 변분 파라미터의 추정과 이전 추정값 사이의 최대 차이를 지정하여 수렴 기준을 정합니다. delta의 값이 작으면 수렴 기준이 엄격해지며, 수렴에 걸리는 시간이 더 길어질 수 있지만, delta의 값이 크면 수렴이 빠르지만 변분 파라미터의 추정이 정확하지 않을 수 있습니다.

glimpse(raw1_lda)

terms(raw1_lda, 10)

raw1_topics <- tidy(raw1_lda, matrix = "beta")
# beta : 토픽에 단어가 들어갈 확률
# gamma : 문서가 각 토픽에 등장할 확률

raw1_top_terms <- raw1_topics %>% 
  group_by(topic) %>%
  top_n(20, beta) %>%
  arrange(topic, -beta)

raw1_top_terms  %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(x="", y="")


# 문서를 토픽별로 분류하기
raw1_topic <- tidy(raw1_lda, matrix = "gamma")
# gamma는 문서가 각 토픽에 등장할 확률

# 문서별로 확률이 가장 높은 토픽 추출
raw1_class <- raw1_topic %>% 
  group_by(document) %>% 
  slice_max(gamma, n = 1)
# slice_max: gamma가 가장 큰 행을 n = 1개 선택

##원문에 확률이 가장 높은 번호 부여
#integer로 변환
raw1_class$document <- as.integer(raw1_class$document)

raw1_class_topic <- left_join(raw1_token_df, raw1_class, by = c("id" = "document"))

# 결측치 제거
raw1_class_topic <- na.omit(raw1_class_topic)

# 토픽별 문서 수 살펴보기
raw1_class_topic_count <- count(raw1_class_topic, topic)

# 토픽별 주요 단어 목록 만들기
raw1_terms <- raw1_top_terms %>% 
  group_by(topic) %>%
  slice_max(beta, n = 5, with_ties = FALSE) %>% 
  summarise(term = paste(term, collapse = ", "))

# 문서 빈도에 주요단어 결합
raw1_topic_count_word <- raw1_class_topic_count %>% 
  left_join(raw1_terms, by = "topic")

# 토픽별 문서 수와 주요 단어로 막대 그래프 만들기
raw1_topic_count_word %>% 
  ggplot(aes(x = topic, y = n, fill = as.factor(topic))) +
  geom_col() +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.1, size = 5) +
  geom_text(aes(label = term),hjust = 1.1,vjust = 0.5, col = "black",fontface = "bold",size = 5)


# analytical look at the word frequencies per topic
raw1_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta))


# using
# install.packages("LDAvis")
library(LDAvis)

# using serVis
topicmodels2LDAvis <- function(x){
  post <- topicmodels::posterior(x)
  if (ncol(post$topics) < 3) stop("The model must contain >= 3 topics")
  mat <- x@wordassignments
  LDAvis::createJSON(
    phi = post$terms, 
    theta = post$topics,
    vocab = colnames(post$terms),
    doc.length = slam::row_sums(mat, na.rm = TRUE),
    term.frequency = slam::col_sums(mat, na.rm = TRUE)
  )
}

raw1_lda %>% glimpse()

# posterior(raw1_lda)$topics = posterior(raw1_lda)[["topics"]]
serVis(topicmodels2LDAvis(raw1_lda))
raw1_lda %>% topicmodels2LDAvis() %>% serVis()


# ############
# topicmodels_json_ldavis <- function(fitted, corpus, doc_term){
#   ## Find required quantities
#   phi <- posterior(fitted)$terms %>% as.matrix
#   theta <- posterior(fitted)$topics %>% as.matrix
#   vocab <- colnames(phi)
#   doc_length <- slam::row_sums(doc_term, na.rm = TRUE)
#   temp_frequency <- slam::col_sums(doc_term, na.rm = TRUE)
# 
#   ## Convert to json
#   json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
#                                  vocab = vocab,
#                                  doc.length = doc_length,
#                                  term.frequency = temp_frequency)
#   
#   return(json_lda)
# }
# 
# # data Optimize
# # fitted
# raw1_lda
# 
# # corpus
# raw1_tokens <- raw1_token_df %>% select(id, 단어)
# names(raw1_tokens) <- c("doc_id", "text")
# 
# raw1_corpus <- VCorpus(DataframeSource(raw1_tokens))
# 
# # doc_term
# raw1_dtm <- raw1_dtm[slam::row_sums(raw1_dtm)>0,]
# 
# # option
# serVis(topicmodels_json_ldavis(raw1_lda, raw1_corpus, raw1_dtm))

# Topic proportions over time
raw1
raw1$일자 <- str_sub(raw1$일자,1,6)

# get mean topic proportions per decade = dynamic topic modeling
library(RColorBrewer)

aggregate(posterior(raw1_lda)$topics, by = list(month = raw1$일자), mean) %>% 
  melt(id.var = "month") %>% # reshape data frame
  ggplot(aes(x = month, y = value, fill = variable)) + 
  ylab("proportion") +
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  scale_fill_brewer(palette = "Dark2")

display.brewer.all()
