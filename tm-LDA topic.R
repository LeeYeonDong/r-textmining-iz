library(janeaustenr)
library(tidyverse)
library(tidytext)
library(topicmodels)

# raw data
austen_books() %>%
 select(book) %>% unique()

tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, 
                                regex("^chapter [\\divxlc]", 
                                      ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)


# 불용어
stopwords_en <- stopwords("english")
chr <- c("can", "mr")

tidy_books$word <- removeWords(tidy_books$word, stopwords_en)

tidy_books$word <- str_replace_all(tidy_books$word, "[^0-9a-z]", NA)
tidy_books <- tidy_books[tidy_books$word != "", ]


# tidy_books 데이터에서 LDA 모델을 만들기 위해 먼저 문서-단어 행렬(Document-Term Matrix, DTM)을 만들어야 합니다. tidytext 패키지의 cast_dtm() 함수를 사용합니다.
dtm <- tidy_books %>%
  count(document = book, word) %>%
  cast_dtm(document, word, n)

dtm %>% class()
dtm %>% glimpse()


# the number of topics - hyper parameter
# install.packages("ldatuning")
library(ldatuning)

dtm_lda_n <- FindTopicsNumber(
  dtm,
  topics = 2:10,
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 12),
)

dtm_lda_n %>% FindTopicsNumber_plot()


# 이제 LDA 모델을 학습시킵니다. 여기서는 예시로 토픽의 수를 10으로 설정합니다. 실제 분석에서는 최적의 토픽 수를 찾기 위해 여러 번 시도해볼 수 있습니다.
m_lda <- LDA(dtm, k = 8, method = "Gibbs", control = list(iter = 500, alpha = 1, delta = 0.1))

# Alpha and Beta Hyperparameters – alpha represents document-topic density and Beta represents topic-word density. Higher the value of alpha, documents are composed of more topics and lower the value of alpha, documents contain fewer topics. On the other hand, higher the beta, topics are composed of a large number of words in the corpus, and with the lower value of beta, they are composed of few words.
# alpha는 문서-토픽 밀도를 나타내며, beta는 토픽-단어 밀도를 나타냅니다. alpha 값이 높아지면 문서는 더 많은 토픽으로 구성되고, alpha 값이 낮아지면 문서는 적은 토픽으로 구성됩니다. 한편, beta 값이 높아지면 토픽은 많은 단어로 구성되고, beta 값이 낮아지면 토픽은 적은 단어로 구성됩니다.
# R 언어에서 Latent Dirichlet Allocation (LDA) 모델을 적합하기 위해 사용되는 Variational Inference 알고리즘의 수렴 허용 오차를 나타내는 delta 옵션에 대해서 설명합니다. delta는 현재 변분 파라미터의 추정과 이전 추정값 사이의 최대 차이를 지정하여 수렴 기준을 정합니다. delta의 값이 작으면 수렴 기준이 엄격해지며, 수렴에 걸리는 시간이 더 길어질 수 있지만, delta의 값이 크면 수렴이 빠르지만 변분 파라미터의 추정이 정확하지 않을 수 있습니다.


m_lda %>% glimpse()

m_lda %>% terms(10)

m_lda_topics <- m_lda %>% tidy(matrix = "beta")
# beta : 토픽에 단어가 들어갈 확률
# gamma : 문서가 각 토픽에 등장할 확률

m_lda_top_terms <- m_lda_topics %>% 
  group_by(topic) %>%
  top_n(20, beta) %>%
  arrange(topic, -beta)

m_lda_top_terms  %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(x="", y="")

# 문서를 토픽별로 분류하기
m_lda_topic <- tidy(m_lda, matrix = "gamma")
# gamma는 문서가 각 토픽에 등장할 확률
m_lda_topic %>% filter(topic == c(3))

nd <- m_lda_topic$document %>% unique() %>% length()

# 문서별로 확률이 가장 높은 토픽 추출
m_lda_topic$id <- rep(1:nd, nrow(m_lda_topic)/nd)
m_lda_topic_class <- m_lda_topic %>% 
  group_by(id) %>% 
  slice_max(gamma, n = 5)
# slice_max: gamma가 가장 큰 행을 n = 5개 선택

##원문에 확률이 가장 높은 번호 부여
tidy_books_df <- left_join(tidy_books, m_lda_topic_class, by = c("book" = "document")) #vlookup

# 토픽별 단어 수 살펴보기
m_lda_top_topic_count <- count(tidy_books_df, topic)

# 토픽별 주요 단어 목록 만들기
m_lda_top_terms <- m_lda_top_terms %>% 
  group_by(topic) %>%
  # slice_max(beta, n = 5, with_ties = FALSE) %>% 
  summarise(term = paste(term, collapse = ", "))

# 문서 빈도에 주요단어 결합
m_lda_top_topic_count_word <- m_lda_top_topic_count %>% 
  left_join(m_lda_top_terms, by = "topic")

m_lda_top_topic_count_word$topic <-
  m_lda_top_topic_count_word$topic %>% as.character()

# 토픽별 문서 수와 주요 단어로 막대 그래프 만들기
m_lda_top_topic_count_word %>% 
  ggplot(aes(x = topic, y = n, fill = as.factor(topic))) +
  geom_col() +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.1, size = 3) +
  geom_text(aes(label = term),hjust = 1.1,vjust = 0.5, col = "black",fontface = "bold",size = 3)


# analytical look at the word frequencies per topic
m_lda_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y", ncol = 2) +
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

m_lda %>% topicmodels2LDAvis() %>% serVis()


serVis(topicmodels2LDAvis(m_lda))
