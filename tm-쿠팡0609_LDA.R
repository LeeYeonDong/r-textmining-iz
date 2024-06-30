# KoNLP useNIADic
install.packages('KoNLP', repos = 'https://forkonlp.r-universe.dev')

# install.packages("multilinguer")
# library(multilinguer)
# install_jdk()
# install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), type = "binary")
# #
# install.packages("remotes")
# remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))
library(KoNLP) #최종적으로 "KoNLP" 패키지를 불러옵니다

#devtools::install_github('haven-jeon/NIADic/NIADic', build_vignettes = TRUE)
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_281')  # 설치한 JAVA version에 따라 달라집니다
buildDictionary(ext_dic = "woorimalsam")  # "woorimalsam" dic을 불러옵니다
useNIADic() 
# Use the Sejong Dictionary
useSejongDic()

# package
library(tidyverse)
library(ggplot2)
library(tm)
library(NLP)
library(qdap)
library(wordcloud2)
library(stringr)
library(rJava)
library(base64enc)
library(RmecabKo)
library(reshape2)
library(tidytext)

## 쿠팡
# 데이터 불러오기 및 전처리(Preprocessing)
# Load the readxl library
library(readxl)

cp_df_raw <- read_csv("D:/대학원/상담/커뮤니케이션학과/쇼핑몰/tb_cp0610_utf8.csv",locale = locale("ko", encoding = "utf-8")) %>% 
  mutate(본문 = str_replace_all(본문, "[^가-힣a-zA-Z0-9.,!?\\s]", " ")) %>% 
  as.data.frame() %>% 
  as_tibble()

cp_df_raw$날짜 <- as.Date(cp_df_raw$날짜, format = "%Y.%m.%d")
cp_df_raw %>% glimpse()

cp_df_raw$category1 %>% unique()

# id 부여
cp_df_raw$id <- c(1:nrow(cp_df_raw))

# 데이터 프레임을 100행씩 쪼개기
cp_df <- cp_df_raw  %>%
  mutate(group = (row_number() - 1) %/% 100) %>%
  group_split(group)

cp_df[[1]]

i = 1

category1 <- cp_df_raw %>% 
  select(category1) %>% 
  unique() %>% unlist() %>% as.vector()

cp_df_pos <- tibble()
i = 1
for(i in 1:length(cp_df)){
  tryCatch({
    # SimplePos22 함수 적용
    cp_df_tmp <- cp_df[[i]] %>%
      select(id, 본문, category1, 날짜)
    
    # 조사 리스트 (일부 조사는 문자열의 마지막에 등장할 때가 많음)
    josa_patterns <- c("은$", "는$", "이$", "가$", "을$", "를$", "에$", "와$", "과$", "도$", "로$", "으로$", "의$", "께$")
    
    # 조사 제거 함수 정의
    remove_josa <- function(text, patterns) {
      for (pattern in patterns) {
        text <- str_remove(text, pattern)
      }
      return(text)
    }
    
  cp_df_pos_tmp <- cp_df_tmp %>% 
  unnest_tokens(input = 본문, output = word, token = "words") %>% 
      mutate(word = str_match(word, '([가-힣]+)')[, 2]) %>%
      mutate(word = sapply(word, remove_josa, patterns = josa_patterns)) %>%
      filter(str_length(word) >= 2)
  
  cp_df_combined_tmp <- cp_df_tmp %>%
    left_join(cp_df_pos_tmp, by = c("id","category1", "날짜"))
  
    cp_df_pos <- bind_rows(cp_df_pos, cp_df_combined_tmp)
    
    cat(i, "th 리스트 작업 완료\n")
  }, error = function(e) {
    message("Error in processing chunk ", i, ": ", e)
  })
}

cp_df_pos %>% glimpse()

# 결과를 엑셀 파일로 저장
library(writexl)
# 데이터 프레임을 CSV 파일로 저장
write.csv(cp_df_pos, "D:/대학원/상담/커뮤니케이션학과/쇼핑몰/cp_df_pos_uns.csv", row.names = FALSE, fileEncoding = 'cp949')

cp_df_pos <- read_csv("D:/대학원/상담/커뮤니케이션학과/쇼핑몰/cp_df_pos_uns.csv", locale=locale("ko", encoding = "cp949"))


# 불용어 삭제
# 불용어 리스트 정의
stop_words <- c("제품", "너무", "도움", "같아요", "있어서", "있습니다", "있어요", "그리고", 
                "후기", "그래서", "다른", "그냥", "리뷰", "조금", "사용할", "합니다", "않고", 
                "있어", "하나", "좋고", "좋습니다", "해서", "후기입니다", "일단", "같습니다", 
                "작성한", "분들", "그래", "입니다", "받아", "요즘", "있고", "있는데", "쿠팡체험단", 
                "하지만", "돼요", "되셨다면", "다시", "경우", "됩니다", "좋아요", "정말", "한번", "진짜", "이번", "아주", "이번", "확인", "때문", "사용하고", "사용하기", "부분", "엄청", "하고", "좋네요")

# 불용어 삭제
cp_df_pos <- cp_df_pos %>%
  filter(!word %in% stop_words)

# 결과 확인
cp_df_pos %>% glimpse()
cp_df_pos %>% select(category1) %>% unique() 


# 최다 빈도 단어 Top30을 뽑습니다
token_count30 <- cp_df_pos %>% 
  # filter(category1 == category1[1]) %>% 
  select(word) %>% 
  table() %>% 
  sort(decreasing = TRUE) %>% 
  head(30)

as.data.frame(token_count30)
token_count30_df <- as.data.frame(token_count30)

cp_df_pos %>% 
  select(category1) %>% unique()

token_count30_사 <- cp_df_pos %>% 
  filter(category1 == "사치품 (Luxury Goods)") %>% 
  select(word) %>% 
  table() %>% 
  sort(decreasing = TRUE) %>% 
  head(30)

as.data.frame(token_count30_사)
token_count30_df_사 <- as.data.frame(token_count30_사)


token_count30_생 <- cp_df_pos %>% 
  filter(category1 == "생필품 (Necessities)") %>% 
  select(word) %>% 
  table() %>% 
  sort(decreasing = TRUE) %>% 
  head(30)

as.data.frame(token_count30_생)
token_count30_df_생 <- as.data.frame(token_count30_생)


# word cloud
library(devtools)
#devtools::install_github("lchiffon/wordcloud2") # 기존 wordcloud2 패키지 제거
library(wordcloud2)
wordcloud2(token_count30, minRotation = 0, maxRotation = 0, color = "blue") 

wordcloud2(data = token_count30, size = 0.5, gridSize = 10, minRotation = 0, maxRotation = 0, color = "blue", shape = 'circle')

wordcloud2(data = token_count30_사, size = 0.5, gridSize = 10, minRotation = 0, maxRotation = 0, color = "blue", shape = 'circle')

wordcloud2(data = token_count30_생, size = 0.5, gridSize = 10, minRotation = 0, maxRotation = 0, color = "blue", shape = 'circle')

# tf-idf
cp_df_pos_idf <- cp_df_pos %>% 
  count(category1, word) %>% 
  filter(str_count(word) > 1) %>% 
  bind_tf_idf(term = word, document = category1, n = n) %>% 
  arrange(-tf_idf)

write_xlsx(cp_df_pos_idf, "D:/대학원/상담/커뮤니케이션학과/쇼핑몰/cp_df_pos_idf_전체.xlsx")


# LDA
raw1_token_df <- read_excel("D:/대학원/상담/커뮤니케이션학과/쇼핑몰/cp_df_pos.xlsx")


# DocumentTermMatrix
library(tm)

# 사치품
raw1_before_dtm <- raw1_token_df %>% 
  group_by(L1) %>% 
  filter(category1 == category1[1]) %>% 
  count(value, sort = TRUE)

raw1_dtm <- cast_dtm(raw1_before_dtm , document = L1, term = value, value = n)
class(raw1_dtm)
View(as.matrix(raw1_dtm[1:10, 1:10]))

# Topic modeling
library(topicmodels)

# the number of topics - hyper parameter
library(ldatuning)

FindTopicsNumber(
  raw1_dtm,
  topics = 2:10,
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 12),
) %>% 
FindTopicsNumber_plot()

# run LDA
set.seed(1029)
raw1_lda <- LDA(raw1_dtm, k = 8, method = "Gibbs", control = list(iter = 500, alpha = 1, delta = 0.1))

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

raw1_top_terms %>%
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


# using
# install.packages("LDAvis")
library(LDAvis)

# using serVis
topicmodels2LDAvis <- function(x){
  post <- topicmodels::posterior(x)
  if (ncol(post$topics) < 3) stop("The model must contain > 2 topics")
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



# 사치품
raw2_before_dtm <- raw1_token_df %>% 
  group_by(L1) %>% 
  filter(category1 == category1[2]) %>% 
  count(value, sort = TRUE)

raw2_dtm <- cast_dtm(raw2_before_dtm , document = L1, term = value, value = n)
class(raw2_dtm)
View(as.matrix(raw2_dtm[1:10, 1:10]))

# Topic modeling
library(topicmodels)

# the number of topics - hyper parameter
library(ldatuning)

FindTopicsNumber(
  raw2_dtm,
  topics = 2:10,
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 12),
) %>% 
  FindTopicsNumber_plot()

# run LDA
set.seed(1029)
raw1_lda <- LDA(raw2_dtm, k = 8, method = "Gibbs", control = list(iter = 500, alpha = 1, delta = 0.1))

glimpse(raw2_lda)

terms(raw2_lda, 10)

raw2_topics <- tidy(raw2_lda, matrix = "beta")
# beta : 토픽에 단어가 들어갈 확률
# gamma : 문서가 각 토픽에 등장할 확률

raw2_top_terms <- raw2_topics %>% 
  group_by(topic) %>%
  top_n(20, beta) %>%
  arrange(topic, -beta)

raw2_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(x="", y="")


# 문서를 토픽별로 분류하기
raw2_topic <- tidy(raw2_lda, matrix = "gamma")
# gamma는 문서가 각 토픽에 등장할 확률

# 문서별로 확률이 가장 높은 토픽 추출
raw2_class <- raw2_topic %>% 
  group_by(document) %>% 
  slice_max(gamma, n = 1)
# slice_max: gamma가 가장 큰 행을 n = 1개 선택

# 토픽별 문서 수 살펴보기
raw2_class_topic_count <- count(raw2_class_topic, topic)

# 토픽별 주요 단어 목록 만들기
raw2_terms <- raw2_top_terms %>% 
  group_by(topic) %>%
  slice_max(beta, n = 5, with_ties = FALSE) %>% 
  summarise(term = paste(term, collapse = ", "))

# 문서 빈도에 주요단어 결합
raw2_topic_count_word <- raw2_class_topic_count %>% 
  left_join(raw2_terms, by = "topic")

# 토픽별 문서 수와 주요 단어로 막대 그래프 만들기
raw2_topic_count_word %>% 
  ggplot(aes(x = topic, y = n, fill = as.factor(topic))) +
  geom_col() +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.1, size = 5) +
  geom_text(aes(label = term),hjust = 1.1,vjust = 0.5, col = "black",fontface = "bold",size = 5)


# using
# install.packages("LDAvis")
library(LDAvis)

# using serVis
topicmodels2LDAvis <- function(x){
  post <- topicmodels::posterior(x)
  if (ncol(post$topics) < 3) stop("The model must contain > 2 topics")
  mat <- x@wordassignments
  LDAvis::createJSON(
    phi = post$terms, 
    theta = post$topics,
    vocab = colnames(post$terms),
    doc.length = slam::row_sums(mat, na.rm = TRUE),
    term.frequency = slam::col_sums(mat, na.rm = TRUE)
  )
}

raw2_lda %>% glimpse()

# posterior(raw1_lda)$topics = posterior(raw1_lda)[["topics"]]
serVis(topicmodels2LDAvis(raw2_lda))
raw2_lda %>% topicmodels2LDAvis() %>% serVis()