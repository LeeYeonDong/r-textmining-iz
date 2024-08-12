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

cp_df_raw <- read_csv("D:/대학원/상담/커뮤니케이션학과/쇼핑몰/tb_cp0610.csv",locale = locale("ko", encoding = "cp949")) %>% 
  mutate(본문 = str_replace_all(본문, "[^가-힣a-zA-Z0-9.,!?\\s]", " ")) %>% 
  as.data.frame() %>% 
  as_tibble()

cp_df_raw %>%
  group_by(keyword) %>%
  summarise(
    상품수 = n_distinct(상품명),
    상품평갯수 = n()
  ) %>% print(n = 30)

cp_df_raw %>% tail()

cp_df_raw$날짜 <- as.Date(cp_df_raw$날짜, format = "%Y.%m.%d")
cp_df_raw %>% glimpse()
cp_df_raw$상품명 %>% glimpse()

cp_df_raw$category1 %>% unique()

# # id 부여
# cp_df_raw$id <- c(1:nrow(cp_df_raw))
# 
# # 데이터 프레임을 100행씩 쪼개기
# cp_df <- cp_df_raw  %>%
#   mutate(group = (row_number() - 1) %/% 100) %>%
#   group_split(group)
# 
# cp_df[[1]]
# 
# category1 <- cp_df_raw %>% 
#   select(category1) %>% 
#   unique() %>% unlist() %>% as.vector()

# # 명사와 형용사만 추출하는 함수 정의
# extract_nouns_adjectives <- function(text) {
#   # SimplePos09를 사용하여 형태소 분석 수행
#   pos_result <- SimplePos09(text)
#   # 명사(N)와 형용사(PA)를 추출
#   word <- unlist(str_extract_all(pos_result, "[가-힣]+/N|[가-힣]+/PA"))
#   # 품사 태그 제거
#   word <- str_remove_all(word, "/[A-Z]+")
#   return(word)
# }

# 조사 패턴 정의
josa_patterns <- c("이", "가", "은", "는", "을", "를", "에", "의", "로", "와", "과", "도", "만", "부터", "까지", "에서", "으로", "라고", "랑", "이랑", "하고")

# remove_josa 함수 정의
remove_josa <- function(text, patterns) {
  for (pattern in patterns) {
    text <- str_remove(text, paste0(pattern, "$"))
  }
  return(text)
}

cp_df_pos <- tibble()


# 텍스트 전처리 함수 정의
preprocess_text <- function(text) {
  text <- str_replace_all(text, "[^가-힣0-9a-zA-Z\\s]", "")  # 특수 문자 제거
  text <- str_squish(text)  # 연속된 공백 제거
  return(text)
}

# 전체 데이터 프레임에 대해 처리 함수 정의
process_data <- function(df) {
  df %>%
    mutate(본문 = preprocess_text(본문)) %>%  # 텍스트 전처리
    rowwise() %>%
    mutate(word = list(SimplePos09(본문))) %>%
    unnest(word) %>%
    rowwise() %>%
    mutate(word = str_split(word, "\\+")) %>%
    unnest(word) %>% 
    filter(str_detect(word, "/N") | str_detect(word, "/PA")) %>% 
    mutate(word = str_remove(word, "/.*")) %>%
    ungroup() %>% 
    filter(str_length(word) >= 2) %>%
    mutate(word = sapply(word, remove_josa, patterns = josa_patterns)) %>%
    mutate(word = as.character(word))  # Ensure 'word' is always a character
}

# 전체 데이터 프레임에 대해 처리 적용
cp_df_pos <- tryCatch({
  process_data(cp_df_raw)
}, error = function(e) {
  message("Error in processing data: ", e)
  return(tibble())  # 빈 데이터프레임 반환
})

# results_yt <- results_yt[!sapply(results_yt, is.null)]
# 
# # 결과를 하나의 데이터프레임으로 병합
# yt_df_pos <- bind_rows(results_yt)
# 
test <- cp_df_pos$id %>% unique() %>% tail(1)

cp_df_pos %>% filter(id == (test-1)) %>% view()


# tf_idf
# 'id'를 문서 ID로 사용하고 'word'를 단어로 사용합니다.
tf_idf_df <- cp_df_pos %>%
  count(id, category1 ,word, sort = TRUE) %>% # 각 단어의 빈도 계산
  filter(n >= 15) %>%   
  bind_tf_idf(term = word, document = id, n = n) %>%
  arrange(desc(tf_idf)) 

write.csv(tf_idf_df, "D:/대학원/상담/커뮤니케이션학과/쇼핑몰/tf_idf_df_쿠팡.csv", row.names = FALSE, fileEncoding = 'cp949')

# 결과를 엑셀 파일로 저장
library(writexl)
# 데이터 프레임을 CSV 파일로 저장
write.csv(cp_df_pos, "D:/대학원/상담/커뮤니케이션학과/쇼핑몰/cp_df_pos.csv", row.names = FALSE, fileEncoding = 'cp949')

cp_df_pos <- read_csv("D:/대학원/상담/커뮤니케이션학과/쇼핑몰/cp_df_pos.csv", locale=locale("ko", encoding = "cp949"))

cp_df_pos %>% glimpse()


# structerd topic modeling
library(topicmodels)
library(lda)
library(slam)
library(stm)
library(dplyr)
library(tidytext)
library(furrr) # try to make it faster
plan(multicore) # parallel processing within a single machine
# plan(multiprocess) #parallel processing across multiple machines
library(tm) # Framework for text mining
library(tidyverse) # Data preparation and pipes %>%
library(ggplot2) # For plotting word frequencies
library(devtools)
# devtools::install_github("lchiffon/wordcloud2") # 기존 wordcloud2 패키지 제거
library(wordcloud2)
library(Rtsne)
library(rsvd)
library(geometry)
library(NLP)
library(ldatuning) 
library(lubridate)

# 데이터 프레임 확인
cp_df_pos %>% glimpse()

cp_df_pos <- cp_df_pos %>%
  filter(!is.na(word) & word != "null")


cp_df_pos %>%
  count(word, sort = TRUE) %>%
  top_n(20, n) %>% 
  wordcloud2(size = 1.0, color = "darkgreen", minRotation = 0, maxRotation = 0)


cp_df_pos %>%
  filter(category1 == "사치품 (Luxury Goods)") %>% 
  count(word, sort = TRUE) %>%
  top_n(20, n) %>% 
  wordcloud2(size = 1.0, color = "darkgreen", minRotation = 0, maxRotation = 0)
  

cp_df_pos %>%
  filter(category1 == "생필품 (Necessities)") %>% 
  count(word, sort = TRUE) %>%
  top_n(20, n) %>% 
  wordcloud2(size = 1.0, color = "darkgreen", minRotation = 0, maxRotation = 0)



# 날짜 변수가 올바른 형식인지 확인
summary(cp_df_pos$날짜)

# 날짜 변수에 NA가 있는지 확인
sum(is.na(cp_df_pos$날짜))
# NA 값이 있는 경우 이를 제거하거나 다른 방법으로 처리
cp_df_pos <- cp_df_pos %>% filter(!is.na(날짜))

# 날짜 변수를 숫자로 변환
cp_df_pos$날짜 <- as.Date(cp_df_pos$날짜, format = "%Y-%m-%d") %>% as.numeric()

# word를 id 별로 통합하여 본문 변수를 대체
cp_df <- cp_df_pos %>%
  group_by(id) %>%
  mutate(본문 = str_c(word, collapse = " ")) %>% 
  select(-word) %>% distinct()

# 필요한 열 추출
cp_df_사 <- cp_df %>%
  select(category1, 날짜, 본문) %>% 
  filter(category1 == "사치품 (Luxury Goods)")

cp_df_생 <- cp_df %>%
  select(category1, 날짜, 본문) %>% 
  filter(category1 == "생필품 (Necessities)")

# 텍스트 처리
cp_processed_사 <- textProcessor(
  documents = cp_df_사$본문, 
  metadata = cp_df_사,
  lowercase = TRUE,             
  removestopwords = TRUE,       
  removenumbers = FALSE,        
  removepunctuation = TRUE,     
  stem = FALSE,                 
  wordLengths = c(2, Inf),      
  sparselevel = 1,              
  verbose = TRUE,               
  onlycharacter = TRUE,         
  striphtml = FALSE,            
  customstopwords = NULL,       
  v1 = FALSE                    
)

cp_processed_생 <- textProcessor(
  documents = cp_df_생$본문, 
  metadata = cp_df_생,
  lowercase = TRUE,             
  removestopwords = TRUE,       
  removenumbers = FALSE,        
  removepunctuation = TRUE,     
  stem = FALSE,                 
  wordLengths = c(2, Inf),      
  sparselevel = 1,              
  verbose = TRUE,               
  onlycharacter = TRUE,         
  striphtml = FALSE,            
  customstopwords = NULL,       
  v1 = FALSE                    
)

# n_thr 할당
plotRemoved(cp_processed_사$documents, lower.thresh = seq(0, 100, by = 5))

plotRemoved(cp_processed_생$documents, lower.thresh = seq(0, 100, by = 5))

n_thr_cp <- 15

# 문서-단어 행렬 준비 (빈도 기준 조정)
cp_processed_out_사 <- prepDocuments(
  cp_processed_사$documents, 
  cp_processed_사$vocab, 
  cp_processed_사$meta, 
  lower.thresh = n_thr_cp # 최소 15회 이상 등장하는 단어만 사용
)

cp_processed_out_생 <- prepDocuments(
  cp_processed_생$documents, 
  cp_processed_생$vocab, 
  cp_processed_생$meta, 
  lower.thresh = n_thr_cp # 최소 15회 이상 등장하는 단어만 사용
)


# searchK 함수 사용 (Spectral 초기화)
findingk_사 <- searchK(
  documents = cp_processed_out_사$documents, 
  vocab = cp_processed_out_사$vocab, 
  K = c(3:10), 
  prevalence = ~ s(날짜), 
  data = cp_processed_out_사$meta, 
  init.type = "Spectral", 
  verbose = TRUE
)

# 결과 플롯
dev.off()
plot(findingk_사)

K_optimal_사 <- 5


findingk_생 <- searchK(
  documents = cp_processed_out_생$documents, 
  vocab = cp_processed_out_생$vocab, 
  K = c(3:10), 
  prevalence = ~ s(날짜), 
  data = cp_processed_out_생$meta, 
  init.type = "Spectral", 
  verbose = TRUE
)

# 결과 플롯
dev.off()
plot(findingk_생)

K_optimal_생 <- 10


# STM 모델 실행
stm_model_사 <- stm(
  documents = cp_processed_out_사$documents, 
  vocab = cp_processed_out_사$vocab, 
  K = K_optimal_사, 
  prevalence = ~ s(날짜), 
  data = cp_processed_out_사$meta, 
  init.type = "Spectral", 
  max.em.its = 75, 
  verbose = TRUE
)

stm_model_생 <- stm(
  documents = cp_processed_out_생$documents, 
  vocab = cp_processed_out_생$vocab, 
  K = K_optimal_생, 
  prevalence = ~ s(날짜), 
  data = cp_processed_out_생$meta, 
  init.type = "Spectral", 
  max.em.its = 75, 
  verbose = TRUE
)


# 토픽-단어 매트릭스 (베타 값)
beta_tidy_사 <- tidy(stm_model_사)
beta_tidy_사 <- beta_tidy_사 %>%
  mutate(beta = as.numeric(format(as.numeric(beta), digits = 4)))
beta_tidy_사_top <- beta_tidy_사 %>%
  group_by(topic) %>%
  slice_max(order_by = beta, n = 3) %>%
  ungroup()
write.csv(beta_tidy_사, "D:/대학원/상담/커뮤니케이션학과/쇼핑몰/beta_tidy_사_cp.csv", row.names = FALSE, fileEncoding = 'cp949')
write.csv(beta_tidy_사_top, "D:/대학원/상담/커뮤니케이션학과/쇼핑몰/beta_tidy_사_cp_top.csv", row.names = FALSE, fileEncoding = 'cp949')

beta_tidy_생 <- tidy(stm_model_생)
beta_tidy_생 <- beta_tidy_생 %>%
  mutate(beta = as.numeric(format(as.numeric(beta), digits = 4)))
beta_tidy_생_top <- beta_tidy_생 %>%
  group_by(topic) %>%
  slice_max(order_by = beta, n = 3) %>%
  ungroup()
write.csv(beta_tidy_생, "D:/대학원/상담/커뮤니케이션학과/쇼핑몰/beta_tidy_생_cp.csv", row.names = FALSE, fileEncoding = 'cp949')
write.csv(beta_tidy_생_top, "D:/대학원/상담/커뮤니케이션학과/쇼핑몰/beta_tidy_생_cp_top.csv", row.names = FALSE, fileEncoding = 'cp949')

# 주제별 단어 빈도 분석
beta_tidy_사 %>%
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
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics")


beta_tidy_생 %>%
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
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics")



# 문서-토픽 매트릭스 (감마 값)
gamma_tidy_사 <- tidy(stm_model_사, matrix = "gamma")
top_gamma_df_사 <- cp_df_사 %>%
  inner_join(gamma_tidy_사, by = c("id" = "document")) %>% 
  group_by(topic) %>%
  slice_max(order_by = gamma, n = 3) %>%
  ungroup()

write.csv(top_gamma_df_사, "D:/대학원/상담/커뮤니케이션학과/쇼핑몰/top_gamma_df_사_cp.csv", row.names = FALSE, fileEncoding = 'cp949')


gamma_tidy_생 <- tidy(stm_model_생, matrix = "gamma")
top_gamma_df_생 <- cp_df_생 %>%
  inner_join(gamma_tidy_생, by = c("id" = "document")) %>% 
  group_by(topic) %>%
  slice_max(order_by = gamma, n = 3) %>%
  ungroup()

write.csv(top_gamma_df_생, "D:/대학원/상담/커뮤니케이션학과/쇼핑몰/top_gamma_df_생_cp.csv", row.names = FALSE, fileEncoding = 'cp949')


# 토픽 간 상관관계 그래프
topic_corr_사 <- topicCorr(stm_model_사)
plot(topic_corr_사)
topic_corr_생 <- topicCorr(stm_model_생)
plot(topic_corr_생)


# 토픽 비율 히스토그램 (랜덤하게 선택한 주제 1부터 15개)
plot(stm_model_사, type = "hist", topics = 1:K_optimal_사, size = K_optimal_사)
plot(stm_model_생, type = "hist", topics = 1:K_optimal_생, size = K_optimal_생)


# 모델 요약
plot(stm_model_사, type = "summary",  n = 5)
plot(stm_model_생, type = "summary",  n = 5)


# 주제 간 관점 차이 (예: 주제 1과 2)
# plot(stm_model_사, type = "perspectives", topics = c(1, 2))
# plot(stm_model_생, type = "perspectives", topics = c(1, 2))

# 모델 데이터 프레임 생성 및 평균 요약
theta_df_사 <- make.dt(stm_model_사)
summarize_all(theta_df_사, mean)

theta_df_생 <- make.dt(stm_model_생)
summarize_all(theta_df_생, mean)

# 토픽 품질 평가
topic_quality_사 <- topicQuality(model = stm_model_사, documents = cp_processed_out_사$documents)
topic_quality_생 <- topicQuality(model = stm_model_생, documents = cp_processed_out_생$documents)





gamma_tidy_생 <- tidy(stm_model_생, matrix = "gamma")
gamma_tidy_생_top <- gamma_tidy_생 %>%
  group_by(topic) %>%
  slice_max(order_by = gamma, n = 3) %>%
  ungroup()
write.csv(gamma_tidy_생, "D:/대학원/상담/커뮤니케이션학과/쇼핑몰/gamma_tidy_생_cp.csv", row.names = FALSE, fileEncoding = 'cp949')
write.csv(gamma_tidy_생_top, "D:/대학원/상담/커뮤니케이션학과/쇼핑몰/gamma_tidy_생_cp_top.csv", row.names = FALSE, fileEncoding = 'cp949')


# 문서 당 토픽 분포 (감마 값)
td_theta_사 <- tidy(stm_model_사, matrix = "theta")
td_theta_생 <- tidy(stm_model_생, matrix = "theta")

selectiontheta_사 <- td_theta_사[td_theta_사$document %in% c(1:3),]
selectiontheta_생 <- td_theta_생[td_theta_생$document %in% c(1:3),]

selectiontheta_사 %>% 
  ggplot(aes(y = gamma, x = as.factor(topic), fill = as.factor(topic))) +
  geom_bar(stat = "identity", alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ document, ncol = 3) +
  labs(title = "Theta values per document",
       y = expression(theta), x = "Topic")

selectiontheta_생 %>% 
  ggplot(aes(y = gamma, x = as.factor(topic), fill = as.factor(topic))) +
  geom_bar(stat = "identity", alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ document, ncol = 3) +
  labs(title = "Theta values per document",
       y = expression(theta), x = "Topic")



















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