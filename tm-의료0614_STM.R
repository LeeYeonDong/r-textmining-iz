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
library(lubridate)

# 데이터 불러오기 및 전처리(Preprocessing)
# Load the readxl library
library(readxl)

yt_df_raw <- read_excel("D:/대학원/상담/커뮤니케이션학과/의료분쟁/df_ytb_com0605.xlsx") %>% 
  mutate(본문 = str_replace_all(댓글, "[^가-힣a-zA-Z0-9.,!?\\s]", " ")) %>% 
  as.data.frame() %>% 
  as_tibble() %>%
  filter(날짜 != "없음")

yt_df_raw %>% glimpse()
yt_df_raw$제목 %>% unique() %>% length()
yt_df_raw$날짜 %>% unique() %>% max()
yt_df_raw$날짜 %>% unique() %>% min()

# 날짜 변환
# 현재 날짜
current_date <- as.Date("2024-06-10")

# 상대적 날짜 변환 함수
convert_relative_date <- function(date_str, current_date) {
  date_str <- gsub("\\(.*\\)", "", date_str) # 수정됨 등 제거
  date_str <- trimws(date_str) # 양쪽 공백 제거
  
  if (grepl("일전", date_str)) {
    days <- as.numeric(gsub("일전", "", date_str))
    return(current_date - days(days))
  } else if (grepl("시간전", date_str)) {
    hours <- as.numeric(gsub("시간전", "", date_str))
    return(current_date - dhours(hours))
  } else if (grepl("분전", date_str)) {
    minutes <- as.numeric(gsub("분전", "", date_str))
    return(current_date - dminutes(minutes))
  } else if (grepl("개월전", date_str)) {
    months <- as.numeric(gsub("개월전", "", date_str))
    return(current_date %m-% months(months))
  } else if (grepl("주전", date_str)) {
    weeks <- as.numeric(gsub("주전", "", date_str))
    return(current_date - dweeks(weeks))
  }
  return(NA) # 알 수 없는 형식
}

yt_df_raw %>% select(날짜) %>% unique() %>% as.data.frame()

# 상대적 날짜를 실제 날짜로 변환
yt_df_raw <- yt_df_raw %>%
  filter(날짜 != "없음") %>% # 날짜가 "없음"이 아닌 행만 선택
  rowwise() %>%
  mutate(날짜 = convert_relative_date(날짜, current_date)) %>%
  ungroup() %>%
  mutate(날짜 = format(날짜, "%Y.%m.%d"))

# 결과 확인
print(yt_df_raw)

# yt_df_raw <- yt_df_raw %>% head(1000)

# id 부여
yt_df_raw$id <- c(1:nrow(yt_df_raw))

# write.csv(dd, "D:/대학원/상담/커뮤니케이션학과/의료분쟁/df_ytb_com0605.csv", row.names = FALSE, fileEncoding = 'UTF-8')
# 
# dd <- read_csv("D:/대학원/상담/커뮤니케이션학과/의료분쟁/df_ytb_com0605.csv",locale = locale("ko", encoding = "UTF-8")) 
# write.csv(dd, "D:/대학원/상담/커뮤니케이션학과/의료분쟁/df_ytb_com0605.csv", row.names = FALSE, fileEncoding = 'cp949')

# # 데이터 프레임을 100행씩 쪼개기
# yt_df <- yt_df_raw  %>%
#   mutate(group = (row_number() - 1) %/% 100) %>%
#   group_split(group)
# 
# yt_df[[1]]
# 
# yt_df_pos <- tibble()


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

yt_df_pos <- tibble()


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
yt_df_pos <- tryCatch({
  process_data(yt_df_raw)
}, error = function(e) {
  message("Error in processing data: ", e)
  return(tibble())  # 빈 데이터프레임 반환
})

# results_yt <- results_yt[!sapply(results_yt, is.null)]
# 
# # 결과를 하나의 데이터프레임으로 병합
# yt_df_pos <- bind_rows(results_yt)
# 
test <- yt_df_pos$id %>% unique() %>% tail(1)

yt_df_pos %>% filter(id == (test-1)) %>% view()

yt_df_pos %>% glimpse()

# tf_idf
# 'id'를 문서 ID로 사용하고 'word'를 단어로 사용합니다.
tf_idf_df <- yt_df_pos %>%
  count(id, word, sort = TRUE) %>%  # 각 단어의 빈도 계산
  filter(n >= 5) %>%   
  bind_tf_idf(term = word, document = id, n = n) %>%
  arrange(desc(tf_idf))

write.csv(tf_idf_df, "D:/대학원/상담/커뮤니케이션학과/의료분쟁/tf_idf_df_유튜브.csv", row.names = FALSE, fileEncoding = 'cp949')


# 주제별 단어 빈도 분석
beta_tidy %>%
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


# 결과를 엑셀 파일로 저장
library(writexl)
# 데이터 프레임을 CSV 파일로 저장
write.csv(yt_df_pos, "D:/대학원/상담/커뮤니케이션학과/의료분쟁/yt_df_pos.csv", row.names = FALSE, fileEncoding = 'utf-8')

yt_df_pos <- read_csv("D:/대학원/상담/커뮤니케이션학과/의료분쟁/yt_df_pos.csv", locale=locale("ko", encoding = "utf-8"))

yt_df_pos %>% glimpse()



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
yt_df_pos %>% glimpse()

yt_df_pos <- yt_df_pos %>%
  filter(!is.na(word) & word != "null")


yt_df_pos %>%
  count(word, sort = TRUE) %>%
  top_n(20, n) %>% 
  wordcloud2(size = 1.0, color = "darkgreen", minRotation = 0, maxRotation = 0)


# 날짜 변수가 올바른 형식인지 확인
summary(yt_df_pos$날짜)

# 날짜 변수에 NA가 있는지 확인
sum(is.na(yt_df_pos$날짜))
# NA 값이 있는 경우 이를 제거하거나 다른 방법으로 처리
yt_df_pos <- yt_df_pos %>% filter(!is.na(날짜))

# 날짜 변수를 숫자로 변환
yt_df_pos$날짜 <- as.Date(yt_df_pos$날짜, format = "%Y.%m.%d") %>% as.numeric()

# word를 id 별로 통합하여 본문 변수를 대체
yt_df <- yt_df_pos %>%
  group_by(id) %>%
  mutate(본문 = str_c(word, collapse = " ")) %>% 
  select(-word) %>% distinct() %>% 
  select(날짜, 본문)

# 텍스트 처리
yt_processed <- textProcessor(
  documents = yt_df$본문, 
  metadata = yt_df,
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
plotRemoved(yt_processed$documents, lower.thresh = seq(0, 100, by = 5))

n_thr <- 5 


# 문서-단어 행렬 준비 (빈도 기준 조정)
yt_processed_out <- prepDocuments(
  yt_processed$documents, 
  yt_processed$vocab, 
  yt_processed$meta, 
  lower.thresh = n_thr # 최소 5회 이상 등장하는 단어만 사용
)

# docs_removed <- yt_processed_out$docs.removed
# yt_processed_out$meta <- yt_processed_out$meta[-docs_removed, ]

# # 데이터 샘플링 (예: 전체 데이터의 10%)
set.seed(123)

sample_indices <- sample(seq_len(length(yt_processed_out$documents)), size = 0.1 * length(yt_processed_out$documents))
sampled_documents <- yt_processed_out$documents[sample_indices]
sampled_meta <- yt_processed_out$meta[sample_indices, ]

yt_findingk <- searchK(
  documents = sampled_documents, 
  vocab = yt_processed_out$vocab, 
  K = c(3:10), 
  prevalence = ~ s(날짜), 
  data = sampled_meta, 
  init.type = "Spectral", 
  verbose = TRUE,
)

# # searchK 함수 사용 (Spectral 초기화)
# yt_findingk <- searchK(
#   documents = yt_processed_out$documents, 
#   vocab = yt_processed_out$vocab, 
#   K = c(3:10), 
#   prevalence = ~ s(날짜), 
#   data = yt_processed_out$meta, 
#   init.type = "Spectral", 
#   verbose = TRUE
# )

# 결과 플롯
dev.off()
plot(yt_findingk)

yt_K_optimal <- 3

# NA 값 확인
sum(is.na(yt_processed_out$meta$날짜))

# NA 값 제거
yt_processed_out$meta <- yt_processed_out$meta[!is.na(yt_processed_out$meta$날짜), ]

# documents와 meta 데이터의 일치 확인
n_docs <- length(yt_processed_out$documents)
n_meta <- nrow(yt_processed_out$meta)

cat("Number of documents:", n_docs, "\n")
cat("Number of meta rows:", n_meta, "\n")

# 일치하지 않는 데이터 제거
if (n_docs != n_meta) {
  # documents와 일치하지 않는 meta의 행 제거
  yt_processed_out$meta <- yt_processed_out$meta[1:n_docs, ]
}

# STM 모델 실행
stm_model <- stm(
  documents = yt_processed_out$documents, 
  vocab = yt_processed_out$vocab, 
  K = yt_K_optimal, 
  prevalence = ~ s(날짜), 
  data = yt_processed_out$meta, 
  init.type = "LDA", 
  max.em.its = 75, 
  verbose = TRUE
)


# 토픽-단어 매트릭스 (베타 값)
beta_tidy <- tidy(stm_model)
beta_tidy <- beta_tidy %>%
  mutate(beta = as.numeric(format(as.numeric(beta), digits = 4)))
beta_tidy_top <- beta_tidy %>%
  group_by(topic) %>%
  slice_max(order_by = beta, n = 3) %>%
  ungroup()
write.csv(beta_tidy, "D:/대학원/상담/커뮤니케이션학과/의료분쟁/beta_tidy.csv", row.names = FALSE, fileEncoding = 'cp949')
write.csv(beta_tidy_top, "D:/대학원/상담/커뮤니케이션학과/의료분쟁/beta_tidy_top.csv", row.names = FALSE, fileEncoding = 'cp949')

# 주제별 단어 빈도 분석
beta_tidy %>%
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
gamma_tidy <- tidy(stm_model, matrix = "gamma")
top_gamma_df <- yt_df %>%
  inner_join(gamma_tidy, by = c("id" = "document")) %>% 
  group_by(topic) %>%
  slice_max(order_by = gamma, n = 3) %>%
  ungroup()

write.csv(top_gamma_df, "D:/대학원/상담/커뮤니케이션학과/의료분쟁/top_gamma_df_ytb.csv", row.names = FALSE, fileEncoding = 'cp949')


# 토픽 간 상관관계 그래프
dev.off()
topic_corr <- topicCorr(stm_model)
plot(topic_corr)


# 토픽 비율 히스토그램 (랜덤하게 선택한 주제 1부터 15개)
plot(stm_model, type = "hist", topics = sample(1:yt_K_optimal, size = 10))

# 모델 요약
plot(stm_model, type = "summary",  n = 5)

# 주제 간 관점 차이 (예: 주제 1과 2)
plot(stm_model, type = "perspectives", topics = c(1, 2))
plot(stm_model, type = "perspectives", topics = c(1, 3))
plot(stm_model, type = "perspectives", topics = c(2, 3))


# 모델 데이터 프레임 생성 및 평균 요약
theta_df <- make.dt(stm_model)
summarize_all(theta_df, mean)

# 토픽 품질 평가
topic_quality <- topicQuality(model = stm_model, documents = yt_processed_out$documents)

# 문서 당 토픽 분포 (감마 값)
td_theta <- tidy(stm_model, matrix = "theta")

selectiontheta <- td_theta[td_theta$document %in% c(1:15),]

selectiontheta %>% 
  ggplot(aes(y = gamma, x = as.factor(topic), fill = as.factor(topic))) +
  geom_bar(stat = "identity", alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ document, ncol = 3) +
  labs(title = "Theta values per document",
       y = expression(theta), x = "Topic")

