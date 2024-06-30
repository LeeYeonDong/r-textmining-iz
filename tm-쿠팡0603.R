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

cp_df_raw <- read_excel("D:/대학원/상담/커뮤니케이션학과/쇼핑몰/tb_cp.xlsx") %>% 
  mutate(본문 = str_replace_all(본문, "[^가-힣a-zA-Z0-9.,!?\\s]", " "))

cp_df_raw$category1 %>% unique()

# category별 쇼핑 후기
cp_df_raw %>%
  count(keyword, category1) %>%
  spread(key = category1, value = n, fill = 0) %>% view()

# id 부여
cp_df_raw$id <- c(1:nrow(cp_df_raw))

glimpse(cp_df_raw) # tidyverse 제공 

# 데이터 프레임을 100행씩 쪼개기
cp_df <- cp_df_raw  %>%
  mutate(group = (row_number() - 1) %/% 100) %>%
  group_split(group)

i = 1

category1 <- cp_df_raw %>% 
  select(category1) %>% 
  unique() %>% unlist() %>% as.vector()

cp_df_pos <- tibble()

for(i in 1:length(cp_df)){
  tryCatch({
    # SimplePos22 함수 적용
    cp_df_tmp <- cp_df[[i]] %>%
      select(본문, category1)
    
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
    
    cp_df_pos <- bind_rows(cp_df_pos, cp_df_pos_tmp)
    
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


cp_df_pos %>% glimpse()

cp_df_pos_사 <- cp_df_pos %>% 
  filter(category1 == "사치품 (Luxury Goods)")

cp_df_pos_생 <- cp_df_pos %>% 
  filter(category1 == "생필품 (Necessities)")


cp_processed <- textProcessor(
  documents = cp_df_pos$word, 
  metadata = cp_df_pos,
  lowercase = TRUE,             # Convert to lower case
  removestopwords = TRUE,       # Remove Stop-Words
  removenumbers = FALSE,        # Remove numbers
  removepunctuation = TRUE,     # Remove punctuation
  stem = FALSE,                 # Stemming
  wordLengths = c(1, Inf),      # Allow shorter words for testing
  sparselevel = 1,              # Keep all words
  verbose = TRUE,               # Show verbose output
  onlycharacter = TRUE,         # Keep only character words
  striphtml = FALSE,            # Do not strip HTML
  customstopwords = NULL,       # Custom stop-words list (NULL for default)
  v1 = FALSE                    # Use default version
)

cp_processed %>% glimpse()


cp_processed_사 <- textProcessor(
  documents = cp_df_pos_사$word, 
  metadata = cp_df_pos,
  lowercase = TRUE,             # Convert to lower case
  removestopwords = TRUE,       # Remove Stop-Words
  removenumbers = FALSE,        # Remove numbers
  removepunctuation = TRUE,     # Remove punctuation
  stem = FALSE,                 # Stemming
  wordLengths = c(1, Inf),      # Allow shorter words for testing
  sparselevel = 1,              # Keep all words
  verbose = TRUE,               # Show verbose output
  onlycharacter = TRUE,         # Keep only character words
  striphtml = FALSE,            # Do not strip HTML
  customstopwords = NULL,       # Custom stop-words list (NULL for default)
  v1 = FALSE                    # Use default version
)

cp_processed_생 <- textProcessor(
  documents = cp_df_pos_생$word, 
  metadata = cp_df_pos,
  lowercase = TRUE,             # Convert to lower case
  removestopwords = TRUE,       # Remove Stop-Words
  removenumbers = FALSE,        # Remove numbers
  removepunctuation = TRUE,     # Remove punctuation
  stem = FALSE,                 # Stemming
  wordLengths = c(1, Inf),      # Allow shorter words for testing
  sparselevel = 1,              # Keep all words
  verbose = TRUE,               # Show verbose output
  onlycharacter = TRUE,         # Keep only character words
  striphtml = FALSE,            # Do not strip HTML
  customstopwords = NULL,       # Custom stop-words list (NULL for default)
  v1 = FALSE                    # Use default version
)

# filter out terms that don’t appear in more than 5 documents,
cp_processed_out <- prepDocuments(cp_processed$documents, cp_processed$vocab, cp_processed$meta, lower.thresh = 5)

cp_processed_out_사 <- prepDocuments(cp_processed_사$documents, cp_processed$vocab, cp_processed$meta, lower.thresh = 5)

cp_processed_out_생 <- prepDocuments(cp_processed_생$documents, cp_processed$vocab, cp_processed$meta, lower.thresh = 5)
# documents	: A list containing the documents in the stm format.
# vocab	: Character vector of vocabulary.
# meta : Data frame or matrix containing the user-supplied metadata for the retained documents

# finding the optimal number of topics
# 1
findingk <- searchK(cp_processed_out$docs, cp_processed_out$vocab, K = c(5:20), prevalence = ~ publisher + s(date), data = meta, verbose = FALSE)

findingk_사 <- searchK(cp_processed_out_사$docs, cp_processed_out$vocab, K = c(5:20), prevalence = ~ publisher + s(date), data = meta, verbose = FALSE)

findingk_생 <- searchK(cp_processed_out_생$docs, cp_processed_out$vocab, K = c(5:20), prevalence = ~ publisher + s(date), data = meta, verbose = FALSE)

plot(findingk) # enlarge plots pane
plot(findingk_사) # enlarge plots pane
plot(findingk_생) # enlarge plots pane

# 2
findingk_ver2 <- searchK(documents = docs, 
                         vocab = vocab,
                         K = c(10,20,30,40,50,60,70), #specify K to try
                         N = 500, # matches 10% default
                         proportion = 0.5, # default
                         heldout.seed = 1029, # optional
                         M = 10, # default
                         cores = 1, # default=1
                         prevalence = ~ publisher + s(date),
                         max.em.its = 75, #was 75
                         data = meta,
                         init.type = "Spectral",
                         verbose = TRUE)
plot(findingk_ver2)

# 3
findingk_ver3.lee_mimno <- stm(documents = docs, 
                               vocab = vocab,
                               K = 0, # K=0 instructs STM to run Lee-Mimno
                               seed = 1029, # randomness now, seed matters
                               prevalence = ~ publisher + s(date),
                               max.em.its = 75,
                               data = meta,
                               init.type = "Spectral",
                               verbose = TRUE)


set.seed(1029)
First_STM <- stm(cp_processed_out$docs, cp_processed_out$vocab, ??(토픽 개수),
                 prevalence = ~ publisher + s(date),
                 data = meta,
                 seed = 15, max.em.its = 5)

First_STM_사 <- stm(cp_processed_out_사$docs, cp_processed_out_사$vocab, ??(토픽 개수),
                 prevalence = ~ publisher + s(date),
                 data = meta,
                 seed = 15, max.em.its = 5)

First_STM_생 <- stm(cp_processed_out_생$docs, cp_processed_out_생vocab, ??(토픽 개수),
                 prevalence = ~ publisher + s(date),
                 data = meta,
                 seed = 15, max.em.its = 5)

# Plot first Topic Model # dev.off() 실행 후 plot 그리기
plot(First_STM)
plot(First_STM_사)
plot(First_STM_생)

Second_STM <- stm(documents = cp_processed_out$docs, vocab = cp_processed_out$vocab, K = ??(토픽 개수), prevalence = ~ publisher + s(date), max.em.its = 75, data = meta, init.type = "Spectral", verbose = FALSE)

Second_STM <- stm(documents = cp_processed_out_사$docs, vocab = cp_processed_out_사$vocab, K = ??(토픽 개수), prevalence = ~ publisher + s(date), max.em.its = 75, data = meta, init.type = "Spectral", verbose = FALSE)

Second_STM <- stm(documents = cp_processed_out_생$docs, vocab = cp_processed_out_생$vocab, K = ??(토픽 개수), prevalence = ~ publisher + s(date), max.em.its = 75, data = meta, init.type = "Spectral", verbose = FALSE)

plot(Second_STM)


# Top Words
Third_STM <- stm(documents = cp_processed_out$documents, 
                 vocab = cp_processed_out$vocab,
                 K = ??, data = cp_processed_out$meta,
                 init.type = "Spectral", seed = 1029)
#Plot
plot(Third_STM)

Third_STM %>% labelTopics()

vocab %>% class()


# top 2 paragraps for Topic 1 to 10
Third_STM %>% findThoughts(texts = cp_processed_out$meta$documents, n = 5, topics = 1:7)

# Graphical display of topic correlations
Third_STM %>% topicCorr() %>% plot()

# Graphical display of convergence
plot(Third_STM$convergence$bound, type = "l",
     ylab = "Approximate Objective",
     main = "Convergence")



# Wordcloud:topic 17 with word distribution
set.seed(1029)
cloud(Third_STM, topic = ??, scale = c(5,1))

# Topic proportions
Third_STM %>% str()

plot(Third_STM, type = "hist", topics = sample(1:7, size = ??)) # redline = median
# "stm" 패키지에서 생성된 "Document-Topic Proportion의 MAP Estimates의 분포" 그래프에서 x 축은 문서의 토픽 비율의 추정 값의 범위를 나타낸다. x 축은 각 문서에 할당된 토픽의 비율을 보여주며, 특정 토픽과의 연관성을 나타낸다. x 축 값은 일반적으로 0과 1 사이의 값으로, 추정된 비율의 최소와 최대 값을 나타낸다. 그래프의 각 막대의 높이는 주어진 토픽 비율을 가진 문서의 빈도 또는 수를 나타낸다.

plot(Third_STM, type = "summary")
plot(Third_STM, type = "labels")
plot(Third_STM, type = "perspectives", topics = c(1,2))

summarize_all(make.dt(Third_STM), mean)

tidy(Third_STM) # beta

tidy(Third_STM, matrix = "gamma") # gamma

# The topicQuality() function plots these values 
# and labels each with its topic number:
topicQuality(model = Third_STM, documents = cp_processed_out$documents)


# the topic distribution per document
td_theta <- tidy(Third_STM, matrix = "theta")

selectiontheta <- td_theta[td_theta$document %in% c(1:15),]

selectiontheta %>% 
  ggplot(aes(y = gamma, x = as.factor(topic), fill = as.factor(topic))) +
  geom_bar(stat = "identity",alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ document, ncol = 3) +
  labs(title = "Theta values per document",
       y = expression(theta), x = "Topic")

# analytical look at the word frequencies per topic
tidy(Third_STM) %>%
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

})





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