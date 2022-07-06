# KoNLP useNIADic
install.packages("multilinguer")
library(multilinguer)
install_jdk()
install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), type = "binary")
#
install.packages("remotes")
devtools::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))
library(KoNLP) #최종적으로 "KoNLP" 패키지를 불러옵니다

devtools::install_github('haven-jeon/NIADic/NIADic', build_vignettes = TRUE)
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jdk-18.0.1.1')  # 설치한 JAVA version에 따라 달라집니다
buildDictionary(ext_dic = "woorimalsam")  # "woorimalsam" dic을 불러옵니다
useNIADic() 

# 고유명사 추가
buildDictionary(ext_dic = "woorimalsam", user_dic=data.frame("엔씨","nqq"),replace_usr_dic = TRUE)
buildDictionary(ext_dic = "woorimalsam", user_dic=data.frame("케이티","nqq"),replace_usr_dic = TRUE)
buildDictionary(ext_dic = "woorimalsam", user_dic=data.frame("엘지","nqq"),replace_usr_dic = TRUE)
buildDictionary(ext_dic = "woorimalsam", user_dic=data.frame("기아","nqq"),replace_usr_dic = TRUE)
buildDictionary(ext_dic = "woorimalsam", user_dic=data.frame("에스케이","nqq"),replace_usr_dic = TRUE)
buildDictionary(ext_dic = "woorimalsam", user_dic=data.frame("신세계","nqq"),replace_usr_dic = TRUE)

# package
library(tidyverse)
library(ggplot2)
library(tm)
library(NLP)
library(qdap)
library(corpus)
library(wordcloud2)
library(stringr)
library(rJava)
library(base64enc)
library(RmecabKo)
library(reshape2)
library(tidytext)


# data
news.naver_야구 <- read_csv(file = "D:/대학원/논문/소논문/뉴스_news.naver_야구.csv", col_names = TRUE, locale=locale('ko',encoding='euc-kr'))
news.naver_야구 %>% str()
news.naver_야구 <- news.naver_야구 %>%  select("언론사_야구","제목_야구","날짜_야구","링크_야구","좋아_야구","훈훈_야구","슬퍼_야구","화나_야구","후속_야구")

sports.news_야구 <- read_csv(file = "D:/대학원/논문/소논문/뉴스_sports.news_야구.csv", col_names = TRUE, locale=locale('ko',encoding='euc-kr'))
sports.news_야구 %>% str()
sports.news_야구 <- sports.news_야구 %>% select("언론사_야구","제목_야구","날짜_야구","링크_야구","좋아_야구","팬이_야구","슬퍼_야구","화나_야구","후속_야구")
names(sports.news_야구) <- c("언론사_야구","제목_야구","날짜_야구","링크_야구","좋아_야구","훈훈_야구","슬퍼_야구","화나_야구","후속_야구")

news_야구 <- rbind(news.naver_야구,sports.news_야구)
news_야구 <- news_야구[!is.na(news_야구$날짜_야구),]

야구 <- news_야구 %>% select("제목_야구")

야구_dim <- 야구 %>% dim()

news_야구$id <- c(1:야구_dim[1])

news_야구 %>% head()

news_야구$제목_야구 <- gsub("포토","",news_야구$제목_야구)
news_야구$제목_야구 <- gsub("오늘","",news_야구$제목_야구)
news_야구$제목_야구 <- gsub("경기","",news_야구$제목_야구)
news_야구$제목_야구 <- gsub("사진","",news_야구$제목_야구)
news_야구$제목_야구 <- gsub("스포츠","",news_야구$제목_야구)
news_야구$제목_야구 <- gsub("종합","",news_야구$제목_야구)
news_야구$제목_야구 <- gsub("다시","",news_야구$제목_야구)
news_야구$제목_야구 <- gsub("치어리더","",news_야구$제목_야구)
news_야구$제목_야구 <- gsub("\u7f8e","미국",news_야구$제목_야구)
news_야구$제목_야구 <- gsub("\u65e5","일본",news_야구$제목_야구)

야구 <- news_야구 %>% select("id","제목_야구")

# 데이터 분할
야구_dim_int <- 야구_dim[1] / 10000
야구_dim_int <- 야구_dim_int %>% ceiling()
n <- 야구_dim_int

야구_sp <- split(야구,rep(1:n,each=10000))

# 데이터 셋 만들기
야구_tb <- list()
야구_data_set <- list()
야구_token <- c()
야구_한글 <- list()
야구_영어 <- list()

## 단어기준 토큰화
for (i in 1:n){

    cat(i, '번째 데이터 리스트 tokenizer', '중 입니다.\n') 
  
  야구_tb[[i]] <- 야구_sp[[i]] %>% tibble()

  야구_tb[[i]] <- 야구_tb[[i]] %>% 
    unnest_tokens(input = 제목_야구, output = word, token = "words", drop = FALSE)
  
  야구_data_set[[i]] <- 야구_tb[[i]]  %>% 
    melt() %>% # 식별자id, 측정 변수variable, 측정치value 형태로 데이터를 재구성하는 함수
    as_tibble() %>% 
    select(4,1,2)
  
  names(야구_data_set[[i]]) <- c("L1","제목_야구","word")
  
  야구_한글[[i]] <- 야구_data_set[[i]] %>%  
    mutate(한글 = str_match(word,'([가-힣]+)')[,2]) %>% ## "한글" variable을 만들고 한글만 저장         
    na.omit() %>% ## ([가-힣]+)/P') 한글만을 선택하는 정규표현식
    mutate(글자수=str_length(한글)) %>%   ## "글자수" variable을 만듭니다 
    filter(str_length(한글)>=2) 
  
  야구_영어[[i]] <- 야구_data_set[[i]] %>%  
    mutate(영어 = str_match(word,'([a-zA-Z]+)')[,2]) %>% ## "한글" variable을 만들고 한글만 저장         
    na.omit() %>% ## 
    mutate(글자수=str_length(영어)) %>%   ## "글자수" variable을 만듭니다 
    filter(str_length(영어)>=3) 
  
  야구_token한글.tmp <- 야구_한글[[i]]$word
  야구_token영어.tmp <- 야구_영어[[i]]$word
  
  야구_token <- append(야구_token,야구_token한글.tmp)
  야구_token <- append(야구_token,야구_token영어.tmp)
}


#### 최다 빈도 단어 Top30을 뽑습니다
야구_token_count <- table(야구_token) ## 객체별 빈도를 셉니다
야구_token_count <- sort(야구_token_count, decreasing = TRUE) ##내림차순 정렬 합니다
야구_token_count30 <- 야구_token_count[1:30]  ## Top 30까지 추립니다

#### 빈도그래프 작성
야구_token_count30df <- 야구_token_count30 %>% as_tibble() ## tibble변환하고 그래프 작성  
names(야구_token_count30df) <- c("야구_token","Freq")

ggplot(야구_token_count30df, aes(x=Freq, y=reorder(야구_token,Freq), fill = Freq)) + 
  geom_col() + 
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  scale_fill_gradient(low = "#5D5D5D", high = "#0000FF") +
  geom_text(aes(label = Freq),hjust = -0.1,size=5) 


#### 워드크라우드 작성
야구_token_count30df %>% wordcloud2(minRotation=0, maxRotation=0, color = 'skyblue')  


#### DTM 만들기
# 변수명 바꾸기
for (i in 1:n){ 
  야구_한글[[i]] <- 야구_한글[[i]] %>% select(1,2,3,5)
  야구_영어[[i]] <- 야구_영어[[i]] %>% select(1,2,3,5)
  
  names(야구_한글[[i]]) <- c("L1","value","단어","글자수")
  names(야구_영어[[i]]) <- c("L1","value","단어","글자수")
}

# 하나의 데이터프레임으로 만들기
L1 <- c()
단어 <- c()
글자수 <- c()

for (i in 1:n){ 
  L1 <- append(L1,야구_한글[[i]]$L1)
  L1 <- append(L1,야구_영어[[i]]$L1)

  단어 <- append(단어,야구_한글[[i]]$단어)
  단어 <- append(단어,야구_영어[[i]]$단어)
  
  글자수 <- append(글자수,야구_한글[[i]]$글자수)
  글자수 <- append(글자수,야구_영어[[i]]$글자수)
}

야구_before_dtm <- tibble(L1,단어,글자수)
야구_dtm <- 야구_before_dtm %>% cast_dtm(document = L1, term = 단어, value = 글자수)

####주제모형
library(topicmodels)

## 주제개수 지정
library(ldatuning)
야구_lda_n <- FindTopicsNumber(
  야구_dtm,
  topics = 2:10,
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 12),
  mc.cores = 2L,
  verbose = TRUE
)

야구_lda_n_plot <- 야구_lda_n %>% FindTopicsNumber_plot()

야구_Griffiths2004 <- (야구_lda_n$Griffiths2004-min(야구_lda_n$Griffiths2004)) / (max(야구_lda_n$Griffiths2004)-min(야구_lda_n$Griffiths2004))

야구_CaoJuan2009 <- (야구_lda_n$CaoJuan2009-min(야구_lda_n$CaoJuan2009)) / (max(야구_lda_n$CaoJuan2009)-min(야구_lda_n$CaoJuan2009))

야구_Arun2010 <- (야구_lda_n$Arun2010-min(야구_lda_n$Arun2010)) / (max(야구_lda_n$Arun2010)-min(야구_lda_n$Arun2010))

야구_Deveaud2014 <- (야구_lda_n$Deveaud2014-min(야구_lda_n$Deveaud2014)) / (max(야구_lda_n$Deveaud2014)-min(야구_lda_n$Deveaud2014))

야구_lda_min <- 야구_CaoJuan2009 + 야구_Arun2010
야구_lda_max <- 야구_Griffiths2004 + 야구_Deveaud2014

야구_lda_k <- data.frame(야구_lda_n$topics,야구_lda_min,야구_lda_max)
야구_lda_k %>% View()

## 주제모형 산출
야구_lda <- LDA(야구_dtm, k=10, method = "Gibbs", control=list(alpha=1, delta=0.1, seed=12))

terms(야구_lda,10)

야구_topics <- 야구_lda %>% tidy(matrix = "beta")

야구_top_terms <- 야구_topics %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) # beta는 토픽에 단어가 들어갈 확률

야구_top_terms_plot <- 야구_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(x="", y="")

##문서를 토픽별로 분류하기
야구_topic <- tidy(야구_lda, matrix="gamma")

##문서별로 확률이 가장 높은 토픽 추출
야구_class <- 야구_topic %>% 
  group_by(document) %>% 
  slice_max(gamma, n=1)

##원문에 확률이 가장 높은 번호 부여
#integer로 변환
야구_class$document <- 야구_class$document %>% as.integer()
#원문에 토픽 번호 부여
news_야구 <- news_야구 %>% mutate(id = row_number())
야구_class_topic <- news_야구 %>% 
  left_join(야구_class, by = c("id" = "document"))
#결합확인
야구_class_topic %>% select(id, topic)

## 토픽별 문서 수 살펴보기
야구_class_topic %>% count(topic)

## 결측치 제거
야구_class_topic <- 야구_class_topic %>% na.omit()

## 토픽별 주요 단어 목록 만들기
야구_terms <- 야구_top_terms %>% 
  group_by(topic) %>%
  slice_max(beta, n=5, with_ties = FALSE) %>% 
  summarise(term = paste(term, collapse = ", "))
  
## 토픽별 문서 빈도 구하기
야구_class_topic_count <- 야구_class_topic %>% count(topic)

## 문서 빈도에 주요단어 결합
야구_topic_count_word <- 야구_class_topic_count %>% 
  left_join(야구_terms, by = "topic") %>% 
  mutate(topic_name = paste("Topic", topic))

## 토픽별 문서 수와 주요 단어로 막대 그래프 만들기
야구_topic_count_word_plot <- 야구_topic_count_word %>% 
  ggplot(aes(x = reorder(topic_name,-topic), y = n, fill = topic_name)) +
  geom_col() +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.1, size = 5) +
  geom_text(aes(label = term),hjust = 1.1,vjust = 0.5,col = "white",fontface = "bold",size = 5)


#### 감성사전
library(readr)
sent_dic <- read_csv("D:/대학원/논문/소논문/스포츠기사/SentiWord_Dict.txt")
names(sent_dic) <- c("value")
sent_dic <- sent_dic$value
sent_dic <- strsplit(sent_dic, split="\t")  

word <- c()
polarity <- c()

for (i in 1:length(sent_dic)){ 
word <- append(word,sent_dic[[i]][1])
polarity <- append(polarity,sent_dic[[i]][2])
}

sent_dic <- tibble(word,polarity)

## 단어기준 토큰화

야구_tb_pol <- tibble()

for (i in 1:n){
     야구_tb_pol <- bind_rows(야구_tb_pol,야구_tb[[i]])
}

## 단어에 감정 점수 부여
library(textclean)
야구_tb_pnn <- 야구_tb_pol %>% 
  left_join(sent_dic,by="word") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity)) %>% 
  mutate(sentiment = ifelse(polarity > 0, "pos", ifelse(polarity < 0, "neg", "neu"))) %>% 
  mutate(id = row_number(), 제목_야구 = str_squish(replace_html(제목_야구)))
야구_tb_pnn$polarity <- 야구_tb_pnn$polarity %>% as.integer()

## 자주 사용된 감정 단어 살펴보기
# 감정분류
야구_tb_pnn_score <- 야구_tb_pnn %>% 
  count(sentiment) %>% 
  mutate(ratio = round((n/sum(n)*100),2))

야구_tb_pnn_score %>% ggplot(aes(x = sentiment, y = ratio, fill = sentiment)) +
  geom_col() +
  geom_text(aes(label = ratio),vjust = 0.5,size=7) +
  scale_x_discrete(limits = c("pos","neu","neg"))

# 막대그래프 만들기
야구_tb_pnn_top <- 야구_tb_pnn %>% 
  filter(sentiment != "neu") %>% 
  count(sentiment, word) %>% 
  group_by(sentiment) %>% 
  slice_max(n, n=20)

ggplot(야구_tb_pnn_top, aes(x=reorder(word,n), y=n, fill=sentiment)) +
  geom_col() +
  labs(x="", y="") + 
  coord_flip() +
  geom_text(aes(label = n),hjust = -0.3) +
  facet_wrap(~ sentiment, scales =  "free") +
  scale_y_continuous(expand = c(0,0), limit=c(0,3000))

# 기사별 감정점수 구하기
야구_tb_pnn_sum <- 야구_tb_pnn %>% 
  group_by(id, 제목_야구) %>% 
  summarise(score = sum(polarity)) %>% 
  ungroup()
# 긍정 댓글
야구_tb_pnn_sum %>% 
  select(score,제목_야구) %>% 
  arrange(-score)
# 부정 댓글
야구_tb_pnn_sum %>% 
  select(score,제목_야구) %>% 
  arrange(score)

## 감정별 단어빈도
야구_tb_pnn_odd <- 야구_tb_pnn %>% 
  unnest_tokens(input = 제목_야구, output = word, token = "words", drop=FALSE) %>% 
  filter(str_detect(word, "[가-힣]") & str_count(word) >= 2)
야구_tb_pnn_freq <- 야구_tb_pnn_odd %>% 
  count(sentiment, word, sort = TRUE)

## 로그 오즈비 구하기 - 긍정 및 부정 기사에서 상대적으로 자주 사용된 단어 파악
야구_tb_pnn_odd_wide <- 야구_tb_pnn_freq %>% 
  filter(sentiment != "neu") %>% 
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = list(n=0))
야구_tb_pnn_odd_wide <- 야구_tb_pnn_odd_wide %>% 
  mutate(log_odds_ratio = log(((pos+1) / (sum(pos +1))) / ((neg+1) / (sum(neg+1)))))

# 로그 오즈비가 가장 큰 단어 10개 추출
야구_tb_pnn_odd_top <- 야구_tb_pnn_odd_wide %>% 
  group_by(sentiment= ifelse(log_odds_ratio > 0, "pos", "neg")) %>% 
  slice_max(abs(log_odds_ratio), n=10, with_ties = FALSE)

야구_tb_pnn_odd_top %>% 
  ggplot(aes(x = reorder(word, log_odds_ratio),y = log_odds_ratio,fill = sentiment))+
  geom_col() +
  labs(x="", y="") + 
  coord_flip() +
  geom_text(aes(label = round(log_odds_ratio,2)),hjust = -0.1,size=6) 

# [포토]기사 전부 제거?