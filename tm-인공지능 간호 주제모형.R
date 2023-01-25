# KoNLP useNIADic
install.packages("multilinguer")
library(multilinguer)
# install_jdk()
install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), type = "binary")
#
install.packages("remotes")
devtools::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))
library(KoNLP) #최종적으로 "KoNLP" 패키지를 불러옵니다

devtools::install_github('haven-jeon/NIADic/NIADic', build_vignettes = TRUE)
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jdk-18.0.1.1')  # 설치한 JAVA version에 따라 달라집니다
buildDictionary(ext_dic = "woorimalsam")  # "woorimalsam" dic을 불러옵니다
useNIADic() 

# package
library(tidyverse)
library(dplyr)
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
library(ggthemes)
library(widyr)
library(ggraph)
library(tidygraph)
library(tidytext)

# data
인공간호 <- read_csv(file = "D:/대학원/논문/인공지능 간호/인공지능간호.csv", col_names = TRUE, locale=locale('ko',encoding='euc-kr'))
인공간호 %>% str()
간호 <- 인공간호 %>% select("키워드","일자")
간호 <- 간호 %>% arrange(-desc(일자))
간호$일자

간호_dim <- 간호 %>% dim()

간호$id <- c(1:간호_dim[1])
간호$일자 <- str_sub(간호$일자,1,4)

# 년도별 추이
간호_년도_table <- 간호$일자 %>% table() %>% as_tibble()
names(간호_년도_table) <- c("날짜_년별","Freq")

날짜_년별_추가 <- c("1995","1997","1998","1999","2000","2003","2004","2007","2010","2011","2012","2013")
zero <- rep(0,length(날짜_년별_추가))

간호_년도_table_추가 <- tibble(날짜_년별_추가,zero)
names(간호_년도_table_추가) <- c("날짜_년별","Freq")

간호_년도_table <- bind_rows(간호_년도_table,간호_년도_table_추가)
간호_년도_table <- 간호_년도_table %>% arrange(-desc(날짜_년별))

ggplot(data = 간호_년도_table, aes(x = 날짜_년별, y = Freq, group = 1)) + 
  geom_line(size = 2, colour="#006600") + 
  geom_point(size = 1, colour="#006600") +
  geom_text(aes(label = Freq),hjust = 1,size=5) +
  geom_hline(yintercept = mean(간호_년도_table$Freq), color='red',linetype='dashed', size = 1) +
  labs(x="", y="") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=45, hjust=1, size = 20))


# 데이터 분할
간호_dim_int <- 간호_dim[1] / 10
간호_dim_int <- 간호_dim_int %>% ceiling()
n <- 간호_dim_int

간호_sp <- split(간호,rep(1:n,each=10))


# 데이터 셋 만들기
간호_tb <- list()
간호_data_set <- list()
간호_token <- tibble()
간호_한글 <- list()
간호_영어 <- list()

## 단어기준 토큰화
for (i in 1:n){
  
  cat(i, '번째 데이터 리스트 tokenizer', '중 입니다.\n') 
  
  간호_tb[[i]] <- 간호_sp[[i]] %>% tibble()
  
  간호_tb[[i]] <- 간호_tb[[i]] %>% 
    unnest_tokens(input = 키워드, output = word, token = "words", drop = FALSE)
  
  간호_data_set[[i]] <- 간호_tb[[i]]  %>% 
    melt() %>% # 식별자id, 측정 변수variable, 측정치value 형태로 데이터를 재구성하는 함수
    as_tibble() %>% 
    dplyr::select(5,1,2,3)
  
  names(간호_data_set[[i]]) <- c("id","키워드","일자","word")
  
  간호_한글[[i]] <- 간호_data_set[[i]] %>%  
    mutate(한글 = str_match(word,'([가-힣]+)')[,2]) %>% ## "한글" variable을 만들고 한글만 저장         
    na.omit() %>% ## ([가-힣]+)/P') 한글만을 선택하는 정규표현식
    mutate(글자수=str_length(한글)) %>%   ## "글자수" variable을 만듭니다 
    filter(str_length(한글)>=2) 
  
  간호_영어[[i]] <- 간호_data_set[[i]] %>%  
    mutate(영어 = str_match(word,'([a-zA-Z]+)')[,2]) %>% ## "한글" variable을 만들고 한글만 저장         
    na.omit() %>% ## 
    mutate(글자수=str_length(영어)) %>%   ## "글자수" variable을 만듭니다 
    filter(str_length(영어)>=3) 
  
  간호_token한글.tmp <- tibble(간호_한글[[i]]$id,간호_한글[[i]]$word,간호_한글[[i]]$일자,간호_한글[[i]]$키워드)
  names(간호_token한글.tmp) <- c("id","word", "일자", "키워드")
  간호_token영어.tmp <- tibble(간호_영어[[i]]$id,간호_영어[[i]]$word,간호_영어[[i]]$일자,간호_영어[[i]]$키워드)
  names(간호_token영어.tmp) <- c("id","word", "일자", "키워드")
  
  간호_token <- bind_rows(간호_token,간호_token한글.tmp)
  간호_token <- bind_rows(간호_token,간호_token영어.tmp)
}
간호_token$일자 <- 간호_token$일자 %>% as.integer()


# 데이터 전처리
제거 <- c()

chr <- c("article", "www.joongdo.co.kr", "view.jsp", "nurl", "jsp", "http")

for(i in 1:length(chr)){
  
  cat(i, '번째 전처리 제거 단어를 찾는 중 입니다.\n') 
  
  del.tmp <- grep(chr[i],간호_token$word)
  제거 <- append(제거,del.tmp)
}

제거 <- 제거 %>% unique()

간호_token <- 간호_token[-제거,]


#### 최다 빈도 단어 Top30을 뽑습니다
token_count_table <- table(간호_token$word) ## 객체별 빈도를 셉니다
token_count <- sort(token_count_table, decreasing = TRUE) ##내림차순 정렬 합니다
token_count30 <- token_count[1:30]  ## Top 30까지 추립니다

#### 빈도그래프 작성
token_count30df <- token_count30 %>% as.data.frame() ## tibble변환하고 그래프 작성  
names(token_count30df) <- c("token","Freq")

token_count30df %>% 
  ggplot(aes(x=Freq, y=reorder(token,Freq), fill = Freq)) + 
  geom_col() + 
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  theme(axis.text.y=element_text(angle=0, hjust=1, size = 20)) +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  scale_fill_gradient(low = "#5D5D5D", high = "#0000FF") +
  geom_text(aes(label = Freq),hjust = -0.1,size=5) 

#### 워드크라우드 작성
token_count30 <- token_count[1:30] 
token_count30df <- token_count30 %>% as.data.frame()
names(token_count30df) <- c("token","Freq")
token_count30df %>% wordcloud2(minRotation=0, maxRotation=0, color = 'black') 


#### 년도별 빈도그래프 및 워드 크라우드
# 90년대
(간호_token %>% filter(일자 %in% c(1990:1999)) %>% select(word) %>% table() %>% sort(decreasing = TRUE))[1:30] %>% as.data.frame() %>% 
  as_tibble() %>% 
  ggplot(aes(x=n, y=reorder(word,Freq), fill = Freq)) + 
  geom_col() + 
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  theme(axis.text.y=element_text(angle=0, hjust=1, size = 20)) +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  scale_fill_gradient(low = "#5D5D5D", high = "#0000FF") +
  geom_text(aes(label = n),hjust = -0.1,size=5)

(간호_token %>% filter(일자 %in% c(1990:1999)) %>% select(word) %>% table() %>% sort(decreasing = TRUE))[1:30] %>% as_tibble() %>% wordcloud2(minRotation=0, maxRotation=0, color = 'black') 

# 00년대
(간호_token %>% filter(일자 %in% c(2000:2009)) %>% select(word) %>% table() %>% sort(decreasing = TRUE))[1:30] %>% as.data.frame() %>% 
  as_tibble() %>% 
  ggplot(aes(x=n, y=reorder(word,Freq), fill = Freq)) + 
  geom_col() + 
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  theme(axis.text.y=element_text(angle=0, hjust=1, size = 20)) +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  scale_fill_gradient(low = "#5D5D5D", high = "#0000FF") +
  geom_text(aes(label = n),hjust = -0.1,size=5)

(간호_token %>% filter(일자 %in% c(2000:2009)) %>% select(word) %>% table() %>% sort(decreasing = TRUE))[1:30] %>% as_tibble() %>% wordcloud2(minRotation=0, maxRotation=0, color = 'black') 

# 10년대 초중반
(간호_token %>% filter(일자 %in% c(2010:2015)) %>% select(word) %>% table() %>% sort(decreasing = TRUE))[1:30] %>% as.data.frame() %>%as_tibble() %>% 
  ggplot(aes(x=n, y=reorder(word,Freq), fill = Freq)) + 
  geom_col() + 
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  theme(axis.text.y=element_text(angle=0, hjust=1, size = 20)) +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  scale_fill_gradient(low = "#5D5D5D", high = "#0000FF") +
  geom_text(aes(label = n),hjust = -0.1,size=5)

(간호_token %>% filter(일자 %in% c(2010:2014)) %>% select(word) %>% table() %>% sort(decreasing = TRUE))[1:30] %>% as_tibble() %>% wordcloud2(minRotation=0, maxRotation=0, color = 'black') 


# 2016
(간호_token %>% filter(일자 == 2016) %>% select(word) %>% table() %>% sort(decreasing = TRUE))[1:30] %>% as_tibble() %>% as.data.frame() %>%
  ggplot(aes(x=n, y=reorder(word,Freq), fill = Freq)) + 
  geom_col() + 
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  theme(axis.text.y=element_text(angle=0, hjust=1, size = 20)) +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  scale_fill_gradient(low = "#5D5D5D", high = "#0000FF") +
  geom_text(aes(label = n),hjust = -0.1,size=5)

(간호_token %>% filter(일자 == 2016) %>% select(word) %>% table() %>% sort(decreasing = TRUE))[1:30] %>% as_tibble() %>% wordcloud2(minRotation=0, maxRotation=0, color = 'black') 

# 2017
(간호_token %>% filter(일자 == 2017) %>% select(word) %>% table() %>% sort(decreasing = TRUE))[1:30] %>% as_tibble() %>% as.data.frame() %>%
  ggplot(aes(x=n, y=reorder(word,Freq), fill = Freq)) + 
  geom_col() + 
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  theme(axis.text.y=element_text(angle=0, hjust=1, size = 20)) +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  scale_fill_gradient(low = "#5D5D5D", high = "#0000FF") +
  geom_text(aes(label = n),hjust = -0.1,size=5)

(간호_token %>% filter(일자 == 2017) %>% select(word) %>% table() %>% sort(decreasing = TRUE))[1:30] %>% as_tibble() %>% wordcloud2(minRotation=0, maxRotation=0, color = 'black') 

# 2018
(간호_token %>% filter(일자 == 2018) %>% select(word) %>% table() %>% sort(decreasing = TRUE))[1:30] %>% as_tibble() %>% as.data.frame() %>% 
  ggplot(aes(x=n, y=reorder(word,Freq), fill = Freq)) + 
  geom_col() + 
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  theme(axis.text.y=element_text(angle=0, hjust=1, size = 20)) +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  scale_fill_gradient(low = "#5D5D5D", high = "#0000FF") +
  geom_text(aes(label = n),hjust = -0.1,size=5)

(간호_token %>% filter(일자 == 2018) %>% select(word) %>% table() %>% sort(decreasing = TRUE))[1:30] %>% as_tibble() %>% wordcloud2(minRotation=0, maxRotation=0, color = 'black') 

# 2019
(간호_token %>% filter(일자 == 2019) %>% select(word) %>% table() %>% sort(decreasing = TRUE))[1:30] %>% as_tibble() %>% as.data.frame() %>% 
  ggplot(aes(x=n, y=reorder(word,Freq), fill = Freq)) + 
  geom_col() + 
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  theme(axis.text.y=element_text(angle=0, hjust=1, size = 20)) +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  scale_fill_gradient(low = "#5D5D5D", high = "#0000FF") +
  geom_text(aes(label = n),hjust = -0.1,size=5)

(간호_token %>% filter(일자 == 2019) %>% select(word) %>% table() %>% sort(decreasing = TRUE))[1:30] %>% as_tibble() %>% wordcloud2(minRotation=0, maxRotation=0, color = 'black') 

# 2020
(간호_token %>% filter(일자 == 2020) %>% select(word) %>% table() %>% sort(decreasing = TRUE))[1:30] %>% as_tibble() %>% as.data.frame() %>% 
  ggplot(aes(x=n, y=reorder(word,Freq), fill = Freq)) + 
  geom_col() + 
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  theme(axis.text.y=element_text(angle=0, hjust=1, size = 20)) +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  scale_fill_gradient(low = "#5D5D5D", high = "#0000FF") +
  geom_text(aes(label = n),hjust = -0.1,size=5)

(간호_token %>% filter(일자 == 2020) %>% select(word) %>% table() %>% sort(decreasing = TRUE))[1:30] %>% as_tibble() %>% wordcloud2(minRotation=0, maxRotation=0, color = 'black') 

# 2021
(간호_token %>% filter(일자 == 2021) %>% select(word) %>% table() %>% sort(decreasing = TRUE))[1:30] %>% as_tibble() %>% as.data.frame() %>% 
  ggplot(aes(x=n, y=reorder(word,Freq), fill = Freq)) + 
  geom_col() + 
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  theme(axis.text.y=element_text(angle=0, hjust=1, size = 20)) +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  scale_fill_gradient(low = "#5D5D5D", high = "#0000FF") +
  geom_text(aes(label = n),hjust = -0.1,size=5)

(간호_token %>% filter(일자 == 2021) %>% select(word) %>% table() %>% sort(decreasing = TRUE))[1:30] %>% as_tibble() %>% wordcloud2(minRotation=0, maxRotation=0, color = 'black') 

# 2022
(간호_token %>% filter(일자 == 2022) %>% select(word) %>% table() %>% sort(decreasing = TRUE))[1:30] %>% as_tibble() %>% as.data.frame() %>% 
  ggplot(aes(x=n, y=reorder(word,Freq), fill = Freq)) + 
  geom_col() + 
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  theme(axis.text.y=element_text(angle=0, hjust=1, size = 20)) +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  scale_fill_gradient(low = "#5D5D5D", high = "#0000FF") +
  geom_text(aes(label = n),hjust = -0.1,size=5)

(간호_token %>% filter(일자 == 2022) %>% select(word) %>% table() %>% sort(decreasing = TRUE))[1:30] %>% as_tibble() %>% wordcloud2(minRotation=0, maxRotation=0, color = 'black') 


#### DTM 만들기
# 변수명 바꾸기
for (i in 1:n){ 
  간호_한글[[i]] <- 간호_한글[[i]] %>% select(1,2,3,5,6)
  간호_영어[[i]] <- 간호_영어[[i]] %>% select(1,2,3,5,6)
  
  names(간호_한글[[i]]) <- c("L1","value","일자","단어","글자수")
  names(간호_영어[[i]]) <- c("L1","value","일자","단어","글자수")
}

# 하나의 데이터프레임으로 만들기
L1 <- c()
단어 <- c()
글자수 <- c()

for (i in 1:n){ 
  L1 <- append(L1,간호_한글[[i]]$L1)
  L1 <- append(L1,간호_영어[[i]]$L1)

  단어 <- append(단어,간호_한글[[i]]$단어)
  단어 <- append(단어,간호_영어[[i]]$단어)
  
  글자수 <- append(글자수,간호_한글[[i]]$글자수)
  글자수 <- append(글자수,간호_영어[[i]]$글자수)
}

# 간호_한글[[1]] %>% view()

간호_before_dtm <- tibble(L1,단어,글자수)
간호_dtm <- 간호_before_dtm %>% cast_dtm(document = L1, term = 단어, value = 글자수)

####주제모형
library(topicmodels)

## 주제개수 지정
library(ldatuning)
간호_lda_n <- FindTopicsNumber(
  간호_dtm,
  topics = 2:10,
  metrics = c("Griffiths2004", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 12),
  mc.cores = 2L,
  verbose = TRUE
)

간호_lda_n_plot <- 간호_lda_n %>% FindTopicsNumber_plot()

간호_Griffiths2004 <- (간호_lda_n$Griffiths2004-min(간호_lda_n$Griffiths2004)) / (max(간호_lda_n$Griffiths2004)-min(간호_lda_n$Griffiths2004))

간호_CaoJuan2009 <- (간호_lda_n$CaoJuan2009-min(간호_lda_n$CaoJuan2009)) / (max(간호_lda_n$CaoJuan2009)-min(간호_lda_n$CaoJuan2009))

간호_Arun2010 <- (간호_lda_n$Arun2010-min(간호_lda_n$Arun2010)) / (max(간호_lda_n$Arun2010)-min(간호_lda_n$Arun2010))

간호_Deveaud2014 <- (간호_lda_n$Deveaud2014-min(간호_lda_n$Deveaud2014)) / (max(간호_lda_n$Deveaud2014)-min(간호_lda_n$Deveaud2014))

간호_lda_min <- 간호_CaoJuan2009 + 간호_Arun2010
간호_lda_max <- 간호_Griffiths2004 + 간호_Deveaud2014

간호_lda_k <- data.frame(간호_lda_n$topics,간호_lda_max-간호_lda_min)
sn <- ((간호_lda_k[order(간호_lda_max-간호_lda_min, decreasing = TRUE), ])$간호_lda_n.topics)[1] # or 5 8
sn <- 5


## 주제모형 산출
간호_lda <- LDA(간호_dtm, k = 5, method = "Gibbs", control=list(alpha = 1, delta = 0.1, seed = 1029))
# Alpha and Beta Hyperparameters – alpha represents document-topic density and Beta represents topic-word density. Higher the value of alpha, documents are composed of more topics and lower the value of alpha, documents contain fewer topics. On the other hand, higher the beta, topics are composed of a large number of words in the corpus, and with the lower value of beta, they are composed of few words.


간호_dtm %>% as.matrix()

terms(간호_lda, 10)

간호_topics <- 간호_lda %>% tidy(matrix = "beta")

간호_top_terms <- 간호_topics %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) # beta는 토픽에 단어가 들어갈 확률

간호_top_terms_plot <- 간호_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(x="", y="")

## 문서를 토픽별로 분류하기
간호_topic <- tidy(간호_lda, matrix = "gamma")

##문서별로 확률이 가장 높은 토픽 추출
간호_class <- 간호_topic %>% 
  group_by(document) %>% 
  slice_max(gamma, n = 1)

##원문에 확률이 가장 높은 번호 부여
#integer로 변환
간호_class$document <- 간호_class$document %>% as.integer()
#원문에 토픽 번호 부여
news_간호 <- 간호 %>% mutate(id = row_number())
간호_class_topic <- news_간호 %>% 
  left_join(간호_class, by = c("id" = "document"))
#결합확인
간호_class_topic %>% select(id, topic)

## 토픽별 문서 수 살펴보기
간호_class_topic_count <- 간호_class_topic %>% count(topic)
## 결측치 제거
간호_class_topic <- 간호_class_topic %>% na.omit()

## 토픽별 주요 단어 목록 만들기
간호_terms <- 간호_top_terms %>% 
  group_by(topic) %>%
  slice_max(beta, n = 5, with_ties = FALSE) %>% 
  summarise(term = paste(term, collapse = ", "))

## 문서 빈도에 주요단어 결합
간호_topic_count_word <- 간호_class_topic_count %>% 
  left_join(간호_terms, by = "topic") %>% 
  mutate(topic_name = paste("Topic", topic))

## 토픽별 문서 수와 주요 단어로 막대 그래프 만들기
간호_topic_count_word_plot <- 간호_topic_count_word %>% 
  ggplot(aes(x = reorder(topic_name,-topic), y = n, fill = topic_name)) +
  geom_col() +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.1, size = 10) +
  geom_text(aes(label = term),hjust = 1.1,vjust = 0.5,col = "black",fontface = "bold",size = 6)



# 동시 출현 네트워크 - 동시 출현 빈도를 이용한 네트워크

# 데이터 전처리
간호_co <- 간호_token %>% 
  select(id, word, 키워드) %>%
  add_count(word) %>% 
  filter(n >= 100) %>% 
  pairwise_count(item = word,
                 feature = id,
                 sort = TRUE) 

간호_co$item1 %>% unique() %>% length()
간호_co$item2 %>% unique() %>% length()

간호_co <- 간호_co[1:200,]

간호_co_graph <- 간호_co %>% 
  as_tbl_graph(directed = FALSE) %>% 
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))

간호_co_graph %>% as.data.frame()

set.seed(1029)

간호_co_graph <- ggraph(간호_co_graph, layout = "fr") +
  geom_edge_link(color = "black", alpha = 1) +
  geom_node_point(color = "#003300", alpha = 1, size = 5) +
  geom_node_text(aes(label = name), repel = TRUE , size = 10) +
  theme_graph()


# 단어 간 상관분석 - 다른 단어에 비해 상대적으로 자주 함께 사용된 단어 파악, 전처리에 용이
library(widyr)
library(tidygraph)
library(ggraph)

간호_cor <- 간호_token %>% 
  select(id, word, 키워드) %>%
  add_count(word) %>% 
  filter(n >= 30) %>% 
  pairwise_cor(item = word,
               feature = id,
               sort = TRUE)

간호_cor_graph <- 간호_cor %>% 
  filter(correlation >= 0.9) %>%
  as_tbl_graph(directed = FALSE) %>% 
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))

간호_cor_graph %>% as.data.frame()


set.seed(1029)

ggraph(간호_cor_graph, layout = "fr") +
  
  geom_edge_link(color = "gray50",
                 aes(edge_alpha = correlation,
                     edge_width = correlation),
                 show.legend = FALSE) +
  scale_edge_width(range = c(1,4)) +
  
  geom_node_point(aes(size = centrality,
                      color = group),
                  show.legend = FALSE) +
  scale_size(range = c(5,10)) +
  
  geom_node_text(aes(label = name),
                 repel = TRUE,
                 size = 5) +
  theme_graph()


# id-idf
간호_id_idf <- 간호_token %>% 
  count(키워드, word) %>% 
  filter(str_count(word) > 1) %>% 
  bind_tf_idf(term = word,
              document = 키워드,
              n = n) %>% 
  arrange(-tf_idf)

write.csv(간호_id_idf, file = "D:/대학원/논문/인공지능 간호/간호_id_idf.csv", fileEncoding = "euc-kr")
