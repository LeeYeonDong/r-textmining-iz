# KoNLP useNIADic
# install.packages("multilinguer")
# library(multilinguer)
# install_jdk()
# install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), type = "binary")
# install.packages('KoNLP', repos = 'https://forkonlp.r-universe.dev')
library(KoNLP) #최종적으로 "KoNLP" 패키지를 불러옵니다

# devtools::install_github('haven-jeon/NIADic/NIADic', build_vignettes = TRUE)
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_281')  # 설치한 JAVA version에 따라 달라집니다
buildDictionary(ext_dic = "woorimalsam")  # "woorimalsam" dic을 불러옵니다
useNIADic()

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
library(ggthemes)
library(widyr)
library(ggraph)
library(tidygraph)

start.time <- Sys.time()

# 데이터 로드
re_df <- read_csv(file = "D:/대학원/논문/부동산_토픽모델링/부동산_df.csv", col_names = TRUE, locale=locale('ko',encoding='euc-kr'))

re_df$id <- c(1:nrow(re_df))
re_df %>% str()


# 데이터 셋 만들기
re_df_tb <- list()
re_df_data_set <- list()
re_df_한글 <- list()
re_df_영어 <- list()
re_df_한영 <- list()

## 단어기준 토큰화
for (i in 1:n){
  
  cat(i, '번째 데이터 리스트 tokenizer', '중 입니다.\n') 
  
  re_df_tb[[i]] <- re_df_sp[[i]] %>% 
    tibble() %>% 
    unnest_tokens(input = 제목, output = word, token = "words", drop = FALSE)
  
  re_df_한글[[i]] <- re_df_tb[[i]] %>%  
    mutate(한글 = str_match(word,'([가-힣]+)')[,2]) %>% ## "한글" variable을 만들고 한글만 저장
    na.omit() %>% ## ([가-힣]+)/P') 한글만을 선택하는 정규표현식
    mutate(글자수 = str_length(한글)) %>%   ## "글자수" variable을 만듭니다 
    filter(글자수>=2) 
  
  re_df_영어[[i]] <- re_df_tb[[i]] %>%  
    mutate(영어 = str_match(word,'([a-zA-Z]+)')[,2]) %>% ## "한글" variable을 만들고 한글만 저장     
    na.omit() %>% ## 
    mutate(글자수 = str_length(영어)) %>%   ## "글자수" variable을 만듭니다 
    filter(글자수>=2) 
  
  re_df_한영[[i]] <- bind_rows(re_df_한글[[i]], re_df_영어[[i]])
  
}

re_df_token <- list()

for (i in 1:n){
  re_df_token <- bind_rows(re_df_token,re_df_한영[[i]])
}

re_df_token <- re_df_token %>% 
  select(-c("한글", "글자수", "영어"))


# 불용어 - 반복 작업 필요
stopwords_kor <- read_csv(file = "D:/대학원/textmining/Doit/stopwords_kor.csv", col_names = TRUE, locale = locale('ko',encoding='utf-8'))

stopwords_kor

chr <- stopwords_kor$stopwords_kor

제거 <- c()

for(i in 1:length(chr)){
  
  cat(i, '번째 전처리 제거 단어를 찾는 중 입니다.\n') 
  
  del.tmp <- grep(chr[i], re_df_token$word)
  제거 <- append(제거,del.tmp)
}

re_df_token <- re_df_token[-제거,]
re_df_token %>% head(1000) %>% view()

re_df_token$word %>% table() %>% sort(decreasing = TRUE) %>% head(100)



#### 감성분석
##감성사전
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
sent_dic$polarity <- sent_dic$polarity %>% as.double()

sent_dic %>% filter(word == "승리")

grep("승리",sent_dic$word)

이기 <- grep("이기",sent_dic$word)
sent_dic[이기,]

sent_dic[7694,2] <- c(2)
sent_dic[7695,2] <- c(2)


## 단어에 감정 점수 부여
re_df_tb_pnn <- re_df_token %>% 
  left_join(sent_dic,by="word") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity)) %>% 
  group_by(id) %>% 
  summarize(sen_pol = sum(polarity))

re_df_tb_pnn$sen_pol %>% table()

re_sen_df <- re_df %>% 
  left_join(re_df_tb_pnn, by="id") %>% 
  filter(!is.na(sen_pol)) %>% 
  mutate(sen_pola = ifelse(sen_pol > 0, 1, ifelse(sen_pol < 0, -1, 0))) %>% 
select(-sen_pol)

re_sen_df$sen_pola %>% table()


write.csv(re_sen_df, file = "D:/대학원/논문/소논문/부동산_감정사전/re_df.csv", row.names=FALSE, fileEncoding = 'cp949')


# bi-lstm sentimental score
ss <- read_csv(file = "D:/대학원/논문/소논문/부동산_감정사전/sentiment_scores.csv", col_names = TRUE, locale=locale('ko',encoding='euc-kr'))

ss %>% head()
ss %>% dim()

ss <- ss %>%
  mutate(Sentiment_Score = ifelse(Sentiment_Score > 0.5, 1, -1))

ss <- ss[grepl("[가-힣]", ss$Word), ]

ss_df <- ss %>% 
  mutate(글자수 = str_length(Word)) %>% 
  rename(word = Word) %>% 
  select(c("word", "Sentiment_Score", "글자수")) %>% 
  filter(글자수 >= 2) 
  

# knu-bi dict 비교
sent_dic_re <- sent_dic %>%
  mutate(polarity = ifelse(polarity > 0, 1, -1))

compare <- ss_df %>% 
  left_join(sent_dic_re, by="word") %>% 
  na.omit() %>% 
  select(-c("글자수")) %>%
  mutate(match_status = ifelse(Sentiment_Score == polarity, "Matched", "Mismatched")) %>%
  rename(KNU_dict = polarity, real_estate_dict = Sentiment_Score) %>%
  mutate(KNU_dict = case_when(
      KNU_dict == 1 ~ "pos",
      KNU_dict == -1 ~ "neg",
      TRUE ~ as.character(KNU_dict)),
      real_estate_dict = case_when(
        real_estate_dict == 1 ~ "pos",
        real_estate_dict == -1 ~ "neg",
        TRUE ~ as.character(real_estate_dict)  
      ))

write.csv(compare, file = "D:/대학원/논문/소논문/부동산_감정사전/compare.csv", row.names=FALSE, fileEncoding = 'cp949')


# frequency table
# 혼동 행렬을 계산합니다.
table(KNU_dict = compare$KNU_dict, real_estate_dict = compare$real_estate_dict)

# Confusion Matrix 생성
conf_matrix <- matrix(c(792, 156, 40, 412), nrow = 2, byrow = TRUE)

# 행과 열 이름 설정
rownames(conf_matrix) <- c("neg", "pos")
colnames(conf_matrix) <- c("neg", "pos")

# Confusion Matrix 출력
print(conf_matrix)

# 성능 지표 계산
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
sensitivity <- conf_matrix[1,1] / sum(conf_matrix[1,])
specificity <- conf_matrix[2,2] / sum(conf_matrix[2,])
precision <- conf_matrix[1,1] / sum(conf_matrix[,1])
f1_score <- 2 * (precision * sensitivity) / (precision + sensitivity)

# 결과 출력
cat("Accuracy:", accuracy, "\n")
cat("Sensitivity (Recall):", sensitivity, "\n")
cat("Specificity:", specificity, "\n")
cat("Precision:", precision, "\n")
cat("F1 Score:", f1_score, "\n")
