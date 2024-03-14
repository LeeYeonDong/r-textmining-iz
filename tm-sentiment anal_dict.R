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

devtools::install_github('haven-jeon/NIADic/NIADic', build_vignettes = TRUE)
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


# raw
raw1 <- read_csv(file = "D:/대학원/논문/소논문/부동산_감정사전/새터_moon_df_com_본문제외.csv", col_names = TRUE, locale=locale('ko',encoding='euc-kr'))

names(raw1) <- c("언론사", "제목", "날짜", "링크")

# 데이터 분할
raw1$id <- c(1:nrow(raw1))

raw1_int <- nrow(raw1) / 100
raw1_int <- raw1_int %>% ceiling()
n <- raw1_int

raw1_sp <- split(raw1, rep(1:n, each = 100))

# 데이터 셋 만들기
raw1_tb <- list()
raw1_data_set <- list()
raw1_한글 <- list()

# 단어기준 토큰화
for (i in 1:n){
  
  cat(i, '번째 데이터 리스트 tokenizer', '중 입니다.\n') 
  
  raw1_tb[[i]] <- raw1_sp[[i]] %>% 
    tibble() %>% 
    unnest_tokens(input = 제목, output = word, token = "words", drop = FALSE) %>%  
    na.omit() %>% ## ([가-힣]+)/P') 한글만을 선택하는 정규표현식
    filter(str_length(word) >= 2) 
  }

raw1_token <- list()

for (i in 1:n){
  raw1_token <- bind_rows(raw1_token, raw1_tb[[i]])
}


# 감성사전
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
sent_dic %>% view()

# 단어에 감정 점수 부여
raw1_tb_pnn <- raw1_token %>% 
  left_join(sent_dic,by="word") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity)) %>% 
  mutate(sentiment = ifelse(polarity > 0, "pos", ifelse(polarity < 0, "neg", "neu"))) 

raw1_tb_pnn$polarity <- raw1_tb_pnn$polarity %>% as.integer()

raw1_tb_pnn_score <- raw1_tb_pnn %>% 
  count(sentiment) %>% 
  mutate(ratio = round((n/sum(n)*100),2))

write.csv(raw1_tb_pnn, file = 'D:/대학원/논문/소논문/부동산_감정사전/raw1_tb_pnn.csv',row.names=FALSE, fileEncoding = 'cp949')


# 문장 별 감정 점수 구하기
raw1_tb_pnn %>% head(120) %>% view()
