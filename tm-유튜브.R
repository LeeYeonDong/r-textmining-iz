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

## 의대증원
# 데이터 불러오기 및 전처리(Preprocessing)
# Load the readxl library
library(readxl)

mu_df_raw <- read_excel("D:/대학원/상담/커뮤니케이션학과/의료분쟁/df_ytb_com.xlsx") %>% 
  mutate(본문 = str_replace_all(댓글_ytb, "[^가-힣a-zA-Z0-9.,!?\\s]", " "))

mu_df_raw <- mu_df_raw[,-1]
  
mu_df_raw %>% select(제목_ytb) %>% unique() %>% nrow()
mu_df_raw %>% select(링크_ytb) %>% unique() %>% nrow()
mu_df_raw %>% select(링크_ytb) %>% unique() %>% slice(1)

