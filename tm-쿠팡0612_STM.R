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