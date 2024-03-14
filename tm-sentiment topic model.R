library(ggplot2)
library(tm)
library(qdap)
library(corpus)
library(wordcloud2)
library(stringr)
library(rJava)
library(tidytext)
library(dplyr)
library(syuzhet)
library(topicmodels)
library(tidytext)
library(tidyverse)
library(remotes)
library(devtools)
install_github("EmilHvitfeldt/textdata")

## data
df_ytb1 <- read_csv(file = "D:/대학원/상담/커뮤니케이션학과/유튜브.csv", col_names = TRUE, locale=locale('ko',encoding='utf-8'))

df_ytb1 %>% head(100)

# df_ytb1 <- df_ytb_com
df_ytb1$id <- c(1:nrow(df_ytb1))

# 데이터 분할
df_ytb1 %>% dim() 

names(df_ytb1) <- c("글쓴이","제목","댓글","날짜","line_number","id")

n <- nrow(df_ytb1) / 1000
n <- n %>% ceiling()

ytb_sp <- split(df_ytb1,rep(1:n,each=1000))

# 데이터 셋 만들기
ytb_tb <- list()
ytb_data_set <- list()
ytb_영어 <- list()

## 단어기준 토큰화
for (i in 1:n){
  
  cat(i, '번째 데이터 리스트 tokenizer', '중 입니다.\n') 
  
  ytb_tb[[i]] <- ytb_sp[[i]] %>% 
    unnest_tokens(input = 댓글, output = word, token = "words", drop = FALSE)
  
  ytb_영어[[i]] <- ytb_tb[[i]] %>%  
    mutate(영어 = str_match(word,'([a-zA-Z]+)')[,2]) %>% ## "영어" variable을 만들고 한글만 저장     
    na.omit() %>% ## 
    mutate(글자수 = str_length(영어)) %>%   ## "글자수" variable을 만듭니다 
    filter(글자수 >= 3) 
}

ytb_token <- list()

for (i in 1:n){
  ytb_token <- bind_rows(ytb_token,ytb_영어[[i]])
}

# 데이터 전처리
삭제 <- c()

chr <- c("the","and","you","this","are","that","not","for","they","all","from","but","ryan","have","just","like","with","what","was","can","then","their","your","who","how","about","come","why","because","will","dear","them","there","being","don't","when","been","one","out","get","more","make","has","where","even","its","only","did","some","his","say","were","most","here","still","came","any","him","also","those","our","than","well","too","want","over","cannot","not")

for(i in 1:length(chr)){
  
  cat(i, '번째 전처리 단어 제거 중 입니다.\n') 
  
  del.tmp <- grep(chr[i],ytb_token$word)
  삭제 <- append(삭제,del.tmp)
}

삭제 <- 삭제 %>% unique()

ytb_token <- ytb_token[-삭제,]

a <- gsub("asians","asian",ytb_token$word)
a <- gsub("blacks","black",a)
a <- gsub("coronavirus","covid",a)
a <- gsub("corona","covid",a)
a <- gsub("virus","covid",a)
a <- gsub("covid19","covid",a)

ytb_token$word <- a

install_github("EmilHvitfeldt/textdata")
get_sentiments("nrc") %>% select("sentiment") %>% unique()
# 감성분석(nrc)
# beta - topic별 단어 분포
ytb_token$word %>% table() %>% wordcloud2()

unique(ytb_token$제목)
nrc_df <- ytb_token %>%
  filter(제목 == unique(ytb_token$제목)[1]) %>% 
  select("line_number","제목", "댓글", "word") %>% 
  arrange(-desc(line_number)) %>%
  inner_join(get_sentiments("nrc")) %>%
#  group_by(line_number,sentiment) %>%
#  tally() %>%
  count(line_number,sentiment) %>% 
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  as.data.frame() %>%
  select(-line_number) %>% t() %>% as.data.frame()

nrc_df %>% view()

colnames(nrc_df) <- nrc_df["word",] %>% as.character()
nrc_df <- nrc_df[-1,]
nrc_df

# nrc_df <- ytb_token %>%
#   # head(1000) %>%
#   arrange(-desc(line_number)) %>%
#   inner_join(get_sentiments("nrc")) %>% 
#   group_by(line_number,word) %>%
#   tally() %>%
#   pivot_wider(names_from = word, values_from = n, values_fill = 0) %>% 
#     as.data.frame() %>%
#     t() %>% as.data.frame()
# 
# colnames(nrc_df) <- nrc_df["word",] %>% as.character()
# nrc_df <- nrc_df[-c(1,2),]
# nrc_df
# 
# nrc_df <- ytb_token %>%
#   # head(1000) %>%
#   arrange(-desc(line_number)) %>%
#   inner_join(get_sentiments("nrc")) %>% 
#   group_by(line_number,word) %>%
#   tally() 

nrc_lda <- ytb_token %>%
  filter(제목 == unique(ytb_token$제목)[1]) %>% 
  select("line_number","제목", "댓글", "word") %>% 
  arrange(-desc(line_number)) %>%
  # inner_join(get_sentiments("nrc")) %>%
  group_by(line_number,word) %>%
  tally() %>% 
  cast_dtm(document = line_number, term = word, value = n) %>% 
  LDA(k = 2, method = "Gibbs", control=list(alpha = 1, delta = 0.1, seed = 1029))

nrc_lda %>% str()

terms(nrc_lda, 20)
