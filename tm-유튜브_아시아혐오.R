#textmining
# install.packages("corpus")
# install.packages("qdap")
# install.packages("tm")
# install.packages("wordcloud2")
# install.packages("topicmodels")
# install.packages("ldatuning")
# install.packages("dplyr")
# install.packages("tidytext")
# install.packages("stringr")
# install.packages("plyr")
# install.packages("rJava")
library(ggplot2)
library(tm)
library(qdap)
library(corpus)
library(wordcloud2)
library(stringr)
library(rJava)
library(tidytext)
library(dplyr)

df_ytb1 <- read_csv(file = "D:/대학원/논문/커뮤니케이션학과/유튜브.csv", col_names = TRUE, locale=locale('ko',encoding='euc-kr'))

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

ytb_token$word %>% view()

## 빈도그래프 작성
ytb_token_word <- ytb_token$word
ytb_token_count <- table(ytb_token_word) ## 객체별 빈도를 셉니다
ytb_token_count <- sort(ytb_token_count, decreasing = TRUE) ##내림차순 정렬 합니다
ytb_token_count50 <- ytb_token_count[1:50] 

grep("asians",ytb_token_count) %>% print()
grep("blacks",ytb_token_count) %>% print()
grep("coronavirus",ytb_token_count) %>% print()
grep("corona",ytb_token_count) %>% print()
grep("virus",ytb_token_count) %>% print()
grep("covid19",ytb_token_count) %>% print()

ytb_token_count50df <- ytb_token_count50 %>% as_tibble() ## tibble변환하고 그래프 작성  
names(ytb_token_count50df) <- c("ytb_token","Freq")

ggplot(ytb_token_count50df, aes(x=Freq, y=reorder(ytb_token,Freq), fill = Freq)) + 
  geom_col() + 
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  scale_fill_gradient(low = "#5D5D5D", high = "#003300") +
  geom_text(aes(label = Freq),hjust = -0.1,size=5) 

## 워드클라우드 작성
ytb_token_count50df %>% wordcloud2(minRotation=0, maxRotation=0, color = "#003300")
ytb_token_count50df %>% wordcloud2(minRotation=0, maxRotation=0)

ytb_token_count <- ytb_token_count %>% as.data.frame()

write.csv(ytb_token_count, file = "D:/대학원/논문/커뮤니케이션학과/유튜브댓글/단어빈도_전체.csv", row.names=FALSE)

# 연관분석
install.packages("widyr")
install.packages("tidygraph")
install.packages("ggraph")
install.packages("openxlsx")
library(openxlsx)
library(widyr)
library(tidygraph)
library(ggraph)

ytb_cor <- ytb_token %>% 
  select(id, word, 댓글) %>% 
  add_count(word) %>% 
  filter(n >= 10) %>% 
  pairwise_cor(item = word,
               feature = id,
               sort = TRUE)


cor_word <- ytb_token_count50df$ytb_token
cor_word <- cor_word[1:10]

for(i in 1:length(cor_word)){
  
  cor_word_file <- ytb_cor %>% 
    filter(item1 == cor_word[i]) %>% 
    slice(1:100)
  
  grep("asians",ytb_token_count) %>% print()
  grep("blacks",ytb_token_count) %>% print()
  grep("coronavirus",ytb_token_count) %>% print()
  grep("corona",ytb_token_count) %>% print()
  grep("virus",ytb_token_count) %>% print()
  grep("covid19",ytb_token_count) %>% print()
  
  cor_word_dir <- paste0("D:/대학원/논문/커뮤니케이션학과/유튜브댓글/연관분석_",cor_word[i],".csv")
  
  write.csv(cor_word_file, file = cor_word_dir, row.names = FALSE)
  
}

cor_word

# visualization : 개별단어
graph_cor <- ytb_cor %>% 
  filter(correlation >= 0.1) %>% # correlation 0.1 이상
  filter(item1 == "racist") %>%
  as_tbl_graph(directed = FALSE) %>% 
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))

set.seed(1029)

ggraph(graph_cor, layout = "fr") +
  
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


# 감성분석(pos-neg)
ytb_token$제목 <- ytb_token$제목 %>% as.factor()

영상제목 <- ytb_token$제목 %>% unique()

ytb_token %>%
  arrange(desc(line_number)) %>% 
  inner_join(get_sentiments("bing")) %>% 
  group_by(제목,line_number,sentiment) %>%
  tally() %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative) %>%
  filter(제목 == 영상제목[5]) %>% 
  ggplot(aes(x = line_number,y= sentiment,)) +
  geom_col(show.legend = FALSE, fill = "black") +
  scale_y_continuous(limits = c(-15,5)) +
  facet_wrap(~제목, scales = "free_x", ncol = 2)

영상제목[5]


# 감성분석(nrc)
ytb_token %>%
  head(100) %>%
  arrange(desc(line_number)) %>%
  inner_join(get_sentiments("nrc")) %>% 
  group_by(제목,line_number,sentiment) %>%
  tally() %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0)

