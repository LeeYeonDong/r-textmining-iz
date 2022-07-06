# R 들어가기
https://r-pyomega.tistory.com/5?category=873554 # R Language / R Studio 설치

## 산술 연산자(arithmetic operators)
(20+3*2)/3
20^2
10 %% 3 # 나눗셈의 나머지
12 %/% 5 # 나눗셈의 몫

## 할당 연산자
x1 <- 1:10
x2 = 101:110
1001:1100 -> x3


## 논리 연산자 (TRUE, FALSE)
10 < 11
100 < 29
10 <= 11
100 <= 29

10 == 10 # 정확히 같음(Exactly equal to)
10 == 29

10 != 29 # 같지 않음(Not equal to)
10 != 10

## 수열(sequence)의 생성
1:10

seq(from = 1, to = 5, by = 0.5)

rep(x = 10, times = 5)
rep(x = 1:5, times = 2)


## 값(value)을 변수에 할당
x <- 3*4
print(x)
x

## 함수에 대한 도움말
help(summary)
?summary
??summary
  
summary(1:10)

## 객체 종류 
벡터_숫자 <- c(1,2,3) # 숫자형 벡터
벡터_숫자1 <- c("1","2","3") # 문자형 벡터
벡터_논리 <- c(TRUE,TRUE, FALSE, TRUE) # 논리형 벡터

행렬 <- matrix(c(1:10), ncol = 2, nrow = 5)
matrix(c(1:10), ncol = 2, byrow = TRUE)
matrix(c(1:10), ncol = 2, byrow = FALSE)

행1 <- c(1:10)
행2 <- c(11:20)

데이터프레임 <- data.frame(행1,행2)

행a <- c(1:10000)
행b <- c(10001:20000)
data.frame(행a,행b)

티블 <- tibble(행a,행b)
리스트 <- list(데이터프레임,티블)

## 객체 확인
class(벡터)
class(행렬)
class(행1)
class(데이터프레임)
class(티블)
class(리스트)

## 객체 변환
벡터_숫자 <- as.character(벡터_숫자)
class(벡터_숫자)

class(as.integer(벡터_숫자))

# 요소(factor) 변환
library(ggplot2)
data(mpg)
mpg
ggplot(mpg, aes(x = cty, y= hwy, colour = cyl)) + 
  geom_point()
# "cyl" 숫자가 높거나 낮은건 단순히 cyl의 차이 
ggplot(mpg, aes(x = cty, y= hwy, colour = factor(cyl))) + 
  geom_point()

## 데이터 프레임 살펴보기
head(mpg,10)
tail(mpg,20)

str(mpg)

dim(mpg)
nrow(mpg)
ncol(mpg)

## 색인(index, 첨자) 종류 
view(mpg)

mpg[1,4]
mpg[20,5]

mpg[1,]
mpg[c(1:10),] # 1:10 가능
mpg[,1]
mpg[,1:3]
mpg[,c("class")]
mpg[,c("cty","hwy")]

# negative index
mpg[-1,]
mpg[-c(1:10),]
mpg[,-1]
mpg[,-c(1:3)]
mpg[,-c("class")]
mpg[,-c("cty","hwy")]

# $(선택 : 벡터)
mpg[,c("manufacturer")] # 콘크리트 바른 벽돌 일부를 통째로 들어내는 격
mpg$manufacturer  # 콘크리트 바른 벽돌을 하나하나 분리

mpg$cty

class(mpg[,c("manufacturer")])
class(mpg$manufacturer)

tibble(mpg[,c("manufacturer")],mpg[,c("cty")],mpg[,c("class")])
tibble(mpg$manufacturer,mpg$cty,mpg$class)

## 속성(attributes) 부여
mpg_df <- tibble(mpg$manufacturer,mpg$cty,mpg$class)
mpg_df %>% head()

names(mpg_df) <- c("manufacturer","cty","class")
mpg_df %>% head()

names(mpg_df) <- NULL
mpg_df %>% head()

a <- c(1:12)
names(a) <- c("학번1", "학번2", "학번3", "학번4", "학번5", "학번6", "학번7", "학번8", "학번9", "학번10", "학번11", "학번12")
a
view(a)

b <- c(1:12)
names(b) <- c("학번1", "학번2", "학번3", "학번4", "학번5", "학번6", "학번7", "학번8", "학번9")


## 리스트 살펴보기
tibble(c(1:10), c("M","N","O"), c("가","나","다","라"))
list_data <- list(c(1:10), c("M","N","O"), c("가","나","다","라"))
list_data

list_data[[1]]
list_data[[1]][9]

list_data[[2]]
list_data[[2]][2]

list_data[[3]][10]


#craw

Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_281') # JAVA 경로 설정 / 버전 마다 다르게

install.packages("tidyverse") # 최초 1번만 설치
install.packages("stringr")
install.packages("dplyr")
install.packages("httr")
install.packages("jsonlite")
install.packages("rJava")
install.packages("rvest")
install.packages("RSelenium")
library(tidyverse) # R을 킬때마다 배번
library(stringr)
library(dplyr)
library(httr)
library(jsonlite)
library(rJava)
library(rvest)
library(RSelenium)

https://r-pyomega.tistory.com/3?category=873554 # R 크롤링 필요 도구들 
https://r-pyomega.tistory.com/6?category=873554 # R rjava설치하기
https://r-pyomega.tistory.com/7?category=873554 # 셀레니움 구동하기

##cmd
#cd C:\r_selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445

# RSelenium
remDr <- remoteDriver(remoteServerAddr="localhost", 
                      port=4445L, 
                      browserName="chrome")
remDr$open()

start_time <- Sys.time()

remDr$navigate("https://www.youtube.com/watch?v=FfMeHzVtnfs")

Sys.sleep(time = 1)

pause <- remDr$findElement("css","button.ytp-play-button") 
pause$clickElement()

Sys.sleep(time = 1)

webElem <- remDr$findElement("css", "body")
webElem$sendKeysToElement(list(key = "end"))

Sys.sleep(time = 1)

# 페이지 스크롤
frontpage <- remDr$getPageSource()[[1]]
body <- frontpage %>% read_html() 

댓글수 <- body %>% 
  html_nodes("yt-formatted-string.count-text") %>%
  html_nodes("span:nth-child(2)") %>%
  html_text()  

댓글수 <- gsub("\\,","",댓글수)
댓글수 <- 댓글수 %>% as.integer()
n <- 댓글수/20 
n <- n %>% ceiling()

webElem <- remDr$findElement("css", "body")
webElem$sendKeysToElement(list(key = "end"))

Sys.sleep(time = 1)

ord <- remDr$findElement("css","div#icon-label") 
ord$clickElement()

Sys.sleep(time = 1)

newst <- remDr$findElement("xpath",'//*[@id="menu"]/a[2]/tp-yt-paper-item/tp-yt-paper-item-body/div[1]') 
newst$clickElement()

for(i in 0:10){
  webElem$sendKeysToElement(list(key = "end"))
  
  Sys.sleep(time = 1)
  
  cat(i,'번 페이지 스크롤 중 입니다.\n')
}

Sys.sleep(time = 1)

## 현재페이지 정보 읽어오기
frontpage <- remDr$getPageSource()[[1]]
body <- frontpage %>% read_html() 

Sys.sleep(time = 1)


글쓴이_ytb1 <- body %>% 
  html_nodes("a#author-text") %>%
  html_nodes("span.style-scope") %>%
  html_text()  

글쓴이_ytb1 <- gsub("\n","",글쓴이_ytb1)
글쓴이_ytb1 <- gsub(" ","",글쓴이_ytb1)

댓글_ytb1 <- body %>% 
  html_nodes("yt-formatted-string#content-text") %>%
  html_text()  

날짜_ytb1 <-  body %>% 
  html_nodes("div#header-author") %>%
  html_nodes("yt-formatted-string.published-time-text") %>%
  html_nodes("a.yt-simple-endpoint") %>%
  html_text()

댓글_ytb1 %>% length()

df_ytb1 <- tibble(글쓴이_ytb1,댓글_ytb1,날짜_ytb1)

end_time <- Sys.time()

end_time - start_time

# 링크 불러오기
# 링크_ytb <- body %>% 
#   html_nodes("a#video-title") %>%
#   html_attr("href")


#textmining
install.packages("corpus")
install.packages("qdap")
install.packages("tm")
install.packages("wordcloud2")
install.packages("topicmodels")
install.packages("ldatuning")
install.packages("dplyr")
install.packages("tidytext")
install.packages("stringr")
install.packages("plyr")
install.packages("rJava")
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


