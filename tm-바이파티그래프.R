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
library(igraph)

# data
인공간호 <- read_csv(file = "D:/대학원/논문/인공지능 간호/인공지능간호.csv", col_names = TRUE, locale=locale('ko',encoding='euc-kr'))
인공간호 %>% str()
간호 <- 인공간호 %>% select("키워드","일자")
간호 <- 간호 %>% arrange(-desc(일자))
간호$일자

간호_dim <- 간호 %>% dim()

간호$id <- c(1:간호_dim[1])
간호$일자 <- str_sub(간호$일자,1,4)

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
    select(5,1,2,3)
  
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

# 그래프 그리기
단어_의미df <- 간호_token %>% 
  select(id,word) %>% 
  filter(id %in% c(1:3)) %>% 
  graph_from_data_frame() 

V(단어_의미df)$type <- bipartite_mapping(단어_의미df)$type 
## bipartite_mapping( )함수는 양자간(TRUE / FALSE)로 구성하는 그래프를 그려주는 함수입니다

V(단어_의미df)$color <- V(단어_의미df)$type
V(단어_의미df)$color <- gsub("FALSE","red",V(단어_의미df)$color)
V(단어_의미df)$color <- gsub("TRUE","blue",V(단어_의미df)$color)
plot(단어_의미df, edge.color="gray30",edge.width=E(단어_의미df)$weight, layout=layout_as_bipartite)