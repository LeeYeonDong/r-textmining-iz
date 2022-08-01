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

# ?????? ?????? ???????????? - ?????? ?????? ????????? ????????? ????????????
간호_co <- 간호_token %>% 
  select(id, word, 키워드) %>%
  add_count(word) %>% 
  filter(n >= 100) %>% 
  pairwise_count(item = word,
                 feature = id,
                 sort = TRUE) 

간호_co <- 간호_co[1:200,]

간호_co_graph <- 간호_co %>% 
  as_tbl_graph()

set.seed(1029)

ggraph(간호_co_graph, layout = "fr") +
  geom_edge_link(color = "black", alpha = 1) +
  geom_node_point(color = "#003300", alpha = 1, size = 5) +
  geom_node_text(aes(label = name), repel = TRUE , size = 10) +
  theme_graph()


# ?????? ??? ???????????? - ?????? ????????? ?????? ??????????????? ?????? ?????? ????????? ?????? ??????, ???????????? ??????
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


# n-gram : ????????? ????????? n?????? ??????
set.seed(1201)

arr <- grid::arrow(type = "closed", length = unit(.20, "inches"))

간호_token %>% 
  group_by(id) %>% 
  summarise(sentence = paste(word, collapse = " ")) %>% 
  unnest_tokens(input = sentence,
                output = trigram,
                token = "ngrams",
                n = 3) %>% 
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%    
  count(word1, word2, word3, sort = TRUE) %>% 
  na.omit() %>% 
  filter(n >= 13) %>% 
  graph_from_data_frame() %>% 
  ggraph(layout = "fr") +
  geom_edge_link(color = "black", alpha = 1, arrow = arr) +
  geom_node_point(color = "#003300", alpha = 1, size = 5) +
  geom_node_text(aes(label = name), repel = TRUE , size = 5) +
  theme_graph()



V(단어_의미df)$type <- bipartite_mapping(단어_의미df)$type 
## bipartite_mapping( )함수는 양자간(TRUE / FALSE)로 구성하는 그래프를 그려주는 함수입니다

## V( ) 함수는 vertex sequence를 생성하는 함수입니다. vertex의 사전적 의미는 "꼭짓점"인데, 그래프에서 확인할 수 있습니다

단어_의미m  <- as_incidence_matrix(단어_의미df) %*% t(as_incidence_matrix(단어_의미df))   ## matrix로 변환 합니다

## 주대각선을 "0"으로 처리
diag(단어_의미m) <- 0  

## 인접 행렬(adjacency matrix)로 변환
단어_의미m <- 단어_의미m %>% graph_from_adjacency_matrix() 


#### 시각화
단어_의미m %>% plot() 

단어_의미m %>%   
  as_tbl_graph() %>% 
  ggraph() + 
  geom_edge_link(aes(start_cap = label_rect(node1.name), end_cap = label_rect(node2.name))) + 
  geom_node_text(aes(label=name))


#### ???????????? ?????????
#### ????????? ?????????
#### ???????????? ???????????? ?????? ????????? ?????????
??????_token_??????df <- split(??????_token,??????_token$id) 

?????? <- c()

for(i in 1:length(??????_token_??????df)){
  cat(i, '?????? ????????? ?????? ????????? ?????? ??? ?????????.\n') 
  del <- ??????_token_??????df[[i]]$word %>% unique()
  del.tmp <- grep(del[i],??????_token_??????df[[i]]$word)
  ?????? <- append(??????,del.tmp)
  
  ??????_token_??????df[[i]] <- ??????_token_??????df[[i]][-??????,]
  ?????? <- c()
}


## ???????????? "???(column)?????? ????????????
??????_token_??????list <- lapply(??????_token_??????df, function(x){ 
  return(x$word)})                                                                   ## ????????? ???????????? ???????????? ????????????

??????_token_??????list %>% head()

# ??? ????????? ???????????? ????????? ??????
rd_n <- 1:length(??????_token_??????list) 
rd_n <- rd_n %>% sample(n, replace = FALSE)   ## n = ???????????? ?????? ?????? ??????
raw_list_n <- ??????_token_??????list[rd_n]

## transactions ??????
names(raw_list_n) <- paste("Tr", 1:length(raw_list_n), sep="") 
raw_list_n_tran <- as(raw_list_n, "transactions")              ##  ?????????????????? ????????? error??? ???????????????

raw_list_n_tran %>% crossTable() %>% view()


## apriori ????????? ??????
raw_list_n_tran_apr <- apriori(raw_list_n_tran, parameter = list(supp=0.2, conf=0.2))



## ????????? ?????? ?????? : ???????????? ?????? -> ???????????? ??????(matrix ?????? data.frame)
rd_n_list_rul <- (raw_list_n_tran_apr %>% sort(by = "lift"))[1:100] %>% labels(ruleSep=" ")

## rules????????? ????????? ????????? ????????? ?????? 
rd_n_list_rul <- gsub("[[:punct:]]","",rd_n_list_rul)
rd_n_list_rul <- sapply(rd_n_list_rul, strsplit, " ",USE.NAMES=F) 

## ??? ????????? ????????? matrix??? ?????? 
rd_n_list_mat <- do.call("rbind", rd_n_list_rul) 

## ???????????? ????????? ?????? ????????? ??????(graph.edgelist ??????)
rd_n_list_mat_rulg <- rd_n_list_mat %>% graph.edgelist(directed=FALSE) 

## ????????? ?????????
plot.igraph(rd_n_list_mat_rulg, 
            vertex.label=V(rd_n_list_mat_rulg)$name, 
            vertex.label.cex = 1,
            vertex.label.color='#000000',  
            vertex.size = 15,
            vertex.color='#E85100',
            vertex.frame.color='#E85100')
