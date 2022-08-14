# KoNLP useNIADic
install.packages("multilinguer")
library(multilinguer)
# install_jdk()
install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), type = "binary")
#
install.packages("remotes")
devtools::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))
library(KoNLP) #ìµœì¢…ì ìœ¼ë¡œ "KoNLP" íŒ¨í‚¤ì§€ë¥¼ ë¶ˆëŸ¬ì˜µë‹ˆë‹¤

devtools::install_github('haven-jeon/NIADic/NIADic', build_vignettes = TRUE)
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jdk-18.0.1.1')  # ì„¤ì¹˜í•œ JAVA versionì— ë”°ë¼ ë‹¬ë¼ì§‘ë‹ˆë‹¤
buildDictionary(ext_dic = "woorimalsam")  # "woorimalsam" dicì„ ë¶ˆëŸ¬ì˜µë‹ˆë‹¤
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
ì¸ê³µê°„í˜¸ <- read_csv(file = "D:/ëŒ€í•™ì›/ë…¼ë¬¸/ì¸ê³µì§€ëŠ¥ ê°„í˜¸/ì¸ê³µì§€ëŠ¥ê°„í˜¸.csv", col_names = TRUE, locale=locale('ko',encoding='euc-kr'))
ì¸ê³µê°„í˜¸ %>% str()
ê°„í˜¸ <- ì¸ê³µê°„í˜¸ %>% select("í‚¤ì›Œë“œ","ì¼ìž")
ê°„í˜¸ <- ê°„í˜¸ %>% arrange(-desc(ì¼ìž))
ê°„í˜¸$ì¼ìž

ê°„í˜¸_dim <- ê°„í˜¸ %>% dim()

ê°„í˜¸$id <- c(1:ê°„í˜¸_dim[1])
ê°„í˜¸$ì¼ìž <- str_sub(ê°„í˜¸$ì¼ìž,1,4)

# ë…„ë„ë³„ ì¶”ì´
ê°„í˜¸_ë…„ë„_table <- ê°„í˜¸$ì¼ìž %>% table() %>% as_tibble()
names(ê°„í˜¸_ë…„ë„_table) <- c("ë‚ ì§œ_ë…„ë³„","Freq")

ë‚ ì§œ_ë…„ë³„_ì¶”ê°€ <- c("1995","1997","1998","1999","2000","2003","2004","2007","2010","2011","2012","2013")
zero <- rep(0,length(ë‚ ì§œ_ë…„ë³„_ì¶”ê°€))

ê°„í˜¸_ë…„ë„_table_ì¶”ê°€ <- tibble(ë‚ ì§œ_ë…„ë³„_ì¶”ê°€,zero)
names(ê°„í˜¸_ë…„ë„_table_ì¶”ê°€) <- c("ë‚ ì§œ_ë…„ë³„","Freq")

ê°„í˜¸_ë…„ë„_table <- bind_rows(ê°„í˜¸_ë…„ë„_table,ê°„í˜¸_ë…„ë„_table_ì¶”ê°€)
ê°„í˜¸_ë…„ë„_table <- ê°„í˜¸_ë…„ë„_table %>% arrange(-desc(ë‚ ì§œ_ë…„ë³„))

ggplot(data = ê°„í˜¸_ë…„ë„_table, aes(x = ë‚ ì§œ_ë…„ë³„, y = Freq, group = 1)) + 
  geom_line(size = 2, colour="#006600") + 
  geom_point(size = 1, colour="#006600") +
  geom_text(aes(label = Freq),hjust = 1,size=5) +
  geom_hline(yintercept = mean(ê°„í˜¸_ë…„ë„_table$Freq), color='red',linetype='dashed', size = 1) +
  labs(x="", y="") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=45, hjust=1, size = 20))


# ë°ì´í„° ë¶„í• 
ê°„í˜¸_dim_int <- ê°„í˜¸_dim[1] / 10
ê°„í˜¸_dim_int <- ê°„í˜¸_dim_int %>% ceiling()
n <- ê°„í˜¸_dim_int

ê°„í˜¸_sp <- split(ê°„í˜¸,rep(1:n,each=10))


# ë°ì´í„° ì…‹ ë§Œë“¤ê¸°
ê°„í˜¸_tb <- list()
ê°„í˜¸_data_set <- list()
ê°„í˜¸_token <- tibble()
ê°„í˜¸_í•œê¸€ <- list()
ê°„í˜¸_ì˜ì–´ <- list()

## ë‹¨ì–´ê¸°ì¤€ í† í°í™”
for (i in 1:n){
  
  cat(i, 'ë²ˆì§¸ ë°ì´í„° ë¦¬ìŠ¤íŠ¸ tokenizer', 'ì¤‘ ìž…ë‹ˆë‹¤.\n') 
  
  ê°„í˜¸_tb[[i]] <- ê°„í˜¸_sp[[i]] %>% tibble()
  
  ê°„í˜¸_tb[[i]] <- ê°„í˜¸_tb[[i]] %>% 
    unnest_tokens(input = í‚¤ì›Œë“œ, output = word, token = "words", drop = FALSE)
  
  ê°„í˜¸_data_set[[i]] <- ê°„í˜¸_tb[[i]]  %>% 
    melt() %>% # ì‹ë³„ìžid, ì¸¡ì • ë³€ìˆ˜variable, ì¸¡ì •ì¹˜value í˜•íƒœë¡œ ë°ì´í„°ë¥¼ ìž¬êµ¬ì„±í•˜ëŠ” í•¨ìˆ˜
    as_tibble() %>% 
    select(5,1,2,3)
  
  names(ê°„í˜¸_data_set[[i]]) <- c("id","í‚¤ì›Œë“œ","ì¼ìž","word")
  
  ê°„í˜¸_í•œê¸€[[i]] <- ê°„í˜¸_data_set[[i]] %>%  
    mutate(í•œê¸€ = str_match(word,'([ê°€-íž£]+)')[,2]) %>% ## "í•œê¸€" variableì„ ë§Œë“¤ê³  í•œê¸€ë§Œ ì €ìž¥         
    na.omit() %>% ## ([ê°€-íž£]+)/P') í•œê¸€ë§Œì„ ì„ íƒí•˜ëŠ” ì •ê·œí‘œí˜„ì‹
    mutate(ê¸€ìžìˆ˜=str_length(í•œê¸€)) %>%   ## "ê¸€ìžìˆ˜" variableì„ ë§Œë“­ë‹ˆë‹¤ 
    filter(str_length(í•œê¸€)>=2) 
  
  ê°„í˜¸_ì˜ì–´[[i]] <- ê°„í˜¸_data_set[[i]] %>%  
    mutate(ì˜ì–´ = str_match(word,'([a-zA-Z]+)')[,2]) %>% ## "í•œê¸€" variableì„ ë§Œë“¤ê³  í•œê¸€ë§Œ ì €ìž¥         
    na.omit() %>% ## 
    mutate(ê¸€ìžìˆ˜=str_length(ì˜ì–´)) %>%   ## "ê¸€ìžìˆ˜" variableì„ ë§Œë“­ë‹ˆë‹¤ 
    filter(str_length(ì˜ì–´)>=3) 
  
  ê°„í˜¸_tokení•œê¸€.tmp <- tibble(ê°„í˜¸_í•œê¸€[[i]]$id,ê°„í˜¸_í•œê¸€[[i]]$word,ê°„í˜¸_í•œê¸€[[i]]$ì¼ìž,ê°„í˜¸_í•œê¸€[[i]]$í‚¤ì›Œë“œ)
  names(ê°„í˜¸_tokení•œê¸€.tmp) <- c("id","word", "ì¼ìž", "í‚¤ì›Œë“œ")
  ê°„í˜¸_tokenì˜ì–´.tmp <- tibble(ê°„í˜¸_ì˜ì–´[[i]]$id,ê°„í˜¸_ì˜ì–´[[i]]$word,ê°„í˜¸_ì˜ì–´[[i]]$ì¼ìž,ê°„í˜¸_ì˜ì–´[[i]]$í‚¤ì›Œë“œ)
  names(ê°„í˜¸_tokenì˜ì–´.tmp) <- c("id","word", "ì¼ìž", "í‚¤ì›Œë“œ")
  
  ê°„í˜¸_token <- bind_rows(ê°„í˜¸_token,ê°„í˜¸_tokení•œê¸€.tmp)
  ê°„í˜¸_token <- bind_rows(ê°„í˜¸_token,ê°„í˜¸_tokenì˜ì–´.tmp)
}
ê°„í˜¸_token$ì¼ìž <- ê°„í˜¸_token$ì¼ìž %>% as.integer()


# ë°ì´í„° ì „ì²˜ë¦¬
ì œê±° <- c()

chr <- c("article", "www.joongdo.co.kr", "view.jsp", "nurl", "jsp", "http")

for(i in 1:length(chr)){
  
  cat(i, 'ë²ˆì§¸ ì „ì²˜ë¦¬ ì œê±° ë‹¨ì–´ë¥¼ ì°¾ëŠ” ì¤‘ ìž…ë‹ˆë‹¤.\n') 
  
  del.tmp <- grep(chr[i],ê°„í˜¸_token$word)
  ì œê±° <- append(ì œê±°,del.tmp)
}

ì œê±° <- ì œê±° %>% unique()

ê°„í˜¸_token <- ê°„í˜¸_token[-ì œê±°,]

# ?????? ?????? ???????????? - ?????? ?????? ????????? ????????? ????????????
ê°„í˜¸_co <- ê°„í˜¸_token %>% 
  select(id, word, í‚¤ì›Œë“œ) %>%
  add_count(word) %>% 
  filter(n >= 100) %>% 
  pairwise_count(item = word,
                 feature = id,
                 sort = TRUE) 

ê°„í˜¸_co <- ê°„í˜¸_co[1:200,]

ê°„í˜¸_co_graph <- ê°„í˜¸_co %>% 
  as_tbl_graph()

set.seed(1029)

ggraph(ê°„í˜¸_co_graph, layout = "fr") +
  geom_edge_link(color = "black", alpha = 1) +
  geom_node_point(color = "#003300", alpha = 1, size = 5) +
  geom_node_text(aes(label = name), repel = TRUE , size = 10) +
  theme_graph()


# ?????? ??? ???????????? - ?????? ????????? ?????? ??????????????? ?????? ?????? ????????? ?????? ??????, ???????????? ??????
ê°„í˜¸_cor <- ê°„í˜¸_token %>% 
  select(id, word, í‚¤ì›Œë“œ) %>%
  add_count(word) %>% 
  filter(n >= 30) %>% 
  pairwise_cor(item = word,
               feature = id,
               sort = TRUE)

ê°„í˜¸_cor_graph <- ê°„í˜¸_cor %>% 
  filter(correlation >= 0.9) %>%
  as_tbl_graph(directed = FALSE) %>% 
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))

set.seed(1029)

ggraph(ê°„í˜¸_cor_graph, layout = "fr") +
  
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

ê°„í˜¸_token %>% 
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



V(ë‹¨ì–´_ì˜ë¯¸df)$type <- bipartite_mapping(ë‹¨ì–´_ì˜ë¯¸df)$type 
## bipartite_mapping( )í•¨ìˆ˜ëŠ” ì–‘ìžê°„(TRUE / FALSE)ë¡œ êµ¬ì„±í•˜ëŠ” ê·¸ëž˜í”„ë¥¼ ê·¸ë ¤ì£¼ëŠ” í•¨ìˆ˜ìž…ë‹ˆë‹¤

## V( ) í•¨ìˆ˜ëŠ” vertex sequenceë¥¼ ìƒì„±í•˜ëŠ” í•¨ìˆ˜ìž…ë‹ˆë‹¤. vertexì˜ ì‚¬ì „ì  ì˜ë¯¸ëŠ” "ê¼­ì§“ì "ì¸ë°, ê·¸ëž˜í”„ì—ì„œ í™•ì¸í•  ìˆ˜ ìžˆìŠµë‹ˆë‹¤

ë‹¨ì–´_ì˜ë¯¸m  <- as_incidence_matrix(ë‹¨ì–´_ì˜ë¯¸df) %*% t(as_incidence_matrix(ë‹¨ì–´_ì˜ë¯¸df))   ## matrixë¡œ ë³€í™˜ í•©ë‹ˆë‹¤

## ì£¼ëŒ€ê°ì„ ì„ "0"ìœ¼ë¡œ ì²˜ë¦¬
diag(ë‹¨ì–´_ì˜ë¯¸m) <- 0  

## ì¸ì ‘ í–‰ë ¬(adjacency matrix)ë¡œ ë³€í™˜
ë‹¨ì–´_ì˜ë¯¸m <- ë‹¨ì–´_ì˜ë¯¸m %>% graph_from_adjacency_matrix() 


#### ì‹œê°í™”
ë‹¨ì–´_ì˜ë¯¸m %>% plot() 

ë‹¨ì–´_ì˜ë¯¸m %>%   
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
