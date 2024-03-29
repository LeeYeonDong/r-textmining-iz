devtools::install_github("rstudio/keras")
devtools::install_github("rstudio/tensorflow")
install_keras()
library(tensorflow)
sess= tf$Session()
install_tensorflow()



library(keras)
library(tensorflow)
library(tokenizers)
library(text2vec)
library(Matrix)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(broom)
library(stringr)
library(tidyr)
library(readr)
library(tm)
library(RTextTools)
library(tidyverse)
library(NLP)
# library(qdap)
library(corpus)
library(wordcloud2)
library(stringr)
library(rJava)
library(base64enc)
library(RmecabKo)
library(reshape2)
library(tidytext)
library(umap)
library(gridExtra)


##########
# 2013??? ???????????? ????????? Word2Vec????????? ???????????? ????????????. ?????? ????????? ??????(Word)??? ??????(Vector)??? ???????????? ???????????????. ?????? ?????????(Embedding). GloVe, Fasttext ?????? ?????? ??????????????? ?????? ????????? ???????????? ??? ????????? ????????? ????????? ??????

# data ??????
??????_sports.news_NC <- read_csv(file = "D:/?????????/??????/?????????/??????_sports.news_NC.csv", col_names = TRUE, locale=locale('ko',encoding='euc-kr'))

names(??????_sports.news_NC) <- c("?????????_NC","??????_NC","??????_NC","??????_NC","??????_NC","??????_NC","??????_NC","??????_NC","??????_NC")

??????_NC <- ??????_sports.news_NC[!is.na(??????_sports.news_NC$??????_NC),]

??????_NC$ID_NC <- c(1:nrow(??????_NC))

??????_NC %>% head()

??????_NC$??????_NC <- gsub("??????","",??????_NC$??????_NC)
??????_NC$??????_NC <- gsub("??????","",??????_NC$??????_NC)
??????_NC$??????_NC <- gsub("??????","",??????_NC$??????_NC)
??????_NC$??????_NC <- gsub("??????","",??????_NC$??????_NC)
??????_NC$??????_NC <- gsub("?????????","",??????_NC$??????_NC)
??????_NC$??????_NC <- gsub("??????","",??????_NC$??????_NC)
??????_NC$??????_NC <- gsub("??????","",??????_NC$??????_NC)
??????_NC$??????_NC <- gsub("????????????","",??????_NC$??????_NC)
??????_NC$??????_NC <- gsub("\u7f8e","??????",??????_NC$??????_NC)
??????_NC$??????_NC <- gsub("\u65e5","??????",??????_NC$??????_NC)

## divide reviewtext into separate words
??????_NC %>% str()
ID??????_NC <- ??????_NC %>% dplyr::select("ID_NC","??????_NC") ## MASS??? ??????

# ????????? ??????
ID??????_NC_dim <- ID??????_NC %>% dim()

ID??????_NC_dim_int <- ID??????_NC_dim[1] / 10000
ID??????_NC_dim_int <- ID??????_NC_dim_int %>% ceiling()
n <- ID??????_NC_dim_int

ID??????_NC_sp <- split(ID??????_NC,rep(1:n, each = 10000))

# ????????? ??? ?????????
ID??????_NC_tb <- list()
ID??????_NC_data_set <- list()
ID??????_NC_token <- c()
ID??????_NC_?????? <- list()
ID??????_NC_?????? <- list()
ID??????_NC <- tibble()

## ???????????? ?????????
for (i in 1:n){
  
  cat(i, '?????? ????????? ????????? tokenizer', '??? ?????????.\n') 
  
  ID??????_NC_tb[[i]] <- ID??????_NC_sp[[i]] %>% 
    tibble() %>% 
    unnest_tokens(input = ??????_NC, output = word_NC, token = "words", drop = FALSE)
  
  names(ID??????_NC_tb[[i]]) <- c("ID_NC","??????_NC","word_NC")
  
  ID??????_NC_??????[[i]] <- ID??????_NC_tb[[i]] %>%  
    mutate(??????_NC = str_match(word_NC,'([???-???]+)')[,2]) %>% ## "??????" variable??? ????????? ????????? ??????         
    na.omit() %>% ## ([???-???]+)/P') ???????????? ???????????? ???????????????
    mutate(?????????_NC = str_length(??????_NC)) %>%   ## "?????????" variable??? ???????????? 
    filter(str_length(??????_NC) >= 2) 
  
  ID??????_NC_??????[[i]] <- ID??????_NC_tb[[i]] %>%  
    mutate(??????_NC = str_match(word_NC,'([a-zA-Z]+)')[,2]) %>% ## "??????" variable??? ????????? ????????? ??????         
    na.omit() %>% ## 
    mutate(?????????_NC = str_length(??????_NC)) %>%   ## "?????????" variable??? ???????????? 
    filter(str_length(??????_NC) >= 2) 
  
  ID??????_NC <- bind_rows(ID??????_NC,ID??????_NC_??????[[i]])
  ID??????_NC <- bind_rows(ID??????_NC,ID??????_NC_??????[[i]])
}

NC_tokens <- ID??????_NC %>% dplyr::select("ID_NC","??????_NC","word_NC","?????????_NC")

## count the number of words per article and plot results
NC_tokens %>% 
  group_by(ID_NC) %>% 
  summarise(n_tokens = n()) %>%
  mutate(n_tokens_binned = cut(n_tokens, breaks = c(1,seq(0,40,3),Inf))) %>% 
  group_by(n_tokens_binned) %>% 
  summarise(n_article = n()) %>% 
  ggplot(aes(x = n_tokens_binned, y = n_article)) + 
  geom_bar(stat = 'identity',fill = 'blue') + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 5, size = 10)) + 
  geom_text(size = 5, aes(label = n_article), position = position_dodge(width = 1), vjust = -0.5)

# ???????????? ?????? ??? ??? ??? 5 ??? ????????? ????????? ?????? ?????? ??? ??????. ?????? ???????????? 60,016?????? ?????? ??? ????????? ???????????????.
NC_tokens %>% 
  group_by(word_NC) %>% 
  summarize(word_freq = n()) %>% 
  mutate(min_5_freq = case_when(word_freq < 5 ~'token frequency : < 5', TRUE ~ 'token frequency : >= 5')) %>% 
  group_by(min_5_freq) %>% 
  summarise(n_tokens = n()) %>% 
  mutate(ratio_tokens = n_tokens / sum(n_tokens)) 

NC_tokens_min5 <- NC_tokens %>% 
  group_by(word_NC) %>% 
  mutate(token_freq = n()) %>%  
  filter(token_freq >= 5) %>% 
  group_by(ID_NC) %>% 
  summarise(NCTextClean = str_c(word_NC, collapse = " "))


# ???????????? ???????????? ???????????? ?????? ??? ????????? ????????? ????????? ?????? ??????. ???????????? ?????? ???????????? input??? ??? ??????. ????????? ????????? ?????? (??????) ??? ?????? ?????? (??????)????????? 2 ?????? ????????? ??????. ?????? ???????????? ?????? ????????? ????????? ??????????????????. ??????????????? ???????????? ??????????????? ???????????? ????????? ?????? (0 ??????)??? ???????????? ????????? ????????? ????????????.

#label ????????? ?????? or ??????
NC_tokens_min5$label <- rbinom(n = length(NC_tokens_min5$ID_NC), size = 1, prob = 0.1)

TextClean_NC <- NC_tokens_min5 %>% select(NCTextClean) %>% pull()
label_NC <- NC_tokens_min5 %>% select(label) %>% pull() %>% as.array()

## word2vec
install.packages("word2vec")
library(word2vec)

set.seed(1029)
TextClean_NC_model <- word2vec(x = TextClean_NC, type = "cbow", dim = 10, iter = 20)
TextClean_NC_embed <- as.matrix(TextClean_NC_model)
TextClean_NC_model_pred <- predict(TextClean_NC_model, c("??????"), type = "embedding")
TextClean_NC_model_lookslike <- predict(TextClean_NC_model, c("??????"), type = "nearest", top_n = 5)

# GLOVE - ???????????? ????????? ???????????? ???????????????
install.packages("text2vec")
library(text2vec)
# Create iterator over tokens
tokens <- space_tokenizer(TextClean_NC)
# Create vocabulary. Terms will be unigrams (simple words).
it <- itoken(tokens, progressbar = FALSE)
vocab <- create_vocabulary(it)

vocab <- prune_vocabulary(vocab, term_count_min = 5L)

# Use our filtered vocabulary
vectorizer <- vocab_vectorizer(vocab)
# use window of 5 for context words
tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L)

glove <- GlobalVectors$new(rank = 50, x_max = 10)
wv_main <- glove$fit_transform(tcm, n_iter = 10, convergence_tol = 0.01, n_threads = 8)

wv_context <- glove$components
word_vectors <-  wv_main + t(wv_context)


NC_word_vectors <- word_vectors["are", , drop = FALSE] - 
  word_vectors["at", , drop = FALSE] + 
  word_vectors["???????????????", , drop = FALSE]
cos_sim = sim2(x = word_vectors, y = NC_word_vectors, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 5)