R_LIBS_SITE="C:\\Program Files\\R\\R-4.2.1\\library"
.libPaths("C:/Program Files/R/R-4.2.1/library")
.libPaths("C:/myRproject")
getwd()

.libPaths()

install.packages("reticulate")
install.packages("devtools")
install.packages("tidyverse")

reticulate::conda_version()
sessionInfo()

library(tensorflow)
use_condaenv("tf")
sess <- tf$Session()

devtools::install_github("rstudio/keras")
devtools::install_github("rstudio/tensorflow")
library(keras)
install_keras()
library(tensorflow)
install_tensorflow()
sess= tf$Session()

library(keras)
library(tensorflow)
# install.packages("tokenizers")
library(tokenizers)
# install.packages("text2vec")
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
# 2013년 구글에서 개발한 Word2Vec이라는 방법론이 있습니다. 이름 그대로 단어(Word)를 벡터(Vector)로 바꿔주는 방법입니다. 이를 임베딩(Embedding). GloVe, Fasttext 같은 다른 방법론과의 비교 단어를 벡터화할 때 단어의 문맥적 의미를 보존

# data 준비
뉴스_sports.news_NC <- read_csv(file = "D:/대학원/논문/소논문/뉴스_sports.news_NC.csv", col_names = TRUE, locale=locale('ko',encoding='euc-kr'))

names(뉴스_sports.news_NC) <- c("언론사_NC","제목_NC","날짜_NC","링크_NC","좋아_NC","훈훈_NC","슬퍼_NC","화나_NC","후속_NC")

뉴스_NC <- 뉴스_sports.news_NC[!is.na(뉴스_sports.news_NC$날짜_NC),]

뉴스_NC$ID_NC <- c(1:nrow(뉴스_NC))

뉴스_NC %>% head()

뉴스_NC$제목_NC <- gsub("포토","",뉴스_NC$제목_NC)
뉴스_NC$제목_NC <- gsub("오늘","",뉴스_NC$제목_NC)
뉴스_NC$제목_NC <- gsub("경기","",뉴스_NC$제목_NC)
뉴스_NC$제목_NC <- gsub("사진","",뉴스_NC$제목_NC)
뉴스_NC$제목_NC <- gsub("스포츠","",뉴스_NC$제목_NC)
뉴스_NC$제목_NC <- gsub("종합","",뉴스_NC$제목_NC)
뉴스_NC$제목_NC <- gsub("다시","",뉴스_NC$제목_NC)
뉴스_NC$제목_NC <- gsub("치어리더","",뉴스_NC$제목_NC)
뉴스_NC$제목_NC <- gsub("\u7f8e","미국",뉴스_NC$제목_NC)
뉴스_NC$제목_NC <- gsub("\u65e5","일본",뉴스_NC$제목_NC)

## divide reviewtext into separate words
뉴스_NC %>% str()
ID제목_NC <- 뉴스_NC %>% dplyr::select("ID_NC","제목_NC") ## MASS와 충돌

# 데이터 분할
ID제목_NC_dim <- ID제목_NC %>% dim()

ID제목_NC_dim_int <- ID제목_NC_dim[1] / 10000
ID제목_NC_dim_int <- ID제목_NC_dim_int %>% ceiling()
n <- ID제목_NC_dim_int

ID제목_NC_sp <- split(ID제목_NC,rep(1:n, each = 10000))

# 데이터 셋 만들기
ID제목_NC_tb <- list()
ID제목_NC_data_set <- list()
ID제목_NC_token <- c()
ID제목_NC_한글 <- list()
ID제목_NC_영어 <- list()
ID제목_NC <- tibble()

## 단어기준 토큰화
for (i in 1:n){
  
  cat(i, '번째 데이터 리스트 tokenizer', '중 입니다.\n') 
  
  ID제목_NC_tb[[i]] <- ID제목_NC_sp[[i]] %>% 
    tibble() %>% 
    unnest_tokens(input = 제목_NC, output = word_NC, token = "words", drop = FALSE)

  names(ID제목_NC_tb[[i]]) <- c("ID_NC","제목_NC","word_NC")
  
  ID제목_NC_한글[[i]] <- ID제목_NC_tb[[i]] %>%  
    mutate(한글_NC = str_match(word_NC,'([가-힣]+)')[,2]) %>% ## "한글" variable을 만들고 한글만 저장         
    na.omit() %>% ## ([가-힣]+)/P') 한글만을 선택하는 정규표현식
    mutate(글자수_NC = str_length(한글_NC)) %>%   ## "글자수" variable을 만듭니다 
    filter(str_length(한글_NC) >= 2) 
  
  ID제목_NC_영어[[i]] <- ID제목_NC_tb[[i]] %>%  
    mutate(영어_NC = str_match(word_NC,'([a-zA-Z]+)')[,2]) %>% ## "한글" variable을 만들고 한글만 저장         
    na.omit() %>% ## 
    mutate(글자수_NC = str_length(영어_NC)) %>%   ## "글자수" variable을 만듭니다 
    filter(str_length(영어_NC) >= 2) 
  
  ID제목_NC <- bind_rows(ID제목_NC,ID제목_NC_한글[[i]])
  ID제목_NC <- bind_rows(ID제목_NC,ID제목_NC_영어[[i]])
}

NC_tokens <- ID제목_NC %>% dplyr::select("ID_NC","제목_NC","word_NC","글자수_NC")

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

# 신경망을 구축 할 때 총 5 개 이상의 빈도를 가진 단어 만 사용. 현재 컬렉션은 60,016개의 고유 한 단어로 내려갑니다.
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


# 신경망이 데이터를 입력으로 취할 수 있도록 토큰을 정수로 변환 필요. 신경망에 직접 텍스트를 input할 수 없다. 신경망 용어로 기사 (샘플) 및 단어 벡터 (특징)가있는 2 차원 행렬이 필요. 이를 위해서는 입력 기능의 길이가 같아야합니다. 아래에서는 텍스트를 벡터화하고 인덱스를 만들고 패딩 (0 추가)을 사용하여 동일한 크기를 만듭니다.

#label 붙이기 긍정 or 부정
NC_tokens_min5$label <- rbinom(n = length(NC_tokens_min5$ID_NC), size = 1, prob = 0.1)

TextClean_NC <- NC_tokens_min5 %>% select(NCTextClean) %>% pull()
label_NC <- NC_tokens_min5 %>% select(label) %>% pull() %>% as.array()


# maximum number of words for a review
max_length_NC <- 150

# 토큰을 벡터화 하고 각각에 고유값(integer)을 부여,
# tokenizer_NC <- text_tokenizer() %>% # Vectorize a text corpus, by turning each text into either a sequence of integers 
#   fit_text_tokenizer(TextClean_NC)

it_tokenizer_NC <- itoken(NC_tokens_min5$NCTextClean, 
                tokenizer = word_tokenizer,
                ids = NC_tokens_min5$ID_NC,
                progressbar = TRUE)

# create a vocabulary out of the tokenset (stopword removal and bi-grams are optional)
vocab_tokenizer_NC <- it_tokenizer_NC %>% create_vocabulary()

# 토큰에 고유값(integer)을 부여하고 이를 시퀀스(list)로 만듬
# sequences_NC <- texts_to_sequences(vocab_tokenizer_NC, TextClean_NC)

vectorizer_NC <- vocab_tokenizer_NC %>% vocab_vectorizer()
tcm_NC <- create_tcm(it_tokenizer_NC, vectorizer_NC, skip_grams_window = 5L)


# 시퀀스(list)를 같은 길이로 만든다 max_length에 맞춘다 (Keras requirement)
input_NC <- pad_sequences(sequences_NC, maxlen = max_length_NC)

cat(paste0('Original text of article number 1029 without interpunction, low frequency words and stopwords:', '\n' ,TextClean_NC[1],'\n\n'))
cat(paste0('What this article looks like converted to integers:'),'\n', (input_NC[1,]),'\n\n')


# word2vec embedding 차원 설정
word2vecdim_NC <- 150

# words index 길이
num_tokens_NC <- unique(tokenizer_NC$word_index) %>% length()

model_NC <- keras_model_sequential() %>% 
  # Specify the maximum input length (150) and input_dim (unique tokens+1) and choose 32 dimensions
  layer_embedding(input_dim = num_tokens_NC + 1, 
                  output_dim = word2vecdim_NC, 
                  input_length = max_length_NC,
                  mask_zero = TRUE) %>% 
  # 3D 행렬 of embeddings into a 2D 행렬 >
  # Shape `(samples, max_length * word2vecdim)`
  layer_flatten() %>% 
  # add a dense layer with 32 units
  layer_dense(units = 32, activation = "relu") %>% # rectifier 함수, 은닉층에 주로 쓰입니다.
  # add the classifier on top
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 1, activation = "sigmoid") # ‘sigmoid’ : 시그모이드 함수, 이진 분류 문제에서 출력층에 주로 쓰입니다 / softmax’ : 소프트맥스 함수, 다중 클래스 분류 문제에서 출력층에 주로 쓰입니다.

model_NC %>% summary()

model_NC %>% compile(
  optimizer = "rmsprop",
  # binary classification(이진 분류) 
  loss = "binary_crossentropy",
  # 정확도 accuracy를 측도로 사용
  metrics = c("acc")
)

history_NC <- model_NC %>% keras::fit(
  input_NC, label_NC,
  # maximum number of iterations
  epochs = 20,
  # how many reviews do we offer in each batch
  batch_size = 50,
  # check train results againts test data
  validation_split = 0.2
)

history_NC %>% plot()

# get embedding matrix, the weights from the model
# give the index back the name of the word for looking up a word embedding (NA for blanks)
word2vec_embedding_len_NC <- c(as.vector(unlist(tokenizer_NC$index_word))) %>% length()
word2vec_embedding_NC <- get_weights(model_NC)[[1]]
word2vec_embedding_NC <- word2vec_embedding_NC[1:word2vec_embedding_len_NC,]

rownames(word2vec_embedding_NC) <- c(as.vector(unlist(tokenizer_NC$index_word)))

# let's look up word 1 ("nc") again, the index shifted with 1 as NAs are now on top of the list 
print(rownames(word2vec_embedding_NC)[1])
word2vec_embedding_NC[1,]

# find words that are related to another word 
token <- "승리"
embedding_vector_승리 <- t(matrix(word2vec_embedding_NC[token,])) 
cos_sim_승리 <- sim2(x = word2vec_embedding_NC, y = embedding_vector_승리 , method = "cosine", norm = "l2") # similarities
cat(paste0('Words from the embedding layer similar to "승리":', '\n'))
print(head(sort(cos_sim_승리[,1], decreasing = TRUE), 10))

# We need to tokenize our already tokenized set as input for text2vec, re-use cleaned text in NC_tokens_min5
it_승리 <- itoken(NC_tokens_min5$NCTextClean, 
             tokenizer = word_tokenizer,
             ids = NC_tokens_min5$ID_NC,
             progressbar = TRUE)

# create a vocabulary out of the tokenset (stopword removal and bi-grams are optional)
vocab_승리 <- it_승리 %>% create_vocabulary() # use uni-grams

# text2vec has the option to prune the vocabulary of low-frequent words
vocab_승리 <- vocab_승리 %>% prune_vocabulary(term_count_min = 5)

# What's in the vocabulary?
print(vocab_승리)

# Vectorize word to integers
vectorizer_승리 <- vocab_vectorizer(vocab_승리)

# Create a Term-Count-Matrix, by default it will use a skipgram window of 5 (symmetrical)
tcm_승리 <- create_tcm(it_승리, vectorizer_승리, skip_grams_window = 5L)

# maximum number of co-occurrences to use in the weighting function, we choose the entire token set divided by 100
x_max_승리 <- length(vocab_승리$doc_count)/100

# set up the embedding matrix and fit model
glove_model_승리 <- GloVe$new(rank = 32, x_max = x_max_승리) 
glove_embedding_승리 <- glove_model_승리$fit_transform(tcm_승리, n_iter = 20, convergence_tol = 0.01, n_threads = 4)

# combine main embedding and context embeddings (sum) into one matrix
glove_embedding_승리 <- glove_embedding_승리 + t(glove_model_승리$components) # the transpose of the context matrix

# 우리는 이전에했던 것과 같은 근처의 단어를 뽑습니다.
word_승리 <- glove_embedding_승리["승리", , drop = FALSE] # wat ligt er dicht bij 'lekker'
cos_sim_승리 <- sim2(x = glove_embedding_승리, y = word_승리, method = "cosine", norm = "l2")
sort(cos_sim_승리[,1], decreasing = TRUE) %>% head(10)

# Word2Vec dimension reduction

word2vec_umap_승리 <- umap(word2vec_embedding_NC, n_components = 2, metric = "cosine", n_neighbors = 25, min_dist = 0.1, spread = 2) # X 및 Y 플롯을 사용하여 결과를 쉽게 시각화 할 수 있도록 임베딩 차원 (32) 수를 2 개로 줄였습니다
word2vec_umap_승리 %>% str()

# GloVe dimension reduction
glove_umap_승리 <- umap(glove_embedding_승리, n_components = 2, metric = "cosine", n_neighbors = 25, min_dist = 0.1, spread=2)
glove_umap_승리$layout %>% dim()

# Put results in a dataframe for ggplot, starting with Word2Vec
df_word2vec_umap_승리 <- as.data.frame(word2vec_umap_승리$layout, stringsAsFactors = FALSE)

# Add the labels of the words to the dataframe
df_word2vec_umap_승리$word <- rownames(word2vec_embedding_NC)
colnames(df_word2vec_umap_승리) <- c("UMAP1", "UMAP2", "word")
df_word2vec_umap_승리$technique <- 'Word2Vec'
cat(paste0('Our Word2Vec embedding reduced to 2 dimensions:', '\n'))
df_word2vec_umap_승리 %>% str()

# Do the same for the GloVe embeddings
df_glove_umap_승리 <- as.data.frame(glove_umap_승리$layout, stringsAsFactors = FALSE)

# Add the labels of the words to the dataframe
df_glove_umap_승리$word <- rownames(glove_embedding_승리)
colnames(df_glove_umap_승리) <- c("UMAP1", "UMAP2", "word")
df_glove_umap_승리$technique <- 'GloVe'
cat(paste0('\n', 'Our GloVe embedding reduced to 2 dimensions:', '\n'))
df_glove_umap_승리 %>% str()

# Combine the datasets
df_umap_승리 <- bind_rows(df_word2vec_umap_승리, df_glove_umap_승리)

ggplot(df_umap_승리) +
  geom_point(aes(x = UMAP1, y = UMAP2), colour = 'blue', size = 0.05) +
  facet_wrap(~technique) +
  labs(title = "Word embedding in 2D using UMAP") +
  theme(plot.title = element_text(hjust = .5, size = 14))

# Plot the bottom part of the GloVe word embedding with labels
ggplot(df_glove_umap_승리[df_glove_umap_승리$UMAP1 > 3.0 & df_glove_umap_승리$UMAP1 < 3.8 & df_glove_umap_승리$UMAP2 > 4.6,]) +
  geom_point(aes(x = UMAP1, y = UMAP2), colour = 'blue', size = 2) +
  geom_text(aes(UMAP1, UMAP2, label = word), size = 2.5, vjust=-1, hjust=0) +
  labs(title = "GloVe word embedding in 2D using UMAP - partial view") +
  theme(plot.title = element_text(hjust = .5, size = 14))

# Plot the word embedding of words that are related for the GloVe model
word_승리 <- glove_embedding_승리["승리", , drop = FALSE] 
cos_sim_승리 <- sim2(x = glove_embedding_승리, y = word_승리, method = "cosine", norm = "l2")
select_승리 <- data.frame(rownames(as.data.frame(head(sort(cos_sim_승리[,1], decreasing = TRUE), 25))))
colnames(select_승리) <- "word"
selected_words_GloVe_승리 <- df_glove_umap_승리 %>% inner_join(y=select_승리, by= "word", match = "all") 

#The ggplot visual for GloVe
ggplot(selected_words_GloVe_승리, aes(x = UMAP1, y = UMAP2, colour = word == '승리')) + 
  geom_point(show.legend = FALSE) + 
  scale_color_manual(values = c('black', 'red')) +
  geom_text(aes(UMAP1, UMAP2, label = word), show.legend = FALSE, size = 2.5, vjust=-1.5, hjust=0) +
  theme(plot.title = element_text(hjust = .5, size = 14))

# And so the same for the Word2Vec model
token <- "승리"
embedding_vector_승리 <- t(matrix(word2vec_embedding_NC[token,])) 
cos_sim_승리 <- sim2(x = word2vec_embedding_NC, y = embedding_vector_승리, method = "cosine", norm = "l2")
select_승리 <- data.frame(rownames(as.data.frame(head(sort(cos_sim_승리[,1], decreasing = TRUE), 25))))
colnames(select_승리) <- "word"
selected_words_Word2Vec_승리 <- df_word2vec_umap_승리 %>% inner_join(y=select_승리, by= "word", match = "all") 

# The ggplot visual for GloVe
ggplot(selected_words_Word2Vec_승리 , aes(x = UMAP1, y = UMAP2, colour = word == '승리')) + 
  geom_point(show.legend = FALSE) + 
  scale_color_manual(values = c('black', 'red')) +
  geom_text(aes(UMAP1, UMAP2, label = word), show.legend = FALSE, size = 2.5, vjust=-1.5, hjust=0) +
  theme(plot.title = element_text(hjust = .5, size = 14))

# Put the results side-by-side for a comparison
grid.arrange(pesto_glove,pesto_word2vec, ncol=2)

