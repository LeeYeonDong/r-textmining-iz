devtools::install_github("rstudio/keras")
devtools::install_github("rstudio/tensorflow")
library(keras)
library(tensorflow)
install_keras()

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
library(qdap)
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
# **labels.csv**: a csv file with 1 / 0 values, indicating whether the review is a review for a Michelin restaurant or not (included key: restoreviewid)
labels <- read.csv(file = 'https://bhciaaablob.blob.core.windows.net/cmotionsnlpblogs/labels.csv',header=TRUE,stringsAsFactors=FALSE)
labels %>% head()

# **restoid.csv**: a csv file with restaurant id's, to be able to determine which reviews belong to which restaurant (included key: restoreviewid)
restoids <- read.csv(file = 'https://bhciaaablob.blob.core.windows.net/cmotionsnlpblogs/restoid.csv',header=TRUE,stringsAsFactors=FALSE)
restoids %>% head()

# **trainids.csv**: a csv file with 1 / 0 values, indicating whether the review should be used for training or testing - we already split the reviews in train/test to enable reuse of the same samples for fair comparisons between techniques (included key: restoreviewid)storage_download(cont, "blogfiles/labels.csv",overwrite =TRUE)
trainids <- read.csv(file = 'https://bhciaaablob.blob.core.windows.net/cmotionsnlpblogs/trainids.csv',header=TRUE,stringsAsFactors=FALSE)
trainids %>% head()

##########

# **reviews.csv**: a csv file with review texts - the fuel for our NLP analyses. (included key: restoreviewid, hence the unique identifier for a review) 
뉴스_sports.news_NC <- read_csv(file = "C:/대학원/논문/소논문/뉴스_sports.news_NC.csv", col_names = TRUE, locale=locale('ko',encoding='euc-kr'))
뉴스_sports.news_NC %>% View()

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
뉴스_NC$제목_NC <- gsub("\u7f8e","",뉴스_NC$제목_NC)
뉴스_NC$제목_NC <- gsub("\u65e5","",뉴스_NC$제목_NC)

## divide reviewtext into separate words
뉴스_NC %>% str()
ID제목_NC <- 뉴스_NC %>% select("ID_NC","제목_NC")

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

## 단어기준 토큰화
for (i in 1:n){
  
  cat(i, '번째 데이터 리스트 tokenizer', '중 입니다.\n') 
  
  ID제목_NC_tb[[i]] <- ID제목_NC_sp[[i]] %>% tibble()
  
  ID제목_NC_tb[[i]] <- ID제목_NC_tb[[i]] %>% 
    unnest_tokens(input = 제목_NC, output = word, token = "words", drop = FALSE)
  
  ID제목_NC_data_set[[i]] <- ID제목_NC_tb[[i]]  %>% 
    melt() %>% # 식별자id, 측정 변수variable, 측정치value 형태로 데이터를 재구성하는 함수
    as_tibble() %>% 
    select(4,1,2)
  
  names(ID제목_NC_data_set[[i]]) <- c("ID_NC","제목_NC","word")
  
  ID제목_NC_한글[[i]] <- ID제목_NC_data_set[[i]] %>%  
    mutate(한글_NC = str_match(word,'([가-힣]+)')[,2]) %>% ## "한글" variable을 만들고 한글만 저장         
    na.omit() %>% ## ([가-힣]+)/P') 한글만을 선택하는 정규표현식
    mutate(글자수_NC = str_length(한글_NC)) %>%   ## "글자수" variable을 만듭니다 
    filter(str_length(한글_NC)>=2) 
  
  ID제목_NC_영어[[i]] <- ID제목_NC_data_set[[i]] %>%  
    mutate(영어_NC = str_match(word,'([a-zA-Z]+)')[,2]) %>% ## "한글" variable을 만들고 한글만 저장         
    na.omit() %>% ## 
    mutate(글자수_NC = str_length(영어_NC)) %>%   ## "글자수" variable을 만듭니다 
    filter(str_length(영어_NC)>=3) 
  
  ID제목_NC_token한글.tmp <- ID제목_NC_한글[[i]]$word
  ID제목_NC_token영어.tmp <- ID제목_NC_영어[[i]]$word
  
  ID제목_NC_token <- append(ID제목_NC_token,ID제목_NC_token한글.tmp)
  ID제목_NC_token <- append(ID제목_NC_token,ID제목_NC_token영어.tmp)
}


NC_tokens %>% head() 

## count the number of words per review and plot results
NC_tokens %>% 
  group_by(ID) %>% 
  summarise(n_tokens = n()) %>%
  mutate(n_tokens_binned = cut(n_tokens, breaks = c(1,seq(0,40,3),Inf))) %>% 
  group_by(n_tokens_binned) %>% 
  summarise(n_reviews = n()) %>% 
  ggplot(aes(x = n_tokens_binned, y = n_reviews)) + 
  geom_bar(stat = 'identity',fill = 'blue') + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 5, size = 10)) + 
  geom_text(size = 5, aes(label = n_reviews), position = position_dodge(width = 1), vjust = -0.5)

# 신경망을 구축 할 때 총 3 개 이상의 빈도를 가진 단어 만 사용합니다. 현재 컬렉션은 37.520 개의 고유 한 단어로 내려갑니다.
NC_tokens %>% 
  group_by(word) %>% 
  summarize(word_freq = n()) %>% 
  mutate(min_3_freq = case_when(word_freq >= 3 ~'token frequency : >= 3', TRUE ~ 'token frequency : < 3')) %>% 
  group_by(min_3_freq) %>% 
  summarise(n_tokens = n()) %>% 
  mutate(ratio_tokens = n_tokens / sum(n_tokens)) 

NC_tokens_min3 <- NC_tokens %>% 
  group_by(word) %>% 
  mutate(token_freq = n()) %>%  
  filter(token_freq >= 3) %>% 
  group_by(ID) %>% 
  summarise(NCTextClean = str_c(word, collapse = " "))

# split reviews and labels into train and test
x_train <- trainids %>% 
  left_join(y = reviews_new, by = "restoReviewId") %>% 
  filter(train == 1) %>% 
  dplyr::select(reviewTextClean) %>% 
  pull()

x_test <- trainids %>% 
  left_join(y=reviews_new, by= "restoReviewId") %>% 
  filter(train == 0) %>% 
  dplyr::select(reviewTextClean) %>%
  pull()

y_train <- trainids %>% 
  left_join(y=labels, by= "restoReviewId") %>% 
  filter(train == 1) %>% 
  dplyr::select(ind_michelin) %>% 
  pull() %>%
  as.array()

y_test <- trainids %>% 
  left_join(y=labels, by= "restoReviewId", match = "all") %>% 
  filter(train == 0) %>% 
  dplyr::select(ind_michelin) %>%
  pull() %>% 
  as.array()


# 레스토랑 리뷰 데이터에서 단어 임베딩을 훈련하기 위해 신경망을 사용하려면 신경망이 데이터를 입력으로 취할 수 있도록 토큰을 정수로 변환해야합니다. 신경망에 직접 텍스트를 공급할 수 없습니다. 신경망 용어로 리뷰 (샘플) 및 단어 벡터 (특징)가있는 2 차원 텐서가 필요합니다. 이를 위해서는 입력 기능의 길이가 같아야합니다. 아래에서는 텍스트를 벡터화하고 인덱스를 만들고 패딩 (0 추가)을 사용하여 동일한 크기를 만듭니다.

# maximum number of words for a review
max_length <- 150

# Vectorize the tokens, each token will receive a unique integer, the index of that token in a dictionary. 
# Remember, we already restricted the corpus to 37.520 unique words.
tokenizer_train <- text_tokenizer() %>% # Vectorize a text corpus, by turning each text into either a sequence of integers 
  fit_text_tokenizer(x_train) # Update tokenizer internal vocabulary based on a list of texts or list of sequences.

tokenizer_test <- text_tokenizer() %>%
  fit_text_tokenizer(x_test)

# and put these integers into a sequence
sequences_train <- texts_to_sequences(tokenizer_train, x_train)
sequences_test <- texts_to_sequences(tokenizer_train, x_test)

# and make sure that every sequence has the same length (Keras requirement)
input_train <- pad_sequences(sequences_train, maxlen = max_length)
input_test <- pad_sequences(sequences_test, maxlen = max_length)

# show an example from the created index (word and vector)
tokenizer_train$word_index[200:204]


# Let us check whether the first non-zero integers map back to the right words: 
print(tokenizer_train$word_index[495])
print(tokenizer_train$word_index[3307])

# how many dimensions do we want our word2vec embedding to have
word2vecdim <- 32

# how many words are in the index
num_tokens <- unique(tokenizer_train$word_index) %>% length()

model <- keras_model_sequential() %>% 
  # Specify the maximum input length (150) and input_dim (unique tokens+1) and choose 32 dimensions
  layer_embedding(input_dim = num_tokens+1, 
                  output_dim = word2vecdim, 
                  input_length = max_length,
                  mask_zero = TRUE) %>% 
  # Flatten the 3D tensor of embeddings into a 2D tensor >
  # Shape `(samples, max_length * word2vecdim)`
  layer_flatten() %>% 
  # add a dense layer with 32 units
  layer_dense(units = 32, activation = "relu") %>%
  # add the classifier on top
  layer_dense(units = 1, activation = "sigmoid") 

model %>% compile(
  optimizer = "rmsprop",
  # we have a binary classification 
  loss = "binary_crossentropy",
  # retrieve accuracy as measure
  metrics = c("acc")
)

history <- model %>% fit(input_train, y_train,
                         # maximum number of iterations
                         epochs = 20,
                         # how many reviews do we offer in each batch
                         batch_size = 500,
                         # check train results againts test data
                         validation_data = list(input_test, y_test)
)

# get embedding matrix, the weights from the model
word2vec_embedding <- get_weights(model)[[1]]

# give the index back the name of the word for looking up a word embedding (NA for blanks)
rownames(word2vec_embedding) <- c('NA',as.vector(unlist(tokenizer_train$index_word)))

# let's look up word 495 ("aanraden") again, the index shifted with 1 as NAs are now on top of the list 
print(rownames(word2vec_embedding)[496])
word2vec_embedding[496,]

# find words that are related to another word 
token <- "pasta"
embedding_vector_pasta <- t(matrix(word2vec_embedding[token,])) 
cos_sim_pasta <- sim2(x = word2vec_embedding, y = embedding_vector_pasta , method = "cosine", norm = "l2")
cat(paste0('Words from the embedding layer similar to "pasta":', '\n'))
print(head(sort(cos_sim_pasta [,1], decreasing = TRUE), 10))

token <- "bier"
embedding_vector <- t(matrix(word2vec_embedding[token,])) 
cos_sim <- sim2(x = word2vec_embedding, y = embedding_vector, method = "cosine", norm = "l2")
cat(paste0('\n', 'Words from the embedding layer similar to "bier":', '\n'))
print(head(sort(cos_sim[,1], decreasing = TRUE), 10))


# We need to tokenize our already tokenized set as input for text2vec, re-use cleaned text in reviews_new
it <- itoken(reviews_new$reviewTextClean, 
             tokenizer = word_tokenizer,
             ids = reviews_new$restoReviewId,
             progressbar = TRUE)

# create a vocabulary out of the tokenset (stopword removal and bi-grams are optional)
vocab <- it %>% create_vocabulary() # use uni-grams

# text2vec has the option to prune the vocabulary of low-frequent words
vocab <- vocab %>% prune_vocabulary(term_count_min = 5)

# What's in the vocabulary?
print(vocab)

# Vectorize word to integers
vectorizer <- vocab_vectorizer(vocab)

# Create a Term-Count-Matrix, by default it will use a skipgram window of 5 (symmetrical)
tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L)

# maximum number of co-occurrences to use in the weighting function, we choose the entire token set divided by 100
x_max <- length(vocab$doc_count)/100

# set up the embedding matrix and fit model
glove_model <- GloVe$new(rank = 32, x_max = x_max) 
glove_embedding <- glove_model$fit_transform(tcm, n_iter = 20, convergence_tol = 0.01, n_threads = 4)

# combine main embedding and context embeddings (sum) into one matrix
glove_embedding <- glove_embedding + t(glove_model$components) # the transpose of the context matrix

# 우리는 이전에했던 것과 같은 근처의 단어를 뽑습니다.
word_pasta <- glove_embedding["pasta", , drop = FALSE] # wat ligt er dicht bij 'lekker'
cos_sim_pasta <- sim2(x = glove_embedding, y = word_pasta, method = "cosine", norm = "l2")
sort(cos_sim_pasta[,1], decreasing = TRUE) %>% head(10)

word_bier <- glove_embedding["bier", , drop = FALSE] 
cos_sim_bier <- sim2(x = glove_embedding, y = word_bier, method = "cosine", norm = "l2")
sort(cos_sim_bier[,1], decreasing = TRUE) %>% head(10)

# Word2Vec dimension reduction
word2vec_umap <- umap(word2vec_embedding, n_components = 2, metric = "cosine", n_neighbors = 25, min_dist = 0.1, spread = 2) # X 및 Y 플롯을 사용하여 결과를 쉽게 시각화 할 수 있도록 임베딩 차원 (32) 수를 2 개로 줄였습니다
word2vec_umap %>% str()

# GloVe dimension reduction
glove_umap <- umap(glove_embedding, n_components = 2, metric = "cosine", n_neighbors = 25, min_dist = 0.1, spread=2)
glove_umap$layout %>% dim()

# Put results in a dataframe for ggplot, starting with Word2Vec
df_word2vec_umap <- as.data.frame(word2vec_umap$layout, stringsAsFactors = FALSE)

# Add the labels of the words to the dataframe
df_word2vec_umap$word <- rownames(word2vec_embedding)
colnames(df_word2vec_umap) <- c("UMAP1", "UMAP2", "word")
df_word2vec_umap$technique <- 'Word2Vec'
cat(paste0('Our Word2Vec embedding reduced to 2 dimensions:', '\n'))
df_word2vec_umap %>% str()

# Do the same for the GloVe embeddings
df_glove_umap <- as.data.frame(glove_umap$layout, stringsAsFactors = FALSE)

# Add the labels of the words to the dataframe
df_glove_umap$word <- rownames(glove_embedding)
colnames(df_glove_umap) <- c("UMAP1", "UMAP2", "word")
df_glove_umap$technique <- 'GloVe'
cat(paste0('\n', 'Our GloVe embedding reduced to 2 dimensions:', '\n'))
df_glove_umap %>% str()

# Combine the datasets
df_umap <- bind_rows(df_word2vec_umap, df_glove_umap)

ggplot(df_umap) +
  geom_point(aes(x = UMAP1, y = UMAP2), colour = 'blue', size = 0.05) +
  facet_wrap(~technique) +
  labs(title = "Word embedding in 2D using UMAP") +
  theme(plot.title = element_text(hjust = .5, size = 14))

# Plot the bottom part of the GloVe word embedding with labels
ggplot(df_glove_umap[df_glove_umap$UMAP1 > 3.0 & df_glove_umap$UMAP1 < 3.8 & df_glove_umap$UMAP2 > 4.6,]) +
  geom_point(aes(x = UMAP1, y = UMAP2), colour = 'blue', size = 2) +
  geom_text(aes(UMAP1, UMAP2, label = word), size = 2.5, vjust=-1, hjust=0) +
  labs(title = "GloVe word embedding in 2D using UMAP - partial view") +
  theme(plot.title = element_text(hjust = .5, size = 14))

# Plot the word embedding of words that are related for the GloVe model
word <- glove_embedding["pesto", , drop = FALSE] 
cos_sim = sim2(x = glove_embedding, y = word, method = "cosine", norm = "l2")
select <- data.frame(rownames(as.data.frame(head(sort(cos_sim[,1], decreasing = TRUE), 25))))
colnames(select) <- "word"
selected_words_GloVe <- df_glove_umap %>% inner_join(y=select, by= "word", match = "all") 

#The ggplot visual for Word2Vec
pesto_glove <- ggplot(selected_words_GloVe, aes(x = UMAP1, y = UMAP2, colour = word == 'pesto')) + 
  geom_point(show.legend = FALSE) + 
  scale_color_manual(values = c('black', 'red')) +
  geom_text(aes(UMAP1, UMAP2, label = word), show.legend = FALSE, size = 2.5, vjust=-1.5, hjust=0) +
  labs(title = "GloVe word embedding of words related to 'pesto'") +
  theme(plot.title = element_text(hjust = .5, size = 14))

# And so the same for the Word2Vec model
token <- "pesto"
embedding_vector <- t(matrix(word2vec_embedding[token,])) 
cos_sim = sim2(x = word2vec_embedding, y = embedding_vector, method = "cosine", norm = "l2")
select <- data.frame(rownames(as.data.frame(head(sort(cos_sim[,1], decreasing = TRUE), 25))))
colnames(select) <- "word"
selected_words_Word2Vec <- df_word2vec_umap %>% inner_join(y=select, by= "word", match = "all") 

# The ggplot visual for GloVe
pesto_word2vec <- ggplot(selected_words_Word2Vec , aes(x = UMAP1, y = UMAP2, colour = word == 'pesto')) + 
  geom_point(show.legend = FALSE) + 
  scale_color_manual(values = c('black', 'red')) +
  geom_text(aes(UMAP1, UMAP2, label = word), show.legend = FALSE, size = 2.5, vjust=-1.5, hjust=0) +
  labs(title = "Word2Vec word embedding of words related to 'pesto'") +
  theme(plot.title = element_text(hjust = .5, size = 14))

# Put the results side-by-side for a comparison
grid.arrange(pesto_glove,pesto_word2vec, ncol=2)

