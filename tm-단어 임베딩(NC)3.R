devtools::install_github("rstudio/keras")
devtools::install_github("rstudio/tensorflow")
install_keras()
library(tensorflow)
sess <- tf$Session()
install_tensorflow()

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
# 2013ë…„ êµ¬ê¸€ì—ì„œ ê°œë°œí•œ Word2Vecì´ë¼ëŠ” ë°©ë²•ë¡ ì´ ìžˆìŠµë‹ˆë‹¤. ì´ë¦„ ê·¸ëŒ€ë¡œ ë‹¨ì–´(Word)ë¥¼ ë²¡í„°(Vector)ë¡œ ë°”ê¿”ì£¼ëŠ” ë°©ë²•ìž…ë‹ˆë‹¤. ì´ë¥¼ ìž„ë² ë”©(Embedding). GloVe, Fasttext ê°™ì€ ë‹¤ë¥¸ ë°©ë²•ë¡ ê³¼ì˜ ë¹„êµ ë‹¨ì–´ë¥¼ ë²¡í„°í™”í•  ë•Œ ë‹¨ì–´ì˜ ë¬¸ë§¥ì  ì˜ë¯¸ë¥¼ ë³´ì¡´

# data ì¤€ë¹„
ë‰´ìŠ¤_sports.news_NC <- read_csv(file = "D:/ëŒ€í•™ì›/ë…¼ë¬¸/ì†Œë…¼ë¬¸/ë‰´ìŠ¤_sports.news_NC.csv", col_names = TRUE, locale=locale('ko',encoding='euc-kr'))

names(ë‰´ìŠ¤_sports.news_NC) <- c("ì–¸ë¡ ì‚¬_NC","ì œëª©_NC","ë‚ ì§œ_NC","ë§í¬_NC","ì¢‹ì•„_NC","í›ˆí›ˆ_NC","ìŠ¬í¼_NC","í™”ë‚˜_NC","í›„ì†_NC")

ë‰´ìŠ¤_NC <- ë‰´ìŠ¤_sports.news_NC[!is.na(ë‰´ìŠ¤_sports.news_NC$ë‚ ì§œ_NC),]

ë‰´ìŠ¤_NC$ID_NC <- c(1:nrow(ë‰´ìŠ¤_NC))

ë‰´ìŠ¤_NC %>% head()

ë‰´ìŠ¤_NC$ì œëª©_NC <- gsub("í¬í† ","",ë‰´ìŠ¤_NC$ì œëª©_NC)
ë‰´ìŠ¤_NC$ì œëª©_NC <- gsub("ì˜¤ëŠ˜","",ë‰´ìŠ¤_NC$ì œëª©_NC)
ë‰´ìŠ¤_NC$ì œëª©_NC <- gsub("ê²½ê¸°","",ë‰´ìŠ¤_NC$ì œëª©_NC)
ë‰´ìŠ¤_NC$ì œëª©_NC <- gsub("ì‚¬ì§„","",ë‰´ìŠ¤_NC$ì œëª©_NC)
ë‰´ìŠ¤_NC$ì œëª©_NC <- gsub("ìŠ¤í¬ì¸ ","",ë‰´ìŠ¤_NC$ì œëª©_NC)
ë‰´ìŠ¤_NC$ì œëª©_NC <- gsub("ì¢…í•©","",ë‰´ìŠ¤_NC$ì œëª©_NC)
ë‰´ìŠ¤_NC$ì œëª©_NC <- gsub("ë‹¤ì‹œ","",ë‰´ìŠ¤_NC$ì œëª©_NC)
ë‰´ìŠ¤_NC$ì œëª©_NC <- gsub("ì¹˜ì–´ë¦¬ë”","",ë‰´ìŠ¤_NC$ì œëª©_NC)
ë‰´ìŠ¤_NC$ì œëª©_NC <- gsub("\u7f8e","ë¯¸êµ­",ë‰´ìŠ¤_NC$ì œëª©_NC)
ë‰´ìŠ¤_NC$ì œëª©_NC <- gsub("\u65e5","ì¼ë³¸",ë‰´ìŠ¤_NC$ì œëª©_NC)

## divide reviewtext into separate words
ë‰´ìŠ¤_NC %>% str()
IDì œëª©_NC <- ë‰´ìŠ¤_NC %>% dplyr::select("ID_NC","ì œëª©_NC") ## MASSì™€ ì¶©ëŒ

# ë°ì´í„° ë¶„í• 
IDì œëª©_NC_dim <- IDì œëª©_NC %>% dim()

IDì œëª©_NC_dim_int <- IDì œëª©_NC_dim[1] / 10000
IDì œëª©_NC_dim_int <- IDì œëª©_NC_dim_int %>% ceiling()
n <- IDì œëª©_NC_dim_int

IDì œëª©_NC_sp <- split(IDì œëª©_NC,rep(1:n, each = 10000))

# ë°ì´í„° ì…‹ ë§Œë“¤ê¸°
IDì œëª©_NC_tb <- list()
IDì œëª©_NC_data_set <- list()
IDì œëª©_NC_token <- c()
IDì œëª©_NC_í•œê¸€ <- list()
IDì œëª©_NC_ì˜ì–´ <- list()
IDì œëª©_NC <- tibble()

## ë‹¨ì–´ê¸°ì¤€ í† í°í™”
for (i in 1:n){
  
  cat(i, 'ë²ˆì§¸ ë°ì´í„° ë¦¬ìŠ¤íŠ¸ tokenizer', 'ì¤‘ ìž…ë‹ˆë‹¤.\n') 
  
  IDì œëª©_NC_tb[[i]] <- IDì œëª©_NC_sp[[i]] %>% 
    tibble() %>% 
    unnest_tokens(input = ì œëª©_NC, output = word_NC, token = "words", drop = FALSE)

  names(IDì œëª©_NC_tb[[i]]) <- c("ID_NC","ì œëª©_NC","word_NC")
  
  IDì œëª©_NC_í•œê¸€[[i]] <- IDì œëª©_NC_tb[[i]] %>%  
    mutate(í•œê¸€_NC = str_match(word_NC,'([ê°€-íž£]+)')[,2]) %>% ## "í•œê¸€" variableì„ ë§Œë“¤ê³  í•œê¸€ë§Œ ì €ìž¥         
    na.omit() %>% ## ([ê°€-íž£]+)/P') í•œê¸€ë§Œì„ ì„ íƒí•˜ëŠ” ì •ê·œí‘œí˜„ì‹
    mutate(ê¸€ìžìˆ˜_NC = str_length(í•œê¸€_NC)) %>%   ## "ê¸€ìžìˆ˜" variableì„ ë§Œë“­ë‹ˆë‹¤ 
    filter(str_length(í•œê¸€_NC) >= 2) 
  
  IDì œëª©_NC_ì˜ì–´[[i]] <- IDì œëª©_NC_tb[[i]] %>%  
    mutate(ì˜ì–´_NC = str_match(word_NC,'([a-zA-Z]+)')[,2]) %>% ## "í•œê¸€" variableì„ ë§Œë“¤ê³  í•œê¸€ë§Œ ì €ìž¥         
    na.omit() %>% ## 
    mutate(ê¸€ìžìˆ˜_NC = str_length(ì˜ì–´_NC)) %>%   ## "ê¸€ìžìˆ˜" variableì„ ë§Œë“­ë‹ˆë‹¤ 
    filter(str_length(ì˜ì–´_NC) >= 2) 
  
  IDì œëª©_NC <- bind_rows(IDì œëª©_NC,IDì œëª©_NC_í•œê¸€[[i]])
  IDì œëª©_NC <- bind_rows(IDì œëª©_NC,IDì œëª©_NC_ì˜ì–´[[i]])
}

NC_tokens <- IDì œëª©_NC %>% dplyr::select("ID_NC","ì œëª©_NC","word_NC","ê¸€ìžìˆ˜_NC")

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

# ì‹ ê²½ë§ì„ êµ¬ì¶• í•  ë•Œ ì´ 5 ê°œ ì´ìƒì˜ ë¹ˆë„ë¥¼ ê°€ì§„ ë‹¨ì–´ ë§Œ ì‚¬ìš©. í˜„ìž¬ ì»¬ë ‰ì…˜ì€ 60,016ê°œì˜ ê³ ìœ  í•œ ë‹¨ì–´ë¡œ ë‚´ë ¤ê°‘ë‹ˆë‹¤.
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


# ì‹ ê²½ë§ì´ ë°ì´í„°ë¥¼ ìž…ë ¥ìœ¼ë¡œ ì·¨í•  ìˆ˜ ìžˆë„ë¡ í† í°ì„ ì •ìˆ˜ë¡œ ë³€í™˜ í•„ìš”. ì‹ ê²½ë§ì— ì§ì ‘ í…ìŠ¤íŠ¸ë¥¼ inputí•  ìˆ˜ ì—†ë‹¤. ì‹ ê²½ë§ ìš©ì–´ë¡œ ê¸°ì‚¬ (ìƒ˜í”Œ) ë° ë‹¨ì–´ ë²¡í„° (íŠ¹ì§•)ê°€ìžˆëŠ” 2 ì°¨ì› í–‰ë ¬ì´ í•„ìš”. ì´ë¥¼ ìœ„í•´ì„œëŠ” ìž…ë ¥ ê¸°ëŠ¥ì˜ ê¸¸ì´ê°€ ê°™ì•„ì•¼í•©ë‹ˆë‹¤. ì•„ëž˜ì—ì„œëŠ” í…ìŠ¤íŠ¸ë¥¼ ë²¡í„°í™”í•˜ê³  ì¸ë±ìŠ¤ë¥¼ ë§Œë“¤ê³  íŒ¨ë”© (0 ì¶”ê°€)ì„ ì‚¬ìš©í•˜ì—¬ ë™ì¼í•œ í¬ê¸°ë¥¼ ë§Œë“­ë‹ˆë‹¤.

#label ë¶™ì´ê¸° ê¸ì • or ë¶€ì •
NC_tokens_min5$label <- rbinom(n = length(NC_tokens_min5$ID_NC), size = 1, prob = 0.1)

TextClean_NC <- NC_tokens_min5 %>% select(NCTextClean) %>% pull()
label_NC <- NC_tokens_min5 %>% select(label) %>% pull() %>% as.array()


# maximum number of words for a review
max_length_NC <- 150

# í† í°ì„ ë²¡í„°í™” í•˜ê³  ê°ê°ì— ê³ ìœ ê°’(integer)ì„ ë¶€ì—¬,
tokenizer_NC <- text_tokenizer() %>% # Vectorize a text corpus, by turning each text into either a sequence of integers 
  fit_text_tokenizer(TextClean_NC)


# í† í°ì— ê³ ìœ ê°’(integer)ì„ ë¶€ì—¬í•˜ê³  ì´ë¥¼ ì‹œí€€ìŠ¤(list)ë¡œ ë§Œë“¬
sequences_NC <- texts_to_sequences(tokenizer_NC, TextClean_NC)

# ì‹œí€€ìŠ¤(list)ë¥¼ ê°™ì€ ê¸¸ì´ë¡œ ë§Œë“ ë‹¤ max_lengthì— ë§žì¶˜ë‹¤ (Keras requirement)
input_NC <- pad_sequences(sequences_NC, maxlen = max_length_NC)

cat(paste0('Original text of article number 1029 without interpunction, low frequency words and stopwords:', '\n' ,TextClean_NC[1],'\n\n'))
cat(paste0('What this article looks like converted to integers:'),'\n', (input_NC[1,]),'\n\n')


# word2vec embedding ì°¨ì› ì„¤ì •
word2vecdim_NC <- 150

# words index ê¸¸ì´
num_tokens_NC <- unique(tokenizer_NC$word_index) %>% length()

model_NC <- keras_model_sequential() %>% 
  # Specify the maximum input length (150) and input_dim (unique tokens+1) and choose 32 dimensions
  layer_embedding(input_dim = num_tokens_NC + 1, 
                  output_dim = word2vecdim_NC, 
                  input_length = max_length_NC,
                  mask_zero = TRUE) %>% 
  # 3D í–‰ë ¬ of embeddings into a 2D í–‰ë ¬ >
  # Shape `(samples, max_length * word2vecdim)`
  layer_flatten() %>% 
  # add a dense layer with 32 units
  layer_dense(units = 32, activation = "relu") %>% # rectifier í•¨ìˆ˜, ì€ë‹‰ì¸µì— ì£¼ë¡œ ì“°ìž…ë‹ˆë‹¤.
  # add the classifier on top
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 1, activation = "sigmoid") # â€˜sigmoidâ€™ : ì‹œê·¸ëª¨ì´ë“œ í•¨ìˆ˜, ì´ì§„ ë¶„ë¥˜ ë¬¸ì œì—ì„œ ì¶œë ¥ì¸µì— ì£¼ë¡œ ì“°ìž…ë‹ˆë‹¤ / softmaxâ€™ : ì†Œí”„íŠ¸ë§¥ìŠ¤ í•¨ìˆ˜, ë‹¤ì¤‘ í´ëž˜ìŠ¤ ë¶„ë¥˜ ë¬¸ì œì—ì„œ ì¶œë ¥ì¸µì— ì£¼ë¡œ ì“°ìž…ë‹ˆë‹¤.

model_NC %>% summary()

model_NC %>% compile(
  optimizer = "rmsprop",
  # binary classification(ì´ì§„ ë¶„ë¥˜) 
  loss = "binary_crossentropy",
  # ì •í™•ë„ accuracyë¥¼ ì¸¡ë„ë¡œ ì‚¬ìš©
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
token <- "ìŠ¹ë¦¬"
embedding_vector_ìŠ¹ë¦¬ <- t(matrix(word2vec_embedding_NC[token,])) 
cos_sim_ìŠ¹ë¦¬ <- sim2(x = word2vec_embedding_NC, y = embedding_vector_ìŠ¹ë¦¬ , method = "cosine", norm = "l2") # similarities
cat(paste0('Words from the embedding layer similar to "ìŠ¹ë¦¬":', '\n'))
print(head(sort(cos_sim_ìŠ¹ë¦¬[,1], decreasing = TRUE), 10))


## part without keras
# We need to tokenize our already tokenized set as input for text2vec, re-use cleaned text in NC_tokens_min5
it_ìŠ¹ë¦¬ <- itoken(NC_tokens_min5$NCTextClean, 
             tokenizer = word_tokenizer,
             ids = NC_tokens_min5$ID_NC,
             progressbar = TRUE)

# create a vocabulary out of the tokenset (stopword removal and bi-grams are optional)
vocab_ìŠ¹ë¦¬ <- it_ìŠ¹ë¦¬ %>% create_vocabulary() # use uni-grams

# text2vec has the option to prune the vocabulary of low-frequent words
vocab_ìŠ¹ë¦¬ <- vocab_ìŠ¹ë¦¬ %>% prune_vocabulary(term_count_min = 5)

# What's in the vocabulary?
vocab_ìŠ¹ë¦¬

# Vectorize word to integers
vectorizer_ìŠ¹ë¦¬ <- vocab_ìŠ¹ë¦¬ %>% vocab_vectorizer()

# Create a Term-Count-Matrix, by default it will use a skipgram window of 5 (symmetrical)
tcm_ìŠ¹ë¦¬ <- create_tcm(it_ìŠ¹ë¦¬, vectorizer_ìŠ¹ë¦¬, skip_grams_window = 5L)

# maximum number of co-occurrences to use in the weighting function, we choose the entire token set divided by 100
x_max_ìŠ¹ë¦¬ <- length(vocab_ìŠ¹ë¦¬$doc_count)/100

# set up the embedding matrix and fit model
glove_model_ìŠ¹ë¦¬ <- GloVe$new(rank = 32, x_max = x_max_ìŠ¹ë¦¬) 
glove_embedding_ìŠ¹ë¦¬ <- glove_model_ìŠ¹ë¦¬$fit_transform(tcm_ìŠ¹ë¦¬, n_iter = 20, convergence_tol = 0.01, n_threads = 4)

# combine main embedding and context embeddings (sum) into one matrix
glove_embedding_ìŠ¹ë¦¬ <- glove_embedding_ìŠ¹ë¦¬ + t(glove_model_ìŠ¹ë¦¬$components) # the transpose of the context matrix

# ìš°ë¦¬ëŠ” ì´ì „ì—í–ˆë˜ ê²ƒê³¼ ê°™ì€ ê·¼ì²˜ì˜ ë‹¨ì–´ë¥¼ ë½‘ìŠµë‹ˆë‹¤.
word_ìŠ¹ë¦¬ <- glove_embedding_ìŠ¹ë¦¬["ìŠ¹ë¦¬", , drop = FALSE] # wat ligt er dicht bij 'lekker'
cos_sim_ìŠ¹ë¦¬ <- sim2(x = glove_embedding_ìŠ¹ë¦¬, y = word_ìŠ¹ë¦¬, method = "cosine", norm = "l2")
sort(cos_sim_ìŠ¹ë¦¬[,1], decreasing = TRUE) %>% head(10)

# Word2Vec dimension reduction
word2vec_umap_ìŠ¹ë¦¬ <- umap(word2vec_embedding_NC, n_components = 2, metric = "cosine", n_neighbors = 25, min_dist = 0.1, spread = 2) # X ë° Y í”Œë¡¯ì„ ì‚¬ìš©í•˜ì—¬ ê²°ê³¼ë¥¼ ì‰½ê²Œ ì‹œê°í™” í•  ìˆ˜ ìžˆë„ë¡ ìž„ë² ë”© ì°¨ì› (32) ìˆ˜ë¥¼ 2 ê°œë¡œ ì¤„ì˜€ìŠµë‹ˆë‹¤
word2vec_umap_ìŠ¹ë¦¬ %>% str()

# GloVe dimension reduction
glove_umap_ìŠ¹ë¦¬ <- umap(glove_embedding_ìŠ¹ë¦¬, n_components = 2, metric = "cosine", n_neighbors = 25, min_dist = 0.1, spread=2)
glove_umap_ìŠ¹ë¦¬$layout %>% dim()

# Put results in a dataframe for ggplot, starting with Word2Vec
df_word2vec_umap_ìŠ¹ë¦¬ <- as.data.frame(word2vec_umap_ìŠ¹ë¦¬$layout, stringsAsFactors = FALSE)

# Add the labels of the words to the dataframe
df_word2vec_umap_ìŠ¹ë¦¬$word <- rownames(word2vec_embedding_NC)
colnames(df_word2vec_umap_ìŠ¹ë¦¬) <- c("UMAP1", "UMAP2", "word")
df_word2vec_umap_ìŠ¹ë¦¬$technique <- 'Word2Vec'
cat(paste0('Our Word2Vec embedding reduced to 2 dimensions:', '\n'))
df_word2vec_umap_ìŠ¹ë¦¬ %>% str()

# Do the same for the GloVe embeddings
df_glove_umap_ìŠ¹ë¦¬ <- as.data.frame(glove_umap_ìŠ¹ë¦¬$layout, stringsAsFactors = FALSE)

# Add the labels of the words to the dataframe
df_glove_umap_ìŠ¹ë¦¬$word <- rownames(glove_embedding_ìŠ¹ë¦¬)
colnames(df_glove_umap_ìŠ¹ë¦¬) <- c("UMAP1", "UMAP2", "word")
df_glove_umap_ìŠ¹ë¦¬$technique <- 'GloVe'
cat(paste0('\n', 'Our GloVe embedding reduced to 2 dimensions:', '\n'))
df_glove_umap_ìŠ¹ë¦¬ %>% str()

# Combine the datasets
df_umap_ìŠ¹ë¦¬ <- bind_rows(df_word2vec_umap_ìŠ¹ë¦¬, df_glove_umap_ìŠ¹ë¦¬)

ggplot(df_umap_ìŠ¹ë¦¬) +
  geom_point(aes(x = UMAP1, y = UMAP2), colour = 'blue', size = 0.05) +
  facet_wrap(~technique) +
  labs(title = "Word embedding in 2D using UMAP") +
  theme(plot.title = element_text(hjust = .5, size = 14))

# Plot the bottom part of the GloVe word embedding with labels
ggplot(df_glove_umap_ìŠ¹ë¦¬[df_glove_umap_ìŠ¹ë¦¬$UMAP1 > 3.0 & df_glove_umap_ìŠ¹ë¦¬$UMAP1 < 3.8 & df_glove_umap_ìŠ¹ë¦¬$UMAP2 > 4.6,]) +
  geom_point(aes(x = UMAP1, y = UMAP2), colour = 'blue', size = 2) +
  geom_text(aes(UMAP1, UMAP2, label = word), size = 2.5, vjust=-1, hjust=0) +
  labs(title = "GloVe word embedding in 2D using UMAP - partial view") +
  theme(plot.title = element_text(hjust = .5, size = 14))

# Plot the word embedding of words that are related for the GloVe model
word_ìŠ¹ë¦¬ <- glove_embedding_ìŠ¹ë¦¬["ìŠ¹ë¦¬", , drop = FALSE] 
cos_sim_ìŠ¹ë¦¬ <- sim2(x = glove_embedding_ìŠ¹ë¦¬, y = word_ìŠ¹ë¦¬, method = "cosine", norm = "l2")
select_ìŠ¹ë¦¬ <- data.frame(rownames(as.data.frame(head(sort(cos_sim_ìŠ¹ë¦¬[,1], decreasing = TRUE), 25))))
colnames(select_ìŠ¹ë¦¬) <- "word"
selected_words_GloVe_ìŠ¹ë¦¬ <- df_glove_umap_ìŠ¹ë¦¬ %>% inner_join(y=select_ìŠ¹ë¦¬, by= "word", match = "all") 

#The ggplot visual for GloVe
ggplot(selected_words_GloVe_ìŠ¹ë¦¬, aes(x = UMAP1, y = UMAP2, colour = word == 'ìŠ¹ë¦¬')) + 
  geom_point(show.legend = FALSE) + 
  scale_color_manual(values = c('black', 'red')) +
  geom_text(aes(UMAP1, UMAP2, label = word), show.legend = FALSE, size = 2.5, vjust=-1.5, hjust=0) +
  theme(plot.title = element_text(hjust = .5, size = 14))

# And so the same for the Word2Vec model
token <- "ìŠ¹ë¦¬"
embedding_vector_ìŠ¹ë¦¬ <- t(matrix(word2vec_embedding_NC[token,])) 
cos_sim_ìŠ¹ë¦¬ <- sim2(x = word2vec_embedding_NC, y = embedding_vector_ìŠ¹ë¦¬, method = "cosine", norm = "l2")
select_ìŠ¹ë¦¬ <- data.frame(rownames(as.data.frame(head(sort(cos_sim_ìŠ¹ë¦¬[,1], decreasing = TRUE), 25))))
colnames(select_ìŠ¹ë¦¬) <- "word"
selected_words_Word2Vec_ìŠ¹ë¦¬ <- df_word2vec_umap_ìŠ¹ë¦¬ %>% inner_join(y=select_ìŠ¹ë¦¬, by= "word", match = "all") 

# The ggplot visual for GloVe
ggplot(selected_words_Word2Vec_ìŠ¹ë¦¬ , aes(x = UMAP1, y = UMAP2, colour = word == 'ìŠ¹ë¦¬')) + 
  geom_point(show.legend = FALSE) + 
  scale_color_manual(values = c('black', 'red')) +
  geom_text(aes(UMAP1, UMAP2, label = word), show.legend = FALSE, size = 2.5, vjust=-1.5, hjust=0) +
  theme(plot.title = element_text(hjust = .5, size = 14))

# Put the results side-by-side for a comparison
grid.arrange(pesto_glove,pesto_word2vec, ncol=2)

