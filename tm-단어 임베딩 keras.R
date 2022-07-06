# loading library
devtools::install_github("rstudio/keras")
devtools::install_github("rstudio/tensorflow")
install_keras()

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


# **reviews.csv**: a csv file with review texts - the fuel for our NLP analyses. (included key: restoreviewid, hence the unique identifier for a review) 
reviews <- read.csv(file = 'https://bhciaaablob.blob.core.windows.net/cmotionsnlpblogs/reviews.csv',header=TRUE,stringsAsFactors=FALSE)

# **labels.csv**: a csv file with 1 / 0 values, indicating whether the review is a review for a Michelin restaurant or not (included key: restoreviewid)
labels <- read.csv(file = 'https://bhciaaablob.blob.core.windows.net/cmotionsnlpblogs/labels.csv',header=TRUE,stringsAsFactors=FALSE)

# **restoid.csv**: a csv file with restaurant id's, to be able to determine which reviews belong to which restaurant (included key: restoreviewid)
restoids <- read.csv(file = 'https://bhciaaablob.blob.core.windows.net/cmotionsnlpblogs/restoid.csv',header=TRUE,stringsAsFactors=FALSE)

# **trainids.csv**: a csv file with 1 / 0 values, indicating whether the review should be used for training or testing - we already split the reviews in train/test to enable reuse of the same samples for fair comparisons between techniques (included key: restoreviewid)storage_download(cont, "blogfiles/labels.csv",overwrite =TRUE)
trainids <- read.csv(file = 'https://bhciaaablob.blob.core.windows.net/cmotionsnlpblogs/trainids.csv',header=TRUE,stringsAsFactors=FALSE)


## divide reviewtext into separate words
reviews_tokens <- reviews %>% 
  select(restoReviewId, reviewTextClean) %>%
  unnest_tokens(word, reviewTextClean)

## count the number of words per review and plot results
reviews_tokens %>% 
  group_by(restoReviewId) %>% summarise(n_tokens = n()) %>%
  mutate(n_tokens_binned = cut(n_tokens, breaks = c(0,seq(10,300,10),Inf))) %>% 
  group_by(n_tokens_binned) %>% summarise(n_reviews = n())

## pass result to ggplot
ggplot(aes(x=n_tokens_binned,y=n_reviews)) + 
  geom_bar(stat='identity',fill='green') + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  geom_text(size=2, aes(label=n_reviews), position=position_dodge(width=0.9), vjust=-0.25)

reviews_tokens %>% 
  group_by(word) %>% summarize(word_freq=n()) %>% 
  mutate(min_5_freq = case_when(word_freq<5~'token frequency: <5',TRUE~'token frequency: >=5')) %>% 
  group_by(min_5_freq) %>% summarise(n_tokens = n()) %>% mutate(pct_tokens = n_tokens / sum(n_tokens))

reviews_tokens %>% head()

reviews_new <- reviews_tokens %>% 
  group_by(word) %>% 
  mutate(token_freq=n()) %>%  
  filter(token_freq>=5) %>% 
  group_by(restoReviewId) %>% 
  summarise(reviewTextClean = str_c(word, collapse = " "))

x_train <- trainids %>% left_join(y=reviews_new, by= "restoReviewId", match = "all") %>% 
  filter(train == 1) %>% select(reviewTextClean) %>% pull()
x_test <- trainids %>% left_join(y=reviews_new, by= "restoReviewId", match = "all") %>% 
  filter(train == 0) %>% select(reviewTextClean) %>% pull()

y_train <- trainids %>% left_join(y=labels, by= "restoReviewId", match = "all") %>% 
  filter(train == 1) %>% select(ind_michelin) %>% pull() %>% as.array()
y_test <- trainids %>% left_join(y=labels, by= "restoReviewId", match = "all") %>% 
  filter(train == 0) %>% select(ind_michelin) %>% pull() %>% as.array()

x_train %>% head()
x_test %>% head()
y_train %>% table()
y_test %>% head()

# maximum number of words for a review
max_length <- 150

# Vectorize the tokens, each token will receive a unique integer, the index of that token in a dictionary. 
# Remember, we already restricted the corpus to 37.520 unique words.
tokenizer_train <- text_tokenizer() %>% fit_text_tokenizer(x_train)
tokenizer_test <- text_tokenizer() %>% fit_text_tokenizer(x_test)

# and put these integers into a sequence
sequences_train <- texts_to_sequences(tokenizer_train, x_train)
sequences_test <- texts_to_sequences(tokenizer_train, x_test)

# and make sure that every sequence has the same length (Keras requirement)
input_train <- pad_sequences(sequences_train, maxlen = max_length)
input_test <- pad_sequences(sequences_test, maxlen = max_length)

# show an example from the created index (word and vector)
tokenizer_train$word_index[200:204]

cat(paste0('Original text of review number 1001 without interpunction, low frequency words and stopwords:', '\n' ,x_train[1001],'\n\n'))
cat(paste0('What this review looks like converted to integers:'),'\n', (input_train[1001,]),'\n\n')
cat(paste0('Mapping back the first word of review 1001 using the dictionary index:', '\n'))

# Let us check whether the first non-zero integers map back to the right words: 
print(tokenizer_train$word_index[495])
print(tokenizer_train$word_index[3307])

word2vecdim <- 32

# how many words are in the index
num_tokens <- length(unique(tokenizer_train$word_index))

model <- keras_model_sequential() %>% 
  # Specify the maximum input length (150) and input_dim (unique tokens+1) and choose 32 dimensions
  layer_embedding(input_dim = num_tokens+1, 
                  output_dim = word2vecdim, 
                  input_length = max_length,
                  mask_zero = TRUE,                 
  ) %>% 
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

history <- model %>% keras::fit(
  input_train, y_train,
  # maximum number of iterations
  epochs = 20,
  # how many reviews do we offer in each batch
  batch_size = 500,
  # check train results againts test data
  validation_data = list(input_test, y_test)
)

input_train %>% dim()
y_train %>% dim()
input_test %>% dim()
y_test %>% dim()

y_test %>% table()

history %>% plot()

# get embedding matrix, the weights from the model
word2vec_embedding <- get_weights(model)[[1]]

# give the index back the name of the word for looking up a word embedding (NA for blanks)
rownames(word2vec_embedding) <- c('NA',as.vector(unlist(tokenizer_train$index_word)))

# let's look up word 495 ("aanraden") again, the index shifted with 1 as NAs are now on top of the list 
print(rownames(word2vec_embedding)[496])
word2vec_embedding[495,]

# find words that are related to another word 
token <- "pasta"
embedding_vector <- t(matrix(word2vec_embedding[token,])) 
cos_sim = sim2(x = word2vec_embedding, y = embedding_vector, method = "cosine", norm = "l2")
cat(paste0('Words from the embedding layer similar to "pasta":', '\n'))
print(head(sort(cos_sim[,1], decreasing = TRUE), 10))

token <- "bier"
embedding_vector <- t(matrix(word2vec_embedding[token,])) 
cos_sim = sim2(x = word2vec_embedding, y = embedding_vector, method = "cosine", norm = "l2")
cat(paste0('\n', 'Words from the embedding layer similar to "bier":', '\n'))
print(head(sort(cos_sim[,1], decreasing = TRUE), 10))

library(text2vec)

# We need to tokenize our already tokenized set as input for text2vec, re-use cleaned text in reviews_new
it <- itoken(reviews_new$reviewTextClean, 
             tokenizer = word_tokenizer,
             ids = reviews_new$restoReviewId,
             progressbar = TRUE)

# create a vocabulary out of the tokenset (stopword removal and bi-grams are optional)
vocab <- create_vocabulary(it) # use uni-grams

# text2vec has the option to prune the vocabulary of low-frequent words
vocab <- prune_vocabulary(vocab, term_count_min = 5)

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
glove_embedding = glove_model$fit_transform(tcm, n_iter = 20, convergence_tol = 0.01, n_threads = 4)

# combine main embedding and context embeddings (sum) into one matrix
glove_embedding = glove_embedding + t(glove_model$components) # the transpose of the context matrix


word <- glove_embedding["pasta", , drop = FALSE] # wat ligt er dicht bij 'lekker'
cos_sim = sim2(x = glove_embedding, y = word, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 10)

word <- glove_embedding["bier", , drop = FALSE] 
cos_sim = sim2(x = glove_embedding, y = word, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 10)


# Word2Vec dimension reduction
word2vec_umap <- umap(word2vec_embedding, n_components = 2, metric = "cosine", n_neighbors = 25, min_dist = 0.1, spread=2)

# GloVe dimension reduction
glove_umap <- umap(glove_embedding, n_components = 2, metric = "cosine", n_neighbors = 25, min_dist = 0.1, spread=2)

# Dimensions of end result
dim(glove_umap)

# Put results in a dataframe for ggplot, starting with Word2Vec
df_word2vec_umap <- as.data.frame(word2vec_umap, stringsAsFactors = FALSE)

# Add the labels of the words to the dataframe
df_word2vec_umap$word <- rownames(word2vec_embedding)
colnames(df_word2vec_umap) <- c("UMAP1", "UMAP2", "word")
df_word2vec_umap$technique <- 'Word2Vec'
cat(paste0('Our Word2Vec embedding reduced to 2 dimensions:', '\n'))
str(df_word2vec_umap)

# Do the same for the GloVe embeddings
df_glove_umap <- as.data.frame(glove_umap, stringsAsFactors = FALSE)

# Add the labels of the words to the dataframe
df_glove_umap$word <- rownames(glove_embedding)
colnames(df_glove_umap) <- c("UMAP1", "UMAP2", "word")
df_glove_umap$technique <- 'GloVe'
cat(paste0('\n', 'Our GloVe embedding reduced to 2 dimensions:', '\n'))
str(df_glove_umap)

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
selected_words <- df_glove_umap %>% inner_join(y=select, by= "word", match = "all") 

#The ggplot visual for Word2Vec
pesto_glove <- ggplot(selected_words, aes(x = UMAP1, y = UMAP2, colour = word == 'pesto')) + 
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
selected_words <- df_word2vec_umap %>% inner_join(y=select, by= "word", match = "all") 

# The ggplot visual for GloVe
pesto_word2vec <- ggplot(selected_words, aes(x = UMAP1, y = UMAP2, colour = word == 'pesto')) + 
  geom_point(show.legend = FALSE) + 
  scale_color_manual(values = c('black', 'red')) +
  geom_text(aes(UMAP1, UMAP2, label = word), show.legend = FALSE, size = 2.5, vjust=-1.5, hjust=0) +
  labs(title = "Word2Vec word embedding of words related to 'pesto'") +
  theme(plot.title = element_text(hjust = .5, size = 14))

library(gridExtra)

# Put the results side-by-side for a comparison
grid.arrange(pesto_glove,pesto_word2vec, ncol=2)