# https://bookdown.org/ahn_media/bookdown-demo/anal3topic.html

library(topicmodels)
library(lda)
library(slam)
library(stm)
library(dplyr)
library(tidytext)
library(furrr) # try to make it faster
plan(multicore) # parallel processing within a single machine
# plan(multiprocess) #parallel processing across multiple machines
library(tm) # Framework for text mining
library(tidyverse) # Data preparation and pipes %>%
library(ggplot2) # For plotting word frequencies
library(devtools)
# devtools::install_github("lchiffon/wordcloud2") # 기존 wordcloud2 패키지 제거
library(wordcloud2)
library(Rtsne)
library(rsvd)
library(geometry)
library(NLP)
library(ldatuning) 
library(lubridate)

# Load data from csv file
system.time({
raw1_df %>% str()


# default parameters
news_processed <- textProcessor(raw1_df$키워드, metadata = raw1_df,
                                lowercase = TRUE, # Convert to lower case
                                removestopwords = TRUE, # Remove Stop-Words
                                removenumbers = TRUE, # Remove numbers
                                removepunctuation = TRUE, # Remove punctuation
                                stem = TRUE, # Stemming
                                wordLengths = c(2,Inf), # remove less than length 3, 한글이면 2
                                sparselevel = 1, #*
                                verbose = TRUE, #*
                                onlycharacter = TRUE, # not def
                                striphtml = FALSE, #*
                                customstopwords = NULL, #*
                                v1 = FALSE) #*

news_processed %>% str()
news_processed %>% class()

# filter out terms that don’t appear in more than 10 documents,
news_processed_out <- prepDocuments(news_processed$documents, news_processed$vocab, news_processed$meta, lower.thresh = 10)
# documents	: A list containing the documents in the stm format.
# vocab	: Character vector of vocabulary.
# meta : Data frame or matrix containing the user-supplied metadata for the retained documents

set.seed(1029)
First_STM <- stm(docs, vocab, 15,
                 prevalence = ~ publisher + s(date),
                 data = meta,
                 seed = 15, max.em.its = 5)

# Plot first Topic Model # dev.off() 실행 후 plot 그리기
plot(First_STM)


Second_STM <- stm(documents = docs, vocab = vocab, K = 18, prevalence = ~ publisher + s(date), max.em.its = 75, data = meta, init.type = "Spectral", verbose = FALSE)

plot(Second_STM)

# finding the optimal number of topics
# 1
findingk <- searchK(docs, vocab, K = c(10:30), prevalence = ~ publisher + s(date), data = meta, verbose = FALSE)

plot(findingk) # enlarge plots pane

# 2
findingk_ver2 <- searchK(documents = docs, 
                         vocab = vocab,
                         K = c(10,20,30,40,50,60,70), #specify K to try
                         N = 500, # matches 10% default
                         proportion = 0.5, # default
                         heldout.seed = 1029, # optional
                         M = 10, # default
                         cores = 1, # default=1
                         prevalence = ~ publisher + s(date),
                         max.em.its = 75, #was 75
                         data = meta,
                         init.type = "Spectral",
                         verbose = TRUE)
plot(findingk_ver2)

# 3
findingk_ver3.lee_mimno <- stm(documents = docs, 
                               vocab = vocab,
                               K = 0, # K=0 instructs STM to run Lee-Mimno
                               seed = 1029, # randomness now, seed matters
                               prevalence = ~ publisher + s(date),
                               max.em.its = 75,
                               data = meta,
                               init.type = "Spectral",
                               verbose = TRUE)
dev.off()
plot(findingk_ver3.lee_mimno)


# Top Words
Third_STM <- stm(documents = news_processed_out$documents, 
                 vocab = news_processed_out$vocab,
                 K = 7, data = news_processed_out$meta,
                 init.type = "Spectral", seed = 1029)
#Plot
plot(Third_STM)

Third_STM %>% labelTopics()

vocab %>% class()

# top 2 paragraps for Topic 1 to 10
Third_STM %>% findThoughts(texts = news_processed_out$meta$documents, n = 5, topics = 1:7)

# Graphical display of topic correlations
Third_STM %>% topicCorr() %>% plot()

# Graphical display of convergence
plot(Third_STM$convergence$bound, type = "l",
     ylab = "Approximate Objective",
     main = "Convergence")

# Wordcloud:topic 17 with word distribution
set.seed(1029)
cloud(Third_STM, topic = 7, scale = c(5,1))

# Topic proportions
Third_STM %>% str()

plot(Third_STM, type = "hist", topics = sample(1:7, size = 7)) # redline = median
# "stm" 패키지에서 생성된 "Document-Topic Proportion의 MAP Estimates의 분포" 그래프에서 x 축은 문서의 토픽 비율의 추정 값의 범위를 나타낸다. x 축은 각 문서에 할당된 토픽의 비율을 보여주며, 특정 토픽과의 연관성을 나타낸다. x 축 값은 일반적으로 0과 1 사이의 값으로, 추정된 비율의 최소와 최대 값을 나타낸다. 그래프의 각 막대의 높이는 주어진 토픽 비율을 가진 문서의 빈도 또는 수를 나타낸다.

plot(Third_STM, type = "summary")
plot(Third_STM, type = "labels")
plot(Third_STM, type = "perspectives", topics = c(1,2))

summarize_all(make.dt(Third_STM), mean)

tidy(Third_STM) # beta

tidy(Third_STM, matrix = "gamma") # gamma


# The topicQuality() function plots these values 
# and labels each with its topic number:
topicQuality(model = Third_STM, documents = news_processed_out$documents)


# the topic distribution per document
td_theta <- tidy(Third_STM, matrix = "theta")

selectiontheta <- td_theta[td_theta$document %in% c(1:15),]

selectiontheta %>% 
  ggplot(aes(y = gamma, x = as.factor(topic), fill = as.factor(topic))) +
  geom_bar(stat = "identity",alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ document, ncol = 3) +
  labs(title = "Theta values per document",
       y = expression(theta), x = "Topic")

# analytical look at the word frequencies per topic
tidy(Third_STM) %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics")

})
