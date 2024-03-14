# n-gram : 연이어 사용된 n개의 단어
# install.packages("igraph")
library(igraph)

set.seed(1201)

arr <- grid::arrow(type = "closed", length = unit(.20, "inches"))

raw3_token_df %>% 
  group_by(id) %>% 
  summarise(sentence = paste(단어, collapse = " ")) %>% 
  unnest_tokens(input = sentence,
                output = trigram,
                token = "ngrams",
                n = 3) %>% 
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%    
  count(word1, word2, word3, sort = TRUE) %>% 
  na.omit() %>% 
  filter(n >= 500) %>% 
  graph_from_data_frame() %>% 
  ggraph(layout = "fr") +
  geom_edge_link(color = "black", alpha = 1, arrow = arr) +
  geom_node_point(color = "#D55E00", alpha = 1, size = 5) +
  geom_node_text(aes(label = name), repel = TRUE , size = 5) +
  theme_graph()
