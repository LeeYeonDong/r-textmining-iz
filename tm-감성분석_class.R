# 감성분석(Emotion Classification)
library(remotes)
install_github("EmilHvitfeldt/textdata")

get_sentiments("nrc") %>% view()
get_sentiments("afinn") %>% view()
get_sentiments("loughran") %>% view()

df_ytb_com$댓글_ytb

install.packages("syuzhet")
library(syuzhet)
ytb_nrc <- df_ytb_com$댓글_ytb %>% get_nrc_sentiment()

ytb_nrc_df <- ytb_nrc %>% t() %>% tibble() %>% rowSums() %>% tibble()
names(ytb_nrc_df) <- c("Freq")
ytb_nrc_df$sentiment <- names(ytb_nrc)

ggplot(ytb_nrc_df, aes(x = sentiment, y = Freq, fill = sentiment)) + 
  geom_col() + 
  theme(legend.position = "none") +
  geom_text(aes(label = Freq), hjust = 0.5, size = 5) 
