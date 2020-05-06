
install_mecab("C:/Rlibs/mecab")

devtools::install_github('haven-jeon/NIADic/NIADic', build_vignettes = TRUE)
library(NIADic)

Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_221')
useNIADic()

##mergeUserDic(data.frame(c("단어"),"품사")) <- 새단어(품사) 추가

sheet1 <- readLines("D:/2040_opinion_1_sheet1.txt") 
word.pos <- readLines("D:/textmining/imotion/wordpos.txt") 
word.neg <- readLines("D:/textmining/imotion/wordneg.txt")


sheet1 <- SimplePos09(sheet1)

sheet1 <- sheet1 %>% melt()
sheet1 <- sheet1 %>% as_tibble()
sheet1_df <- sheet1 %>% select(3,1)


sheet1_df_n <- sheet1_df %>% mutate(명사=str_match(value,'([가-힣]+)/N')[,2]) %>% na.omit()
sheet1_df_p <- sheet1_df %>% mutate(용언=str_match(value,'([가-힣]+)/P')[,2]) %>% na.omit()
sheet1_df_m <- sheet1_df %>% mutate(수식언=str_match(value,'([가-힣]+)/M')[,2]) %>% na.omit()

word.n <- sheet1_df_n[,3]
word.n <- word.n$명사
word.n <- word.n %>% unlist()
word.n <- word.n %>% as.vector()
word.n <- gsub("\\^","",word.n)
word.n <- gsub("^.{1}$","",word.n)
word.n <- gsub('\\d+',"", word.n)
word.n <- word.n %>% as.list()
word.n[word.n ==""] <- NULL
word.n <- word.n %>% unlist()
word.n <- word.n %>% as.data.frame()

word.p <- sheet1_df_p[,3]
word.p <- word.p$용언
word.p <- word.p %>% unlist()
word.p <- word.p %>% as.vector()
word.p <- gsub("\\^","",word.p)
word.p <- gsub("^.{1}$","",word.p)
word.p <- gsub('\\d+',"", word.p)
word.p <- word.p %>% as.list()
word.p[word.p ==""] <- NULL
word.p <- word.p %>% unlist()
word.p <- word.p %>% as.data.frame()

word.m <- sheet1_df_m[,3]
word.m <- word.m$수식언
word.m <- word.m %>% unlist()
word.m <- word.m %>% as.vector()
word.m <- gsub("\\^","",word.m)
word.m <- gsub("^.{1}$","",word.m)
word.m <- gsub('\\d+',"", word.m)
word.m <- word.m %>% as.list()
word.m[word.m ==""] <- NULL
word.m <- word.m %>% unlist()
word.m <- word.m %>% as.data.frame()

word <- rbind(word.n, word.p, word.m)


word <- word %>% unlist()
word <- word %>% as.vector()




scores <- laply(word, function(word, word.pos, word.neg) {
  
  words <- unlist(word)  
  
  pos.matches <- match(words, word.pos)           # words의 단어를 positive에서 matching
  neg.matches <- match(words, word.neg)
  
  pos.matches <- !is.na(pos.matches)            # NA 제거, 위치(숫자)만 추출
  neg.matches <- !is.na(neg.matches)
  
  score <-  sum(pos.matches) - sum(neg.matches)  # 긍정 - 부정   
}, word.pos, word.neg)

scores.df <-  data.frame(score=scores, text=word)



scores.df$color[scores.df$score >=1] = "긍정"
scores.df$color[scores.df$score ==0] = "중립"
scores.df$color[scores.df$score < 0] = "부정"

scores.df.t <- table(scores.df$color)
scores.df.t <- scores.df.t %>% as.data.frame()

scores.df.t_g <- ggplot(scores.df.t, aes(x="", y=Freq, fill=Var1)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) +
  theme(legend.position="right") +
  scale_fill_manual(values=c("#567ace", "#d9598c", "#fcf695")) +
  theme(legend.title=element_blank()) +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())

scores.df_g <- ggplot(scores.df, aes(x=color, fill=color)) + 
  geom_bar() +
  coord_polar() +
  theme(legend.position="right") +
  scale_fill_manual(values=c("#567ace", "#d9598c", "#fcf695")) +
  theme(legend.title=element_blank()) +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())


