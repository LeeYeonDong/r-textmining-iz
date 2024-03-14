# KoNLP useNIADic
install.packages('KoNLP', repos = 'https://forkonlp.r-universe.dev')

# install.packages("multilinguer")
# library(multilinguer)
# install_jdk()
# install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), type = "binary")
# #
# install.packages("remotes")
# remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))
library(KoNLP) #최종적으로 "KoNLP" 패키지를 불러옵니다

devtools::install_github('haven-jeon/NIADic/NIADic', build_vignettes = TRUE)
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_281')  # 설치한 JAVA version에 따라 달라집니다
buildDictionary(ext_dic = "woorimalsam")  # "woorimalsam" dic을 불러옵니다
useNIADic() 

# package
library(tidyverse)
library(ggplot2)
library(tm)
library(NLP)
library(qdap)
library(wordcloud2)
library(stringr)
library(rJava)
library(base64enc)
library(RmecabKo)
library(reshape2)
library(tidytext)


# 엔씨
엔씨df <- read_csv(file = "D:/대학원/논문/소논문/뉴스_sports.news_NC.csv", col_names = TRUE, locale=locale('ko',encoding='euc-kr'))

names(엔씨df) <- c("언론사","제목","날짜","링크","좋아","훈훈","슬퍼","화나","후속")

엔씨df$날짜 <- 엔씨df$날짜 %>% as.character()

엔씨df <- 엔씨df %>% arrange(-desc(날짜))
엔씨df <- 엔씨df %>% 
  filter(제목 != "NA") %>% 
  filter(제목 != "수동확인") 

엔씨df$구분 <- c("엔씨")
엔씨df$id <- c(1:length(엔씨df$제목))

# 롯데
롯데df <- read_csv(file = "D:/대학원/논문/소논문/뉴스_sports.news_롯데.csv", col_names = TRUE, locale=locale('ko',encoding='euc-kr'))

names(롯데df) <- c("언론사","제목","날짜","링크","좋아","훈훈","슬퍼","화나","후속")

롯데df$날짜 <- 롯데df$날짜 %>% as.character()

롯데df <- 롯데df %>% arrange(-desc(날짜))
롯데df <- 롯데df %>% 
  filter(제목 != "NA")
롯데df$구분 <- c("롯데")
롯데df$id <- c(1:length(롯데df$제목))

# 엔씨 + 롯데
엔롯df <- bind_rows(엔씨df, 롯데df)

엔롯df$제목 <- gsub("포토","",엔롯df$제목)
엔롯df$제목 <- gsub("사진","",엔롯df$제목)

# 월별 추이
# 엔씨
엔씨df_월별 <- 엔롯df %>% filter(구분 == "엔씨")
엔씨df_월별$날짜 <- str_sub(엔씨df_월별$날짜,1,7)

엔씨df_월별_table <- 엔씨df_월별$날짜 %>% table() %>% as_tibble()

names(엔씨df_월별_table) <- c("날짜_월별","Freq")

ggplot(data = 엔씨df_월별_table, aes(x = 날짜_월별, y = Freq, group = 1)) + 
  geom_line(size = 2, colour="#006600") + 
  geom_point(size = 1, colour="#006600") +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  geom_text(aes(label = Freq),hjust = 1,size=5) +
  geom_hline(yintercept = mean(엔씨df_월별_table$Freq), color='red',linetype='dashed', size = 1) +
  labs(x="", y="")


# 롯데
롯데df_월별 <- 엔롯df %>% filter(구분 == "롯데")
롯데df_월별$날짜 <- str_sub(롯데df_월별$날짜,1,7)

롯데df_월별_table <- 롯데df_월별$날짜 %>% table() %>% as_tibble()

names(롯데df_월별_table) <- c("날짜_월별","Freq")

ggplot(data = 롯데df_월별_table, aes(x = 날짜_월별, y = Freq, group = 1)) + 
  geom_line(size = 2, colour="#006600") + 
  geom_point(size = 1, colour="#006600") +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  geom_text(aes(label = Freq),hjust = 1,size=5) +
  geom_hline(yintercept = mean(롯데df_월별_table$Freq), color='red',linetype='dashed', size = 1) +
  labs(x="", y="")


# 일별 추이
# 엔씨
엔씨df_일별 <- 엔롯df %>% filter(구분 == "엔씨")
엔씨df_일별$날짜 <- str_sub(엔씨df_일별$날짜,1,11)

엔씨df_일별_table <- 엔씨df_일별$날짜 %>% table() %>% as_tibble()

names(엔씨df_일별_table) <- c("날짜_일별","Freq")

ggplot(data = 엔씨df_일별_table, aes(x = 날짜_일별, y = Freq, group = 1)) + 
  geom_line(size = 2, colour="#006600") + 
  geom_point(size = 1, colour="#006600") +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  geom_text(aes(label = Freq),hjust = 1,size=5) +
  geom_hline(yintercept = mean(엔씨df_일별_table$Freq), color='red',linetype='dashed', size = 1) +
  labs(x="", y="")

# 롯데
롯데df_일별 <- 엔롯df %>% filter(구분 == "롯데")
롯데df_일별$날짜 <- str_sub(롯데df_일별$날짜,1,11)

롯데df_일별_table <- 롯데df_일별$날짜 %>% table() %>% as_tibble()

names(롯데df_일별_table) <- c("날짜_일별","Freq")

ggplot(data = 롯데df_일별_table, aes(x = 날짜_일별, y = Freq, group = 1)) + 
  geom_line(size = 2, colour="#006600") + 
  geom_point(size = 1, colour="#006600") +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  geom_text(aes(label = Freq),hjust = 1,size=5) +
  geom_hline(yintercept = mean(롯데df_일별_table$Freq), color='red',linetype='dashed', size = 1) +
  labs(x="", y="")



#### text mining
# 데이터 분할
엔롯df2 <- 엔롯df %>% 
  select("id", "제목", "날짜", "구분")

엔롯df2 %>% dim() # 114936 4

엔롯_int <- nrow(엔롯df2) / 10000
엔롯_int <- 엔롯_int %>% ceiling()
n <- 엔롯_int

엔롯df_sp <- split(엔롯df2,rep(1:n,each=10000))

# 데이터 셋 만들기
엔롯_tb <- list()
엔롯_data_set <- list()
엔롯_한글 <- list()
엔롯_영어 <- list()
엔롯_한영 <- list()

## 단어기준 토큰화
for (i in 1:n){
  
  cat(i, '번째 데이터 리스트 tokenizer', '중 입니다.\n') 
  
  엔롯_tb[[i]] <- 엔롯df_sp[[i]] %>% 
    tibble() %>% 
    unnest_tokens(input = 제목, output = word, token = "words", drop = FALSE)
  
  names(엔롯_tb[[i]]) <- c("id","제목","날짜","구분","word")
  
  엔롯_한글[[i]] <- 엔롯_tb[[i]] %>%  
    mutate(한글 = str_match(word,'([가-힣]+)')[,2]) %>% ## "한글" variable을 만들고 한글만 저장
    na.omit() %>% ## ([가-힣]+)/P') 한글만을 선택하는 정규표현식
    mutate(글자수 = str_length(한글)) %>%   ## "글자수" variable을 만듭니다 
    filter(str_length(한글)>=2) 
  
  엔롯_영어[[i]] <- 엔롯_tb[[i]] %>%  
    mutate(영어 = str_match(word,'([a-zA-Z]+)')[,2]) %>% ## "한글" variable을 만들고 한글만 저장     
    na.omit() %>% ## 
    mutate(글자수 = str_length(영어)) %>%   ## "글자수" variable을 만듭니다 
    filter(str_length(영어)>=2) 
  
  엔롯_한영[[i]] <- bind_rows(엔롯_한글[[i]],엔롯_영어[[i]])
  
}

엔롯_token <- list()

for (i in 1:n){
  엔롯_token <- bind_rows(엔롯_token,엔롯_한영[[i]])
}

# 05월 - 엔씨
엔롯_token$월별 <- str_sub(엔롯_token$날짜,6,7)

엔씨_token_05 <- 엔롯_token %>% 
  filter(구분 == "엔씨") %>% 
  # filter(월별 == "05")

엔씨_token_word_05 <- 엔씨_token_05$word
엔씨_token_count_05 <- table(엔씨_token_word_05) ## 객체별 빈도를 셉니다
엔씨_token_count_05 <- sort(엔씨_token_count_05, decreasing = TRUE) ##내림차순 정렬 합니다
엔씨_token_count50_05 <- 엔씨_token_count_05[1:50]  

## 빈도그래프 작성
엔씨_token_count50df_05 <- 엔씨_token_count50_05 %>% as_tibble() ## tibble변환하고 그래프 작성  
names(엔씨_token_count50df_05) <- c("엔씨_token","Freq")

ggplot(엔씨_token_count50df_05, aes(x=Freq, y=reorder(엔씨_token,Freq), fill = Freq)) + 
  geom_col() + 
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  scale_fill_gradient(low = "#5D5D5D", high = "#003300") +
  geom_text(aes(label = Freq),hjust = -0.1,size=5) 

## 워드클라우드 작성
엔씨_token_count50df_05 %>% wordcloud2(minRotation=0, maxRotation=0, color = "#003300")


# 05월 - 롯데
엔롯_token$월별 <- str_sub(엔롯_token$날짜,6,7)

롯데_token_05 <- 엔롯_token %>% 
  filter(구분 == "롯데") %>% 
  # filter(월별 == "05")

롯데_token_word_05 <- 롯데_token_05$word
롯데_token_count_05 <- table(롯데_token_word_05) ## 객체별 빈도를 셉니다
롯데_token_count_05 <- sort(롯데_token_count_05, decreasing = TRUE) ##내림차순 정렬 합니다
롯데_token_count50_05 <- 롯데_token_count_05[1:50]  

## 빈도그래프 작성
롯데_token_count50df_05 <- 롯데_token_count50_05 %>% as_tibble() ## tibble변환하고 그래프 작성  
names(롯데_token_count50df_05) <- c("롯데_token","Freq")

ggplot(롯데_token_count50df_05, aes(x=Freq, y=reorder(롯데_token,Freq), fill = Freq)) + 
  geom_col() + 
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  scale_fill_gradient(low = "#5D5D5D", high = "#003300") +
  geom_text(aes(label = Freq),hjust = -0.1,size=5) 

## 워드클라우드 작성
롯데_token_count50df_05 %>% wordcloud2(minRotation=0, maxRotation=0, color = "#003300")


# 08월 - 엔씨
엔롯_token$월별 <- str_sub(엔롯_token$날짜,6,7)

엔씨_token_08 <- 엔롯_token %>% 
  filter(구분 == "엔씨") %>% 
  filter(월별 == "08")

엔씨_token_word_08 <- 엔씨_token_08$word
엔씨_token_count_08 <- table(엔씨_token_word_08) ## 객체별 빈도를 셉니다
엔씨_token_count_08 <- sort(엔씨_token_count_08, decreasing = TRUE) ##내림차순 정렬 합니다
엔씨_token_count50_08 <- 엔씨_token_count_08[1:50]  

## 빈도그래프 작성
엔씨_token_count50df_08 <- 엔씨_token_count50_08 %>% as_tibble() ## tibble변환하고 그래프 작성  
names(엔씨_token_count50df_08) <- c("엔씨_token","Freq")

ggplot(엔씨_token_count50df_08, aes(x=Freq, y=reorder(엔씨_token,Freq), fill = Freq)) + 
  geom_col() + 
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  scale_fill_gradient(low = "#5D5D5D", high = "#003300") +
  geom_text(aes(label = Freq),hjust = -0.1,size=5) 

## 워드클라우드 작성
엔씨_token_count50df_08 %>% wordcloud2(minRotation=0, maxRotation=0, color = "#003300")


# 08월 - 롯데
엔롯_token$월별 <- str_sub(엔롯_token$날짜,6,7)

롯데_token_08 <- 엔롯_token %>% 
  filter(구분 == "롯데") %>% 
  filter(월별 == "08")

롯데_token_word_08 <- 롯데_token_08$word
롯데_token_count_08 <- table(롯데_token_word_08) ## 객체별 빈도를 셉니다
롯데_token_count_08 <- sort(롯데_token_count_08, decreasing = TRUE) ##내림차순 정렬 합니다
롯데_token_count50_08 <- 롯데_token_count_08[1:50]  

## 빈도그래프 작성
롯데_token_count50df_08 <- 롯데_token_count50_08 %>% as_tibble() ## tibble변환하고 그래프 작성  
names(롯데_token_count50df_08) <- c("롯데_token","Freq")

ggplot(롯데_token_count50df_08, aes(x=Freq, y=reorder(롯데_token,Freq), fill = Freq)) + 
  geom_col() + 
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  scale_fill_gradient(low = "#5D5D5D", high = "#003300") +
  geom_text(aes(label = Freq),hjust = -0.1,size=5) 

## 워드클라우드 작성
롯데_token_count50df_08 %>% wordcloud2(minRotation=0, maxRotation=0, color = "#003300")


# 11월 - 엔씨
엔롯_token$월별 <- str_sub(엔롯_token$날짜,6,7)

엔씨_token_11 <- 엔롯_token %>% 
  filter(구분 == "엔씨") %>% 
  filter(월별 == "11")

엔씨_token_word_11 <- 엔씨_token_11$word
엔씨_token_count_11 <- table(엔씨_token_word_11) ## 객체별 빈도를 셉니다
엔씨_token_count_11 <- sort(엔씨_token_count_11, decreasing = TRUE) ##내림차순 정렬 합니다
엔씨_token_count50_11 <- 엔씨_token_count_11[1:50]  

## 빈도그래프 작성
엔씨_token_count50df_11 <- 엔씨_token_count50_11 %>% as_tibble() ## tibble변환하고 그래프 작성  
names(엔씨_token_count50df_11) <- c("엔씨_token","Freq")

ggplot(엔씨_token_count50df_11, aes(x=Freq, y=reorder(엔씨_token,Freq), fill = Freq)) + 
  geom_col() + 
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  scale_fill_gradient(low = "#5D5D5D", high = "#003300") +
  geom_text(aes(label = Freq),hjust = -0.1,size=5) 

## 워드클라우드 작성
엔씨_token_count50df_11 %>% wordcloud2(minRotation=0, maxRotation=0, color = "#003300")


# 11월 - 롯데
엔롯_token$월별 <- str_sub(엔롯_token$날짜,6,7)

롯데_token_11 <- 엔롯_token %>% 
  filter(구분 == "롯데") %>% 
  filter(월별 == "11")

롯데_token_word_11 <- 롯데_token_11$word
롯데_token_count_11 <- table(롯데_token_word_11) ## 객체별 빈도를 셉니다
롯데_token_count_11 <- sort(롯데_token_count_11, decreasing = TRUE) ##내림차순 정렬 합니다
롯데_token_count50_11 <- 롯데_token_count_11[1:50]  

## 빈도그래프 작성
롯데_token_count50df_11 <- 롯데_token_count50_11 %>% as_tibble() ## tibble변환하고 그래프 작성  
names(롯데_token_count50df_11) <- c("롯데_token","Freq")

ggplot(롯데_token_count50df_11, aes(x=Freq, y=reorder(롯데_token,Freq), fill = Freq)) + 
  geom_col() + 
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  scale_fill_gradient(low = "#5D5D5D", high = "#003300") +
  geom_text(aes(label = Freq),hjust = -0.1,size=5) 

## 워드클라우드 작성
롯데_token_count50df_11 %>% wordcloud2(minRotation=0, maxRotation=0, color = "#003300")


#### 감성분석
##감성사전
sent_dic <- read_csv("D:/대학원/논문/소논문/스포츠기사/SentiWord_Dict.txt")
names(sent_dic) <- c("value")
sent_dic <- sent_dic$value
sent_dic <- strsplit(sent_dic, split="\t")  

word <- c()
polarity <- c()

for (i in 1:length(sent_dic)){ 
  word <- append(word,sent_dic[[i]][1])
  polarity <- append(polarity,sent_dic[[i]][2])
}

sent_dic <- tibble(word,polarity)


# 전체
## 단어에 감정 점수 부여
엔롯_tb_pnn <- 엔롯_token %>% 
  left_join(sent_dic,by="word") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity)) %>% 
  mutate(sentiment = ifelse(polarity > 0, "pos", ifelse(polarity < 0, "neg", "neu"))) 

엔롯_tb_pnn$월별 <- str_sub(엔롯_tb_pnn$날짜,1,7)

엔롯_tb_pnn$polarity <- 엔롯_tb_pnn$polarity %>% as.integer()

엔롯_tb_pnn_score <- 엔롯_tb_pnn %>% 
  count(sentiment) %>% 
  mutate(ratio = round((n/sum(n)*100),2))

# 연도별 neg vs pos 추이
# 엔씨
엔롯_tb_pnn %>% 
  filter(구분 == "엔씨") %>% 
  group_by(월별) %>% 
  count(sentiment) %>% 
  mutate(ratio = round((n/sum(n)*100),2)) %>% 
  filter(sentiment != "neu") %>% 
  ggplot(aes(x = 월별, y = ratio, color = sentiment, group = sentiment)) +
  geom_line() +
  geom_text(aes(label = ratio),vjust = 0.5,size=7) + theme(legend.position = "bottom")

# 롯데
엔롯_tb_pnn %>% 
  filter(구분 == "롯데") %>% 
  group_by(월별) %>% 
  count(sentiment) %>% 
  mutate(ratio = round((n/sum(n)*100),2)) %>% 
  filter(sentiment != "neu") %>% 
  ggplot(aes(x = 월별, y = ratio, color = sentiment, group = sentiment)) +
  geom_line() +
  geom_text(aes(label = ratio),vjust = 0.5,size=7) + theme(legend.position = "bottom")


# 05월
# 엔씨
엔씨_tb_pnn_05 <- 엔롯_tb_pnn %>% 
  filter(구분 == "엔씨") %>% 
  # filter(월별 == "2020.05")

# 빈출 감정단어 파악 50개
엔씨_tb_pnn_top_05 <- 엔씨_tb_pnn_05 %>% 
  filter(sentiment != "neu") %>% 
  count(sentiment, word) %>% 
  group_by(sentiment) %>% 
  slice_max(n, n=50)

ggplot(엔씨_tb_pnn_top_05, aes(x=reorder(word,n), y=n, fill=sentiment)) +
  geom_col() +
  labs(x="", y="") + 
  coord_flip() +
  geom_text(aes(label = n),hjust = -0.3) +
  facet_wrap(~ sentiment, scales =  "free") +
  scale_y_continuous(expand = c(0,0), limit=c(0,1200)) + theme(legend.position = "bottom")

## 긍정 부정 기사에 어떤 단어가 자주 사용?
엔씨_tb_pnn_freq_05 <- 엔씨_tb_pnn_05 %>% 
  count(sentiment, word, sort = TRUE)

## 로그 오즈비 구하기 - 긍정 및 부정 기사에서 상대적으로 자주 사용된 단어 파악
엔씨_tb_pnn_odd_wide_05 <- 엔씨_tb_pnn_freq_05 %>% 
  filter(sentiment != "neu") %>% 
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = list(n=0))

엔씨_tb_pnn_odd_wide_05 <- 엔씨_tb_pnn_odd_wide_05 %>% 
  mutate(log_odds_ratio = log(((pos+1) / (sum(pos +1))) / ((neg+1) / (sum(neg+1)))))

# 로그 오즈비가 가장 큰 단어 10개 추출
엔씨_tb_pnn_odd_top_05 <- 엔씨_tb_pnn_odd_wide_05 %>% 
  group_by(sentiment= ifelse(log_odds_ratio > 0, "pos", "neg")) %>% 
  slice_max(abs(log_odds_ratio), n=10, with_ties = FALSE)

엔씨_tb_pnn_odd_top_05 %>% 
  ggplot(aes(x = reorder(word, log_odds_ratio),y = log_odds_ratio,fill = sentiment))+
  geom_col() +
  labs(x="", y="") + 
  coord_flip() +
  geom_text(aes(label = round(log_odds_ratio,2)),hjust = -0.1,size=6) 


# 05월
# 롯데
롯데_tb_pnn_05 <- 엔롯_tb_pnn %>% 
  filter(구분 == "롯데") %>% 
  # filter(월별 == "2020.05")

# 빈출 감정단어 파악 50개
롯데_tb_pnn_top_05 <- 롯데_tb_pnn_05 %>% 
  filter(sentiment != "neu") %>% 
  count(sentiment, word) %>% 
  group_by(sentiment) %>% 
  slice_max(n, n=50)

ggplot(롯데_tb_pnn_top_05, aes(x=reorder(word,n), y=n, fill=sentiment)) +
  geom_col() +
  labs(x="", y="") + 
  coord_flip() +
  geom_text(aes(label = n),hjust = -0.3) +
  facet_wrap(~ sentiment, scales =  "free") +
  scale_y_continuous(expand = c(0,0), limit=c(0,1200)) + theme(legend.position = "bottom")

## 긍정 부정 기사에 어떤 단어가 자주 사용?
롯데_tb_pnn_freq_05 <- 롯데_tb_pnn_05 %>% 
  count(sentiment, word, sort = TRUE)

## 로그 오즈비 구하기 - 긍정 및 부정 기사에서 상대적으로 자주 사용된 단어 파악
롯데_tb_pnn_odd_wide_05 <- 롯데_tb_pnn_freq_05 %>% 
  filter(sentiment != "neu") %>% 
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = list(n=0))

롯데_tb_pnn_odd_wide_05 <- 롯데_tb_pnn_odd_wide_05 %>% 
  mutate(log_odds_ratio = log(((pos+1) / (sum(pos +1))) / ((neg+1) / (sum(neg+1)))))

# 로그 오즈비가 가장 큰 단어 10개 추출
롯데_tb_pnn_odd_top_05 <- 롯데_tb_pnn_odd_wide_05 %>% 
  group_by(sentiment= ifelse(log_odds_ratio > 0, "pos", "neg")) %>% 
  slice_max(abs(log_odds_ratio), n=10, with_ties = FALSE)

롯데_tb_pnn_odd_top_05 %>% 
  ggplot(aes(x = reorder(word, log_odds_ratio),y = log_odds_ratio,fill = sentiment))+
  geom_col() +
  labs(x="", y="") + 
  coord_flip() +
  geom_text(aes(label = round(log_odds_ratio,2)),hjust = -0.1,size=6) 


# 08월
# 엔씨
엔씨_tb_pnn_08 <- 엔롯_tb_pnn %>% 
  filter(구분 == "엔씨") %>% 
  filter(월별 == "2020.08")

# 빈출 감정단어 파악 50개
엔씨_tb_pnn_top_08 <- 엔씨_tb_pnn_08 %>% 
  filter(sentiment != "neu") %>% 
  count(sentiment, word) %>% 
  group_by(sentiment) %>% 
  slice_max(n, n=50)

ggplot(엔씨_tb_pnn_top_08, aes(x=reorder(word,n), y=n, fill=sentiment)) +
  geom_col() +
  labs(x="", y="") + 
  coord_flip() +
  geom_text(aes(label = n),hjust = -0.3) +
  facet_wrap(~ sentiment, scales =  "free") +
  scale_y_continuous(expand = c(0,0), limit=c(0,150)) + theme(legend.position = "bottom")

## 긍정 부정 기사에 어떤 단어가 자주 사용?
엔씨_tb_pnn_freq_08 <- 엔씨_tb_pnn_08 %>% 
  count(sentiment, word, sort = TRUE)

## 로그 오즈비 구하기 - 긍정 및 부정 기사에서 상대적으로 자주 사용된 단어 파악
엔씨_tb_pnn_odd_wide_08 <- 엔씨_tb_pnn_freq_08 %>% 
  filter(sentiment != "neu") %>% 
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = list(n=0))

엔씨_tb_pnn_odd_wide_08 <- 엔씨_tb_pnn_odd_wide_08 %>% 
  mutate(log_odds_ratio = log(((pos+1) / (sum(pos +1))) / ((neg+1) / (sum(neg+1)))))

# 로그 오즈비가 가장 큰 단어 10개 추출
엔씨_tb_pnn_odd_top_08 <- 엔씨_tb_pnn_odd_wide_08 %>% 
  group_by(sentiment= ifelse(log_odds_ratio > 0, "pos", "neg")) %>% 
  slice_max(abs(log_odds_ratio), n=10, with_ties = FALSE)

엔씨_tb_pnn_odd_top_08 %>% 
  ggplot(aes(x = reorder(word, log_odds_ratio),y = log_odds_ratio,fill = sentiment))+
  geom_col() +
  labs(x="", y="") + 
  coord_flip() +
  geom_text(aes(label = round(log_odds_ratio,2)),hjust = -0.1,size=6) 

# 롯데
롯데_tb_pnn_08 <- 엔롯_tb_pnn %>% 
  filter(구분 == "롯데") %>% 
  filter(월별 == "2020.0508")

# 빈출 감정단어 파악 50개
롯데_tb_pnn_top_08 <- 롯데_tb_pnn_08 %>% 
  filter(sentiment != "neu") %>% 
  count(sentiment, word) %>% 
  group_by(sentiment) %>% 
  slice_max(n, n=50)

ggplot(롯데_tb_pnn_top_08, aes(x=reorder(word,n), y=n, fill=sentiment)) +
  geom_col() +
  labs(x="", y="") + 
  coord_flip() +
  geom_text(aes(label = n),hjust = -0.3) +
  facet_wrap(~ sentiment, scales =  "free") +
  scale_y_continuous(expand = c(0,0), limit=c(0,150)) + theme(legend.position = "bottom")

## 긍정 부정 기사에 어떤 단어가 자주 사용?
롯데_tb_pnn_freq_08 <- 롯데_tb_pnn_08 %>% 
  count(sentiment, word, sort = TRUE)

## 로그 오즈비 구하기 - 긍정 및 부정 기사에서 상대적으로 자주 사용된 단어 파악
롯데_tb_pnn_odd_wide_08 <- 롯데_tb_pnn_freq_08 %>% 
  filter(sentiment != "neu") %>% 
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = list(n=0))

롯데_tb_pnn_odd_wide_08 <- 롯데_tb_pnn_odd_wide_08 %>% 
  mutate(log_odds_ratio = log(((pos+1) / (sum(pos +1))) / ((neg+1) / (sum(neg+1)))))

# 로그 오즈비가 가장 큰 단어 10개 추출
롯데_tb_pnn_odd_top_08 <- 롯데_tb_pnn_odd_wide_08 %>% 
  group_by(sentiment= ifelse(log_odds_ratio > 0, "pos", "neg")) %>% 
  slice_max(abs(log_odds_ratio), n=10, with_ties = FALSE)

롯데_tb_pnn_odd_top_08 %>% 
  ggplot(aes(x = reorder(word, log_odds_ratio),y = log_odds_ratio,fill = sentiment))+
  geom_col() +
  labs(x="", y="") + 
  coord_flip() +
  geom_text(aes(label = round(log_odds_ratio,2)),hjust = -0.1,size=6) 


# 11월
# 롯데
롯데_tb_pnn_11 <- 엔롯_tb_pnn %>% 
  filter(구분 == "롯데") %>% 
  filter(월별 == "11")

# 빈출 감정단어 파악 50개
롯데_tb_pnn_top_11 <- 롯데_tb_pnn_11 %>% 
  filter(sentiment != "neu") %>% 
  count(sentiment, word) %>% 
  group_by(sentiment) %>% 
  slice_max(n, n=50)

ggplot(롯데_tb_pnn_top_11, aes(x=reorder(word,n), y=n, fill=sentiment)) +
  geom_col() +
  labs(x="", y="") + 
  coord_flip() +
  geom_text(aes(label = n),hjust = -0.3) +
  facet_wrap(~ sentiment, scales =  "free") +
  scale_y_continuous(expand = c(0,0), limit=c(0,150)) + theme(legend.position = "bottom")

## 긍정 부정 기사에 어떤 단어가 자주 사용?
롯데_tb_pnn_freq_11 <- 롯데_tb_pnn_11 %>% 
  count(sentiment, word, sort = TRUE)

## 로그 오즈비 구하기 - 긍정 및 부정 기사에서 상대적으로 자주 사용된 단어 파악
롯데_tb_pnn_odd_wide_11 <- 롯데_tb_pnn_freq_11 %>% 
  filter(sentiment != "neu") %>% 
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = list(n=0))

롯데_tb_pnn_odd_wide_11 <- 롯데_tb_pnn_odd_wide_11 %>% 
  mutate(log_odds_ratio = log(((pos+1) / (sum(pos +1))) / ((neg+1) / (sum(neg+1)))))

# 로그 오즈비가 가장 큰 단어 10개 추출
롯데_tb_pnn_odd_top_11 <- 롯데_tb_pnn_odd_wide_11 %>% 
  group_by(sentiment= ifelse(log_odds_ratio > 0, "pos", "neg")) %>% 
  slice_max(abs(log_odds_ratio), n=10, with_ties = FALSE)

롯데_tb_pnn_odd_top_11 %>% 
  ggplot(aes(x = reorder(word, log_odds_ratio),y = log_odds_ratio,fill = sentiment))+
  geom_col() +
  labs(x="", y="") + 
  coord_flip() +
  geom_text(aes(label = round(log_odds_ratio,2)),hjust = -0.1,size=6) 

# 승리 단어 분석하기


# 단어간 상관분석 - 다른 단어에 비해 상대적으로 자주 함께 사용된 단어 파악
library(widyr)
library(tidygraph)
library(ggraph)

# 05월
# 엔씨
엔씨_cor_05 <- 엔롯_tb_pnn %>% 
  filter(구분 == "엔씨") %>% 
  select(id, word, 제목, 월별) %>% 
  # filter(월별 == "2020.05") %>%
  add_count(word) %>% 
  filter(n >= 20) %>% 
  pairwise_cor(item = word,
               feature = id,
               sort = TRUE)

엔씨_cor_05_graph <- 엔씨_cor_05 %>% 
  filter(correlation >= 0.5) %>% 
  as_tbl_graph(directed = FALSE) %>% 
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))

set.seed(1029)

ggraph(엔씨_cor_05_graph, layout = "fr") +
  
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


# 롯데
롯데_cor_05 <- 엔롯_tb_pnn %>% 
  filter(구분 == "롯데") %>% 
  select(id, word, 제목, 월별) %>% 
  # filter(월별 == "2020.05") %>%
  add_count(word) %>% 
  filter(n >= 20) %>% 
  pairwise_cor(item = word,
               feature = id,
               sort = TRUE)

롯데_cor_05_graph <- 롯데_cor_05 %>% 
  filter(correlation >= 0.5) %>% 
  as_tbl_graph(directed = FALSE) %>% 
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))

set.seed(1029)

ggraph(롯데_cor_05_graph, layout = "fr") +
  
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

# 08월
# 엔씨
엔씨_cor_08 <- 엔롯_tb_pnn %>% 
  filter(구분 == "엔씨") %>% 
  select(id, word, 제목, 월별) %>% 
  filter(월별 == "08") %>% 
  add_count(word) %>% 
  filter(n >= 5) %>% 
  pairwise_cor(item = word,
               feature = id,
               sort = TRUE)

엔씨_cor_08_graph <- 엔씨_cor_08 %>% 
  filter(correlation >= 0.5) %>% 
  as_tbl_graph(directed = FALSE) %>% 
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))

set.seed(1029)

ggraph(엔씨_cor_08_graph, layout = "fr") +
  
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

# 롯데
롯데_cor_08 <- 엔롯_tb_pnn %>% 
  filter(구분 == "롯데") %>% 
  select(id, word, 제목, 월별) %>% 
  filter(월별 == "08") %>% 
  add_count(word) %>% 
  filter(n >= 5) %>% 
  pairwise_cor(item = word,
               feature = id,
               sort = TRUE)

롯데_cor_08_graph <- 롯데_cor_08 %>% 
  filter(correlation >= 0.5) %>% 
  as_tbl_graph(directed = FALSE) %>% 
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))

set.seed(1029)

ggraph(롯데_cor_08_graph, layout = "fr") +
  
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



# 롯데
롯데_cor_05 <- 엔롯_tb_pnn %>% 
  filter(구분 == "롯데") %>% 
  select(id, word, 제목, 월별) %>% 
  filter(월별 == "05") %>% 
  add_count(word) %>% 
  filter(n >= 5) %>% 
  pairwise_cor(item = word,
               feature = id,
               sort = TRUE)

롯데_cor_05_graph <- 롯데_cor_05 %>% 
  filter(correlation >= 0.5) %>% 
  as_tbl_graph(directed = FALSE) %>% 
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))

set.seed(1029)

ggraph(롯데_cor_05_graph, layout = "fr") +
  
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


# 11월
# 엔씨
엔씨_cor_11 <- 엔롯_tb_pnn %>% 
  filter(구분 == "엔씨") %>% 
  select(id, word, 제목, 월별) %>% 
  filter(월별 == "11") %>% 
  add_count(word) %>% 
  filter(n >= 5) %>% 
  pairwise_cor(item = word,
               feature = id,
               sort = TRUE)

엔씨_cor_11_graph <- 엔씨_cor_11 %>% 
  filter(correlation >= 0.5) %>% 
  as_tbl_graph(directed = FALSE) %>% 
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))

set.seed(1029)

ggraph(엔씨_cor_11_graph, layout = "fr") +
  
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

# 롯데
롯데_cor_11 <- 엔롯_tb_pnn %>% 
  filter(구분 == "롯데") %>% 
  select(id, word, 제목, 월별) %>% 
  filter(월별 == "11") %>% 
  add_count(word) %>% 
  filter(n >= 5) %>% 
  pairwise_cor(item = word,
               feature = id,
               sort = TRUE)

롯데_cor_11_graph <- 롯데_cor_11 %>% 
  filter(correlation >= 0.5) %>% 
  as_tbl_graph(directed = FALSE) %>% 
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))

set.seed(1029)

ggraph(롯데_cor_11_graph, layout = "fr") +
  
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

