# KoNLP useNIADic
# install.packages("multilinguer")
# library(multilinguer)
# install_jdk()
# install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), type = "binary")
# install.packages('KoNLP', repos = 'https://forkonlp.r-universe.dev')
library(KoNLP) #최종적으로 "KoNLP" 패키지를 불러옵니다

# devtools::install_github('haven-jeon/NIADic/NIADic', build_vignettes = TRUE)
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_281')  # 설치한 JAVA version에 따라 달라집니다
buildDictionary(ext_dic = "woorimalsam")  # "woorimalsam" dic을 불러옵니다
useNIADic()

# package
library(tidyverse)
library(ggplot2)
library(tm)
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
library(ggthemes)
library(widyr)
library(ggraph)
library(tidygraph)

start.time <- Sys.time()

# 엔씨
엔씨df <- read_csv(file = "D:/대학원/논문/소논문/뉴스_sports.news_NC.csv", col_names = TRUE, locale=locale('ko',encoding='euc-kr'))

names(엔씨df) <- c("언론사","제목","날짜","링크","좋아","훈훈","슬퍼","화나","후속")

엔씨df$날짜 <- 엔씨df$날짜 %>% as.character()

엔씨df <- 엔씨df %>% arrange(-desc(날짜))
엔씨df <- 엔씨df %>% 
  filter(제목 != "NA") %>% 
  filter(제목 != "수동확인") 

엔씨df$구분 <- c("엔씨")

# 엔씨df_제목필터 <- grep("NC",엔씨df$제목)
# 엔씨df <- 엔씨df[엔씨df_제목필터,]

# 중복제거
중복제거 <- duplicated(엔씨df$제목)
엔씨df$중복제거 <- 중복제거

엔씨df <- 엔씨df %>% filter(중복제거 == "FALSE")
엔씨df$id <- c(1:length(엔씨df$제목))

# 롯데
롯데df <- read_csv(file = "D:/대학원/논문/소논문/뉴스_sports.news_롯데.csv", col_names = TRUE, locale=locale('ko',encoding='euc-kr'))

names(롯데df) <- c("언론사","제목","날짜","링크","좋아","훈훈","슬퍼","화나","후속")

롯데df$날짜 <- 롯데df$날짜 %>% as.character()

롯데df <- 롯데df %>% arrange(-desc(날짜))
롯데df <- 롯데df %>% 
  filter(제목 != "NA") %>% 
  filter(제목 != "수동확인") 

롯데df$구분 <- c("롯데")

# 롯데df_제목필터 <- grep("롯데",롯데df$제목)
# 롯데df <- 롯데df[롯데df_제목필터,]


# 중복제거
중복제거 <- duplicated(롯데df$제목)
롯데df$중복제거 <- 중복제거

롯데df <- 롯데df %>% filter(중복제거 == "FALSE")
롯데df$id <- c(1:length(롯데df$제목))

# 엔씨 + 롯데
엔롯df <- bind_rows(엔씨df, 롯데df)


# 월별 추이
# 엔씨
엔씨df_월별 <- 엔롯df %>% filter(구분 == "엔씨")
엔씨df_월별$날짜 <- str_sub(엔씨df_월별$날짜,1,7)

엔씨df_월별_table <- 엔씨df_월별$날짜 %>% table() %>% as_tibble()

names(엔씨df_월별_table) <- c("날짜_월별","Freq")

ggplot(data = 엔씨df_월별_table, aes(x = 날짜_월별, y = Freq, group = 1)) + 
  geom_line(size = 2, colour="#006600") + 
  geom_point(size = 1, colour="#006600") +
  geom_text(aes(label = Freq),hjust = 1,size=5) +
  geom_hline(yintercept = mean(엔씨df_월별_table$Freq), color='red',linetype='dashed', size = 1) +
  labs(x="", y="") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=45, hjust=1))


# 롯데
롯데df_월별 <- 엔롯df %>% filter(구분 == "롯데")
롯데df_월별$날짜 <- str_sub(롯데df_월별$날짜,1,7)

롯데df_월별_table <- 롯데df_월별$날짜 %>% table() %>% as_tibble()

names(롯데df_월별_table) <- c("날짜_월별","Freq")

ggplot(data = 롯데df_월별_table, aes(x = 날짜_월별, y = Freq, group = 1)) + 
  geom_line(size = 2, colour="#006600") + 
  geom_point(size = 1, colour="#006600") +
  geom_text(aes(label = Freq),hjust = 1,size=5) +
  geom_hline(yintercept = mean(롯데df_월별_table$Freq), color='red',linetype='dashed', size = 1) +
  labs(x="", y="") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=45, hjust=1))


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

엔씨df_일별_table %>% arrange(-Freq)

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

롯데df_일별_table %>% arrange(-Freq)

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
    filter(글자수>=2) 
  
  엔롯_영어[[i]] <- 엔롯_tb[[i]] %>%  
    mutate(영어 = str_match(word,'([a-zA-Z]+)')[,2]) %>% ## "한글" variable을 만들고 한글만 저장     
    na.omit() %>% ## 
    mutate(글자수 = str_length(영어)) %>%   ## "글자수" variable을 만듭니다 
    filter(글자수>=2) 
  
  엔롯_한영[[i]] <- bind_rows(엔롯_한글[[i]],엔롯_영어[[i]])
  
}

엔롯_token <- list()

for (i in 1:n){
  엔롯_token <- bind_rows(엔롯_token,엔롯_한영[[i]])
}

# 데이터 전처리
제거 <- c()

chr <- c("골프","토토","야구토토","로드FC","티샷","김효주","이달의 소녀",  "사진","포토","SS","ss","SC","MD","MK","톡톡","베이스볼","브레이크","야구선임기자의","핀치히터","성일만","핫포커스","야구세상","정오의","프리뷰","김근한","골든크로스", "배지헌","브러시백","부산오픈","아이에스동서","ARC","FC","RAOD","로드","오훈규","강등","기자의","산책","대한체육회장","이기흥","여자오픈","칸타타","올해의","동아스포츠대상","치어리더","HD","KB","금융","스타챔피언십","프로야구","종합","md","mk","이달의","소녀","스폐셜","회차","발매","마니아포커스","엠스플","exx","배중현의","톺아보기","out","in","gtout","롯데렌터카","여자대회","한경","bc","레이디스컵","제주삼다수","한경","민학수의","all","that","golf","아프리카","tv","사무총장","장동철","천일평의","방어","가는","체육인대회","arc","fc","road","김동현","박시원","마니아","노트","고등래퍼","이정현")

for(i in 1:length(chr)){
  
  cat(i, '번째 전처리 제거 단어를 찾는 중 입니다.\n') 
  
  del.tmp <- grep(chr[i],엔롯_token$word)
  제거 <- append(제거,del.tmp)
}

제거 <- 제거 %>% unique()

엔롯_token <- 엔롯_token[-제거,]


# 엔씨
엔롯_token$월별 <- str_sub(엔롯_token$날짜,6,7)

엔씨_token <- 엔롯_token %>% 
  filter(구분 == "엔씨")
  
엔씨_token_word <- 엔씨_token$word
엔씨_token_count <- table(엔씨_token_word) ## 객체별 빈도를 셉니다
엔씨_token_count <- sort(엔씨_token_count, decreasing = TRUE) ##내림차순 정렬 합니다
엔씨_token_count50 <- 엔씨_token_count[1:30]  

## 빈도그래프 작성
엔씨_token_count50df <- 엔씨_token_count50 %>% as_tibble() ## tibble변환하고 그래프 작성  
names(엔씨_token_count50df) <- c("엔씨_token","Freq")

ggplot(엔씨_token_count50df, aes(x=Freq, y=reorder(엔씨_token,Freq), fill = Freq)) + 
  geom_col() + 
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  labs(x="", y="") + 
  scale_fill_gradient(low = "#5D5D5D", high = "#003300") +
  geom_text(aes(label = Freq),hjust = -0.1,size=5) +
  scale_x_continuous(expand = c(0,0), limit=c(0,10000)) + 
  theme_minimal() +
  theme(legend.position = "none")

엔씨_token_count50df %>% view()

## 워드클라우드 작성
엔씨_token_count50df %>% wordcloud2(minRotation=0, maxRotation=0, color = "#003300")

# 월별
엔씨_token_11 <- 엔롯_token %>% 
  filter(구분 == "엔씨") %>% 
  filter(월별 == "11")

엔씨_token_11_word <- 엔씨_token_11$word
엔씨_token_11_count <- table(엔씨_token_11_word) 
엔씨_token_11_count <- sort(엔씨_token_11_count, decreasing = TRUE) 
엔씨_token_11_count30 <- 엔씨_token_11_count[1:30]  

엔씨_token_11_count30df <- 엔씨_token_11_count30 %>% as_tibble() 
names(엔씨_token_11_count30df) <- c("엔씨_token_11","Freq")

엔씨_token_11_count30df %>% view()


# 롯데
엔롯_token$월별 <- str_sub(엔롯_token$날짜,6,7)

롯데_token <- 엔롯_token %>% 
  filter(구분 == "롯데")
  
롯데_token_word <- 롯데_token$word
롯데_token_count <- table(롯데_token_word) ## 객체별 빈도를 셉니다
롯데_token_count <- sort(롯데_token_count, decreasing = TRUE) ##내림차순 정렬 합니다
롯데_token_count50 <- 롯데_token_count[1:30]  

## 빈도그래프 작성
롯데_token_count50df <- 롯데_token_count50 %>% as_tibble() ## tibble변환하고 그래프 작성  
names(롯데_token_count50df) <- c("롯데_token","Freq")

ggplot(롯데_token_count50df, aes(x=Freq, y=reorder(롯데_token,Freq), fill = Freq)) + 
  geom_col() + 
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  scale_fill_gradient(low = "#5D5D5D", high = "#003300") +
  geom_text(aes(label = Freq),hjust = -0.1,size=5) +
  scale_x_continuous(expand = c(0,0), limit=c(0,8000)) + 
  theme_minimal()

## 워드클라우드 작성
롯데_token_count50df %>% wordcloud2(minRotation=0, maxRotation=0, color = "#003300")

# 월별
롯데_token_06 <- 엔롯_token %>% 
  filter(구분 == "롯데") %>% 
  filter(월별 == "06")

롯데_token_06_word <- 롯데_token_06$word
롯데_token_06_count <- table(롯데_token_06_word) 
롯데_token_06_count <- sort(롯데_token_06_count, decreasing = TRUE) 
롯데_token_06_count30 <- 롯데_token_06_count[1:30]  

롯데_token_06_count30df <- 롯데_token_06_count30 %>% as_tibble() 
names(롯데_token_06_count30df) <- c("롯데_token_06","Freq")

롯데_token_06_count30df %>% view()

# 일별
엔롯_token$일별 <- str_sub(엔롯_token$날짜,1,11)
롯데_token_0311 <- 엔롯_token %>% 
  filter(구분 == "롯데") %>% 
  filter(일별 == "2021.03.11.")

롯데_token_0311_word <- 롯데_token_0311$word
롯데_token_0311_count <- table(롯데_token_0311_word) 
롯데_token_0311_count <- sort(롯데_token_0311_count, decreasing = TRUE) 
롯데_token_0311_count30 <- 롯데_token_0311_count[1:20]  

롯데_token_0311_count30df <- 롯데_token_0311_count30 %>% as_tibble() 
names(롯데_token_0311_count30df) <- c("롯데_token_06","Freq")

롯데_token_0311_count30df %>% wordcloud2(minRotation=0, maxRotation=0, color = "#003300")

롯데_token_0506 <- 엔롯_token %>% 
  filter(구분 == "롯데") %>% 
  filter(일별 == "2020.05.06.")

롯데_token_0506_word <- 롯데_token_0506$word
롯데_token_0506_count <- table(롯데_token_0506_word) 
롯데_token_0506_count <- sort(롯데_token_0506_count, decreasing = TRUE) 
롯데_token_0506_count30 <- 롯데_token_0506_count[1:20]  

롯데_token_0506_count30df <- 롯데_token_0506_count30 %>% as_tibble() 
names(롯데_token_0506_count30df) <- c("롯데_token_06","Freq")

롯데_token_0506_count30df %>% wordcloud2(minRotation=0, maxRotation=0, color = "#003300")

롯데_token_0918 <- 엔롯_token %>% 
  filter(구분 == "롯데") %>% 
  filter(일별 == "2020.09.18.")

롯데_token_0918_word <- 롯데_token_0918$word
롯데_token_0918_count <- table(롯데_token_0918_word) 
롯데_token_0918_count <- sort(롯데_token_0918_count, decreasing = TRUE) 
롯데_token_0918_count30 <- 롯데_token_0918_count[1:20]  

롯데_token_0918_count30df <- 롯데_token_0918_count30 %>% as_tibble() 
names(롯데_token_0918_count30df) <- c("롯데_token_06","Freq")

롯데_token_0918_count30df %>% wordcloud2(minRotation=0, maxRotation=0, color = "#003300")


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
sent_dic$polarity <- sent_dic$polarity %>% as.double()

sent_dic %>% filter(word == "승리")

grep("승리",sent_dic$word)

이기 <- grep("이기",sent_dic$word)
sent_dic[이기,]

sent_dic[7694,2] <- c(2)
sent_dic[7695,2] <- c(2)
sent_dic %>% view()

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
g <- 엔롯_tb_pnn %>% 
  filter(구분 == "엔씨") %>% 
  group_by(월별) %>% 
  count(sentiment) %>% 
  mutate(ratio = round((n/sum(n)*100),2)) %>% 
  filter(sentiment != "neu")

g %>% ggplot(aes(x = 월별, y = ratio, group = sentiment)) +
  geom_line(aes(color = sentiment), size = 1) +
  geom_text(aes(label = ratio),vjust = 0.5,size=5) +
  geom_col(aes(x = 월별, y = n * (max(ratio)/max(n)*0.7), fill = sentiment), position = "dodge", alpha = 0.8) +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ . * (max(g$n)/max(g$ratio)/0.7), name="")) +
  labs(x = "", y ="", size = 1) +
  theme_minimal() +
  theme(legend.position = "bottom")


# 롯데
g <- 엔롯_tb_pnn %>% 
  filter(구분 == "롯데") %>% 
  group_by(월별) %>% 
  count(sentiment) %>% 
  mutate(ratio = round((n/sum(n)*100),2)) %>% 
  filter(sentiment != "neu")

g %>% ggplot(aes(x = 월별, y = ratio, group = sentiment)) +
  geom_line(aes(color = sentiment), size = 1) +
  geom_text(aes(label = ratio),vjust = 0.5,size=5) +
  geom_col(aes(x = 월별, y = n * (max(ratio)/max(n)*0.7), fill = sentiment), position = "dodge", alpha = 0.8) +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ . * (max(g$n)/max(g$ratio)/0.7), name="")) +
  labs(x = "", y ="", size = 1) +
  theme_minimal() +
  theme(legend.position = "bottom")


# 엔씨
엔씨_tb_pnn <- 엔롯_tb_pnn %>% 
  filter(구분 == "엔씨")
  
  # 빈출 감정단어 파악 30개
  엔씨_tb_pnn_top <- 엔씨_tb_pnn %>% 
  filter(sentiment != "neu") %>% 
  count(sentiment, word) %>% 
  group_by(sentiment) %>% 
  slice_max(n, n=20)


ggplot(엔씨_tb_pnn_top, aes(x=reorder(word,n), y=n, fill=sentiment)) +
  geom_col() +
  labs(x="", y="") + 
  coord_flip() +
  geom_text(aes(label = n),hjust = -0.3) +
  facet_wrap(~ sentiment, scales =  "free") +
  scale_y_continuous(expand = c(0,0), limit=c(0,1200)) +
  theme_minimal() +
  theme(legend.position = "bottom")

## 긍정 부정 기사에 어떤 단어가 자주 사용?-논문X
엔씨_tb_pnn_freq <- 엔씨_tb_pnn %>% 
  count(sentiment, word, sort = TRUE)

## 로그 오즈비 구하기 - 긍정 및 부정 기사에서 상대적으로 자주 사용된 단어 파악
엔씨_tb_pnn_odd_wide <- 엔씨_tb_pnn_freq %>% 
  filter(sentiment != "neu") %>% 
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = list(n=0))

엔씨_tb_pnn_odd_wide <- 엔씨_tb_pnn_odd_wide %>% 
  mutate(log_odds_ratio = log(((pos+1) / (sum(pos +1))) / ((neg+1) / (sum(neg+1)))))

# 로그 오즈비가 가장 큰 단어 10개 추출
엔씨_tb_pnn_odd_top <- 엔씨_tb_pnn_odd_wide %>% 
  group_by(sentiment= ifelse(log_odds_ratio > 0, "pos", "neg")) %>% 
  slice_max(abs(log_odds_ratio), n=10, with_ties = FALSE)

엔씨_tb_pnn_odd_top %>% 
  ggplot(aes(x = reorder(word, log_odds_ratio),y = log_odds_ratio,fill = sentiment))+
  geom_col() +
  labs(x="", y="") + 
  coord_flip() +
  geom_text(aes(label = round(log_odds_ratio,2)),hjust = -0.1,size=6) + 
  theme_minimal() +
  theme(legend.position = "bottom")


# 롯데
롯데_tb_pnn <- 엔롯_tb_pnn %>% 
  filter(구분 == "롯데")
  
# 빈출 감정단어 파악 50개
롯데_tb_pnn_top <- 롯데_tb_pnn %>% 
  filter(sentiment != "neu") %>% 
  count(sentiment, word) %>% 
  group_by(sentiment) %>% 
  slice_max(n, n=20)

롯데_tb_pnn_top %>% view()

ggplot(롯데_tb_pnn_top, aes(x=reorder(word,n), y=n, fill=sentiment)) +
  geom_col() +
  labs(x="", y="") + 
  coord_flip() +
  geom_text(aes(label = n),hjust = -0.3) +
  facet_wrap(~ sentiment, scales =  "free") +
  scale_y_continuous(expand = c(0,0), limit=c(0,1200)) +
  theme_minimal() +
  theme(legend.position = "bottom")

## 긍정 부정 기사에 어떤 단어가 자주 사용?
롯데_tb_pnn_freq <- 롯데_tb_pnn %>% 
  count(sentiment, word, sort = TRUE)

## 로그 오즈비 구하기 - 긍정 및 부정 기사에서 상대적으로 자주 사용된 단어 파악
롯데_tb_pnn_odd_wide <- 롯데_tb_pnn_freq %>% 
  filter(sentiment != "neu") %>% 
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = list(n=0))

롯데_tb_pnn_odd_wide <- 롯데_tb_pnn_odd_wide %>% 
  mutate(log_odds_ratio = log(((pos+1) / (sum(pos +1))) / ((neg+1) / (sum(neg+1)))))

# 로그 오즈비가 가장 큰 단어 10개 추출
롯데_tb_pnn_odd_top <- 롯데_tb_pnn_odd_wide %>% 
  group_by(sentiment= ifelse(log_odds_ratio > 0, "pos", "neg")) %>% 
  slice_max(abs(log_odds_ratio), n=10, with_ties = FALSE)

롯데_tb_pnn_odd_top %>% 
  ggplot(aes(x = reorder(word, log_odds_ratio),y = log_odds_ratio,fill = sentiment))+
  geom_col() +
  labs(x="", y="") + 
  coord_flip() +
  geom_text(aes(label = round(log_odds_ratio,2)),hjust = -0.1,size=6) +
  theme_minimal() +
  theme(legend.position = "bottom")



# 동시 출현 네트워크 - 동시 출현 빈도를 이용한 네트워크
# 엔씨
엔씨_co <- 엔롯_tb_pnn %>% 
  filter(구분 == "엔씨") %>% 
  select(id, word, 제목, 월별) %>%
  add_count(word) %>% 
  filter(n >= 30) %>% 
  pairwise_count(item = word,
                 feature = id,
                 sort = TRUE) 

엔씨_co <- 엔씨_co[1:60,]

엔씨_co_graph <- 엔씨_co %>% 
  as_tbl_graph()

ggraph(엔씨_co_graph) +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name))


set.seed(1201)
ggraph(엔씨_co_graph, layout = "fr") +
  geom_edge_link(color = "black", alpha = 1) +
  geom_node_point(color = "#003300", alpha = 1, size = 5) +
  geom_node_text(aes(label = name), repel = TRUE , size = 5) +
  theme_graph()

# 롯데
롯데_co <- 엔롯_tb_pnn %>% 
  filter(구분 == "롯데") %>% 
  select(id, word, 제목, 월별) %>%
  add_count(word) %>% 
  filter(n >= 30) %>% 
  pairwise_count(item = word,
                 feature = id,
                 sort = TRUE) 

롯데_co <- 롯데_co[1:50,]

롯데_co_graph <- 롯데_co %>% 
  as_tbl_graph()

ggraph(롯데_co_graph) +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name))


set.seed(1201)
ggraph(롯데_co_graph, layout = "fr") +
  geom_edge_link(color = "black", alpha = 1) +
  
  geom_node_point(color = "#003300", alpha = 1, size = 5) +
  
  geom_node_text(aes(label = name), repel = TRUE , size = 5) +
  
  theme_graph()



# 단어 간 상관분석 - 다른 단어에 비해 상대적으로 자주 함께 사용된 단어 파악, 전처리에 용이
library(widyr)
library(tidygraph)
library(ggraph)

# 엔씨
엔씨_cor <- 엔롯_tb_pnn %>% 
  filter(구분 == "엔씨") %>% 
  select(id, word, 제목, 월별) %>%
  add_count(word) %>% 
  filter(n >= 15) %>% 
  pairwise_cor(item = word,
               feature = id,
               sort = TRUE)


엔씨_cor_graph <- 엔씨_cor %>% 
  filter(correlation >= 0.5) %>%
  as_tbl_graph(directed = FALSE) %>% 
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))

set.seed(1029)

ggraph(엔씨_cor_graph, layout = "fr") +
  
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
롯데_cor <- 엔롯_tb_pnn %>% 
  filter(구분 == "롯데") %>% 
  select(id, word, 제목, 월별) %>% 
  add_count(word) %>% 
  filter(n >= 15) %>% 
  pairwise_cor(item = word,
               feature = id,
               sort = TRUE)

롯데_cor_graph <- 롯데_cor %>% 
  filter(correlation >= 0.5) %>% 
  as_tbl_graph(directed = FALSE) %>% 
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))

set.seed(1029)

ggraph(롯데_cor_graph, layout = "fr") +
  
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


# # 승리 단어 분석하기
# 승리_cor <- 엔롯_tb_pnn %>% 
#   filter(구분 == "엔씨") %>% 
#   select(id, word, 제목, 월별) %>%
#   add_count(word) %>% 
#   filter(n >= 30) %>% 
#   pairwise_cor(item = word,
#                feature = id,
#                sort = TRUE) %>% 
#   filter(item1 == "승리")
# 
# 승리_cor <- 승리_cor[1:30,]
# 
# # 상관분석
# 승리_cor_graph <- 승리_cor %>% 
#   # filter(correlation >= 0.5) %>%
#   as_tbl_graph(directed = FALSE) %>% 
#   mutate(centrality = centrality_degree(),
#          group = as.factor(group_infomap()))
# 
# set.seed(1029)
# 
# ggraph(승리_cor_graph, layout = "fr") +
#   
#   geom_edge_link(color = "gray50",
#                  aes(edge_alpha = correlation,
#                      edge_width = correlation),
#                  show.legend = FALSE) +
#   scale_edge_width(range = c(1,4)) +
#   
#   geom_node_point(aes(size = centrality,
#                       color = group),
#                   show.legend = FALSE) +
#   scale_size(range = c(5,10)) +
#   
#   geom_node_text(aes(label = name),
#                  repel = TRUE,
#                  size = 5) +
#   theme_graph()


# n-gram : 연이어 사용된 n개의 단어
# 엔씨
# install.packages("igraph")
library(igraph)

set.seed(1201)

arr <- grid::arrow(type = "closed", length = unit(.20, "inches"))

엔롯_tb_pnn %>%
  filter(구분 == "엔씨") %>% 
  group_by(id) %>% 
  summarise(sentence = paste(word, collapse = " ")) %>% 
  unnest_tokens(input = sentence,
                output = trigram,
                token = "ngrams",
                n = 3) %>% 
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%    
  count(word1, word2, word3, sort = TRUE) %>% 
  na.omit() %>% 
  filter(n >= 17) %>% 
  graph_from_data_frame() %>% 
ggraph(layout = "fr") +
  geom_edge_link(color = "black", alpha = 1, arrow = arr) +
  geom_node_point(color = "#003300", alpha = 1, size = 5) +
  geom_node_text(aes(label = name), repel = TRUE , size = 5) +
  theme_graph()


# 롯데
set.seed(1201)

엔롯_tb_pnn %>%
  filter(구분 == "롯데") %>% 
  group_by(id) %>% 
  summarise(sentence = paste(word, collapse = " ")) %>% 
  unnest_tokens(input = sentence,
                output = trigram,
                token = "ngrams",
                n = 3) %>% 
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%    
  count(word1, word2, word3, sort = TRUE) %>% 
  na.omit() %>% 
  filter(n >= 17) %>% 
  graph_from_data_frame() %>% 
  ggraph(layout = "fr") +
  geom_edge_link(color = "black", alpha = 1, arrow = arr) +
  geom_node_point(color = "#003300", alpha = 1, size = 5) +
  geom_node_text(aes(label = name), repel = TRUE , size = 5) +
  theme_graph()

# 기사 제목별 감정점수 구하기
엔롯_tb_pnn %>% 
  filter(구분 == "엔씨") %>% 
  group_by(id, 제목, 날짜, 구분) %>% 
  summarise(score = sum(polarity)) %>% 
  ungroup() %>% 
  select(score,제목,날짜,구분) %>% 
  arrange(score) %>% head(40) %>% View()


end.time <- Sys.time()

end.time - start.time