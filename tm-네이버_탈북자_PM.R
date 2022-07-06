# KoNLP useNIADic
install.packages("multilinguer")
library(multilinguer)
install_jdk()
install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), type = "binary")
#
install.packages("remotes")
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))
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
library(corpus)
library(wordcloud2)
library(stringr)
library(rJava)
library(base64enc)
library(RmecabKo)
library(reshape2)
library(tidytext)


# 탈북-han
탈북_park_com_본문제외 <- read_csv(file = "C:/대학원/논문/텍스트마이닝상담/탈북_park_df_com_본문제외.csv", col_names = TRUE, locale=locale('ko',encoding='euc-kr'))

탈북_moon_com_본문제외 <- read_csv(file = "C:/대학원/논문/텍스트마이닝상담/탈북_moon_df_com_본문제외.csv", col_names = TRUE, locale=locale('ko',encoding='euc-kr'))

탈북_park_com_본문제외 <- 탈북_park_com_본문제외 %>% 
  select(날짜_park,언론사_park,제목_park,링크_park)
탈북_park_com_본문제외$정부 <- c("park")
names(탈북_park_com_본문제외) <- c("날짜_탈북","언론사_탈북","제목_탈북","링크_탈북","정부_탈북")

탈북_moon_com_본문제외 <- 탈북_moon_com_본문제외 %>% 
  select(날짜_moon,언론사_moon,제목_moon,링크_moon)
탈북_moon_com_본문제외$정부 <- c("moon")
names(탈북_moon_com_본문제외) <- c("날짜_탈북","언론사_탈북","제목_탈북","링크_탈북","정부_탈북")

탈북_park_com_본문제외$날짜_탈북 <- 탈북_park_com_본문제외$날짜_탈북 %>% as.character()
탈북_moon_com_본문제외$날짜_탈북 <- 탈북_moon_com_본문제외$날짜_탈북 %>% as.character()

탈북_com <- bind_rows(탈북_park_com_본문제외,탈북_moon_com_본문제외)
탈북_com <- 탈북_com %>% arrange(-desc(날짜_탈북))
탈북_com <- 탈북_com %>% 
  filter(제목_탈북 != "NA")

# 정부별 
탈북_com$정부_탈북 %>% table()

# 언론사별
탈북_com_table <- 탈북_com$언론사_탈북 %>% 
  table() %>% 
  data.frame() %>% 
  arrange(-Freq)

탈북_com_table50 <- 탈북_com_table[1:53,] %>% as_tibble() ## tibble변환하고 그래프 작성  
names(탈북_com_table50) <- c("언론사_탈북","Freq")
탈북_com_table50 %>% tail()

ggplot(탈북_com_table50, aes(x = Freq, y= reorder(언론사_탈북,Freq), fill = Freq)) + 
  geom_col() + 
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  scale_fill_gradient(low = "#5D5D5D", high = "#0000FF") +
  geom_text(aes(label = Freq),hjust = -0.1,size=5) 

# 정부 X 언론사별
library(vcd)
탈북_com_50 <- 탈북_com %>% filter(언론사_탈북 %in% 탈북_com_table50$언론사_탈북)

mtable <- with(탈북_com_50, table(정부_탈북,언론사_탈북))
mtable <- mtable %>% as_tibble()
mtable$정부_탈북 <- factor(mtable$정부_탈북, levels = c("park","moon")) # 범례 순서 바꾸기

ggplot(mtable, aes(x = reorder(언론사_탈북,n), y = n, fill = 정부_탈북))+
  geom_bar(stat='identity') +
  geom_text(aes(label = n),hjust = -0.1,size = 5) +
  scale_fill_manual(values = c("red","blue")) +
  labs(x="", y="") +
  coord_flip() 

#년별
탈북_com_년별 <- 탈북_com
탈북_com_년별$날짜_탈북 <- str_sub(탈북_com_년별$날짜_탈북,1,4)

탈북_com_년별_table <- 탈북_com_년별$날짜_탈북 %>% table() %>% as_tibble()

names(탈북_com_년별_table) <- c("날짜_년별","Freq")

ggplot(data = 탈북_com_년별_table, aes(x = 날짜_년별, y = Freq, group = 1)) + 
  geom_line(size = 2, colour="#006600") + 
  geom_point(size = 1, colour="#006600") +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  geom_text(aes(label = Freq),hjust = -0.1,size=5) +
  geom_vline(xintercept = 탈북_com_년별_table$날짜_년별[5], color = "red", linetype = 'dashed', size = 1) +
  geom_hline(yintercept = mean(탈북_com_년별_table$Freq), color='red',linetype='dashed', size = 1) +
  labs(x="", y="")

# 월별 추이
탈북_com_월별 <- 탈북_com
탈북_com_월별$날짜_탈북 <- str_sub(탈북_com_월별$날짜_탈북,1,7)

탈북_com_월별_table <- 탈북_com_월별$날짜_탈북 %>% table() %>% as_tibble()

names(탈북_com_월별_table) <- c("날짜_월별","Freq")

ggplot(data = 탈북_com_월별_table, aes(x = 날짜_월별, y = Freq, group = 1)) + 
  geom_line(size = 2, colour="#006600") + 
  geom_point(size = 1, colour="#006600") +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  geom_text(aes(label = Freq),hjust = -0.1,size=5) +
  geom_vline(xintercept = 탈북_com_월별_table$날짜_월별[44], color = "red", linetype = 'dashed', size = 1) +
  geom_hline(yintercept = mean(탈북_com_월별_table$Freq), color='red',linetype='dashed', size = 1) +
  labs(x="", y="")


# 일별 추이
탈북_com_일별 <- 탈북_com
탈북_com_일별$날짜_탈북 <- str_sub(탈북_com_일별$날짜_탈북,1,10)

탈북_com_일별_table <- 탈북_com_일별$날짜_탈북 %>% table() %>% as_tibble()

탈북_com_일별_table %>% View()

names(탈북_com_일별_table) <- c("날짜_일별","Freq")

ggplot(data = 탈북_com_일별_table, aes(x = 날짜_일별, y = Freq, group = 1)) + 
  geom_line(size = 2, colour="#006600") + 
  geom_point(size = 1, colour="#006600") +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  geom_text(aes(label = Freq),hjust = -0.1,size=4) +
  geom_vline(xintercept = 탈북_com_일별_table$날짜_일별[1276], color = "red", linetype = 'dashed', size = 1) +
  geom_hline(yintercept = mean(탈북_com_일별_table$Freq), color='red',linetype='dashed', size = 1) +
  coord_flip() +
  labs(x="", y="")

#### text mining
# 데이터 분할
id_탈북 <- c(1:length(탈북_com$제목_탈북))
탈북_naver_date <- data.frame(id_탈북,탈북_com$제목_탈북,탈북_com$날짜_탈북,탈북_com$정부_탈북)
names(탈북_naver_date) <- c("id_탈북","제목_탈북","날짜_탈북","정부_탈북")

탈북_int <- length(탈북_naver_date$제목_탈북) / 10000
탈북_int <- 탈북_int %>% ceiling()
n <- 탈북_int

탈북_sp <- split(탈북_naver_date,rep(1:n,each=10000))

# 데이터 셋 만들기
탈북_tb <- list()
탈북_data_set <- list()
탈북_token <- c()
탈북_한글 <- list()
탈북_영어 <- list()

## 단어기준 토큰화
for (i in 1:n){
  
  cat(i, '번째 데이터 리스트 tokenizer', '중 입니다.\n') 
  
  탈북_tb[[i]] <- 탈북_sp[[i]] %>% 
    tibble() %>% 
    unnest_tokens(input = 제목_탈북, output = word, token = "words", drop = FALSE)
  
  탈북_data_set[[i]] <- 탈북_tb[[i]]  %>% 
    melt() %>% # 식별자id, 측정 변수variable, 측정치value 형태로 데이터를 재구성하는 함수
    as_tibble() %>% 
    select(6,1,2,3,4)
  
  names(탈북_data_set[[i]]) <- c("L1","제목_탈북","날짜_탈북","정부_탈북","word")
  
  탈북_한글[[i]] <- 탈북_data_set[[i]] %>%  
    mutate(한글 = str_match(word,'([가-힣]+)')[,2]) %>% ## "한글" variable을 만들고 한글만 저장         
    na.omit() %>% ## ([가-힣]+)/P') 한글만을 선택하는 정규표현식
    mutate(글자수 = str_length(한글)) %>%   ## "글자수" variable을 만듭니다 
    filter(str_length(한글)>=2) 
  
  탈북_영어[[i]] <- 탈북_data_set[[i]] %>%  
    mutate(영어 = str_match(word,'([a-zA-Z]+)')[,2]) %>% ## "한글" variable을 만들고 한글만 저장         
    na.omit() %>% ## 
    mutate(글자수 = str_length(영어)) %>%   ## "글자수" variable을 만듭니다 
    filter(str_length(영어)>=3) 
  
  탈북_token한글.tmp <- 탈북_한글[[i]]$word
  탈북_token영어.tmp <- 탈북_영어[[i]]$word
  
  탈북_token <- append(탈북_token,탈북_token한글.tmp)
  탈북_token <- append(탈북_token,탈북_token영어.tmp)
}


## 최다 빈도 단어 Top50을 뽑습니다
탈북_token_count <- table(탈북_token) ## 객체별 빈도를 셉니다
탈북_token_count <- sort(탈북_token_count, decreasing = TRUE) ##내림차순 정렬 합니다
탈북_token_count50 <- 탈북_token_count[1:50]  ## Top 50까지 추립니다

## 빈도그래프 작성
탈북_token_count50df <- 탈북_token_count50 %>% as_tibble() ## tibble변환하고 그래프 작성  
names(탈북_token_count50df) <- c("탈북_token","Freq")

ggplot(탈북_token_count50df, aes(x=Freq, y=reorder(탈북_token,Freq), fill = Freq)) + 
  geom_col() + 
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  scale_fill_gradient(low = "#5D5D5D", high = "#003300") +
  geom_text(aes(label = Freq),hjust = -0.1,size=5) 

## 워드클라우드 작성
탈북_token_count50df %>% wordcloud2(minRotation=0, maxRotation=0, color = "#003300") 


#### 감성분석
##감성사전
library(readr)
sent_dic <- read_csv("C:/대학원/논문/소논문/스포츠기사/SentiWord_Dict.txt")
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

## 단어기준 토큰화
탈북_tb_pol <- tibble()

for (i in 1:n){
  탈북_tb_pol <- bind_rows(탈북_tb_pol,탈북_tb[[i]])
}

## 단어에 감정 점수 부여
library(textclean)
탈북_tb_pnn <- 탈북_tb_pol %>% 
  left_join(sent_dic,by="word") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity)) %>% 
  mutate(sentiment = ifelse(polarity > 0, "pos", ifelse(polarity < 0, "neg", "neu"))) %>% 
  mutate(제목_탈북 = str_squish(replace_html(제목_탈북))) %>% 
  mutate(날짜_탈북 = str_squish(replace_html(날짜_탈북))) %>% 
  mutate(정부_탈북 = str_squish(replace_html(정부_탈북)))

탈북_tb_pnn$polarity <- 탈북_tb_pnn$polarity %>% as.integer()

탈북_tb_pnn_score <- 탈북_tb_pnn %>% 
  count(sentiment) %>% 
  mutate(ratio = round((n/sum(n)*100),2))

탈북_tb_pnn_score %>% 
  ggplot(aes(x = sentiment, y = ratio, fill = sentiment)) +
  geom_col() +
  geom_text(aes(label = ratio),vjust = 0.5,size=7) +
  scale_x_discrete(limits = c("pos","neu","neg"))

# 빈출 감정단어 파악 50개
탈북_tb_pnn_top <- 탈북_tb_pnn %>% 
  filter(sentiment != "neu") %>% 
  count(sentiment, word) %>% 
  group_by(sentiment) %>% 
  slice_max(n, n=50)

ggplot(탈북_tb_pnn_top, aes(x=reorder(word,n), y=n, fill=sentiment)) +
  geom_col() +
  labs(x="", y="") + 
  coord_flip() +
  geom_text(aes(label = n),hjust = -0.3) +
  facet_wrap(~ sentiment, scales =  "free") +
  scale_y_continuous(expand = c(0,0), limit=c(0,400))

# 기사 제목별 감정점수 구하기
탈북_tb_pnn_sum <- 탈북_tb_pnn %>% 
  group_by(id_탈북, 제목_탈북, 날짜_탈북, 정부_탈북) %>% 
  summarise(score = sum(polarity)) %>% 
  ungroup()

# 긍정 기사
탈북_tb_pnn_sum %>% 
  select(score,제목_탈북,날짜_탈북,정부_탈북) %>% 
  arrange(-score)

# 부정 기사
탈북_tb_pnn_sum %>% 
  select(score,제목_탈북,날짜_탈북, 정부_탈북) %>% 
  arrange(score)

# 정부별 긍정·부정 기사제목 갯수
# park 긍정
탈북_tb_pnn_sum %>% 
  select(score,제목_탈북,날짜_탈북,정부_탈북) %>% 
  arrange(-score) %>% 
  filter(score > 0) %>% 
  filter(정부_탈북 == "park")

# park 부정
탈북_tb_pnn_sum %>% 
  select(score,제목_탈북,날짜_탈북,정부_탈북) %>% 
  arrange(-score) %>% 
  filter(score < 0) %>% 
  filter(정부_탈북 == "park")

# moon 긍정
탈북_tb_pnn_sum %>% 
  select(score,제목_탈북,날짜_탈북,정부_탈북) %>% 
  arrange(-score) %>% 
  filter(score > 0) %>% 
  filter(정부_탈북 == "moon")

# moon 부정
탈북_tb_pnn_sum %>% 
  select(score,제목_탈북,날짜_탈북,정부_탈북) %>% 
  arrange(-score) %>% 
  filter(score < 0) %>% 
  filter(정부_탈북 == "moon")



## 긍정 부정 기사에 어떤 단어가 자주 사용?
탈북_tb_pnn_odd <- 탈북_tb_pnn %>% 
  unnest_tokens(input = 제목_탈북, output = word, token = "words", drop=FALSE) %>% 
  filter(str_detect(word, "[가-힣]") & str_count(word) >= 2)

탈북_tb_pnn_freq <- 탈북_tb_pnn_odd %>% 
  count(sentiment, word, sort = TRUE)

## 로그 오즈비 구하기 - 긍정 및 부정 기사에서 상대적으로 자주 사용된 단어 파악
탈북_tb_pnn_odd_wide <- 탈북_tb_pnn_freq %>% 
  filter(sentiment != "neu") %>% 
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = list(n=0))

탈북_tb_pnn_odd_wide <- 탈북_tb_pnn_odd_wide %>% 
  mutate(log_odds_ratio = log(((pos+1) / (sum(pos +1))) / ((neg+1) / (sum(neg+1)))))


# 로그 오즈비가 가장 큰 단어 10개 추출
탈북_tb_pnn_odd_top <- 탈북_tb_pnn_odd_wide %>% 
  group_by(sentiment= ifelse(log_odds_ratio > 0, "pos", "neg")) %>% 
  slice_max(abs(log_odds_ratio), n=10, with_ties = FALSE)

탈북_tb_pnn_odd_top %>% 
  ggplot(aes(x = reorder(word, log_odds_ratio),y = log_odds_ratio,fill = sentiment))+
  geom_col() +
  labs(x="", y="") + 
  coord_flip() +
  geom_text(aes(label = round(log_odds_ratio,2)),hjust = -0.1,size=6) 
