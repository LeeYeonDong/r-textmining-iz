# KoNLP useNIADic
install.packages("multilinguer")
library(multilinguer)
install_jdk()
install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), type = "binary")
#
install.packages("remotes")
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))
library(KoNLP) #최종적으로 "KoNLP" 패키지를 불러옵니다 가장먼저 실행하기

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


# 암호화폐
암호df <- read_csv(file = "D:/대학원/논문/텍스트마이닝상담/암호비트가상/암호_df_com_본문제외.csv", col_names = TRUE, locale=locale('ko',encoding='euc-kr'))

암호df$날짜_암호 <- 암호df$날짜_암호 %>% as.character()

암호df <- 암호df %>% arrange(-desc(날짜_암호))
암호df <- 암호df %>% 
  filter(제목_암호 != "NA")
암호df$구분 <- c("암호화폐")

names(암호df) <- c("언론사","제목","날짜","링크","구분")

암호df_제목필터 <- grep("암호화폐",암호df$제목)

암호df <- 암호df[암호df_제목필터,]


# 비트코인
비트df <- read_csv(file = "D:/대학원/논문/텍스트마이닝상담/암호비트가상/비트_df_com_본문제외.csv", col_names = TRUE, locale=locale('ko',encoding='euc-kr'))

비트df$날짜_비트 <- 비트df$날짜_비트 %>% as.character()

비트df <- 비트df %>% arrange(-desc(날짜_비트))
비트df <- 비트df %>% 
  filter(제목_비트 != "NA")
비트df$구분 <- c("비트코인")

names(비트df) <- c("언론사","제목","날짜","링크","구분")

비트df_제목필터 <- grep("비트코인",비트df$제목)

비트df <- 비트df[비트df_제목필터,]


# 가상화폐
가상df <- read_csv(file = "D:/대학원/논문/텍스트마이닝상담/암호비트가상/가상_df_com_본문제외.csv", col_names = TRUE, locale=locale('ko',encoding='euc-kr'))

가상df$날짜_가상 <- 가상df$날짜_가상 %>% as.character()

가상df <- 가상df %>% arrange(-desc(날짜_가상))
가상df <- 가상df %>% 
  filter(제목_가상 != "NA")
가상df$구분 <- c("가상화폐")

names(가상df) <- c("언론사","제목","날짜","링크","구분")

가상df_제목필터 <- grep("가상화폐",가상df$제목)

가상df <- 가상df[가상df_제목필터,]


# 암호+비트+가상
암호비트가상df <- bind_rows(암호df,비트df,가상df)

# 키워드별 기사량
암호비트가상df$구분 %>% table() %>% as_tibble() %>% arrange(desc(n))

# 언론사별
# 전체
암호비트가상df$언론사 %>% table() %>% as_tibble() %>% arrange(desc(n))

# 암호화폐
암호비트가상df %>% filter(구분 == "암호화폐") %>% select(언론사) %>% table() %>% as_tibble() %>% arrange(desc(n))
# 비트코인
암호비트가상df %>% filter(구분 == "비트코인") %>% select(언론사) %>% table() %>% as_tibble() %>% arrange(desc(n))
# 가상화폐
암호비트가상df %>% filter(구분 == "가상화폐") %>% select(언론사) %>% table() %>% as_tibble() %>% arrange(desc(n))

# 중복기사 제거
암호비트가상df <- 암호비트가상df[-which(duplicated(암호비트가상df$링크)),]

암호비트가상df$제목 <- gsub("네트워크","",암호비트가상df$제목)
암호비트가상df$제목 <- gsub("ytn","",암호비트가상df$제목)
암호비트가상df$제목 <- gsub("뉴스","",암호비트가상df$제목)

# 년별 추이
암호비트가상df_년별 <- 암호비트가상df
암호비트가상df_년별$날짜 <- str_sub(암호비트가상df$날짜,1,4)

암호비트가상df_년별_table <- 암호비트가상df_년별$날짜 %>% table() %>% as_tibble()

names(암호비트가상df_년별_table) <- c("날짜_년별","Freq")

ggplot(data = 암호비트가상df_년별_table, aes(x = 날짜_년별, y = Freq, group = 1)) + 
  geom_line(size = 2, colour="#006600") + 
  geom_point(size = 1, colour="#006600") +
  theme(axis.text.x=element_text(angle = 0, hjust = 1)) +
  geom_text(aes(label = Freq),hjust = -0.1,size = 5) +
  geom_hline(yintercept = mean(암호비트가상df_년별_table$Freq), color='red',linetype='dashed', size = 1) +
  labs(x="", y="") 

library(simplevis)
gg_bar(암호비트가상df_년별_table,날짜_년별, Freq)
gg_line(암호비트가상df_년별_table,날짜_년별, Freq)

# 월별 추이
암호비트가상df_월별 <- 암호비트가상df
암호비트가상df_월별$날짜 <- str_sub(암호비트가상df_월별$날짜,1,7)

암호비트가상df_월별_table <- 암호비트가상df_월별$날짜 %>% table() %>% as_tibble()

names(암호비트가상df_월별_table) <- c("날짜_월별","Freq")

ggplot(data = 암호비트가상df_월별_table, aes(x = 날짜_월별, y = Freq, group = 1)) + 
  geom_line(size = 2, colour="#006600") + 
  geom_point(size = 1, colour="#006600") +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  geom_text(aes(label = Freq),hjust = 1,size=5) +
  geom_hline(yintercept = mean(암호비트가상df_월별_table$Freq), color='red',linetype='dashed', size = 1) +
  labs(x="", y="")
  

#### text mining
# 데이터 분할
id <- c(1:length(암호비트가상df$제목))
암호비트가상df2 <- data.frame(id,암호비트가상df$제목,암호비트가상df$날짜,암호비트가상df$구분)
names(암호비트가상df2) <- c("id","제목","날짜","구분")

암호비트가상_int <- nrow(암호비트가상df2) / 10000
암호비트가상_int <- 암호비트가상_int %>% ceiling()
n <- 암호비트가상_int

암호비트가상df_sp <- split(암호비트가상df2,rep(1:n,each=10000))

# 데이터 셋 만들기
암호비트가상_tb <- list()
암호비트가상_data_set <- list()
암호비트가상_한글 <- list()
암호비트가상_영어 <- list()
암호비트가상_한영 <- list()

## 단어기준 토큰화
for (i in 1:n){
  
  cat(i, '번째 데이터 리스트 tokenizer', '중 입니다.\n') 
  
  암호비트가상_tb[[i]] <- 암호비트가상df_sp[[i]] %>% 
    tibble() %>% 
    unnest_tokens(input = 제목, output = word, token = "words", drop = FALSE)
  
  names(암호비트가상_tb[[i]]) <- c("id","제목","날짜","구분","word")
  
  암호비트가상_한글[[i]] <- 암호비트가상_tb[[i]] %>%  
    mutate(한글 = str_match(word,'([가-힣]+)')[,2]) %>% ## "한글" variable을 만들고 한글만 저장
    na.omit() %>% ## ([가-힣]+)/P') 한글만을 선택하는 정규표현식
    mutate(글자수 = str_length(한글)) %>%   ## "글자수" variable을 만듭니다 
    filter(str_length(한글)>=2) 
  
  암호비트가상_영어[[i]] <- 암호비트가상_tb[[i]] %>%  
    mutate(영어 = str_match(word,'([a-zA-Z]+)')[,2]) %>% ## "한글" variable을 만들고 한글만 저장     
    na.omit() %>% ## 
    mutate(글자수 = str_length(영어)) %>%   ## "글자수" variable을 만듭니다 
    filter(str_length(영어)>=3) 
  
  암호비트가상_한영[[i]] <- bind_rows(암호비트가상_한글[[i]],암호비트가상_영어[[i]])

  }

암호비트가상_token <- list()

for (i in 1:n){
  암호비트가상_token <- bind_rows(암호비트가상_token,암호비트가상_한영[[i]])
}


## 최다 빈도 단어 Top50을 뽑습니다
#전체
암호비트가상_token_word <- 암호비트가상_token$word
암호비트가상_token_count <- table(암호비트가상_token_word) ## 객체별 빈도를 셉니다
암호비트가상_token_count <- sort(암호비트가상_token_count, decreasing = TRUE) ##내림차순 정렬 합니다
암호비트가상_token_count50 <- 암호비트가상_token_count[1:50]  ## Top 50까지 추립니다

## 빈도그래프 작성
암호비트가상_token_count50df <- 암호비트가상_token_count50 %>% as_tibble() ## tibble변환하고 그래프 작성  
names(암호비트가상_token_count50df) <- c("암호비트가상_token","Freq")

ggplot(암호비트가상_token_count50df, aes(x=Freq, y=reorder(암호비트가상_token,Freq), fill = Freq)) + 
  geom_col() + 
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  scale_fill_gradient(low = "#5D5D5D", high = "#003300") +
  geom_text(aes(label = Freq),hjust = -0.1,size=4) 

## 워드클라우드 작성
암호비트가상_token_count50df %>% wordcloud2(minRotation=0, maxRotation=0, color = "#003300") 


# 2012년
암호비트가상_token$연도 <- str_sub(암호비트가상_token$날짜,1,4)

암호비트가상_token_12 <- 암호비트가상_token %>% 
  filter(연도 == "2012")

암호비트가상_token_word_12 <- 암호비트가상_token_12$word
암호비트가상_token_count_12 <- table(암호비트가상_token_word_12) ## 객체별 빈도를 셉니다
암호비트가상_token_count_12 <- sort(암호비트가상_token_count_12, decreasing = TRUE) ##내림차순 정렬 합니다
암호비트가상_token_count50_12 <- 암호비트가상_token_count_12[1:50]  

## 빈도그래프 작성
암호비트가상_token_count50df_12 <- 암호비트가상_token_count50_12 %>% as_tibble() ## tibble변환하고 그래프 작성  
names(암호비트가상_token_count50df_12) <- c("암호비트가상_token","Freq")

ggplot(암호비트가상_token_count50df_12, aes(x=Freq, y=reorder(암호비트가상_token,Freq), fill = Freq)) + 
  geom_col() + 
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  scale_fill_gradient(low = "#5D5D5D", high = "#003300") +
  geom_text(aes(label = Freq),hjust = -0.1,size=5) 

## 워드클라우드 작성
암호비트가상_token_count50df_12 %>% wordcloud2(minRotation=0, maxRotation=0, color = "#003300")


# 2013년
암호비트가상_token_13 <- 암호비트가상_token %>% 
  filter(연도 == "2013")

암호비트가상_token_word_13 <- 암호비트가상_token_13$word
암호비트가상_token_count_13 <- table(암호비트가상_token_word_13) ## 객체별 빈도를 셉니다
암호비트가상_token_count_13 <- sort(암호비트가상_token_count_13, decreasing = TRUE)
암호비트가상_token_count50_13 <- 암호비트가상_token_count_13[1:50]  

## 빈도그래프 작성
암호비트가상_token_count50df_13 <- 암호비트가상_token_count50_13 %>% as_tibble() ## tibble변환하고 그래프 작성  
names(암호비트가상_token_count50df_13) <- c("암호비트가상_token","Freq")

ggplot(암호비트가상_token_count50df_13, aes(x=Freq, y=reorder(암호비트가상_token,Freq), fill = Freq)) + 
  geom_col() + 
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  scale_fill_gradient(low = "#5D5D5D", high = "#003300") +
  geom_text(aes(label = Freq),hjust = -0.1,size=5) 

## 워드클라우드 작성
암호비트가상_token_count50df_13 %>% wordcloud2(minRotation=0, maxRotation=0, color = "#003300")


# 2014년
암호비트가상_token_14 <- 암호비트가상_token %>% 
  filter(연도 == "2014")

암호비트가상_token_word_14 <- 암호비트가상_token_14$word
암호비트가상_token_count_14 <- table(암호비트가상_token_word_14) ## 객체별 빈도를 셉니다
암호비트가상_token_count_14 <- sort(암호비트가상_token_count_14, decreasing = TRUE)
암호비트가상_token_count50_14 <- 암호비트가상_token_count_14[1:50]  

## 빈도그래프 작성
암호비트가상_token_count50df_14 <- 암호비트가상_token_count50_14 %>% as_tibble() ## tibble변환하고 그래프 작성  
names(암호비트가상_token_count50df_14) <- c("암호비트가상_token","Freq")

ggplot(암호비트가상_token_count50df_14, aes(x=Freq, y=reorder(암호비트가상_token,Freq), fill = Freq)) + 
  geom_col() + 
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  scale_fill_gradient(low = "#5D5D5D", high = "#003300") +
  geom_text(aes(label = Freq),hjust = -0.1,size=5) 

## 워드클라우드 작성
암호비트가상_token_count50df_14 %>% wordcloud2(minRotation=0, maxRotation=0, color = "#003300")


# 2015년
암호비트가상_token_15 <- 암호비트가상_token %>% 
  filter(연도 == "2015")

암호비트가상_token_word_15 <- 암호비트가상_token_15$word
암호비트가상_token_count_15 <- table(암호비트가상_token_word_15) ## 객체별 빈도를 셉니다
암호비트가상_token_count_15 <- sort(암호비트가상_token_count_15, decreasing = TRUE)
암호비트가상_token_count50_15 <- 암호비트가상_token_count_15[1:50]  

## 빈도그래프 작성
암호비트가상_token_count50df_15 <- 암호비트가상_token_count50_15 %>% as_tibble() ## tibble변환하고 그래프 작성  
names(암호비트가상_token_count50df_15) <- c("암호비트가상_token","Freq")

ggplot(암호비트가상_token_count50df_15, aes(x=Freq, y=reorder(암호비트가상_token,Freq), fill = Freq)) + 
  geom_col() + 
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  scale_fill_gradient(low = "#5D5D5D", high = "#003300") +
  geom_text(aes(label = Freq),hjust = -0.1,size=5) 

## 워드클라우드 작성
암호비트가상_token_count50df_15 %>% wordcloud2(minRotation=0, maxRotation=0, color = "#003300")


# 2016년
암호비트가상_token_16 <- 암호비트가상_token %>% 
  filter(연도 == "2016")

암호비트가상_token_word_16 <- 암호비트가상_token_16$word
암호비트가상_token_count_16 <- table(암호비트가상_token_word_16) ## 객체별 빈도를 셉니다
암호비트가상_token_count_16 <- sort(암호비트가상_token_count_16, decreasing = TRUE)
암호비트가상_token_count50_16 <- 암호비트가상_token_count_16[1:50]  

## 빈도그래프 작성
암호비트가상_token_count50df_16 <- 암호비트가상_token_count50_16 %>% as_tibble() ## tibble변환하고 그래프 작성  
names(암호비트가상_token_count50df_16) <- c("암호비트가상_token","Freq")

ggplot(암호비트가상_token_count50df_16, aes(x=Freq, y=reorder(암호비트가상_token,Freq), fill = Freq)) + 
  geom_col() + 
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  scale_fill_gradient(low = "#5D5D5D", high = "#003300") +
  geom_text(aes(label = Freq),hjust = -0.1,size=5) 

## 워드클라우드 작성
암호비트가상_token_count50df_16 %>% wordcloud2(minRotation=0, maxRotation=0, color = "#003300")


# 2017년
암호비트가상_token_17 <- 암호비트가상_token %>% 
  filter(연도 == "2017")

암호비트가상_token_word_17 <- 암호비트가상_token_17$word
암호비트가상_token_count_17 <- table(암호비트가상_token_word_17) ## 객체별 빈도를 셉니다
암호비트가상_token_count_17 <- sort(암호비트가상_token_count_17, decreasing = TRUE)
암호비트가상_token_count50_17 <- 암호비트가상_token_count_17[1:50]  

## 빈도그래프 작성
암호비트가상_token_count50df_17 <- 암호비트가상_token_count50_17 %>% as_tibble() ## tibble변환하고 그래프 작성  
names(암호비트가상_token_count50df_17) <- c("암호비트가상_token","Freq")

ggplot(암호비트가상_token_count50df_17, aes(x=Freq, y=reorder(암호비트가상_token,Freq), fill = Freq)) + 
  geom_col() + 
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  scale_fill_gradient(low = "#5D5D5D", high = "#003300") +
  geom_text(aes(label = Freq),hjust = -0.1,size=5) 

## 워드클라우드 작성
암호비트가상_token_count50df_17 %>% wordcloud2(minRotation=0, maxRotation=0, color = "#003300")


# 2018년
암호비트가상_token_18 <- 암호비트가상_token %>% 
  filter(연도 == "2018")

암호비트가상_token_word_18 <- 암호비트가상_token_18$word
암호비트가상_token_count_18 <- table(암호비트가상_token_word_18) ## 객체별 빈도를 셉니다
암호비트가상_token_count_18 <- sort(암호비트가상_token_count_18, decreasing = TRUE)
암호비트가상_token_count50_18 <- 암호비트가상_token_count_18[1:50]  

## 빈도그래프 작성
암호비트가상_token_count50df_18 <- 암호비트가상_token_count50_18 %>% as_tibble() ## tibble변환하고 그래프 작성  
names(암호비트가상_token_count50df_18) <- c("암호비트가상_token","Freq")

ggplot(암호비트가상_token_count50df_18, aes(x=Freq, y=reorder(암호비트가상_token,Freq), fill = Freq)) + 
  geom_col() + 
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  scale_fill_gradient(low = "#5D5D5D", high = "#003300") +
  geom_text(aes(label = Freq),hjust = -0.1,size=5) 

## 워드클라우드 작성
암호비트가상_token_count50df_18 %>% wordcloud2(minRotation=0, maxRotation=0, color = "#003300")


# 2019년
암호비트가상_token_19 <- 암호비트가상_token %>% 
  filter(연도 == "2019")

암호비트가상_token_word_19 <- 암호비트가상_token_19$word
암호비트가상_token_count_19 <- table(암호비트가상_token_word_19) ## 객체별 빈도를 셉니다
암호비트가상_token_count_19 <- sort(암호비트가상_token_count_19, decreasing = TRUE)
암호비트가상_token_count50_19 <- 암호비트가상_token_count_19[1:50]   

## 빈도그래프 작성
암호비트가상_token_count50df_19 <- 암호비트가상_token_count50_19 %>% as_tibble() ## tibble변환하고 그래프 작성  
names(암호비트가상_token_count50df_19) <- c("암호비트가상_token","Freq")

ggplot(암호비트가상_token_count50df_19, aes(x=Freq, y=reorder(암호비트가상_token,Freq), fill = Freq)) + 
  geom_col() + 
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  scale_fill_gradient(low = "#5D5D5D", high = "#003300") +
  geom_text(aes(label = Freq),hjust = -0.1,size=5) 

## 워드클라우드 작성
암호비트가상_token_count50df_19 %>% wordcloud2(minRotation=0, maxRotation=0, color = "#003300")


# 2020년
암호비트가상_token_20 <- 암호비트가상_token %>% 
  filter(연도 == "2020")

암호비트가상_token_word_20 <- 암호비트가상_token_20$word
암호비트가상_token_count_20 <- table(암호비트가상_token_word_20) ## 객체별 빈도를 셉니다
암호비트가상_token_count_20 <- sort(암호비트가상_token_count_20, decreasing = TRUE)
암호비트가상_token_count50_20 <- 암호비트가상_token_count_20[1:50]  

## 빈도그래프 작성
암호비트가상_token_count50df_20 <- 암호비트가상_token_count50_20 %>% as_tibble() ## tibble변환하고 그래프 작성  
names(암호비트가상_token_count50df_20) <- c("암호비트가상_token","Freq")

ggplot(암호비트가상_token_count50df_20, aes(x=Freq, y=reorder(암호비트가상_token,Freq), fill = Freq)) + 
  geom_col() + 
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  scale_fill_gradient(low = "#5D5D5D", high = "#003300") +
  geom_text(aes(label = Freq),hjust = -0.1,size=5) 

## 워드클라우드 작성
암호비트가상_token_count50df_20 %>% wordcloud2(minRotation=0, maxRotation=0, color = "#003300")


# 2021년
암호비트가상_token_21 <- 암호비트가상_token %>% 
  filter(연도 == "2021")

암호비트가상_token_word_21 <- 암호비트가상_token_21$word
암호비트가상_token_count_21 <- table(암호비트가상_token_word_21) ## 객체별 빈도를 셉니다
암호비트가상_token_count_21 <- sort(암호비트가상_token_count_21, decreasing = TRUE)
암호비트가상_token_count50_21 <- 암호비트가상_token_count_21[1:50]  

## 빈도그래프 작성
암호비트가상_token_count50df_21 <- 암호비트가상_token_count50_21 %>% as_tibble() ## tibble변환하고 그래프 작성  
names(암호비트가상_token_count50df_21) <- c("암호비트가상_token","Freq")

ggplot(암호비트가상_token_count50df_21, aes(x=Freq, y=reorder(암호비트가상_token,Freq), fill = Freq)) + 
  geom_col() + 
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  scale_fill_gradient(low = "#5D5D5D", high = "#003300") +
  geom_text(aes(label = Freq),hjust = -0.1,size=5) 

## 워드클라우드 작성
암호비트가상_token_count50df_21 %>% wordcloud2(minRotation=0, maxRotation=0, color = "#003300")


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
library(textclean)
암호비트가상_tb_pnn <- 암호비트가상_token %>% 
  left_join(sent_dic,by="word") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity)) %>% 
  mutate(sentiment = ifelse(polarity > 0, "pos", ifelse(polarity < 0, "neg", "neu"))) %>% 
  mutate(제목 = str_squish(replace_html(제목))) %>% # str_squish
  mutate(날짜 = str_squish(replace_html(날짜))) %>% # replace_html
  mutate(정부 = str_squish(replace_html(구분))) 

암호비트가상_tb_pnn$연도 <- str_sub(암호비트가상_tb_pnn$날짜,1,4)

암호비트가상_tb_pnn$polarity <- 암호비트가상_tb_pnn$polarity %>% as.integer()

암호비트가상_tb_pnn_score <- 암호비트가상_tb_pnn %>% 
  count(sentiment) %>% 
  mutate(ratio = round((n/sum(n)*100),2))

# 연도별 neg vs pos 추이
암호비트가상_tb_pnn %>% 
  group_by(연도) %>%
  filter(연도 != "2012") %>% 
  count(sentiment) %>% 
  mutate(ratio = round((n/sum(n)*100),2)) %>% 
  filter(sentiment != "neu") %>% 
  ggplot(aes(x = 연도, y = ratio, color = sentiment, group = sentiment)) +
  geom_line() +
  geom_text(aes(label = ratio),vjust = 0.5,size=7) + theme(legend.position = "bottom")


# 빈출 감정단어 파악 50개
암호비트가상_tb_pnn_top <- 암호비트가상_tb_pnn %>% 
  filter(연도 != "2012") %>% 
  filter(sentiment != "neu") %>% 
  count(sentiment, word) %>% 
  group_by(sentiment) %>% 
  slice_max(n, n=50)

ggplot(암호비트가상_tb_pnn_top, aes(x=reorder(word,n), y=n, fill=sentiment)) +
  geom_col() +
  labs(x="", y="") + 
  coord_flip() +
  geom_text(aes(label = n),hjust = -0.3) +
  facet_wrap(~ sentiment, scales =  "free") +
  scale_y_continuous(expand = c(0,0), limit=c(0,1500)) + theme(legend.position = "bottom")


# 기사 제목별 감정점수 구하기
암호비트가상_tb_pnn_sum <- 암호비트가상_tb_pnn %>% 
  group_by(id, 제목, 날짜, 구분) %>% 
  summarise(score = sum(polarity)) %>% 
  ungroup() # ungroup

# 긍정 기사
암호비트가상_tb_pnn_sum %>% 
  select(score,제목,날짜,구분) %>% 
  arrange(score) %>% head(10) %>% View()

# 부정 기사
암호비트가상_tb_pnn_sum %>% 
  select(score,제목,날짜,구분) %>% 
  arrange(score) %>% head(10) %>% view()

## 긍정 부정 기사에 어떤 단어가 자주 사용?
암호비트가상_tb_pnn_odd <- 암호비트가상_tb_pnn %>% 
  unnest_tokens(input = 제목, output = word, token = "words", drop=FALSE) %>% 
  filter(str_detect(word, "[가-힣]+") & str_count(word) >= 2)

암호비트가상_tb_pnn_freq <- 암호비트가상_tb_pnn_odd %>% 
  count(sentiment, word, sort = TRUE)

## 로그 오즈비 구하기 - 긍정 및 부정 기사에서 상대적으로 자주 사용된 단어 파악
암호비트가상_tb_pnn_odd_wide <- 암호비트가상_tb_pnn_freq %>% 
  filter(sentiment != "neu") %>% 
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = list(n=0))

암호비트가상_tb_pnn_odd_wide <- 암호비트가상_tb_pnn_odd_wide %>% 
  mutate(log_odds_ratio = log(((pos+1) / (sum(pos +1))) / ((neg+1) / (sum(neg+1)))))

# 로그 오즈비가 가장 큰 단어 10개 추출
암호비트가상_tb_pnn_odd_top <- 암호비트가상_tb_pnn_odd_wide %>% 
  group_by(sentiment= ifelse(log_odds_ratio > 0, "pos", "neg")) %>% 
  slice_max(abs(log_odds_ratio), n=10, with_ties = FALSE)

암호비트가상_tb_pnn_odd_top %>% 
  ggplot(aes(x = reorder(word, log_odds_ratio),y = log_odds_ratio,fill = sentiment))+
  geom_col() +
  labs(x="", y="") + 
  coord_flip() +
  geom_text(aes(label = round(log_odds_ratio,2)),hjust = -0.1,size=6)


#2013
암호비트가상_tb_pnn_13 <- 암호비트가상_tb_pnn %>% 
  filter(연도 == "2013")

# 빈출 감정단어 파악 50개
암호비트가상_tb_pnn_top_13 <- 암호비트가상_tb_pnn_13 %>% 
  filter(sentiment != "neu") %>% 
  count(sentiment, word) %>% 
  group_by(sentiment) %>% 
  slice_max(n, n=50)

ggplot(암호비트가상_tb_pnn_top_13, aes(x=reorder(word,n), y=n, fill=sentiment)) +
  geom_col() +
  labs(x="", y="") + 
  coord_flip() +
  geom_text(aes(label = n),hjust = -0.3) +
  facet_wrap(~ sentiment, scales =  "free") +
  scale_y_continuous(expand = c(0,0), limit=c(0,25)) + theme(legend.position = "bottom")

# 기사 제목별 감정점수 구하기
암호비트가상_tb_pnn_sum_13 <- 암호비트가상_tb_pnn_13 %>% 
  group_by(id, 제목, 날짜, 구분) %>% 
  summarise(score = sum(polarity)) %>% 
  ungroup() # ungroup

# 긍정 기사
암호비트가상_tb_pnn_sum_13 %>% 
  select(score,제목,날짜,구분) %>% 
  arrange(-score) %>% head(10) %>% view()

# 부정 기사
암호비트가상_tb_pnn_sum_13 %>% 
  select(score,제목,날짜,구분) %>% 
  arrange(score) %>% head(10)  %>% view()

## 긍정 부정 기사에 어떤 단어가 자주 사용?
암호비트가상_tb_pnn_odd_13 <- 암호비트가상_tb_pnn_13 %>% 
  unnest_tokens(input = 제목, output = word, token = "words", drop=FALSE) %>% 
  filter(str_detect(word, "[가-힣]+") & str_count(word) >= 2)

암호비트가상_tb_pnn_freq_13 <- 암호비트가상_tb_pnn_odd_13 %>% 
  count(sentiment, word, sort = TRUE)

## 로그 오즈비 구하기 - 긍정 및 부정 기사에서 상대적으로 자주 사용된 단어 파악
암호비트가상_tb_pnn_odd_wide_13 <- 암호비트가상_tb_pnn_freq_13 %>% 
  filter(sentiment != "neu") %>% 
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = list(n=0))

암호비트가상_tb_pnn_odd_wide_13 <- 암호비트가상_tb_pnn_odd_wide_13 %>% 
  mutate(log_odds_ratio = log(((pos+1) / (sum(pos +1))) / ((neg+1) / (sum(neg+1)))))

# 로그 오즈비가 가장 큰 단어 10개 추출
암호비트가상_tb_pnn_odd_top_13 <- 암호비트가상_tb_pnn_odd_wide_13 %>% 
  group_by(sentiment= ifelse(log_odds_ratio > 0, "pos", "neg")) %>% 
  slice_max(abs(log_odds_ratio), n=10, with_ties = FALSE)

암호비트가상_tb_pnn_odd_top_13 %>% 
  ggplot(aes(x = reorder(word, log_odds_ratio),y = log_odds_ratio,fill = sentiment))+
  geom_col() +
  labs(x="", y="") + 
  coord_flip() +
  geom_text(aes(label = round(log_odds_ratio,2)),hjust = -0.1,size=6) 


#2014
암호비트가상_tb_pnn_14 <- 암호비트가상_tb_pnn %>% 
  filter(연도 == "2014")

# 빈출 감정단어 파악 50개
암호비트가상_tb_pnn_top_14 <- 암호비트가상_tb_pnn_14 %>% 
  filter(sentiment != "neu") %>% 
  count(sentiment, word) %>% 
  group_by(sentiment) %>% 
  slice_max(n, n=50)

ggplot(암호비트가상_tb_pnn_top_14, aes(x=reorder(word,n), y=n, fill=sentiment)) +
  geom_col() +
  labs(x="", y="") + 
  coord_flip() +
  geom_text(aes(label = n),hjust = -0.3) +
  facet_wrap(~ sentiment, scales =  "free") +
  scale_y_continuous(expand = c(0,0), limit=c(0,50))+ theme(legend.position = "bottom")

# 기사 제목별 감정점수 구하기
암호비트가상_tb_pnn_sum_14 <- 암호비트가상_tb_pnn_14 %>% 
  group_by(id, 제목, 날짜, 구분) %>% 
  summarise(score = sum(polarity)) %>% 
  ungroup() # ungroup

# 긍정 기사
암호비트가상_tb_pnn_sum_14 %>% 
  select(score,제목,날짜,구분) %>% 
  arrange(-score) %>% head(10) %>% view()

# 부정 기사
암호비트가상_tb_pnn_sum_14 %>% 
  select(score,제목,날짜,구분) %>% 
  arrange(score) %>% head(10)  %>% view()

## 긍정 부정 기사에 어떤 단어가 자주 사용?
암호비트가상_tb_pnn_odd_14 <- 암호비트가상_tb_pnn_14 %>% 
  unnest_tokens(input = 제목, output = word, token = "words", drop=FALSE) %>% 
  filter(str_detect(word, "[가-힣]+") & str_count(word) >= 2)

암호비트가상_tb_pnn_freq_14 <- 암호비트가상_tb_pnn_odd_14 %>% 
  count(sentiment, word, sort = TRUE)

## 로그 오즈비 구하기 - 긍정 및 부정 기사에서 상대적으로 자주 사용된 단어 파악
암호비트가상_tb_pnn_odd_wide_14 <- 암호비트가상_tb_pnn_freq_14 %>% 
  filter(sentiment != "neu") %>% 
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = list(n=0))

암호비트가상_tb_pnn_odd_wide_14 <- 암호비트가상_tb_pnn_odd_wide_14 %>% 
  mutate(log_odds_ratio = log(((pos+1) / (sum(pos +1))) / ((neg+1) / (sum(neg+1)))))

# 로그 오즈비가 가장 큰 단어 10개 추출
암호비트가상_tb_pnn_odd_top_14 <- 암호비트가상_tb_pnn_odd_wide_14 %>% 
  group_by(sentiment= ifelse(log_odds_ratio > 0, "pos", "neg")) %>% 
  slice_max(abs(log_odds_ratio), n=10, with_ties = FALSE)

암호비트가상_tb_pnn_odd_top_14 %>% 
  ggplot(aes(x = reorder(word, log_odds_ratio),y = log_odds_ratio,fill = sentiment))+
  geom_col() +
  labs(x="", y="") + 
  coord_flip() +
  geom_text(aes(label = round(log_odds_ratio,2)),hjust = -0.1,size=6) 


#2015
암호비트가상_tb_pnn_15 <- 암호비트가상_tb_pnn %>% 
  filter(연도 == "2015")

# 빈출 감정단어 파악 50개
암호비트가상_tb_pnn_top_15 <- 암호비트가상_tb_pnn_15 %>% 
  filter(sentiment != "neu") %>% 
  count(sentiment, word) %>% 
  group_by(sentiment) %>% 
  slice_max(n, n=50)

ggplot(암호비트가상_tb_pnn_top_15, aes(x=reorder(word,n), y=n, fill=sentiment)) +
  geom_col() +
  labs(x="", y="") + 
  coord_flip() +
  geom_text(aes(label = n),hjust = -0.3) +
  facet_wrap(~ sentiment, scales =  "free") +
  scale_y_continuous(expand = c(0,0), limit=c(0,10))+ theme(legend.position = "bottom")

# 기사 제목별 감정점수 구하기
암호비트가상_tb_pnn_sum_15 <- 암호비트가상_tb_pnn_15 %>% 
  group_by(id, 제목, 날짜, 구분) %>% 
  summarise(score = sum(polarity)) %>% 
  ungroup() # ungroup

# 긍정 기사
암호비트가상_tb_pnn_sum_15 %>% 
  select(score,제목,날짜,구분) %>% 
  arrange(-score) %>% head(10) %>% view()

# 부정 기사
암호비트가상_tb_pnn_sum_15 %>% 
  select(score,제목,날짜,구분) %>% 
  arrange(score) %>% head(10)  %>% view()

## 긍정 부정 기사에 어떤 단어가 자주 사용?
암호비트가상_tb_pnn_odd_15 <- 암호비트가상_tb_pnn_15 %>% 
  unnest_tokens(input = 제목, output = word, token = "words", drop=FALSE) %>% 
  filter(str_detect(word, "[가-힣]+") & str_count(word) >= 2)

암호비트가상_tb_pnn_freq_15 <- 암호비트가상_tb_pnn_odd_15 %>% 
  count(sentiment, word, sort = TRUE)

## 로그 오즈비 구하기 - 긍정 및 부정 기사에서 상대적으로 자주 사용된 단어 파악
암호비트가상_tb_pnn_odd_wide_15 <- 암호비트가상_tb_pnn_freq_15 %>% 
  filter(sentiment != "neu") %>% 
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = list(n=0))

암호비트가상_tb_pnn_odd_wide_15 <- 암호비트가상_tb_pnn_odd_wide_15 %>% 
  mutate(log_odds_ratio = log(((pos+1) / (sum(pos +1))) / ((neg+1) / (sum(neg+1)))))

# 로그 오즈비가 가장 큰 단어 10개 추출
암호비트가상_tb_pnn_odd_top_15 <- 암호비트가상_tb_pnn_odd_wide_15 %>% 
  group_by(sentiment= ifelse(log_odds_ratio > 0, "pos", "neg")) %>% 
  slice_max(abs(log_odds_ratio), n=10, with_ties = FALSE)

암호비트가상_tb_pnn_odd_top_15 %>% 
  ggplot(aes(x = reorder(word, log_odds_ratio),y = log_odds_ratio,fill = sentiment))+
  geom_col() +
  labs(x="", y="") + 
  coord_flip() +
  geom_text(aes(label = round(log_odds_ratio,2)),hjust = -0.1,size=6) 


#2016
암호비트가상_tb_pnn_16 <- 암호비트가상_tb_pnn %>% 
  filter(연도 == "2016")

# 빈출 감정단어 파악 50개
암호비트가상_tb_pnn_top_16 <- 암호비트가상_tb_pnn_16 %>% 
  filter(sentiment != "neu") %>% 
  count(sentiment, word) %>% 
  group_by(sentiment) %>% 
  slice_max(n, n=50)

ggplot(암호비트가상_tb_pnn_top_16, aes(x=reorder(word,n), y=n, fill=sentiment)) +
  geom_col() +
  labs(x="", y="") + 
  coord_flip() +
  geom_text(aes(label = n),hjust = -0.3) +
  facet_wrap(~ sentiment, scales =  "free") +
  scale_y_continuous(expand = c(0,0), limit=c(0,15))+ theme(legend.position = "bottom")

# 기사 제목별 감정점수 구하기
암호비트가상_tb_pnn_sum_16 <- 암호비트가상_tb_pnn_16 %>% 
  group_by(id, 제목, 날짜, 구분) %>% 
  summarise(score = sum(polarity)) %>% 
  ungroup() # ungroup

# 긍정 기사
암호비트가상_tb_pnn_sum_16 %>% 
  select(score,제목,날짜,구분) %>% 
  arrange(-score) %>% head(10) %>% view()

# 부정 기사
암호비트가상_tb_pnn_sum_16 %>% 
  select(score,제목,날짜,구분) %>% 
  arrange(score) %>% head(10)  %>% view()

## 긍정 부정 기사에 어떤 단어가 자주 사용?
암호비트가상_tb_pnn_odd_16 <- 암호비트가상_tb_pnn_16 %>% 
  unnest_tokens(input = 제목, output = word, token = "words", drop=FALSE) %>% 
  filter(str_detect(word, "[가-힣]+") & str_count(word) >= 2)

암호비트가상_tb_pnn_freq_16 <- 암호비트가상_tb_pnn_odd_16 %>% 
  count(sentiment, word, sort = TRUE)

## 로그 오즈비 구하기 - 긍정 및 부정 기사에서 상대적으로 자주 사용된 단어 파악
암호비트가상_tb_pnn_odd_wide_16 <- 암호비트가상_tb_pnn_freq_16 %>% 
  filter(sentiment != "neu") %>% 
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = list(n=0))

암호비트가상_tb_pnn_odd_wide_16 <- 암호비트가상_tb_pnn_odd_wide_16 %>% 
  mutate(log_odds_ratio = log(((pos+1) / (sum(pos +1))) / ((neg+1) / (sum(neg+1)))))

# 로그 오즈비가 가장 큰 단어 10개 추출
암호비트가상_tb_pnn_odd_top_16 <- 암호비트가상_tb_pnn_odd_wide_16 %>% 
  group_by(sentiment= ifelse(log_odds_ratio > 0, "pos", "neg")) %>% 
  slice_max(abs(log_odds_ratio), n=10, with_ties = FALSE)

암호비트가상_tb_pnn_odd_top_16 %>% 
  ggplot(aes(x = reorder(word, log_odds_ratio),y = log_odds_ratio,fill = sentiment))+
  geom_col() +
  labs(x="", y="") + 
  coord_flip() +
  geom_text(aes(label = round(log_odds_ratio,2)),hjust = -0.1,size=6) 


#2017
암호비트가상_tb_pnn_17 <- 암호비트가상_tb_pnn %>% 
  filter(연도 == "2017")

# 빈출 감정단어 파악 50개
암호비트가상_tb_pnn_top_17 <- 암호비트가상_tb_pnn_17 %>% 
  filter(sentiment != "neu") %>% 
  count(sentiment, word) %>% 
  group_by(sentiment) %>% 
  slice_max(n, n=50)

ggplot(암호비트가상_tb_pnn_top_17, aes(x=reorder(word,n), y=n, fill=sentiment)) +
  geom_col() +
  labs(x="", y="") + 
  coord_flip() +
  geom_text(aes(label = n),hjust = -0.3) +
  theme(axis.text.y = element_text(size = 6.5,face = 'bold')) +
  facet_wrap(~ sentiment, scales =  "free") +
  scale_y_continuous(expand = c(0,0), limit=c(0,50))+ theme(legend.position = "bottom")

# 기사 제목별 감정점수 구하기
암호비트가상_tb_pnn_sum_17 <- 암호비트가상_tb_pnn_17 %>% 
  group_by(id, 제목, 날짜, 구분) %>% 
  summarise(score = sum(polarity)) %>% 
  ungroup() # ungroup

# 긍정 기사
암호비트가상_tb_pnn_sum_17 %>% 
  select(score,제목,날짜,구분) %>% 
  arrange(-score) %>% head(10) %>% view()

# 부정 기사
암호비트가상_tb_pnn_sum_17 %>% 
  select(score,제목,날짜,구분) %>% 
  arrange(score) %>% head(10)  %>% view()

## 긍정 부정 기사에 어떤 단어가 자주 사용?
암호비트가상_tb_pnn_odd_17 <- 암호비트가상_tb_pnn_17 %>% 
  unnest_tokens(input = 제목, output = word, token = "words", drop=FALSE) %>% 
  filter(str_detect(word, "[가-힣]") & str_count(word) >= 2)

암호비트가상_tb_pnn_freq_17 <- 암호비트가상_tb_pnn_odd_17 %>% 
  count(sentiment, word, sort = TRUE)

## 로그 오즈비 구하기 - 긍정 및 부정 기사에서 상대적으로 자주 사용된 단어 파악
암호비트가상_tb_pnn_odd_wide_17 <- 암호비트가상_tb_pnn_freq_17 %>% 
  filter(sentiment != "neu") %>% 
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = list(n=0))

암호비트가상_tb_pnn_odd_wide_17 <- 암호비트가상_tb_pnn_odd_wide_17 %>% 
  mutate(log_odds_ratio = log(((pos+1) / (sum(pos +1))) / ((neg+1) / (sum(neg+1)))))

# 로그 오즈비가 가장 큰 단어 10개 추출
암호비트가상_tb_pnn_odd_top_17 <- 암호비트가상_tb_pnn_odd_wide_17 %>% 
  group_by(sentiment= ifelse(log_odds_ratio > 0, "pos", "neg")) %>% 
  slice_max(abs(log_odds_ratio), n=10, with_ties = FALSE)

암호비트가상_tb_pnn_odd_top_17 %>% 
  ggplot(aes(x = reorder(word, log_odds_ratio),y = log_odds_ratio,fill = sentiment))+
  geom_col() +
  labs(x="", y="") + 
  coord_flip() +
  geom_text(aes(label = round(log_odds_ratio,2)),hjust = -0.1,size=6)


#2018
암호비트가상_tb_pnn_18 <- 암호비트가상_tb_pnn %>% 
  filter(연도 == "2018")

# 빈출 감정단어 파악 50개
암호비트가상_tb_pnn_top_18 <- 암호비트가상_tb_pnn_18 %>% 
  filter(sentiment != "neu") %>% 
  count(sentiment, word) %>% 
  group_by(sentiment) %>% 
  slice_max(n, n=50)

ggplot(암호비트가상_tb_pnn_top_18, aes(x=reorder(word,n), y=n, fill=sentiment)) +
  geom_col() +
  labs(x="", y="") + 
  coord_flip() +
  geom_text(aes(label = n),hjust = -0.3) +
  theme(axis.text.y = element_text(size = 6.5)) +
  facet_wrap(~ sentiment, scales =  "free") +
  scale_y_continuous(expand = c(0,0), limit=c(0,230)) + theme(legend.position = "bottom")


# 기사 제목별 감정점수 구하기
암호비트가상_tb_pnn_sum_18 <- 암호비트가상_tb_pnn_18 %>% 
  group_by(id, 제목, 날짜, 구분) %>% 
  summarise(score = sum(polarity)) %>% 
  ungroup() # ungroup

# 긍정 기사
암호비트가상_tb_pnn_sum_18 %>% 
  select(score,제목,날짜,구분) %>% 
  arrange(-score) %>% head(10) %>% view()

# 부정 기사
암호비트가상_tb_pnn_sum_18 %>% 
  select(score,제목,날짜,구분) %>% 
  arrange(score) %>% head(10)  %>% view()

## 긍정 부정 기사에 어떤 단어가 자주 사용?
암호비트가상_tb_pnn_odd_18 <- 암호비트가상_tb_pnn_18 %>% 
  unnest_tokens(input = 제목, output = word, token = "words", drop=FALSE) %>% 
  filter(str_detect(word, "[가-힣]") & str_count(word) >= 2)

암호비트가상_tb_pnn_freq_18 <- 암호비트가상_tb_pnn_odd_18 %>% 
  count(sentiment, word, sort = TRUE)

## 로그 오즈비 구하기 - 긍정 및 부정 기사에서 상대적으로 자주 사용된 단어 파악
암호비트가상_tb_pnn_odd_wide_18 <- 암호비트가상_tb_pnn_freq_18 %>% 
  filter(sentiment != "neu") %>% 
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = list(n=0))

암호비트가상_tb_pnn_odd_wide_18 <- 암호비트가상_tb_pnn_odd_wide_18 %>% 
  mutate(log_odds_ratio = log(((pos+1) / (sum(pos +1))) / ((neg+1) / (sum(neg+1)))))

# 로그 오즈비가 가장 큰 단어 10개 추출
암호비트가상_tb_pnn_odd_top_18 <- 암호비트가상_tb_pnn_odd_wide_18 %>% 
  group_by(sentiment= ifelse(log_odds_ratio > 0, "pos", "neg")) %>% 
  slice_max(abs(log_odds_ratio), n=10, with_ties = FALSE)

암호비트가상_tb_pnn_odd_top_18 %>% 
  ggplot(aes(x = reorder(word, log_odds_ratio),y = log_odds_ratio,fill = sentiment))+
  geom_col() +
  labs(x="", y="") + 
  coord_flip() +
  geom_text(aes(label = round(log_odds_ratio,2)),hjust = -0.1,size=6) 


#2019
암호비트가상_tb_pnn_19 <- 암호비트가상_tb_pnn %>% 
  filter(연도 == "2019")

# 빈출 감정단어 파악 50개
암호비트가상_tb_pnn_top_19 <- 암호비트가상_tb_pnn_19 %>% 
  filter(sentiment != "neu") %>% 
  count(sentiment, word) %>% 
  group_by(sentiment) %>% 
  slice_max(n, n=50)

ggplot(암호비트가상_tb_pnn_top_19, aes(x=reorder(word,n), y=n, fill=sentiment)) +
  geom_col() +
  labs(x="", y="") + 
  coord_flip() +
  geom_text(aes(label = n),hjust = -0.3) +
  theme(axis.text.y = element_text(size = 6.5, face = "bold")) +
  facet_wrap(~ sentiment, scales =  "free") +
  scale_y_continuous(expand = c(0,0), limit=c(0,530)) + theme(legend.position = "bottom")

# 기사 제목별 감정점수 구하기
암호비트가상_tb_pnn_sum_19 <- 암호비트가상_tb_pnn_19 %>% 
  group_by(id, 제목, 날짜, 구분) %>% 
  summarise(score = sum(polarity)) %>% 
  ungroup() # ungroup

# 긍정 기사
암호비트가상_tb_pnn_sum_19 %>% 
  select(score,제목,날짜,구분) %>% 
  arrange(-score) %>% head(11) %>% view()

# 부정 기사
암호비트가상_tb_pnn_sum_19 %>% 
  select(score,제목,날짜,구분) %>% 
  arrange(score) %>% head(10)  %>% view()

## 긍정 부정 기사에 어떤 단어가 자주 사용?
암호비트가상_tb_pnn_odd_19 <- 암호비트가상_tb_pnn_19 %>% 
  unnest_tokens(input = 제목, output = word, token = "words", drop=FALSE) %>% 
  filter(str_detect(word, "[가-힣]") & str_count(word) >= 2)

암호비트가상_tb_pnn_freq_19 <- 암호비트가상_tb_pnn_odd_19 %>% 
  count(sentiment, word, sort = TRUE)

## 로그 오즈비 구하기 - 긍정 및 부정 기사에서 상대적으로 자주 사용된 단어 파악
암호비트가상_tb_pnn_odd_wide_19 <- 암호비트가상_tb_pnn_freq_19 %>% 
  filter(sentiment != "neu") %>% 
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = list(n=0))

암호비트가상_tb_pnn_odd_wide_19 <- 암호비트가상_tb_pnn_odd_wide_19 %>% 
  mutate(log_odds_ratio = log(((pos+1) / (sum(pos +1))) / ((neg+1) / (sum(neg+1)))))

# 로그 오즈비가 가장 큰 단어 10개 추출
암호비트가상_tb_pnn_odd_top_19 <- 암호비트가상_tb_pnn_odd_wide_19 %>% 
  group_by(sentiment= ifelse(log_odds_ratio > 0, "pos", "neg")) %>% 
  slice_max(abs(log_odds_ratio), n=10, with_ties = FALSE)

암호비트가상_tb_pnn_odd_top_19 %>% 
  ggplot(aes(x = reorder(word, log_odds_ratio),y = log_odds_ratio,fill = sentiment))+
  geom_col() +
  labs(x="", y="") + 
  coord_flip() +
  geom_text(aes(label = round(log_odds_ratio,2)),hjust = -0.1,size=6) 


#2020
암호비트가상_tb_pnn_20 <- 암호비트가상_tb_pnn %>% 
  filter(연도 == "2020")

# 빈출 감정단어 파악 50개
암호비트가상_tb_pnn_top_20 <- 암호비트가상_tb_pnn_20 %>% 
  filter(sentiment != "neu") %>% 
  count(sentiment, word) %>% 
  group_by(sentiment) %>% 
  slice_max(n, n=50)

ggplot(암호비트가상_tb_pnn_top_20, aes(x=reorder(word,n), y=n, fill=sentiment)) +
  geom_col() +
  labs(x="", y="") + 
  coord_flip() +
  geom_text(aes(label = n),hjust = -0.3) +
  facet_wrap(~ sentiment, scales =  "free") +
  theme(axis.text.y = element_text(size = 6.5, face = "bold")) +
  scale_y_continuous(expand = c(0,0), limit=c(0,2200)) + theme(legend.position = "bottom")

# 기사 제목별 감정점수 구하기
암호비트가상_tb_pnn_sum_20 <- 암호비트가상_tb_pnn_20 %>% 
  group_by(id, 제목, 날짜, 구분) %>% 
  summarise(score = sum(polarity)) %>% 
  ungroup() # ungroup

# 긍정 기사
암호비트가상_tb_pnn_sum_20 %>% 
  select(score,제목,날짜,구분) %>% 
  arrange(-score) %>% head(10) %>% view()

# 부정 기사
암호비트가상_tb_pnn_sum_20 %>% 
  select(score,제목,날짜,구분) %>% 
  arrange(score) %>% head(10)  %>% view()

## 긍정 부정 기사에 어떤 단어가 자주 사용?
암호비트가상_tb_pnn_odd_20 <- 암호비트가상_tb_pnn_20 %>% 
  unnest_tokens(input = 제목, output = word, token = "words", drop=FALSE) %>% 
  filter(str_detect(word, "[가-힣]") & str_count(word) >= 2)

암호비트가상_tb_pnn_freq_20 <- 암호비트가상_tb_pnn_odd_20 %>% 
  count(sentiment, word, sort = TRUE)

## 로그 오즈비 구하기 - 긍정 및 부정 기사에서 상대적으로 자주 사용된 단어 파악
암호비트가상_tb_pnn_odd_wide_20 <- 암호비트가상_tb_pnn_freq_20 %>% 
  filter(sentiment != "neu") %>% 
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = list(n=0))

암호비트가상_tb_pnn_odd_wide_20 <- 암호비트가상_tb_pnn_odd_wide_20 %>% 
  mutate(log_odds_ratio = log(((pos+1) / (sum(pos +1))) / ((neg+1) / (sum(neg+1)))))

# 로그 오즈비가 가장 큰 단어 10개 추출
암호비트가상_tb_pnn_odd_top_20 <- 암호비트가상_tb_pnn_odd_wide_20 %>% 
  group_by(sentiment= ifelse(log_odds_ratio > 0, "pos", "neg")) %>% 
  slice_max(abs(log_odds_ratio), n=10, with_ties = FALSE)

암호비트가상_tb_pnn_odd_top_20 %>% 
  ggplot(aes(x = reorder(word, log_odds_ratio),y = log_odds_ratio,fill = sentiment))+
  geom_col() +
  labs(x="", y="") + 
  coord_flip() +
  geom_text(aes(label = round(log_odds_ratio,2)),hjust = -0.1,size=6) 


#2021
암호비트가상_tb_pnn_21 <- 암호비트가상_tb_pnn %>% 
  filter(연도 == "2021")

# 빈출 감정단어 파악 50개
암호비트가상_tb_pnn_top_21 <- 암호비트가상_tb_pnn_21 %>% 
  filter(sentiment != "neu") %>% 
  count(sentiment, word) %>% 
  group_by(sentiment) %>% 
  slice_max(n, n=50)

ggplot(암호비트가상_tb_pnn_top_21, aes(x=reorder(word,n), y=n, fill=sentiment)) +
  geom_col() +
  labs(x="", y="") + 
  coord_flip() +
  geom_text(aes(label = n),hjust = -0.3) +
  facet_wrap(~ sentiment, scales =  "free") +
  theme(axis.text.y = element_text(size = 6.5, face = "bold")) +
  scale_y_continuous(expand = c(0,0), limit=c(0,330)) + theme(legend.position = "bottom")

# 기사 제목별 감정점수 구하기
암호비트가상_tb_pnn_sum_21 <- 암호비트가상_tb_pnn_21 %>% 
  group_by(id, 제목, 날짜, 구분) %>% 
  summarise(score = sum(polarity)) %>% 
  ungroup() # ungroup

# 긍정 기사
암호비트가상_tb_pnn_sum_21 %>% 
  select(score,제목,날짜,구분) %>% 
  arrange(-score) %>% head(10) %>% view()

# 부정 기사
암호비트가상_tb_pnn_sum_21 %>% 
  select(score,제목,날짜,구분) %>% 
  arrange(score) %>% head(10)  %>% view()

## 긍정 부정 기사에 어떤 단어가 자주 사용?
암호비트가상_tb_pnn_odd_21 <- 암호비트가상_tb_pnn_21 %>% 
  unnest_tokens(input = 제목, output = word, token = "words", drop=FALSE) %>% 
  filter(str_detect(word, "[가-힣]") & str_count(word) >= 2)

암호비트가상_tb_pnn_freq_21 <- 암호비트가상_tb_pnn_odd_21 %>% 
  count(sentiment, word, sort = TRUE)

## 로그 오즈비 구하기 - 긍정 및 부정 기사에서 상대적으로 자주 사용된 단어 파악
암호비트가상_tb_pnn_odd_wide_21 <- 암호비트가상_tb_pnn_freq_21 %>% 
  filter(sentiment != "neu") %>% 
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = list(n=0))

암호비트가상_tb_pnn_odd_wide_21 <- 암호비트가상_tb_pnn_odd_wide_21 %>% 
  mutate(log_odds_ratio = log(((pos+1) / (sum(pos +1))) / ((neg+1) / (sum(neg+1)))))

# 로그 오즈비가 가장 큰 단어 10개 추출
암호비트가상_tb_pnn_odd_top_21 <- 암호비트가상_tb_pnn_odd_wide_21 %>% 
  group_by(sentiment= ifelse(log_odds_ratio > 0, "pos", "neg")) %>% 
  slice_max(abs(log_odds_ratio), n=10, with_ties = FALSE)

암호비트가상_tb_pnn_odd_top_21 %>% 
  ggplot(aes(x = reorder(word, log_odds_ratio),y = log_odds_ratio,fill = sentiment))+
  geom_col() +
  labs(x="", y="") + 
  coord_flip() +
  geom_text(aes(label = round(log_odds_ratio,2)),hjust = -0.1,size=6) 


# 단어간 상관분석 - 다른 단어에 비해 상대적으로 자주 함께 사용된 단어 파악
library(widyr)

암호비트가상_cor <- 암호비트가상_tb_pnn %>% 
  select(id, word, 제목, 연도) %>% 
  add_count(word) %>% 
  filter(n >= 30) %>% 
  pairwise_cor(item = word,
               feature = id,
               sort = TRUE)

library(tidygraph)
library(ggraph)

# 전체
graph_cor <- 암호비트가상_cor %>% 
  filter(correlation >= 0.5) %>% 
  as_tbl_graph(directed = FALSE) %>% 
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))

set.seed(1029)

ggraph(graph_cor, layout = "fr") +
  
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

# 2012
암호비트가상_cor_12 <- 암호비트가상_tb_pnn %>% 
  select(id, word, 제목, 연도) %>% 
  filter(연도 == "2012") %>% 
  add_count(word) %>% 
  filter(n >= 5) %>% 
  pairwise_cor(item = word,
               feature = id,
               sort = TRUE)
  
graph_cor_12 <- 암호비트가상_cor_12 %>% 
  filter(correlation >= 0.5) %>% 
  as_tbl_graph(directed = FALSE) %>% 
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))

set.seed(1029)

ggraph(graph_cor_12, layout = "fr") +
  
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


# 2013
암호비트가상_cor_13 <- 암호비트가상_tb_pnn %>% 
  select(id, word, 제목, 연도) %>% 
  filter(연도 == "2013") %>% 
  add_count(word) %>% 
  filter(n >=5) %>% 
  pairwise_cor(item = word,
               feature = id,
               sort = TRUE)

graph_cor_13 <- 암호비트가상_cor_13 %>% 
  filter(correlation >= 0.5) %>% 
  as_tbl_graph(directed = FALSE) %>% 
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))

set.seed(1029)

ggraph(graph_cor_13, layout = "fr") +
  
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


# 2014
암호비트가상_cor_14 <- 암호비트가상_tb_pnn %>% 
  select(id, word, 제목, 연도) %>% 
  filter(연도 == "2014") %>% 
  add_count(word) %>% 
  filter(n >= 7) %>% 
  pairwise_cor(item = word,
               feature = id,
               sort = TRUE)

graph_cor_14 <- 암호비트가상_cor_14 %>% 
  filter(correlation >= 0.5) %>% 
  as_tbl_graph(directed = FALSE) %>% 
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))

set.seed(1029)

ggraph(graph_cor_14, layout = "fr") +
  
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


# 2015
암호비트가상_cor_15 <- 암호비트가상_tb_pnn %>% 
  select(id, word, 제목, 연도) %>% 
  filter(연도 == "2015") %>% 
  add_count(word) %>% 
  filter(n >= 5) %>% 
  pairwise_cor(item = word,
               feature = id,
               sort = TRUE)

graph_cor_15 <- 암호비트가상_cor_15 %>% 
  filter(correlation >= 0.5) %>% 
  as_tbl_graph(directed = FALSE) %>% 
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))

set.seed(1029)

ggraph(graph_cor_15, layout = "fr") +
  
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


# 2016
암호비트가상_cor_16 <- 암호비트가상_tb_pnn %>% 
  select(id, word, 제목, 연도) %>% 
  filter(연도 == "2016") %>% 
  add_count(word) %>% 
  filter(n >= 5) %>% 
  pairwise_cor(item = word,
               feature = id,
               sort = TRUE)

graph_cor_16 <- 암호비트가상_cor_16 %>% 
  filter(correlation >= 0.5) %>% 
  as_tbl_graph(directed = FALSE) %>% 
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))

set.seed(1029)

ggraph(graph_cor_16, layout = "fr") +
  
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


# 2017
암호비트가상_cor_17 <- 암호비트가상_tb_pnn %>% 
  select(id, word, 제목, 연도) %>% 
  filter(연도 == "2017") %>% 
  add_count(word) %>% 
  filter(n >= 10) %>% 
  pairwise_cor(item = word,
               feature = id,
               sort = TRUE)

graph_cor_17 <- 암호비트가상_cor_17 %>% 
  filter(correlation >= 0.5) %>% 
  as_tbl_graph(directed = FALSE) %>% 
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))

set.seed(1029)

ggraph(graph_cor_17, layout = "fr") +
  
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


# 2018
암호비트가상_cor_18 <- 암호비트가상_tb_pnn %>% 
  select(id, word, 제목, 연도) %>% 
  filter(연도 == "2018") %>% 
  add_count(word) %>% 
  filter(n >= 15) %>% 
  pairwise_cor(item = word,
               feature = id,
               sort = TRUE)

graph_cor_18 <- 암호비트가상_cor_18 %>% 
  filter(correlation >= 0.5) %>% 
  as_tbl_graph(directed = FALSE) %>% 
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))

set.seed(1029)

ggraph(graph_cor_18, layout = "fr") +
  
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


# 2019
암호비트가상_cor_19 <- 암호비트가상_tb_pnn %>% 
  select(id, word, 제목, 연도) %>% 
  filter(연도 == "2019") %>% 
  add_count(word) %>% 
  filter(n >= 8) %>% 
  pairwise_cor(item = word,
               feature = id,
               sort = TRUE)

graph_cor_19 <- 암호비트가상_cor_19 %>% 
  filter(correlation >= 0.5) %>% 
  as_tbl_graph(directed = FALSE) %>% 
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))

set.seed(1029)

ggraph(graph_cor_19, layout = "fr") +
  
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


# 2020
암호비트가상_cor_20 <- 암호비트가상_tb_pnn %>% 
  select(id, word, 제목, 연도) %>% 
  filter(연도 == "2020") %>% 
  add_count(word) %>% 
  filter(n >= 10) %>% 
  pairwise_cor(item = word,
               feature = id,
               sort = TRUE)

graph_cor_20 <- 암호비트가상_cor_20 %>% 
  filter(correlation >= 0.5) %>% 
  as_tbl_graph(directed = FALSE) %>% 
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))

set.seed(1029)

ggraph(graph_cor_20, layout = "fr") +
  
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


# 2021
암호비트가상_cor_21 <- 암호비트가상_tb_pnn %>% 
  select(id, word, 제목, 연도) %>% 
  filter(연도 == "2021") %>% 
  add_count(word) %>% 
  filter(n >= 15) %>% 
  pairwise_cor(item = word,
               feature = id,
               sort = TRUE)

graph_cor_21 <- 암호비트가상_cor_21 %>% 
  filter(correlation >= 0.5) %>% 
  as_tbl_graph(directed = FALSE) %>% 
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))

set.seed(1029)

ggraph(graph_cor_21, layout = "fr") +
  
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