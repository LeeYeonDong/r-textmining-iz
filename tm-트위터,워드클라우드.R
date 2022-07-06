필요 라이브러리(library)

library(base64enc)

library(KoNLP)

library(NIADic)

library(RmecabKo)

library(rtweet)

library(tidyverse)

library(igraph)

library(twitteR)





#### KoNLP 사전작업

install_mecab("C:/Rlibs/mecab") 
devtools::install_github('haven-jeon/NIADic/NIADic', build_vignettes = TRUE)
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_241') 
buildDictionary(ext_dic = "woorimalsam") 
useNIADic()


#### 텍스트 파일 추출
izone_ko_word <- sapply(izone_ko,function(t) t$getText()) 


#### 한글 - 품사별 처리
izone_ko_words <- izone_ko_word %>% SimplePos09() 



#### 데이터 셋 만들기
izone_ko_words <- izone_ko_words %>%   
  melt() %>%  
  as_tibble() %>% 
  select(3,1)    ## 3열과 1열 추출 



#### 명사 용언 수식언만 추출하기
## 명사 추출
izone_ko_명사 <- izone_ko_words %>%  
  mutate(명사=str_match(value,'([가-힣]+)/N')[,2]) %>%      ## "명사" variable을 만들고 한글만 저장                   
  
  na.omit() %>%                                      ## ([가-힣]+)/P') 한글 중 명사(N)만을 선택하는 정규표현식
  
  mutate(글자수=str_length(명사)) %>%   ## "글자수" variable을 만듭니다 
  
  filter(str_length(명사)>=2)                               ## 2글자 이상만 추려냅니다




## 용언 추출

izone_ko_용언 <- izone_ko_words %>%  
  mutate(용언=str_match(value,'([가-힣]+)/P')[,2]) %>%   ## "용언" variable을 만들고 한글만 저장 
  na.omit() %>%                           ## ([가-힣]+)/P') 한글 중 용언(P)만을 선택하는 정규표현식
  
  mutate(글자수=str_length(용언)) %>%        ## "글자수" variable을 만듭니다 
  filter(str_length(용언)>=2)                         ##  2글자 이상만 추려냅니다


## 수식언 추출

izone_ko_수식언 <- izone_ko_words %>%  
  mutate(수식언=str_match(value,'([가-힣]+)/M')[,2]) %>%    ## "수식언" variable을 만들고 한글만 저장
  na.omit() %>%                                ## ([가-힣]+)/M') 한글 중 수식언(M)만을 선택하는 정규표현식
  mutate(글자수=str_length(수식언)) %>%  ## "글자수" variable을 만듭니다 
  filter(str_length(수식언)>=2)                 ##  2글자 이상만 추려냅니다




#### 데이터 전처리

##명사 전처리 

izone_ko.명사 <- izone_ko_명사$명사 
izone_ko.명사 <- izone_ko.명사 %>% unlist() 
izone_ko.명사 <- izone_ko.명사 %>% as.vector() 
izone_ko.명사 <- str_replace_all(izone_ko.명사, "\\^","")          ## 특수문자를 처리합니다
izone_ko.명사 <- str_replace_all(izone_ko.명사, "^.{1}$","")         ## 혹시라도  들어갈 수 있는 한글자를 처리합니다
izone_ko.명사 <- str_replace_all(izone_ko.명사, "\\d+","") 
izone_ko.명사 <- izone_ko.명사 %>% as.list() 
izone_ko.명사[izone_ko.명사 ==""] <- NULL 
izone_ko.명사 <- izone_ko.명사 %>% unlist()                         ## 공백을 제거합니다
izone_ko.명사 <- izone_ko.명사 %>% as.data.frame()


##용언 전처리

izone_ko.용언 <- izone_ko_용언$용언 
izone_ko.용언 <- izone_ko.용언 %>% unlist() 
izone_ko.용언 <- izone_ko.용언 %>% as.vector() 
izone_ko.용언 <- str_replace_all(izone_ko.용언, "\\^","")       ## 특수문자를 처리합니다
izone_ko.용언 <- str_replace_all(izone_ko.용언, "^.{1}$","")      ## 혹시라도  들어갈 수 있는 한글자를 처리합니다
izone_ko.용언 <- izone_ko.용언 %>% as.list() 
izone_ko.용언[izone_ko.용언 ==""] <- NULL 
izone_ko.용언 <- izone_ko.용언 %>% unlist()                    ## 공백을 제거합니다
izone_ko.용언 <- izone_ko.용언 %>% as.data.frame() 


##수식언 전처리

izone_ko.수식언 <- izone_ko_수식언$수식언 
izone_ko.수식언 <- izone_ko.수식언 %>% unlist() 
izone_ko.수식언 <- izone_ko.수식언 %>% as.vector() 
izone_ko.수식언 <- str_replace_all(izone_ko.수식언, "\\^","")    ## 특수문자를 처리합니다
izone_ko.수식언 <- str_replace_all(izone_ko.수식언, "^.{1}$","")   ## 혹시라도  들어갈 수 있는 한글자를 처리합니다
izone_ko.수식언 <- izone_ko.수식언 %>% as.list() 
izone_ko.수식언[izone_ko.수식언 ==""] <- NULL 
izone_ko.수식언 <- izone_ko.수식언 %>% unlist()                ## 공백을 제거합니다
izone_ko.수식언 <- izone_ko.수식언 %>% as.data.frame()          



#### 명사 용언 수식언을 묶어서 하나로 만듭니다
izone_ko <- bind_rows(izone_ko.명사,izone_ko.용언,izone_ko.수식언)


#### 최다 빈도 단어 Top30을 뽑습니다 시각화에 적합한 수준으로 데이터를 추려봅니다

izone_ko_count <- table(izone_ko)                       ## 객체별 빈도를 셉니다
izone_ko_count <- sort(izone_ko_count, decreasing = TRUE)         ##내림차순 정렬 합니다

izone_ko_count30 <- izone_ko_count[1:30]            ## Top 30까지 추립니다


#### 빈도그래프 작성
izone_ko_count30df <- izone_ko_count30 %>% as.data.frame()             ## data frame변환하고 그래프 작성 
ggplot(izone_ko_count30df, aes(x=izone_ko, y=Freq)) + geom_bar(stat="identity")


#### 워드크라우드 작성
izone_ko_count %>% wordcloud2()       

## 최다 빈출 단어 제거
izone_ko_count[2:length(izone_ko_count)] %>% wordcloud2()   