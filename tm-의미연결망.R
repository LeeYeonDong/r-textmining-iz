
library(base64enc)
library(arules)
library(KoNLP)
library(NIADic)
library(RmecabKo)
library(tidyverse)
library(igraph)

#### KoNLP 사전작업

install_mecab("C:/Rlibs/mecab") 
devtools::install_github('haven-jeon/NIADic/NIADic', build_vignettes = TRUE)
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_241') 
buildDictionary(ext_dic = "woorimalsam") 
useNIADic()



#### 텍스트 파일 불러오기 
fiesta <- readLines("D:/fiesta.txt")


#### 한글 - 품사별 처리

fiesta <- fiesta  %>% SimplePos09()  


#### 데이터 셋 만들기

fiesta <- fiesta %>%   
  melt() %>%  
  as_tibble() %>% 
  select(3,1)    ## 3열과 1열 추출


#### 명사 용언 수식언만 추출하기
## 명사 추출

fiesta_명사 <- fiesta %>% 
  mutate(명사=str_match(value,'([가-힣]+)/N')[,2]) %>%     ## "명사" variable을 만들고 한글만 저장                  
  
  na.omit() %>%                                           ## ([가-힣]+)/P') 한글 중 용언(P)만을 선택하는 정규표현식
  
  mutate(글자수=str_length(명사)) %>%   ## "글자수" variable을 만듭니다                                  
  
  filter(str_length(명사)>=2)                              ## 2글자 이상만 추려냅니다


## 용언 추출

fiesta_용언 <- fiesta %>% 
  mutate(용언=str_match(value,'([가-힣]+)/P')[,2]) %>%   ## "용언" variable을 만들고 한글만 저장 
  na.omit() %>%                           ## ([가-힣]+)/P') 한글 중 용언(P)만을 선택하는 정규표현식
  
  mutate(글자수=str_length(용언)) %>%        ## "글자수" variable을 만듭니다 
  filter(str_length(용언)>=2)                             ##  2글자 이상만 추려냅니다


## 수식언 추출

fiesta_수식언 <- fiesta %>% 
  mutate(수식언=str_match(value,'([가-힣]+)/M')[,2]) %>%    ## "수식언" variable을 만들고 한글만 저장
  na.omit() %>%                                ## ([가-힣]+)/M') 한글 중 수식언(M)만을 선택하는 정규표현식
  mutate(글자수=str_length(수식언)) %>%  ## "글자수" variable을 만듭니다 
  filter(str_length(수식언)>=2)                 ##  2글자 이상만 추려냅니다


#### 품사 추출 파일을 모아 데이터 프레임(Data Frame)으로 만들기

fiesta_의미 <- bind_rows(fiesta_명사, 
                       fiesta_용언, 
                       fiesta_수식언)


View(fiesta_의미)


#### 품사별 추출

fiesta_의미_명사 <- fiesta_의미 %>% 
  select(3, 1) %>%                      ## 3열(명사)과 1열 추출
  na.omit() 

fiesta_의미_용언 <- fiesta_의미 %>% 
  select(5, 1) %>%                        ## 5열(용언)과 1열 추출
  na.omit() 

fiesta_의미_수식언 <-fiesta_의미 %>% 
  select(6, 1) %>%                        ## 6열(수식언)과 1열 추출
  na.omit()


#### 품사별 글자수를 "단어"로 통합

fiesta_의미_명사 <- rename(fiesta_의미_명사,
                       c(단어 = 명사))

## 명사, 용언, 수식언을 "단어"변수로 통합하기 위해 변수명 "단어"로 변경


fiesta_의미_용언 <- rename(fiesta_의미_용언,
                       c(단어 = 용언))

## 명사, 용언, 수식언을 "단어"변수로 통합하기 위해 변수명 "단어"로 변경


fiesta_의미_수식언 <- rename(fiesta_의미_수식언,
                        c(단어 = 수식언))

## 명사, 용언, 수식언을 "단어"변수로 통합하기 위해 변수명 "단어"로 변경

fiesta_의미_단어 <- bind_rows(fiesta_의미_명사,
                          fiesta_의미_용언,
                          fiesta_의미_수식언)

## 변경한 변수명 "단어"로 기준으로 통합



#### 그래프 그리기

단어_의미df <- fiesta_의미_단어 %>% graph_from_data_frame() 

## igraph 형태에 맞게 데이터프레임을 변환합니다



## matrix로 변환
단어_의미m  <- as_incidence_matrix(단어_의미df ) %*% t(as_incidence_matrix(단어_의미df))   ## matrix로 변환 합니다


## 주대각선을 "0"으로 처리
diag(단어_의미m) <- 0  


## 인접 행렬(adjacency matrix)로 변환
단어_의미m <- 단어_의미m %>% graph_from_adjacency_matrix() 



#### 시각화
단어_의미m %>% plot() 

단어_의미m %>%   
  as_tbl_graph() %>% 
  ggraph() + 
  geom_edge_link(aes(start_cap = label_rect(node1.name), end_cap = label_rect(node2.name))) + 
  geom_node_text(aes(label=name))

