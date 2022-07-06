

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
  select(3,1)              ## 3열과 1열 추출


#### 명사 용언 수식언만 추출하기
## 명사 추출

izone_ko_명사 <- izone_ko_words %>% 
  mutate(명사=str_match(value,'([가-힣]+)/N')[,2]) %>%  ## "명사" variable을 만들고 한글만 저장                    
  
  na.omit() %>%                                     
  
  filter(str_length(명사)>=2)                                               ## 2글자 이상만 추려냅니다



## 용언 추출

izone_ko_용언 <- izone_ko_words %>% 
  mutate(용언=str_match(value,'([가-힣]+)/P')[,2]) %>%   ## "용언" variable을 만들고 한글만 저장 
  na.omit() %>%                                                                 ## ([가-힣]+)/P') 한글 중 용언(P)만을 선택하는 정규표현식
  
  mutate(글자수=str_length(용언)) %>%                           ## "글자수" variable을 만듭니다 
  filter(str_length(용언)>=2)                                                ##  2글자 이상만 추려냅니다



## 수식언 추출

izone_ko_수식언 <- izone_ko_words %>% 
  mutate(수식언=str_match(value,'([가-힣]+)/M')[,2]) %>%   ## "수식언" variable을 만들고 한글만 저장
  na.omit() %>%                                                                    ## ([가-힣]+)/M') 한글 중 수식언(M)만을 선택하는 정규표현식
  mutate(글자수=str_length(수식언)) %>%                              ## "글자수" variable을 만듭니다 
  filter(str_length(수식언)>=2)                                                 ##  2글자 이상만 추려냅니다




#### 품사 추출 파일을 모아 데이터 프레임(Data Frame)으로 만들기

izone_ko_연관 <- bind_rows(izone_ko_명사, 
                         izone_ko_용언, 
                         izone_ko_수식언)


View(izone_ko_연관)


#### 품사별 추출
izone_ko_연관_명사 <- izone_ko_연관 %>%  
  select(3, 1) %>%                      ## 3열(명사)과 1열 추출
  na.omit() 

izone_ko_연관_용언 <- izone_ko_연관 %>%  
  select(5, 1) %>%                        ## 5열(용언)과 1열 추출
  na.omit() 

izone_ko_연관_수식언 <- izone_ko_연관 %>%  
  select(6, 1) %>%                        ## 6열(수식언)과 1열 추출
  na.omit() 


#### 품사별 글자수를 "단어"로 통합
izone_ko_연관_명사 <- rename(izone_ko_연관_명사, 
                         c(단어 = 명사))

## 명사, 용언, 수식언을 "단어"변수로 통합하기 위해 변수명 "단어"로 변경
izone_ko_연관_용언 <- rename(izone_ko_연관_용언, 
                         c(단어 = 명사))

## 명사, 용언, 수식언을 "단어"변수로 통합하기 위해 변수명 "단어"로 변경
izone_ko_연관_수식언 <- rename(izone_ko_연관_수식언, 
                          c(단어 = 명사))

## 명사, 용언, 수식언을 "단어"변수로 통합하기 위해 변수명 "단어"로 변경
izone_ko_연관_단어 <- bind_rows(izone_ko_연관_명사, 
                            izone_ko_연관_용언, 
                            izone_ko_연관_수식언)


#### 연관분석 그래프에 맞게 리스트 만들기

izone_ko_연관_단어df <- split(izone_ko_연관_단어,izone_ko_연관_단어$L1)      ## 리스트를 "열(column)별로 쪼갭니다
izone_ko_연관_단어list <- lapply(izone_ko_연관_단어df, function(x){ 
  return(x$단어)})                                                                   ## 아래의 그림처럼 리스트를 만듭니다

rd_n <- 1:length(raw_list) 
rd_n <- rd_n %>% sample(n, replace = FALSE)   ## n = 추출하고 싶은 칼럼 개수
raw_list_n <- raw_list[rd_n]


## transactions 생성

names(rd_n_list) <- paste("Tr", 1:length(rd_n_list), sep="") 
rd_n_list_tran <- as(rd_n_list, "transactions")              ##  중복데이터가 있으면 error가 발생합니다


trans <- izone_ko_tran %>% crossTable() 
View(trans)             


## apriori 함수를 사용

izone_ko_tran_apr <- apriori(izone_ko_tran, parameter = list(supp=0.05, conf=0.05))
izone_ko_tran_apr %>% summary()




## 데이터 구조 변경 : 연관규칙 결과 -> 행렬구조 변경(matrix 또는 data.frame)

rd_n_list_rul <- izone_ko_tran_apr %>%  labels(ruleSep=" ")  ## labels 함수로 rules변수의 내용을 입력


## rules변수의 내용을 리스트 구조로 변경 
rd_n_list_rul <- sapply(rd_n_list_rul, strsplit, " ",USE.NAMES=F) 


## 행 단위로 묶어서 matrix로 반환 
rd_n_list_mat <- do.call("rbind", rd_n_list_rul) 


## 그래프를 그리기 위해 데이터 변환(graph.edgelist 함수)
rd_n_list_mat_rulg <- rd_n_list_mat %>% graph.edgelist(directed=FALSE)  


## 그래프 그리기

plot.igraph(rd_n_list_mat_rulg, 
            
            vertex.label=V(rd_n_list_mat_rulg)$name, 
            vertex.label.cex=1.5,
            
            vertex.label.color='#000000',  
            vertex.size=30,
            
            vertex.color='#E85100',
            
            vertex.frame.color='#E85100')