# 주석 : ctrl + shift + c

# KoNLP useNIADic
install.packages('KoNLP', repos = 'https://forkonlp.r-universe.dev')

install.packages("multilinguer")
library(multilinguer)
install_jdk()
install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), type = "binary")
#
install.packages("remotes")
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))
library(KoNLP) #최종적으로 "KoNLP" 패키지를 불러옵니다

# devtools::install_github('haven-jeon/NIADic/NIADic', build_vignettes = TRUE) 위 명령어로 안되면 실행


# https://r-pyomega.tistory.com/6 
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_281')  # 설치한 JAVA version에 따라 달라집니다

buildDictionary(ext_dic = "woorimalsam")  # "woorimalsam" dic을 불러옵니다
useNIADic() 
# Use the Sejong Dictionary
useSejongDic()

## 에러가 발생 할 경우
# 1. https://repo1.maven.org/maven2/org/scala-lang/scala-library/2.11.8/scala-library-2.11.8.jar 여기에 접속하셔서 <scala-library-2.11.8> 파일을 다운
# 2. C:\Users\선생님 노트북 이름\AppData\Local\R\win-library\4.2\KoNLP\java에 가셔서 다운받은 scala-library-2.11.8 파일을 붙여넣기한다
# 3. C:\Program Files\R\R-4.2.2\library 로 간다
# 4. 아래 KoNLP 다운받아 C:\Program Files\R\R-4.2.2\library에 붙여넣은 다음 KoNLP폴더를 압축을 푼다
# 5. R studio를 모두 끄고 다시 실행한다


#### 02-1 형태소 분석

# # 토큰별 품사가 필요한 이유
# 1. 대부분 중요 토큰은 영어 : 동사, 명사, 형용사
# 한글 : 용언(동사, 형용사), 명사
# 2. 불필요한 데이터 전처리
# 3. 품사별 비교 분석 연구

library(KoNLP)
library(dplyr)
library(tibble)

# KoNLP를 사용하여 simplePos09로 토큰화
text1 <- tibble(value = c("대한민국은 민주공화국이다. 대한민국의 주권은 국민에게 있고, 모든 권력은 국민으로부터 나온다."))

text1 %>% SimplePos09() # 리스트 형태

text_simplePos <- text %>%
  mutate(word = map(value, function(x) {
    # lapply()와 유사하게 리스트 형태의 데이터를 처리합니다.
    words <- SimplePos09(x)
    words <- unlist(strsplit(as.character(words), "\\+"))  # 토큰화된 단어들을 +로 분리
    words  # 품사 정보 유지
  })) %>%
  unnest(word) %>%
  separate(word, into = c("word", "Pos09"), sep = "/")

text_simplePos %>%
  filter(Pos09 %in% c("V", "PV", "PA", "N"))
# 더욱 세부적인 품사 분류 함수 SimplePos22()


## 형태소 분석기를 이용해 토큰화하기 - 명사 추출
text <- tibble(value = c("대한민국은 민주공화국이다", " 대한민국의 주권은 국민에게 있고, 모든 권력은 국민으로부터 나온다."))

text1 <- tibble(value = c("대한민국은 민주공화국이다. 대한민국의 주권은 국민에게 있고, 모든 권력은 국민으로부터 나온다."))

text$value %>% extractNoun()


## `unnest_tokens()`를 이용해 명사 추출하기
library(tidytext)

## 명사 추출
text %>%
  unnest_tokens(input = value,        # 분석 대상
                output = word,        # 출력 변수명
                token = extractNoun)  # 토큰화 함수 

## 띄어쓰기 기준 추출
text %>%
  unnest_tokens(input = value,    
                output = word,    
                token = "words")

# value 변수 남기기
text %>%
  unnest_tokens(input = value,    
                output = word,    
                token = "words", 
                drop = FALSE)

text1 %>%
  mutate(word = map(value, extractNoun)) %>%  # 명사 추출
  unnest(word)  # 리스트를 풀어서 단어별로 행을 생성

text1 %>%
  unnest_tokens(input = value,    
                output = word,    
                token = "words")


# map() 
# map() 함수는 purrr 패키지에서 제공하는 함수로, 주어진 벡터나 리스트의 각 요소에 대해 지정한 함수를 적용하는 데 사용됩니다. map() 함수는 리스트를 반환하며, 이를 통해 데이터를 일관성 있게 처리할 수 있습니다.

# map(.x, .f, ...)
# .x: 입력 벡터 또는 리스트.
# .f: 적용할 함수. 익명 함수 또는 함수의 이름이 올 수 있습니다.
# ...: 추가적인 인자들. .f에 전달될 수 있는 추가 인자들입니다.

texts <- list("apple", "banana", "cherry")
map(texts, toupper)



## 연설문 불러오기
fil = "D:/대학원/강의/2024-2 실험계획법/Doit_textmining-main/Data/speech_moon.txt"

raw_moon <- readLines(con = fil, encoding = "UTF-8")

## 기본적인 전처리
moon <- raw_moon %>% 
  str_remove_all(pattern = "[^가-힣\\s]") %>%  
  # 한글과 공백만 남기기
  str_squish() %>%# 공백을 1칸으로 줄이기
  as_tibble() %>% 
  filter(value != "")  # 빈 문자열 제거

word_noun <- moon %>%
  unnest_tokens(input = value,
                output = word,
                token = extractNoun)

word_noun


#### 02-2 명사 빈도 분석하기
### 단어 빈도 구하기
## 띄어쓰기 기준 추출
moon %>%
  unnest_tokens(input = value,
                output = word,
                token = "words") %>%
  count(word, sort = T) %>%
  filter(str_count(word) > 1)

## 명사 추출
moon %>%
  unnest_tokens(input = value,
                output = word,
                token = extractNoun) %>%
  count(word, sort = T) %>%
  filter(str_count(word) > 1)


### 막대 그래프 만들기
word_noun

library(ggplot2)

word_noun %>% 
  head(20) %>% 
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "Steel Blue") + # 막대 그래프
  coord_flip() + # x와 y 축을 뒤집어 그래프를 수평으로 회전
  geom_text(aes(label = n), hjust = -0.3) + # 막대 위 또는 옆에 해당 빈도수(n)를 텍스트로 표시. hjust = -0.3으로 텍스트가 막대 밖에 약간 떨어져 표시
  labs(x = NULL) + # x 축의 라벨을 제거
  theme(text = element_text(family = "nanumgothic")) +
theme(title = element_text(size = 12), 
      legend.position = "none") # 그래프 이름, 범례 제거


#### 02-3 특정 단어가 사용된 문장 살펴보기
sentences_moon <- raw_moon %>%
  str_squish() %>%
  as_tibble() %>%
  unnest_tokens(input = value,
                output = sentence,
                token = "sentences")

## 특정 단어가 사용된 문장 추출하기
sentences_moon %>% 
  filter(str_detect(sentence, "국민"))

## 실제 가장 많이 쓰는 데이터프레임 토큰화 폼
raw_moon %>% head(10)

raw_moon %>% 
  str_remove_all(pattern = "[^가-힣\\s]") %>%  
  # 한글과 공백만 남기기
  str_squish() %>%# 공백을 1칸으로 줄이기
  as_tibble() %>% 
  filter(value != "") %>%   # 빈 문자열 제거
  mutate(id = row_number()) %>% 
  unnest_tokens(input = value, 
                output = word, 
                token = "words", 
                drop = FALSE) %>% 
  select(id, value, word)








