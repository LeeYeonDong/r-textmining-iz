### 01-1 텍스트 전처리
## 연설문 불러오기
fil = "D:/대학원/강의/2024-2 실험계획법/Doit_textmining-main/Data/speech_moon.txt"

# 가장 기본적인 방법으로, 텍스트 파일을 줄 단위로 읽어오는 방법
raw_moon <- readLines(con = fil, encoding = "UTF-8")

# 파일을 테이블 형태로 읽어올 수 있습니다. 이 방법은 파일에 구분자가 있을 때 유용합니다.
raw_moon <- read.table(file = fil, header = FALSE, sep = "\n", fileEncoding = "UTF-8")

# CSV 파일 형식처럼 텍스트 파일을 읽을 수 있습니다. 기본적으로 콤마(,) 구분자를 사용하지만, 구분자를 변경할 수 있습니다.
raw_moon <- read.csv(file = fil, header = FALSE, sep = "\n", fileEncoding = "UTF-8")

# 텍스트 파일을 더 세밀하게 제어하여 읽어올 수 있습니다. 예를 들어, 공백을 기준으로 데이터를 나누어 읽는 데 유용합니다.
library(readr)
raw_moon <- read_file(file = fil)

# data.table 패키지를 사용하여 파일을 빠르고 효율적으로 읽을 수 있습니다.
library(data.table)
raw_moon <- fread(file = fil, header = FALSE, encoding = "UTF-8", sep = "\n")

library(tidyverse)
raw_moon %>% head()


## 불필요한 문자 제거하기
txt <- "치킨은!! 맛있다. xyz 정말 맛있다!@#"

library(stringr)

str_replace_all(string = txt, pattern = "[^가-힣]", replacement = " ") # 한글 제외 모두 지우기

str_replace_all(string = txt, pattern = "[^가-힣]", replacement = "A")

# 1. !@만 지우기
str_remove_all(string = txt, pattern = "[!@]")

# 2. # 지우기
str_remove_all(string = txt, pattern = "#")

# 3. 특수문자는 모두 지우기
str_remove_all(string = txt, pattern = "[^\\w\\s가-힣]")

# 4. 문장부호 지우기
str_remove_all(string = txt, pattern = "[[:punct:]]")

# 5. 영어 모두 지우기
str_remove_all(string = txt, pattern = "[a-zA-Z]")

# 6. 모든 공백은 공백 1칸으로 만들기
txt1 <- str_replace_all(string = txt, pattern = "[^가-힣]", replacement = " ")
str_squish(string = txt1)

str_replace_all(string = raw_moon, pattern = "[^가-힣]", replacement = " ")

moon <- raw_moon %>% 
  str_replace_all(pattern = "[^가-힣]", replacement = " ")


## 데이터를 tibble 구조로 바꾸기
library(dplyr)
moon <- moon %>% as_tibble()
moon

# # tibble 구조
# - 한 행에 한 단락이 들어있음
# - 긴 문장은 Console 창에서 보기 편할 만큼 일부만 출력
# - 행과 열의 수를 알 수 있음
# - 변수의 자료형을 알 수 있음

## 전처리 작업 한 번에 하기
raw_moon %>% head(10)

moon <- raw_moon %>% 
  str_remove_all(pattern = "[^가-힣\\s]") %>%  
  # 한글과 공백만 남기기
  str_squish() %>%# 공백을 1칸으로 줄이기
  as_tibble() %>% 
  filter(value != "")  # 빈 문자열 제거

moon

### 01-2 토큰화하기
# # 토큰화(tokenization)
# - 토큰(token): 텍스트의 기본 단위(ex: 단락, 문장, 단어, 형태소)
# - 토큰화: 텍스트를 토큰으로 나누는 작업
text <- tibble(value = "대한민국은 민주공화국이다. 대한민국의 주권은 국민에게 있고, 모든 권력은 국민으로부터 나온다.")

text

library(tidytext)

text %>%
  unnest_tokens(input = value,        # 토큰화할 텍스트
                output = word,        # 토큰을 담을 변수명
                token = "sentences")  # 문장 기준

text %>%
  unnest_tokens(input = value,   
                output = word,   
                token = "words")  # 단어 기준

text %>%
  unnest_tokens(input = value,   
                output = word,   
                token = "characters")  # 문자 기준


word_space <- moon %>%
  unnest_tokens(input = value,   
                output = word,   
                token = "words")

### 01-3 단어 빈도 분석하기
## 단어 빈도 구하기 - `count()`
word_space <- word_space %>%
  count(word, sort = TRUE)

## 한 글자로 된 단어 제거하기 - `fitler(str_count())`
## `str_count()` 문자열의 글자 수 구하기
str_count("배")
str_count("사과")

## 두 글자 이상만 남기기
word_space <- word_space %>%
  filter(str_count(word) > 1)

## 한 번에 작업하기
word_space <- moon %>%
  unnest_tokens(input = value,   
                output = word,   
                token = "words") %>%
  count(word, sort = TRUE) %>%
  filter(str_count(word) > 1)

## 자주 사용된 단어 추출하기
top20 <- word_space %>%
  head(20)

## 막대 그래프 만들기 - `geom_col()`
library(ggplot2)

ggplot(top20, aes(x = reorder(word, n), y = n)) +  # 단어 빈도순 정렬
  geom_col() +
  coord_flip() 

# 그래프 다듬기
top20

# 빈도별 미정렬
ggplot(top20, aes(x = word, y = n, fill = word)) +
  geom_col() +
  geom_text(aes(label = n), hjust = -0.3) +            # 막대 밖 빈도 표시
  labs(title = "문재인 대통령 출마 연설문 단어 빈도",  # 그래프 제목
       x = NULL, y = NULL) +                           # 축 이름 삭제
  theme(title = element_text(size = 12),
        legend.position = "none") +                    # 범례 제거
  scale_fill_manual(values = scales::hue_pal()(20))    # 20가지 색상을 적용


# 빈도별 오름차순 정렬
ggplot(top20, aes(x = reorder(word, n), y = n, fill = word)) +
  geom_col() +
  geom_text(aes(label = n), hjust = -0.3) +            
  # 막대 밖 빈도 표시
  labs(title = "문재인 대통령 출마 연설문 단어 빈도",  
       # 그래프 제목
       x = NULL, y = NULL) +                          
  # 축 이름 삭제
  theme(title = element_text(size = 12),
        legend.position = "none") +                    
  # 범례 제거
  scale_fill_manual(values = scales::hue_pal()(20))    
# 20가지 색상을 적용

# 빈도별 내림차순 정렬
ggplot(top20, aes(x = reorder(word, -n), y = n, fill = word)) +
  geom_col() +
  geom_text(aes(label = n), hjust = -0.3) +            
  # 막대 밖 빈도 표시
  labs(title = "문재인 대통령 출마 연설문 단어 빈도",  
       # 그래프 제목
       x = NULL, y = NULL) +                          
  # 축 이름 삭제
  theme(title = element_text(size = 12),
        legend.position = "none") +                    
  # 범례 제거
  scale_fill_manual(values = scales::hue_pal()(20))    
# 20가지 색상을 적용


# https://smbar.tistory.com/62 색상 지정
top20 %>% 
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "Steel Blue") +                       # 단일 색상 적용
  geom_text(aes(label = n), hjust = -0.3) +          # 막대 밖 빈도 표시
  labs(title = "문재인 대통령 출마 연설문 단어 빈도",  # 그래프 제목
       x = NULL, y = NULL) +                         # 축 이름 삭제
  theme(title = element_text(size = 12), 
        legend.position = "none")                    # 범례 제거

# coord_flip()
ggplot(top20, aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "steel Blue") +                       # 단일 색상 적용
  geom_text(aes(label = n), hjust = -0.3) +          # 막대 밖 빈도 표시
  labs(title = "문재인 대통령 출마 연설문 단어 빈도",  # 그래프 제목
       x = NULL, y = NULL) +                         # 축 이름 삭제
  theme(title = element_text(size = 12), 
        legend.position = "none")  +  # 범례 제거
  coord_flip()


## 워드 클라우드 만들기 - `geom_text_wordcloud()`
#시간이 오래 걸림
library(ggwordcloud)

word_space %>% 
  ggplot(aes(label = word, size = n)) +
  geom_text_wordcloud(seed = 1234) +
  scale_radius(limits = c(3, NA),    
               # 최소, 최대 단어 빈도
               range = c(3, 30))   


# 워드 클라우드 생성
library(wordcloud2)

wordcloud2(data = word_space, size = 1.0, color = "random-light")

# 빈도수 기준
word_space %>%
  filter(n >= 5) %>% 
  wordcloud2(size = 1.0, color = "#4682b4", rotateRatio = 0)

word_space %>%
  filter(n >= 5) %>% 
  wordcloud2(size = 1.0, color = "#4682b4", rotateRatio = 0) 


# 순위 기준
word_space %>%
  head(20) %>% 
  wordcloud2(size = 1.0, color = "#4682b4", rotateRatio = 0)

# 빈도별 색상
library(scales)
colors <- scales::gradient_n_pal(c("#66aaf2", "#004EA1"))(
  rescale(word_space$n)
)
word_space %>%
  head(20) %>% 
  wordcloud2(size = 1.0, rotateRatio = 0, color = colors)



