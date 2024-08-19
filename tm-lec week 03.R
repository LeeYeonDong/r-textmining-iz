#### 03-1 단어 빈도 비교하기
### 데이터 불러오기
## 문재인 대통령 연설문 불러오기
library(KoNLP)
library(dplyr)
library(tibble)
library(tidyverse)
library(dplyr)
library(tidytext)

moon <- readLines("D:/대학원/강의/2024-2 실험계획법/Doit_textmining-main/Data/speech_moon.txt", encoding = "UTF-8") %>%
  as_tibble() %>%
  mutate(president = "moon")

# 박근혜 대통령 연설문 불러오기
park <- readLines("D:/대학원/강의/2024-2 실험계획법/Doit_textmining-main/Data/speech_park.txt", encoding = "UTF-8") %>%
  as_tibble() %>%
  mutate(president = "park")


### 집단별 단어 빈도 구하기
## 데이터 합치기
bind_speeches <- bind_rows(moon, park) %>%
  select(president, value)

# 기본적인 전처리
library(stringr)

speeches <- bind_speeches %>%
  mutate(value = str_replace_all(value, "[^가-힣\\s]", " "),
         value = str_squish(value)) %>% 
  filter(value != "") 

speeches <- speeches %>%
  unnest_tokens(input = value,
                output = word,
                token = extractNoun, 
                drop = FALSE)


## 두 연설문의 단어 빈도 구하기
freq <- speeches %>%
  count(president, word) %>%   # 연설문 및 단어별 빈도
  filter(str_count(word) > 1) 

## 연설문에 가장 많이 사용된 단어 추출하기
freq %>% glimpse()

top10 <- freq %>% 
  group_by(president) %>% 
  slice_max(n, n = 10)

top10 %>% 
  filter(president == "park")

top10 %>% 
  filter(president == "moon")

## 단어 빈도 동점 처리
freq %>% 
  group_by(president) %>% 
  slice_max(n, n = 10, with_ties = FALSE)


### 막대 그래프 만들기
library(ggplot2)

## 1. 변수의 항목별로 그래프만들기
top10 %>% 
  ggplot(aes(x = reorder(word, n), y = n, fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president)

## 2. 그래프별 y축 설정하기
top10 %>% 
  ggplot(aes(x = reorder(word, n), y = n, fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president, scale = "free_y")

## 3. 특정 단어 제외하고 막대 그래프 만들기
freq %>%
  filter(word != "국민") %>%
  group_by(president) %>%
  slice_max(n, n = 10, with_ties = F) %>% 
  ggplot(aes(x = reorder(word, n), y = n, fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president, scales = "free_y")

## 4. 축 정렬하기
top10 %>% 
ggplot(aes(x = reorder_within(word, n, president), y = n, fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president, scales = "free_y")

## 5. 변수 항목 제거하기
top10 %>% 
  ggplot(aes(x = reorder_within(word, n, president), y = n, fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president, scales = "free_y") +
  scale_x_reordered() +
  labs(x = NULL) +                                    # x축 삭제
  theme(text = element_text(family = "nanumgothic"))


# 수정 버전
top10 %>% 
  ggplot(aes(x = reorder_within(word, n, president), y = n, fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president, scales = "free_y") +
  scale_x_reordered() +  # reorder_within을 사용한 경우 scale_x_reordered() 추가 필요
  labs(x = NULL, y = "빈도", title = "President별 단어 빈도")+
  theme(legend.position = "none")  # 범례를 아래로 이동

top10 %>% 
  ggplot(aes(x = reorder_within(word, n, president), y = n, fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president, scales = "free_y") +
  scale_x_reordered() +  # reorder_within을 사용한 경우 scale_x_reordered() 추가 필요
  labs(x = NULL, y = "빈도", title = "President별 단어 빈도") +
  theme(legend.position = "bottom")  # 범례를 아래로 이동 

top10 %>% 
  ggplot(aes(x = reorder_within(word, n, president), y = n, fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president, scales = "free_y") +
  scale_x_reordered() +  # reorder_within을 사용한 경우 scale_x_reordered() 추가 필요
  labs(x = NULL, y = "빈도", title = "President별 단어 빈도")+
  theme(legend.position = "none") + # 범례를 제거
  theme_minimal()

# theme_bw(), theme_void(), theme_grey(), theme_classic(),
# theme_dark(), theme_light(), theme_linedraw(), 
# theme_minimal()


#### 03-2 오즈비: 상대적으로  중요한 단어 비교하기
## Long form을 Wide form으로 변환하기
df_long <- freq %>%
  group_by(president) %>%
  slice_max(n, n = 10) %>%
  filter(word %in% c("국민", "우리", "정치", "행복"))

library(tidyr)

df_wide <- df_long %>%
  pivot_wider(names_from = president,
              values_from = n)

df_wide <- df_long %>%
  pivot_wider(names_from = president,
              values_from = n,
              values_fill = list(n = 0))


## 연설문 단어 빈도를 Wide form으로 변환하기
freq_wide <- freq %>%
  pivot_wider(names_from = president,
              values_from = n,
              values_fill = list(n = 0))

### 오즈비 구하기
## 1. 단어의 비중을 나타낸 변수 추가하기
freq_wide <- freq_wide %>%
  mutate(ratio_moon  = ((moon + 1)/(sum(moon + 1))),  # moon에서 단어의 비중
         ratio_park  = ((park + 1)/(sum(park + 1))))

# 한번에 odds_ratio 구하기
# freq_wide <- freq %>%
#   pivot_wider(names_from = president,
#               values_from = n,
#               values_fill = list(n = 0)) %>% 
#   mutate(odds_ratio = ((moon + 1)/(sum(moon + 1)))/
#            ((park + 1)/(sum(park + 1))))


## 2. 오즈비 변수 추가하기
freq_wide <- freq_wide %>%
  mutate(odds_ratio = ratio_moon/ratio_park)

# - 오즈비를 보면 단어가 어떤 텍스트에서 상대적으로 더 많이 사용됐는지 알 수 있음
# - "moon"에서 상대적인 비중 클수록 1보다 큰 값
# - "park"에서 상대적인 비중 클수록 1보다 작은 값
# - 두 연설문에서 단어 비중 같으면 1
freq_wide %>%
  arrange(-odds_ratio)

freq_wide %>%
  arrange(-odds_ratio) %>% tail()

# 상대적으로 중요한 단어 추출하기
freq_wide %>%
  filter(rank(odds_ratio) <= 10 | rank(-odds_ratio) <= 10)

freq_wide %>% 
  mutate(ranking = rank(odds_ratio))

freq_wide %>% 
  mutate(ranking = rank(-odds_ratio)) %>% 
  arrange(ranking) # arrange(desc(ranking))

freq_wide %>% 
  mutate(ranking = rank(-odds_ratio)) %>% 
  arrange(desc(ranking))


### 막대 그래프 만들기
## 1. 비중이 큰 연설문을 나타낸 변수 추가하기
top10 <- freq_wide %>%
  filter(rank(odds_ratio) <= 10 | rank(-odds_ratio) <= 10)

top10 <- top10 %>%
  mutate(president = ifelse(odds_ratio > 1, "moon", "park"), n = ifelse(odds_ratio > 1, moon, park))

top10

## 2. 막대 그래프 만들기
## 3. 그래프별로 축 설정하기
top10 %>% 
ggplot(aes(x = reorder_within(word, n, president), y = n, fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president, scales = "free_y") +
  scale_x_reordered() +
  labs(x = NULL) + 
  theme_minimal()

### 주요 단어가 사용된 문장 살펴보기
## 1. 원문을 문장 기준으로 토큰화하기
bind_speeches <- bind_rows(moon, park) %>%
  select(president, value)

speeches_sentence <- bind_speeches %>%
  as_tibble() %>%
  unnest_tokens(input = value,
                output = sentence,
                token = "sentences")

## 2. 주요 단어가 사용된 문장 추출하기 - `str_detect()`
speeches_sentence %>%
  filter(president == "moon" & str_detect(sentence, "복지국가"))

speeches_sentence %>%
  filter(president == "park" & str_detect(sentence, "행복"))

# 중요도가 비슷한 단어 살펴보기
freq_wide %>%
  filter(moon >= 5 & park >= 5) %>%
  arrange(abs(1 - odds_ratio)) %>%
  head(10)


#### 03-3 로그 오즈비로 단어 비교하기
## 로그 오즈비 구하기
# - 부호와 크기를 보면 단어가 어느 연설문에서 더 중요한지 알 수 있음
# - 0보다 큰 양수일수록 `"moon`"에서 비중이 큼
# - 0보다 작은 음수일수록 `"park"`에서 비중이 큼
# - 0에 가까우면 두 연설문에서 비중 비슷함
freq_wide <- freq_wide %>%
  mutate(log_odds_ratio = log(odds_ratio))
freq_wide <- freq_wide %>% 
select(word, moon, park, log_odds_ratio, odds_ratio)

freq_wide %>% 
  arrange(log_odds_ratio)

freq_wide %>% 
  arrange(-log_odds_ratio)

freq_wide %>% 
  arrange(abs(log_odds_ratio))

## 로그 오즈비를 이용해 중요한 단어 비교하기
top10 <- freq_wide %>%
  group_by(president = ifelse(log_odds_ratio > 0, "moon", "park")) %>%
  slice_max(abs(log_odds_ratio), n = 10, with_ties = F)

## 막대 그래프 만들기
top10 %>% 
  ggplot(aes(x = reorder(word, log_odds_ratio), y = log_odds_ratio, fill = president)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL) 


#### 03-4 TF-IDF: 여러 텍스트의 단어 비교하기
## 1. 단어 빈도 구하기
library(readr)
raw_speeches <- read_csv("D:/대학원/강의/2024-2 실험계획법/Doit_textmining-main/Data/speeches_presidents.csv")
raw_speeches

# 기본적인 전처리
speeches <- raw_speeches %>%
  mutate(value = str_replace_all(value, "[^가-힣]", " "),
         value = str_squish(value))

# 토큰화
speeches <- speeches %>%
  unnest_tokens(input = value,
                output = word,
                token = extractNoun)

# 단어 빈도 구하기
freq <- speeches %>%
  count(president, word) %>%
  filter(str_count(word) > 1)

## 2. TF-IDF 구하기
freq <- freq %>%
  bind_tf_idf(term = word,           # 단어
              document = president,  # 텍스트 구분 기준
              n = n) %>%             # 단어 빈도
  arrange(-tf_idf)

freq %>% 
  filter(president == "문재인")

freq %>% 
  filter(president == "박근혜")

freq %>% 
  filter(president == "이명박")

freq %>% 
  filter(president == "노무현")

### 막대 그래프 만들기
# 주요 단어 추출
top10 <- freq %>%
  group_by(president) %>%
  slice_max(tf_idf, n = 10, with_ties = F)

# 그래프 순서 정하기
top10$president <- factor(top10$president, levels = c("문재인", "박근혜", "이명박", "노무현"))

# 막대 그래프 만들기
top10 %>% 
  ggplot(aes(x = reorder_within(word, tf_idf, president), y = tf_idf, fill = president)) +
  geom_col(show.legend = F) +
  coord_flip() +
  facet_wrap(~ president, scales = "free", ncol = 2) +
  scale_x_reordered() +
  labs(x = NULL) + 
  theme_minimal()
