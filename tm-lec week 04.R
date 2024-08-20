#### 04-1 감정 사전 활용하기
library(KoNLP)
buildDictionary(ext_dic = "woorimalsam")  # "woorimalsam" dic을 불러옵니다
useNIADic() 
# Use the Sejong Dictionary
library(tidyverse)


### 감정 사전 살펴보기
library(dplyr)
library(readr)
dic <- read_csv("D:/대학원/강의/2024-2 실험계획법/Doit_textmining-main/Data/knu_sentiment_lexicon.csv")

dic %>%
  filter(word %in% c("좋은", "나쁜"))

dic %>% view()


### 문장의 감정 점수 구하기
## 1. 단어 기준으로 토큰화하기
df <- tibble(sentence = c("디자인 예쁘고 마감도 좋아서 만족스럽다.", "디자인은 괜찮다. 그런데 마감이 나쁘고 가격도 비싸다."))
df

library(tidytext)

df <- df %>%
  unnest_tokens(input = sentence,
                output = word,
                token = "words",
                drop = F)

df 

## 단어에 감정 점수 부여하기
df <- df %>%
  left_join(dic, by = "word") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))

df

## 3. 문장별로 감정 점수 합산하기
score_df <- df %>%
  group_by(sentence) %>%
  summarise(score = sum(polarity))

score_df


#### 04-2 댓글 감정 분석하기
raw_news_comment <- read_csv("D:/대학원/강의/2024-2 실험계획법/Doit_textmining-main/Data/news_comment_parasite.csv")

### 기본적인 전처리
library(textclean)

(raw_news_comment %>% 
  select(reply))[1,] %>% as.vector()

news_comment <- raw_news_comment %>%
  mutate(id = row_number(), # 각 행에 대해 고유한 ID
         reply = str_squish(replace_html(reply)))
# reply 열의 HTML 태그를 제거

glimpse(news_comment)

## 단어 기준으로 토큰화하고 감정 점수 부여하기
# 토큰화
word_comment <- news_comment %>%
  unnest_tokens(input = reply,
                output = word,
                token = "words",
                drop = F)

word_comment %>%
  select(word, reply)

# 감정 점수 부여
word_comment <- word_comment %>%
  left_join(dic, by = "word") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))

word_comment %>%
  select(word, polarity)


### 자주 사용된 감정 단어 살펴보기
## 1. 감정 분류하기
word_comment <- word_comment %>%
  mutate(sentiment = ifelse(polarity == 2, "pos",
                            ifelse(polarity == -2, "neg", "neu")))

word_comment %>% 
  select(word, sentiment)

word_comment %>%
  count(sentiment)

## 추가 설정
# word_comment1 <- word_comment %>%
#   mutate(sentiment = ifelse(polarity > 0, "pos",
#                             ifelse(polarity < 0, "neg", "neu")))
# 
# word_comment1 %>% 
#   select(word, sentiment)
# 
# word_comment1 %>%
#   count(sentiment)

## 2. 막대 그래프 만들기
top10_sentiment <- word_comment %>%
  filter(sentiment != "neu") %>%
  count(sentiment, word) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10)

top10_sentiment %>% view()

# group_by 기능 확인
# word_comment %>%
#   filter(sentiment != "neu") %>%
#   count(sentiment, word) %>%
#  # group_by(sentiment) %>%
#   slice_max(n, n = 10) %>% view()

# 막대 그래프 만들기
library(ggplot2)

top10_sentiment %>% 
  ggplot(aes(x = reorder(word, n), y = n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) +
  facet_wrap(~ sentiment, scales = "free") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) + # y 축의 범위를 확장합니다. 하한은 0.05, 상한은 0.15 비율로 확장되어 막대의 상하 여백을 추가
  labs(x = NULL)

top10_sentiment %>% 
  ggplot(aes(x = reorder(word, n), y = n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) +
  facet_wrap(~ sentiment, scales = "free") +
  scale_y_continuous(limits = c(0, 100)) + # y축의 범위가 0에서 100까지 고정
  labs(x = NULL)

top10_sentiment %>% 
  ggplot(aes(x = reorder(word, n), y = n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) +
  facet_wrap(~ sentiment, scales = "free") +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) + #  y축의 눈금을 수동으로 설정
  labs(x = NULL)

### 댓글별 감정 점수 구하고 댓글 살펴보기
## 1. 댓글별 감정 점수 구하기
score_comment <- word_comment %>%
  group_by(id, reply) %>%
  summarise(score = sum(polarity)) %>%
  ungroup()

score_comment %>%
  select(score, reply)

## 2. 감정 점수 높은 댓글 살펴보기
score_comment %>%
  select(score, reply) %>%
  arrange(-score)

### 감정 경향 살펴보기
## 1. 감정 점수 빈도 구하기
score_comment %>%
  count(score)

## 2. 감정 분류하고 막대 그래프 만들기
# 감정 분류하기
score_comment <- score_comment %>%
  mutate(sentiment = ifelse(score >=  1, "pos",
                     ifelse(score <= -1, "neg", "neu")))

# 감정 빈도와 비율 구하기
frequency_score <- score_comment %>%
  count(sentiment) %>%
  mutate(ratio = n/sum(n)*100)

frequency_score

# 막대 그래프 만들기
frequency_score %>% 
  ggplot(aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.3) +
  scale_x_discrete(limits = c("pos", "neu", "neg"))


#### 04-3 감정 범주별 주요 단어 살펴보기
### 감정 범주별 단어 빈도 구하기
## 1. 토큰화하고 두 글자 이상 한글 단어만 남기기
comment <- score_comment %>%
  unnest_tokens(input = reply,          # 단어 기준 토큰화
                output = word,
                token = "words",
                drop = F) %>%
  filter(str_detect(word, "[가-힣]") &  # 한글 추출
           str_count(word) >= 2)          # 두 글자 이상 추출

## 2. 감정 범주별 빈도 구하기
frequency_word <- comment %>%
  count(sentiment, reply, word, sort = T)

frequency_word %>% view()

# 긍정 댓글 고빈도 단어
frequency_word %>%
  filter(sentiment == "pos")

# 부정 댓글 고빈도 단어
frequency_word %>%
  filter(sentiment == "neg")


### 상대적으로 자주 사용된 단어 비교하기
## 1. 로그 오즈비 구하기
library(tidyr)
comment_wide <- frequency_word %>%
  filter(sentiment != "neu") %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = list(n = 0))

comment_wide

# 로그 오즈비 구하기
comment_wide <- comment_wide %>%
  mutate(log_odds_ratio = log(((pos + 1) / (sum(pos + 1))) /
                                ((neg + 1) / (sum(neg + 1)))))

comment_wide %>% 
  select(reply, word, log_odds_ratio)

# log_odds_ratio 값이 0에 가까울수록 해당 단어가 긍정적 코멘트와 부정적 코멘트에서 비슷한 빈도로 사용된다는 것을 의미합니다.
# 양수의 log_odds_ratio 값은 해당 단어가 긍정적 코멘트에서 상대적으로 더 많이 사용된다는 것을 의미하며, 음수의 log_odds_ratio 값은 해당 단어가 부정적 코멘트에서 더 많이 사용된다는 것을 의미합니다.

## 2. 로그 오즈비가 가장 큰 단어 10개씩 추출하기
top10 <- comment_wide %>%
  group_by(sentiment = ifelse(log_odds_ratio > 0, "pos", "neg")) %>%
  slice_max(abs(log_odds_ratio), n = 10, with_ties = F)

top10

## 3. 막대 그래프 만들기
top10 %>% 
  ggplot(aes(x = reorder(word, log_odds_ratio),
                  y = log_odds_ratio,
                  fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL)

## 04-4 감정 사전 수정하기
grep("소름", dic$word)
dic$polarity[4707]
dic$polarity[4707] <- 1

#### unnest_tokens 대신 simplePos9 사용
