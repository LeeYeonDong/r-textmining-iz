library(tidyverse)
library(lubridate)
# install.packages("philentropy")
library(philentropy)
library(dplyr)
library(scales)
library(RColorBrewer)
library(writexl)


# 데이터 불러오기
df1 <- read_csv(file = "D:/대학원/논문/소논문/부동산_토픽모델링/df_topics_words0731.csv", 
         locale=locale("ko",encoding="cp949"))

names(df1) <- c("Topic", "Words", "importance")

write_xlsx(df1, "D:/대학원/논문/소논문/부동산_토픽모델링/topics_over_time0731_df1.xlsx")

# df2 <- read_csv(file = "D:/대학원/논문/소논문/부동산_토픽모델링/yeondong_rst.csv", 
#                 locale=locale("ko",encoding="utf-8"))

df0630 <- read.csv(
  file = "D:/대학원/논문/소논문/부동산_토픽모델링/yeondong_rst0630.csv",
  header = TRUE,
  sep = ",",
  colClasses = c(
    "Timestamp" = "character",
    "서울" = "numeric",
    "부산" = "numeric",
    "대구" = "numeric",
    "인천" = "numeric",
    "광주" = "numeric",
    "대전" = "numeric",
    "울산" = "numeric"
  ),
  fileEncoding = "UTF-8"
) %>%
  pivot_longer(cols = c(서울, 부산, 대구, 인천, 광주, 대전, 울산),# 필요 변수 추가
               names_to = "sig_nm",
               values_to = "meanprc") %>%
  mutate(
    Timestamp = paste0(Timestamp, "_01"), # "_01"을 추가하여 "yyyy-mm-dd" 형식으로 변환
    Timestamp = ymd(Timestamp) # 날짜 형식으로 변환
  ) #아파트 실거래 가격 지수

# 데이터 불러오기
df_raw <- read_csv(file = "D:/대학원/논문/소논문/부동산_토픽모델링/부동산_수정_df.csv",
                locale=locale("ko",encoding="utf-8"))


df_token <- read_csv(file = "D:/대학원/논문/소논문/부동산_토픽모델링/df_token0731.csv",                      locale=locale("ko",encoding="utf-8")) %>%
  mutate(Timestamp = as.POSIXct(날짜, format = "%Y-%m-%d %H:%M:%S"))

write_xlsx(df_token, "D:/대학원/논문/소논문/부동산_토픽모델링/df_token1.xlsx")

# 토큰화 과정
# 토큰화 과정에서 다음과 같은 이유로 데이터의 크기가 줄어들 수 있습니다:
#   불용어 제거: 일반적으로 의미가 적은 단어들(예: "그리고", "하지만" 등 불용어)이 제거됩니다.
# 특정 길이 이하 단어 제거: 너무 짧거나 긴 단어들이 제거될 수 있습니다.
# 특정 품사 제거: 명사, 동사, 형용사 등 특정 품사만 남기고 다른 품사를 제거할 수 있습니다.
# 
# BERTopic 적용
# BERTopic 적용 과정에서도 데이터의 일부가 제외될 수 있습니다:
#     의미 없는 토픽 제거: 의미 없는 토픽이나 너무 일반적인 단어들이 포함된 토픽이 제거될 수 있습니다.
# 빈도 기준 필터링: 너무 적게 등장하는 단어나 토픽이 제거될 수 있습니다.
# 임베딩 과정 중 데이터 손실: 텍스트를 임베딩 벡터로 변환하는 과정에서 일부 데이터가 손실될 수 있습니다.


# final_df <- df1 %>%
#   left_join(df_token, by = "Timestamp") %>%
#   select(id, Words, Topic, Frequency, Timestamp)
# 
# final_df %>%
#   select(id) %>% unique()

# 주제 대표 단어
# 각 Topic에서 Frequency가 가장 높은 단어 선별
representative_words <- df1 %>%
  filter(Topic != -1) %>% # Topic이 -1이 아닌 데이터만 필터링
  group_by(Topic) %>% 
  slice_max(order_by = importance, n = 3) %>% # 각 Topic에서 빈도수가 가장 높은 단어 선택
  summarise(Words = str_c(Words, collapse = ", "), .groups = 'drop') %>% # 단어들을 쉼표로 묶어서 문자열로 변환 
  mutate(Words = str_replace_all(Words, "[a-z]+", function(x) str_to_upper(x)))

write_xlsx(representative_words, "D:/대학원/논문/소논문/부동산_토픽모델링/representative_words.xlsx")


# 기준
sig_nm_uni <- df0630$sig_nm %>% unique()
topic <- (df1$Topic %>% unique() %>% sort())[-1]

year <- as.character(2012:2022)
 
df0630 <- df0630 %>%
  filter(substr(as.character(Timestamp), 1, 4) %in% as.character(year)) %>%
  mutate(Timestamp = format(Timestamp, "%Y-%m"))

df_token <- df_token %>% 
  mutate(Timestamp = format(Timestamp, "%Y-%m"))

# df0630의 meanprc 정규화
df0630 <- df0630 %>%
  group_by(sig_nm) %>%
  mutate(meanprc_norm = rescale(meanprc))

# 색상 설정
color_values <- c("기사 빈도" = "black", "광주" = "orange", "대구" = "darkgreen", "부산" = "blue", "울산" = "purple", "대전" = "brown", "서울" = "cyan", "인천" = "pink", "word" = "black")

# 기사별 빈도
df_token %>%
  group_by(Timestamp) %>%
  summarise(value = n()) %>%
  mutate(value = rescale(value)) %>%
  mutate(Type = "기사 빈도") %>%
  bind_rows(df0630 %>%
  mutate(Type = sig_nm, value = meanprc_norm) %>%
  ungroup() %>% select(Timestamp, value, Type)) %>%
  ggplot(aes(x = Timestamp, y = value, color = Type, group = Type)) +
  geom_line(linetype = "solid", size = 1) +
  scale_color_manual(values = color_values, guide = guide_legend(title = "Type")) +
  labs(title = "Article Frequency and price Trends",
       x = "Timestamp",
       y = "Normalized Values",
       color = "Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 4), legend.position = "bottom")


# 기사빈도와 아파트 실거래 가격지수 KL-divergence
i = 1
article_house_kl_df <- tibble()

for (i in 1:length(sig_nm_uni)) {
    tryCatch({
article_freq <- df_token %>%
  group_by(Timestamp) %>%
  summarise(value = n()) %>%
  mutate(value = rescale(value)) %>%
  mutate(Type = "기사 빈도") %>%
  bind_rows(df0630 %>%
  mutate(Type = sig_nm, value = meanprc_norm) %>%
  ungroup() %>% select(Timestamp, value, Type)) %>% 
  filter(Type == "기사 빈도")

house_price.tmp <- df_token %>%
  group_by(Timestamp) %>%
  summarise(value = n()) %>%
  mutate(value = rescale(value)) %>%
  mutate(Type = "기사 빈도") %>%
  bind_rows(df0630 %>%
  mutate(Type = sig_nm, value = meanprc_norm) %>%
  ungroup() %>% select(Timestamp, value, Type)) %>% 
  filter(Type == sig_nm_uni[i]) 
  
# 분포로 만들기
article_freq$dist <- article_freq$value / sum(article_freq$value)

house_price.tmp$dist <- house_price.tmp$value / sum(house_price.tmp$value)

# 단어빈도-아파트 실거래 KL
article_house_kl.tmp <- rbind(article_freq$dist, house_price.tmp$dist) %>%  
  distance(method = "kullback-leibler")

pr.tmp <- sig_nm_uni[i]

article_house_kl_df.tmp <- tibble(pr.tmp, article_house_kl.tmp)

article_house_kl_df <- bind_rows(article_house_kl_df, article_house_kl_df.tmp)
}, error = function(e) {
  message("Error in iteration i=", i, ", j=", j, ": ", conditionMessage(e))
  # 에러가 발생해도 계속 진행하도록 설정
})
}

print(article_house_kl_df)


# # A tibble: 5 × 2
# Topic Words               
# <dbl> <chr>               
#   1     0 부동산, 아파트, 분양
# 2     1 GDP, ETF, 증시      
# 3     2 들썩, 비싼, 벌써    
# 4     3 대통령, 정부, 부동산
# 5     4 쌍용, 쓸이, 싸움    
df_token %>% glimpse()

df_token %>%
  filter(str_detect(제목, "부동산")) %>% # "word" detect
  group_by(Timestamp) %>%
  summarise(value = n()) %>%
  mutate(value = rescale(value)) %>%
  mutate(Type = "word") %>%
  bind_rows(df0630 %>%
  mutate(Type = sig_nm, value = meanprc_norm) %>%
  ungroup() %>% select(Timestamp, value, Type)) %>%
  ggplot(aes(x = Timestamp, y = value, color = Type, group = Type)) +
  geom_line(linetype = "solid", size = 1) +
  scale_color_manual(values = color_values, guide = guide_legend(title = "Type")) +
  labs(title = "Article Frequency and price Trends",
       x = "Timestamp",
       y = "Normalized Values",
       color = "Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 4), legend.position = "bottom")


# 특정 단어 빈도와 아파트 실거래 가격지수 KL-divergence
sig_nm_uni
topicwords_df <- representative_words %>%
  filter(Topic != 4) %>% 
  separate_rows(Words, sep = ", ")

i = 1
j = 1

house_word_df <- tibble()

for (i in 1:nrow(topicwords_df)) {
  for (j in 1:length(sig_nm_uni)) {
    tryCatch({
      df2_meanprc_df.tmp <- df0630 %>% 
        filter(sig_nm == sig_nm_uni[j]) # 아파트 실거래 지수 - 지역:서울
      
      df1_word_df.tmp <- df_token %>%
    filter(str_detect(제목, topicwords_df$Words[i])) %>% # "word" detect
        group_by(Timestamp) %>%
        summarise(value = n()) 
      
      df1_word_df.tmp <- df2_meanprc_df.tmp %>% 
        ungroup() %>% 
        select(Timestamp) %>% 
        left_join(df1_word_df.tmp, by = "Timestamp") %>%
        mutate(value = replace_na(value, 0))
      
      # 분포로 만들기
      df2_meanprc_df.tmp$dist <- df2_meanprc_df.tmp$meanprc / sum(df2_meanprc_df.tmp$meanprc)
      
      df1_word_df.tmp$dist <- df1_word_df.tmp$value / sum(df1_word_df.tmp$value)
      
      # 단어빈도-아파트 실거래 KL
      word_house_kl.tmp <- rbind(df2_meanprc_df.tmp$dist, df1_word_df.tmp$dist) %>%  
        distance(method = "kullback-leibler")
      
      tc.tmp <- topicwords_df$Topic[i] 
      pr.tmp <- sig_nm_uni[j]
      topicword.tmp <- topicwords_df$Words[i]
      
      house_word_df.tmp <- tibble(pr.tmp, tc.tmp, topicword.tmp, word_house_kl.tmp)
      
      house_word_df <- bind_rows(house_word_df, house_word_df.tmp)
    }, error = function(e) {
      message("Error in iteration i=", i, ", j=", j, ": ", conditionMessage(e))
      # 에러가 발생해도 계속 진행하도록 설정
    })
  }
}

names(house_word_df) <- c("District", "Topic", "Words", "word_house_kl")

write.csv(house_word_df, file = "D:/대학원/논문/소논문/부동산_토픽모델링/house_word_df_kl.csv", row.names=FALSE, fileEncoding = 'cp949')

word_house_kl_Topic <- house_word_df %>%
  group_by(Topic) %>%
  summarise(mean_word_house_kl = mean(word_house_kl, na.rm = TRUE))

word_house_kl_Topic %>% 
  arrange(mean_word_house_kl)

word_house_kl_District <- house_word_df %>%
  group_by(District) %>%
  summarise(mean_word_house_kl = mean(word_house_kl, na.rm = TRUE))

word_house_kl_District %>% 
  arrange(mean_word_house_kl)

word_house_kl_Words <- house_word_df %>%
  group_by(Topic, Words) %>%
  summarise(mean_word_house_kl = mean(word_house_kl, na.rm = TRUE))

word_house_kl_Words %>% 
  arrange(mean_word_house_kl) %>% print()

write.csv(word_house_kl_Words, file = "D:/대학원/논문/소논문/부동산_토픽모델링/word_house_kl_Words.csv", row.names=FALSE, fileEncoding = 'cp949')
