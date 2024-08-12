library(tidyverse)
library(lubridate)
# install.packages("philentropy")
library(philentropy)
library(dplyr)
library(scales)
library(RColorBrewer)
library(writexl)


# 데이터 불러오기
df1 <- read_csv(file = "D:/대학원/논문/소논문/부동산_토픽모델링/topics_over_time0715.csv", 
         locale=locale("ko",encoding="utf-8"))

write_xlsx(df1, "D:/대학원/논문/소논문/부동산_토픽모델링/topics_over_time0715_df1.xlsx")

# df2 <- read_csv(file = "D:/대학원/논문/소논문/부동산_토픽모델링/yeondong_rst.csv", 
#                 locale=locale("ko",encoding="utf-8"))

df0630 <- read.csv(
  file = "D:/대학원/논문/소논문/부동산_토픽모델링/yeondong_rst0630_01.csv",
  header = TRUE,
  sep = ",",
  colClasses = c(
    "Timestamp" = "character",
    "도심권" = "numeric",
    "동북권" = "numeric",
    "서북권" = "numeric",
    "서남권" = "numeric",
    "동남권" = "numeric"
  ),
  fileEncoding = "UTF-8"
) %>%
  pivot_longer(cols = c(서울, 도심권, 동북권, 서북권, 서남권, 동남권),# 필요 변수 추가
               names_to = "sig_nm",
               values_to = "meanprc") %>%
  mutate(
    Timestamp = paste0(Timestamp, "_01"), # "_01"을 추가하여 "yyyy-mm-dd" 형식으로 변환
    Timestamp = ymd(Timestamp) # 날짜 형식으로 변환
  ) #아파트 실거래 가격 지수

# 데이터 불러오기
df_raw <- read_csv(file = "D:/대학원/논문/소논문/부동산_토픽모델링/부동산_수정_df.csv",
                locale=locale("ko",encoding="utf-8"))


df_token <- read_csv(file = "D:/대학원/논문/소논문/부동산_토픽모델링/df_token.csv", 
                     locale=locale("ko",encoding="utf-8")) %>%
  mutate(Timestamp = as.POSIXct(날짜, format = "%Y-%m-%d %H:%M:%S"))

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
  separate_rows(Words, sep = ", ") %>% # 단어들을 개별 행으로 분리
  group_by(Topic, Words) %>%
  summarise(Total_Frequency = sum(Frequency, na.rm = TRUE), .groups = 'drop') %>% # 각 단어의 빈도수 합계 계산
  group_by(Topic) %>%
  slice_max(order_by = Total_Frequency, n = 5) %>% # 각 Topic에서 빈도수가 가장 높은 단어 선택
  summarise(Words = str_c(Words, collapse = ", "), .groups = 'drop') # 단어들을 쉼표로 묶어서 문자열로 변환


write_xlsx(representative_words, "D:/대학원/논문/소논문/부동산_토픽모델링/representative_words.xlsx")


# 기준
topic <- (df1$Topic %>% unique() %>% sort())[-1]

df1$year <- df1$Timestamp %>% substr(1,4) %>% as.integer()

year <- df1$year %>% unique()

df1 <- df1 %>% filter(!year %in% c(2023, 2024)) %>%
  mutate(Timestamp = format(Timestamp, "%Y-%m")) %>% select(-year)

df0630 <- df0630 %>%
  filter(substr(as.character(Timestamp), 1, 4) %in% as.character(year)) %>%
  mutate(Timestamp = format(Timestamp, "%Y-%m"))


# kl 산출
sig_nm_uni <- df0630$sig_nm %>% unique() %>% sort()

topic_vec <- c()
sig_nm_vec <- c()
kl_vec <- c()

i = 1
j = 1

for (j in 1:length(sig_nm_uni)){
for (i in 1:length(topic)){

df1_freq_df.tmp <- df1 %>% filter(Topic == topic[i]) %>% group_by(Timestamp) %>% summarise(total = sum(Frequency, na.rm = TRUE))

df2_meanprc_df.tmp <- df0630 %>% filter(sig_nm == sig_nm_uni[j])

merged_df.tmp <- merge(df1_freq_df.tmp, df2_meanprc_df.tmp, by = "Timestamp")

# total과 meanprc를 확률 분포로 변환
total_dist.tmp <- merged_df.tmp$total / sum(merged_df.tmp$total)
meanprc_dist.tmp <- merged_df.tmp$meanprc / sum(merged_df.tmp$meanprc)

# philentropy 패키지를 사용하려면 두 분포를 행렬 형식으로 제공해야 합니다.
dist_matrix.tmp <- rbind(total_dist.tmp, meanprc_dist.tmp)
kl.tmp <- distance(dist_matrix.tmp, method = "kullback-leibler")


topic_vec <- append(topic_vec,topic[i])
sig_nm_vec <- append(sig_nm_vec,sig_nm_uni[j])
kl_vec <- append(kl_vec,kl.tmp)

cat("현재", topic[i], "th topic의,", sig_nm_uni[j],"의 kl-divergence을 구하고 있음\n")

}
}

kl_df <- tibble(topic_vec, sig_nm_vec, kl_vec)
kl_df %>% arrange(kl_vec)
kl_df %>% arrange(-kl_vec)

write.csv(kl_df, file = "D:/대학원/논문/소논문/부동산_토픽모델링/kl_df0728.csv", row.names=FALSE, fileEncoding = 'cp949')

  

# graph
df1_g <- df1 %>% 
  group_by(Timestamp, Topic) %>% 
  summarise(Frequency = sum(Frequency, na.rm = TRUE), .groups = 'drop') %>% 
  filter(Topic != -1)

# 0. 기사 빈도-meanprc
# 날짜 변환 함수
convert_to_ym <- function(date_str) {
  # 12번째 글자 이후로 삭제
  truncated_date <- substr(date_str, 1, 11)
  
  # 연도와 월 추출
  year <- substr(truncated_date, 1, 4)
  month <- substr(truncated_date, 6, 7)
  
  # 연도-월 형식으로 변환
  paste0(year, "-", month)
}

# 변환된 날짜 추가
df1$year <- df1$Timestamp %>% substr(1,4) %>% as.integer()

df1 <- df1 %>% filter(!year %in% c(2023, 2024))

# Timestamp별 빈도 계산
timestamp_freq <- df1 %>%
  count(Timestamp) %>%
  rename(Frequency = n)

# 빈도 정규화
timestamp_freq <- timestamp_freq %>%
  mutate(Frequency_norm = rescale(Frequency))


# df0630의 meanprc 정규화
df0630 <- df0630 %>%
  group_by(sig_nm) %>%
  mutate(meanprc_norm = rescale(meanprc))

# 두 데이터 프레임을 하나로 합치기
combined_df <- timestamp_freq %>%
  mutate(Type = "Frequency", Value = Frequency_norm) %>%
  select(Timestamp, Type, Value) %>%
  bind_rows(df0630 %>%
              mutate(Type = sig_nm, Value = meanprc_norm) %>%
              select(Timestamp, Type, Value))


# 꺾은선 그래프 그리기
ggplot(combined_df, aes(x = Timestamp, y = Value, group = Type)) +
  geom_line(data = subset(combined_df, Type == "Frequency"), 
            linetype = "longdash", 
            color = "grey", 
            size = 0.5) + 
  geom_line(data = subset(combined_df, Type != "Frequency"),
            color = "black", 
            size = 1) +
  labs(title = "Frequency and Sig_nm Trends",
       x = "Timestamp",
       y = "Normalized Values",
       color = "Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 4),
        legend.position = "none")




# 단어별 추세
# 두 데이터 프레임을 하나로 합치기
# 월별 전체 기사제목 중에서 "부동산"을 포함한 기제사목의 비율 # 갈 수록 기사제목에 "부동산"을 포함한 비율이 낮아질까?
df1 %>% select(Topic) %>% unique() %>% table()

df1 %>%
  filter(str_detect(Words, "서울")) %>% # "word" detect
  group_by(Timestamp) %>%
  summarise(Frequency = sum(Frequency)) %>%
  mutate(Frequency_norm = rescale(Frequency)) %>% 
  select(-Frequency) %>%
  mutate(Type = "Frequency", Value = Frequency_norm) %>% 
  select(Timestamp, Type, Value) %>%
  bind_rows(df0630 %>%
              mutate(Type = sig_nm, Value = meanprc_norm) %>%
              select(Timestamp, Type, Value)) %>% 
  ggplot(aes(x = Timestamp, y = Value, color = Type, group = Type)) +
  geom_line(data = . %>% filter(Type == "Frequency"), linetype = "longdash", size = 0.5, color = "grey") +
  geom_line(data = . %>% filter(Type != "Frequency"), linetype = "solid", size = 1, color = "black") +
  labs(title = "Frequency '서울' and price Trends",
       x = "Timestamp",
       y = "Normalized Values",
       color = "Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 4),
        legend.position = "none")


# topic 0 - 부동산:정반대 / 아파트:정반대 / 분양:정반대 / 시장:X / 서울:부합
# topic 2 - 쌍용:X / 쓸이:X / 씨티:약 부합(선행) / 싸움:X / 싸게:X
# topic 3 - etf:X / 증시:약 정반대 / 美中:X / ipo:X / 지표:정반대
# topic 4 - 비싼:X / 비싸:X / 불씨:X / 가장:X / 날씨:X
# topic 5 - kb:X / 국민은행:X / 금융:X / 자산운용:약 부합(선행) / 점포:X

# 전체 기사 빈도와 아파트 거래 가격지수의 추세 그래프 



# 1. df1_g의 Topic별 Timestamp에 따른 Frequency 추세 꺾은선 그래프
# df1_g의 Frequency
df1_g <- df1_g %>%
  group_by(Topic) %>%
  mutate(Frequency_norm = rescale(Frequency))

# df1_g 그래프
ggplot(df1_g, aes(x = Timestamp, y = Frequency_norm, color = as.factor(Topic), group = Topic)) +
  geom_line(size = 0.5) +
  labs(title = "Topic-wise Frequency Trends",
       x = "Timestamp",
       y = "Normalized Frequency",
       color = "Topic") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom")

# 2. df0630의 sig_nm별 Timestamp에 따른 meanprc 추세 꺾은선 그래프
# df0630의 meanprc 정규화
df0630 <- df0630 %>%
  group_by(sig_nm) %>%
  mutate(meanprc_norm = rescale(meanprc))

# df0630 그래프
ggplot(df0630, aes(x = Timestamp, y = meanprc_norm, color = sig_nm, group = sig_nm)) +
  geom_line(size = 0.5) +
  labs(title = "Sig_nm-wise Mean Price Trends",
       x = "Timestamp",
       y = "Normalized Mean Price",
       color = "Sig_nm") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom")

# 3. 두 그래프를 한 그래프에 겹쳐 그리기 + 
# 두 데이터 프레임을 하나로 결합하기 위해 long format으로 변환
df1_g_long <- df1_g %>%
  select(Timestamp, Topic, Frequency_norm) %>%
  mutate(Type = paste0("Topic_", Topic))

df0630_long <- df0630 %>%
  select(Timestamp, sig_nm, meanprc_norm) %>%
  rename(Frequency_norm = meanprc_norm) %>%
  mutate(Type = sig_nm)


# 색상 설정
num_topics <- length(unique(df1_g_long$Type))
num_sig_nm <- length(unique(df0630_long$Type))

grey_colors <- rep("grey", num_topics)
blue_colors <- rep("blue", num_sig_nm)

color_values <- c(grey_colors, blue_colors)
names(color_values) <- c(unique(df1_g_long$Type), unique(df0630_long$Type))

# 특정단어
df_word <- df1 %>%
  filter(str_detect(Words, "서울")) %>% # "word" detect
  group_by(Timestamp) %>%
  summarise(Frequency = sum(Frequency)) %>%
  mutate(Frequency_norm = rescale(Frequency)) %>% 
  select(-Frequency) %>%
  mutate(Type = "Frequency", Value = Frequency_norm) %>% 
  select(Timestamp, Type, Value)

combined_df <- bind_rows(df1_g_long, df0630_long) %>% 
  bind_rows(df_word)

# 그래프 그리기 토픽-아파트 실거래 가격 지수
combined_df %>% 
  ggplot(aes(x = Timestamp, y = Frequency_norm, color = Type, group = Type)) +
  scale_color_manual(values = color_values) +
  labs(title = "Combined Topic and Sig_nm Trends",
       x = "Timestamp",
       y = "Normalized Values",
       color = "Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 4),  # 글자 크기 줄이기
        legend.position = "none",
        legend.title = element_text(size = 6),  # 범례 제목 글자 크기 줄이기
        legend.text = element_text(size = 6)) +  # 범례 
  geom_line(data = subset(combined_df, str_detect(Type, "^Topic")), linetype = "twodash", size = 0.5, color = "grey") + # "Topic"으로 시작하는 모든 경우 + # 토픽 빈도
  geom_line(data = subset(combined_df, Type %in% c("서울", "도심권", "동북권", "서북권", "서남권", "동남권")), size = 1, color = "black")  # 지역

# 그래프 그리기
combined_df %>% 
  ggplot(aes(x = Timestamp, y = Frequency_norm, color = Type, group = Type)) +
  scale_color_manual(values = color_values) +
  labs(title = "Combined Topic ,Sig_nm, '서울' Trends",
       x = "Timestamp",
       y = "Normalized Values",
       color = "Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 4),  # 글자 크기 줄이기
        legend.position = "none",
        legend.title = element_text(size = 6),  # 범례 제목 글자 크기 줄이기
        legend.text = element_text(size = 6)) +  # 범례 
  geom_line(data = subset(combined_df, Type %in% c("Topic_0")), size = 0.5, color = "darkgreen") + # 토픽 빈도
  geom_line(data = subset(combined_df, Type %in% c("서울", "도심권", "동북권", "서북권", "서남권", "동남권")), size = 1) + # 지역
  geom_line(data = subset(combined_df, Type %in% c("Topic_14")), size = 0.5, color = "orange") + # 특정 단어
  annotate("text", x = "2015-06", y = 0.8, label = "KL : 0.046", color = "darkgreen", size = 7, hjust = 0) +
  annotate("text", x = "2018-01", y = 0.1, label = "KL : 1.022", color = "orange", size = 7, hjust = 0)


# 그래프 그리기
combined_df %>% 
  ggplot(aes(x = Timestamp, y = Frequency_norm, color = Type, group = Type)) +
  scale_color_manual(values = color_values) +
  labs(title = "Combined Topic ,Sig_nm, '서울' Trends",
       x = "Timestamp",
       y = "Normalized Values",
       color = "Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 4),  # 글자 크기 줄이기
        legend.position = "none",
        legend.title = element_text(size = 6),  # 범례 제목 글자 크기 줄이기
        legend.text = element_text(size = 6)) +  # 범례 
geom_line(data = subset(combined_df, Type %in% c("Topic_0")), linetype = "twodash", size = 0.5, color = "darkgreen") + # 토픽 빈도
  geom_line(data = subset(combined_df, Type %in% c("서울", "도심권", "동북권", "서북권", "서남권", "동남권")), size = 1, color = "black") + # 지역
  geom_line(data = subset(combined_df, Type == "Frequency"), aes(y = Value), size = 0.5, color = "orange") + # 특정 단어
  annotate("text", x = "2013-01", y = 0.9, label = "Topic 0", color = "darkgreen", size = 3, hjust = 0) +
  annotate("text", x = "2020-01", y = 0, label = "Seoul", color = "orange", size = 3, hjust = 0)
  
# topic 0 - 부동산:정반대 / 아파트:정반대 / 분양:정반대 / 시장:X / 서울:부합
# topic 2 - 쌍용:X / 쓸이:X / 씨티:약 부합(선행) / 싸움:X / 싸게:X
# topic 3 - etf:X / 증시:약 정반대 / 美中:X / ipo:X / 지표:정반대
# topic 4 - 비싼:X / 비싸:X / 불씨:X / 가장:X / 날씨:X
# topic 5 - kb:X / 국민은행:X / 금융:X / 자산운용:약 부합(선행) / 점포:X



# 특정 단어 빈도와 아파트 실거래 가격지수 KL-divergence
topic
sig_nm_uni
topicwords_df <- representative_words %>%
  separate_rows(Words, sep = ", ")

i = 1
j = 1

pr <- c()
topicword <- c()
tp <- c()

topic_house_word_df <- tibble()

# 예시 
for (i in 1:nrow(topicwords_df)) {
  for (j in 1:length(sig_nm_uni)) {
    
    df1_freq_df.tmp <- df1 %>% 
      filter(Topic == topicwords_df$Topic[i]) %>% 
      group_by(Timestamp) %>% 
      summarise(total = sum(Frequency, na.rm = TRUE)) # topic - 서울이 속한 topic0
    
    df2_meanprc_df.tmp <- df0630 %>% 
      filter(sig_nm == sig_nm_uni[j]) # 아파트 실거래 지수 - 지역:서울
    
    df1_word_df.tmp <- df1 %>% # 특정단어 "서울"
      filter(Topic == topicwords_df$Topic[i]) %>% 
      group_by(Timestamp) %>% 
      filter(str_detect(Words, topicwords_df$Words[i])) %>% # "word" detect
      group_by(Timestamp) %>%
      summarise(Frequency = sum(Frequency))
    # 특정 단어 빈도 # 수정!!!!!!!!
    
    merged_df.tmp <- merge(df1_freq_df.tmp, df2_meanprc_df.tmp, by = "Timestamp") %>% 
      merge(df1_word_df.tmp, by = "Timestamp")
    
    # total과 meanprc를 확률 분포로 변환
    total_dist.tmp <- merged_df.tmp$total / sum(merged_df.tmp$total) # topic
    meanprc_dist.tmp <- merged_df.tmp$meanprc / sum(merged_df.tmp$meanprc) # 아파트 실거래 지수
    Frequency_dist.tmp <- merged_df.tmp$Frequency / sum(merged_df.tmp$Frequency) # 특정 단어 빈도
    
    # topic-아파트 실거래 KL
    topic_house_kl.tmp <- rbind(total_dist.tmp, meanprc_dist.tmp) %>% 
      distance(method = "kullback-leibler")
    
    # 단어빈도-아파트 실거래 KL
    word_house_kl.tmp <- rbind(Frequency_dist.tmp, meanprc_dist.tmp) %>%  
      distance(method = "kullback-leibler")
    
    pr.tmp <- sig_nm_uni[j]
    topicword.tmp <- topicwords_df$Words[i]
    topic.tmp <- topicwords_df$Topic[i]
    
    topic_house_word_df.tmp <- tibble(topic.tmp, pr.tmp, topicword.tmp, topic_house_kl.tmp, word_house_kl.tmp)
    
    topic_house_word_df <- bind_rows(topic_house_word_df, topic_house_word_df.tmp)
    
  }
}


names(topic_house_word_df) <- c("토픽","지역", "토픽_단어", "topic_house_kl", "word_house_kl", "kl_diff")
  
topic_house_word_df$kl_diff <- topic_house_word_df$topic_house_kl - topic_house_word_df$word_house_kl

write.csv(topic_house_word_df, file = "D:/대학원/논문/소논문/부동산_토픽모델링/topic_house_word_df_kl.csv", row.names=FALSE, fileEncoding = 'cp949')
