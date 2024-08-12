library(tidyverse)
library(lubridate)
# install.packages("philentropy")
library(philentropy)
library(dplyr)
library(scales)
library(RColorBrewer)



# 데이터 불러오기
df1 <- read_csv(file = "D:/대학원/논문/소논문/부동산_토픽모델링/topics_over_time0715.csv", 
                locale=locale("ko",encoding="utf-8"))

# df2 <- read_csv(file = "D:/대학원/논문/소논문/부동산_토픽모델링/yeondong_rst.csv", 
#                 locale=locale("ko",encoding="utf-8"))

df0630 <- read.csv(
  file = "D:/대학원/논문/소논문/부동산_토픽모델링/yeondong_rst0630.csv",
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
  pivot_longer(cols = c(서울, 도심권, 동북권, 서북권, 서남권, 동남권, 경기, 인천, 수도권),# 필요 변수 추가
               names_to = "sig_nm",
               values_to = "meanprc") %>%
  mutate(
    Timestamp = paste0(Timestamp, "_01"), # "_01"을 추가하여 "yyyy-mm-dd" 형식으로 변환
    Timestamp = ymd(Timestamp) # 날짜 형식으로 변환
  ) #아파트 실거래 가격 지수

df0630 %>% select(sig_nm) %>% unique()

# 데이터 불러오기
df_raw <- read_csv(file = "D:/대학원/논문/소논문/부동산_토픽모델링/부동산_수정_df.csv",
                   locale=locale("ko",encoding="utf-8"))


# park-moon
# 기준
topic <- (df1$Topic %>% unique() %>% sort())[-1]

df1$year <- df1$Timestamp %>% substr(1,4) %>% as.integer()

year <- df1$year %>% unique()

df1 <- df1 %>% 
  filter(!year %in% c(2023, 2024)) %>%
  mutate(Timestamp = format(Timestamp, "%Y-%m")) %>% 
  mutate(Period = case_when(
    Timestamp >= "2013-02" & Timestamp <= "2017-04" ~ "Park",
    Timestamp >= "2017-05" & Timestamp <= "2022-05" ~ "Moon",
    TRUE ~ "Other"
  )) %>% 
  select(-year) 

df0630 <- df0630 %>%
  filter(substr(as.character(Timestamp), 1, 4) %in% as.character(year)) %>%
  mutate(Timestamp = substr(as.character(Timestamp), 1, 7)) %>%
  mutate(Period = case_when(
    Timestamp >= "2013-02" & Timestamp <= "2017-04" ~ "Park",
    Timestamp >= "2017-05" & Timestamp <= "2022-05" ~ "Moon",
    TRUE ~ "Other"
  ))

df0630$Period <- factor(df0630$Period, levels = c("Park", "Moon", "Other"))



# kl 산출
sig_nm_uni <- df0630$sig_nm %>% unique() %>% sort()
pmo <- ((df1 %>% select(Period) %>% unique() %>% as.vector())$Period)[-1]

topic_vec <- c()
sig_nm_vec <- c()
kl_vec <- c()
Period <- c()

i = 1
j = 1
k = 1

for (j in 1:length(sig_nm_uni)){
  for (i in 1:length(topic)){
    for (k in 1:length(pmo)){
      
    df1_freq_df.tmp <- df1 %>% 
      filter(Period == pmo[k]) %>% 
      filter(Topic == topic[i]) %>% group_by(Timestamp) %>% summarise(total = sum(Frequency, na.rm = TRUE))
    
    df2_meanprc_df.tmp <- df0630 %>% 
      filter(Period == pmo[k]) %>% 
      filter(sig_nm == sig_nm_uni[j])
    
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
    Period <- append(Period,kl.tmp)
    
    cat("현재", topic[i], "th topic,", sig_nm_uni[j], pmo[k], "의 kl-divergence을 구하고 있음\n")
    }
  }
}

kl_df_pm <- tibble(topic_vec, sig_nm_vec, kl_vec, Period)

write.csv(kl_df_pm, file = "D:/대학원/논문/소논문/부동산_토픽모델링/kl_df0722_pm.csv", row.names=FALSE, fileEncoding = 'cp949')



# graph
df1_g <- df1 %>% 
  group_by(Timestamp, Topic) %>% 
  summarise(Frequency = sum(Frequency, na.rm = TRUE), .groups = 'drop') %>% 
  mutate(Period = case_when(
    Timestamp >= "2013-02" & Timestamp <= "2017-04" ~ "Park",
    Timestamp >= "2017-05" & Timestamp <= "2022-05" ~ "Moon",
    TRUE ~ "Other"
  )) %>% 
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
df_raw <- df_raw %>%
  mutate(Timestamp = sapply(날짜, convert_to_ym)) %>% 
  mutate(Period = case_when(
    Timestamp >= "2013-02" & Timestamp <= "2017-04" ~ "Park",
    Timestamp >= "2017-05" & Timestamp <= "2022-05" ~ "Moon",
    TRUE ~ "Other"
  )) 

df_raw$year <- df_raw$Timestamp %>% substr(1,4) %>% as.integer()


# Timestamp별 빈도 계산
timestamp_freq <- df_raw %>%
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
# 꺾은선 그래프 그리기
# Period 열 추가
combined_df <- timestamp_freq %>%
  mutate(Type = "Frequency", Value = Frequency_norm) %>%
  select(Timestamp, Type, Value) %>%
  bind_rows(df0630 %>%
  ungroup() %>%   
  mutate(Type = sig_nm, Value = meanprc_norm) %>%
  select(Timestamp, Type, Value)) %>%
  mutate(Period = case_when(
    Timestamp >= "2013-02" & Timestamp <= "2017-04" ~ "Park",
    Timestamp >= "2017-05" & Timestamp <= "2022-05" ~ "Moon",
    TRUE ~ "Other"
  )) 

combined_df$Period <- factor(combined_df$Period, levels = c("Park", "Moon", "Other"))


# 그래프 그리기
ggplot(combined_df %>% filter(Period != "Other"), aes(x = Timestamp, y = Value, color = Type, group = Type)) +
  geom_line(linewidth = 0.5) +
  labs(title = "Frequency and Sig_nm Trends",
       x = "Timestamp",
       y = "Normalized Values",
       color = "Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 4),
        legend.position = "none") +
  scale_color_manual(values = c("Frequency" = "grey", setNames(rep("blue", length(unique(df0630$sig_nm))), unique(df0630$sig_nm)))) +
  facet_wrap(~ Period, scales = "free_x")



# 단어별 추세
# 두 데이터 프레임을 하나로 합치기
df1 %>%
  filter(Period != "Other") %>% 
  filter(str_detect(Words, "서울")) %>% # "word" detect
  group_by(Timestamp) %>%
  summarise(Frequency = sum(Frequency)) %>%
  mutate(Frequency_norm = rescale(Frequency)) %>% 
  select(-Frequency) %>%
  mutate(Type = "Frequency", Value = Frequency_norm) %>% 
  select(Timestamp, Type, Value) %>%
  bind_rows(df0630 %>%
  mutate(Type = sig_nm, Value = meanprc_norm) %>% 
  ungroup() %>% 
  select(Timestamp, Type, Value, Period)) %>% 
  filter(Period != "Other") %>% 
  ggplot(aes(x = Timestamp, y = Value, color = Type, group = Type)) +
  geom_line(linewidth = 0.5) +
  labs(title = "Frequency '서울' and price Trends",
       x = "Timestamp",
       y = "Normalized Values",
       color = "Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 4),
        legend.position = "none") +
  scale_color_manual(values = c("Frequency" = "grey", setNames(rep("blue", length(unique(df0630$sig_nm))), unique(df0630$sig_nm)))) +
  facet_wrap(~ Period, scales = "free_x")



# 1. df1_g의 Topic별 Timestamp에 따른 Frequency 추세 꺾은선 그래프
# df1_g의 Frequency
df1_g <- df1_g %>%
  group_by(Topic) %>%
  mutate(Frequency_norm = rescale(Frequency))

df1_g$Period <- factor(df1_g$Period, levels = c("Park", "Moon", "Other"))

# df1_g 그래프
df1_g %>% 
  filter(Period != "Other") %>% 
  ggplot(aes(x = Timestamp, y = Frequency_norm, color = as.factor(Topic), group = Topic)) +
  geom_line(size = 0.5) +
  labs(title = "Topic-wise Frequency Trends",
       x = "Timestamp",
       y = "Normalized Frequency",
       color = "Topic") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") +
  facet_wrap(~ Period, scales = "free_x")


# 2. df0630의 sig_nm별 Timestamp에 따른 meanprc 추세 꺾은선 그래프
# df0630의 meanprc 정규화
df0630 <- df0630 %>%
  group_by(sig_nm) %>%
  mutate(meanprc_norm = rescale(meanprc))

# df0630 그래프
df0630 %>% 
  filter(Period != "Other") %>%
ggplot(aes(x = Timestamp, y = meanprc_norm, color = sig_nm, group = sig_nm)) +
  geom_line(size = 0.5) +
  labs(title = "Sig_nm-wise Mean Price Trends",
       x = "Timestamp",
       y = "Normalized Mean Price",
       color = "Sig_nm") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") +
  facet_wrap(~ Period, scales = "free_x")

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
  select(Timestamp, Type, Value) %>% 
  ungroup()

combined_df_long <- bind_rows(df1_g_long, df0630_long %>% 
  bind_rows(df_word)) %>%  
  mutate(Period = case_when(
    Timestamp >= "2013-02" & Timestamp <= "2017-04" ~ "Park",
    Timestamp >= "2017-05" & Timestamp <= "2022-05" ~ "Moon",
    TRUE ~ "Other"
  ))

combined_df_long$Period <- factor(combined_df_long$Period, levels = c("Park", "Moon", "Other"))


# 그래프 그리기
combined_df_long %>%
  filter(Period != "Other") %>%  # "Other" Period 데이터 제거
  ggplot(aes(x = Timestamp, y = Frequency_norm, color = Type, group = Type)) +
  geom_line(linewidth = 0.5) +
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
  geom_line(data = subset(combined_df, Period != "Other" & Type %in% c("Topic_0")), size = 1, color = "darkgreen") +  # "Other"를 제외한 데이터
  geom_line(data = subset(combined_df, Period != "Other" & Type %in% c("서울")), size = 1, color = "red") +  # "Other"를 제외한 데이터
  geom_line(data = subset(combined_df, Period != "Other" & Type == "Frequency"), aes(y = Value), size = 1, color = "orange")  +  # "Other"를 제외한 데이터
  facet_wrap(~ Period, scales = "free_x")   # 특정 단어

# 전체데이터
# topic 0 - 부동산:정반대 / 아파트:정반대 / 분양:정반대 / 시장:X / 서울:부합
# topic 2 - 쌍용:X / 쓸이:X / 씨티:약 부합(선행) / 싸움:X / 싸게:X
# topic 3 - etf:X / 증시:약 정반대 / 美中:X / ipo:X / 지표:정반대
# topic 4 - 비싼:X / 비싸:X / 불씨:X / 가장:X / 날씨:X
# topic 5 - kb:X / 국민은행:X / 금융:X / 자산운용:약 부합(선행) / 점포:X





# 특정 단어 빈도와 아파트 실거래 가격지수 KL-divergence
i = 1
j = 1

# 예시 : 서울
# park
df1_freq_df.tmp <- df1 %>% 
  filter(Period == "Park") %>% 
filter(Topic == topic[1]) %>% group_by(Timestamp) %>% summarise(total = sum(Frequency, na.rm = TRUE)) # topic - 서울이 속한 topic0

df2_meanprc_df.tmp <- df0630 %>%
  filter(Period == "Park") %>% 
  filter(sig_nm == sig_nm_uni[7]) # 아파트 실거래 지수 - 지역:서울

df1_word_df.tmp <- df1 %>% # 특정단어 "서울"
  filter(Period == "Park") %>% 
  filter(str_detect(Words, "서울")) %>% # "word" detect
  group_by(Timestamp) %>%
  summarise(Frequency = sum(Frequency))
# 특정 단어 빈도

merged_df.tmp_park <- merge(df1_freq_df.tmp, df2_meanprc_df.tmp, by = "Timestamp") %>%  merge(df1_word_df.tmp, by = "Timestamp")

merged_df.tmp_park %>% glimpse()

# total과 meanprc를 확률 분포로 변환
total_dist.tmp_park <- merged_df.tmp_park$total / sum(merged_df.tmp_park$total) # topic
meanprc_dist.tmp_park <- merged_df.tmp_park$meanprc / sum(merged_df.tmp_park$meanprc) # 아파트 실거래 지수
Frequency_dist.tmp_park <- merged_df.tmp_park$Frequency / sum(merged_df.tmp_park$Frequency) # 특정 단어 빈도

# topic-아파트 실거래 KL
rbind(total_dist.tmp_park, meanprc_dist.tmp_park) %>% 
  distance(method = "kullback-leibler")

# 단어빈도-아파트 실거래 KL
rbind(Frequency_dist.tmp_park, meanprc_dist.tmp_park) %>%  
  distance( method = "kullback-leibler")


# moon
df1_freq_df.tmp <- df1 %>% 
  filter(Period == "Moon") %>% 
  filter(Topic == topic[1]) %>% group_by(Timestamp) %>% summarise(total = sum(Frequency, na.rm = TRUE)) # topic - 서울이 속한 topic0

df2_meanprc_df.tmp <- df0630 %>%
  filter(Period == "Moon") %>% 
  filter(sig_nm == sig_nm_uni[7]) # 아파트 실거래 지수 - 지역:서울

df1_word_df.tmp <- df1 %>% # 특정단어 "서울"
  filter(Period == "Moon") %>% 
  filter(str_detect(Words, "서울")) %>% # "word" detect
  group_by(Timestamp) %>%
  summarise(Frequency = sum(Frequency))
# 특정 단어 빈도

merged_df.tmp_moon <- merge(df1_freq_df.tmp, df2_meanprc_df.tmp, by = "Timestamp") %>%  merge(df1_word_df.tmp, by = "Timestamp")

merged_df.tmp_moon %>% glimpse()

# total과 meanprc를 확률 분포로 변환
total_dist.tmp_moon <- merged_df.tmp_moon$total / sum(merged_df.tmp_moon$total) # topic
meanprc_dist.tmp_moon <- merged_df.tmp_moon$meanprc / sum(merged_df.tmp_moon$meanprc) # 아파트 실거래 지수
Frequency_dist.tmp_moon <- merged_df.tmp_moon$Frequency / sum(merged_df.tmp_moon$Frequency) # 특정 단어 빈도

# topic-아파트 실거래 KL
rbind(total_dist.tmp_moon, meanprc_dist.tmp_moon) %>% 
  distance(method = "kullback-leibler")

# 단어빈도-아파트 실거래 KL
rbind(Frequency_dist.tmp_moon, meanprc_dist.tmp_moon) %>%  
  distance( method = "kullback-leibler")