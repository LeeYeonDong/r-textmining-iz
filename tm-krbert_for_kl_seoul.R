library(tidyverse)
library(lubridate)
# install.packages("philentropy")
library(philentropy)

# 데이터 불러오기
df1_s <- read_csv(file = "D:/대학원/논문/소논문/부동산_토픽모델링/topics_over_time0702.csv", 
                locale=locale("ko",encoding="utf-8"))

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
  pivot_longer(cols = c(도심권, 동북권, 서북권, 서남권, 동남권, 서울, 수도권, 경기, 인천),
               names_to = "sig_nm",
               values_to = "meanprc") %>%
  mutate(
    Timestamp = paste0(Timestamp, "_01"), # "_01"을 추가하여 "yyyy-mm-dd" 형식으로 변환
    Timestamp = ymd(Timestamp) # 날짜 형식으로 변환
  ) #아파트 실거래 가격 지수

df0630 

# 주제 대표 단어
# 각 Topic에서 Frequency가 가장 높은 단어 선별
representative_words_s <- df1_s %>%
  filter(Topic != -1) %>% # Topic이 -1이 아닌 데이터만 필터링
  separate_rows(Words, sep = ", ") %>% # 단어들을 개별 행으로 분리
  group_by(Topic, Words) %>%
  summarise(Total_Frequency = sum(Frequency, na.rm = TRUE), .groups = 'drop') %>% # 각 단어의 빈도수 합계 계산
  group_by(Topic) %>%
  slice_max(order_by = Total_Frequency, n = 5) %>% # 각 Topic에서 빈도수가 가장 높은 단어 선택
  summarise(Words = str_c(Words, collapse = ", "), .groups = 'drop') # 단어들을 쉼표로 묶어서 문자열로 변환


# 기준
topic <- (df1_s$Topic %>% unique() %>% sort())[-1]

df1_s$year <- df1_s$Timestamp %>% substr(1,4) %>% as.integer()

year <- df1_s$year %>% unique()

df1_s <- df1_s %>% filter(!year %in% c(2023, 2024)) %>%
  mutate(Timestamp = format(Timestamp, "%Y-%m")) %>% select(-year)

df0630 <- df0630 %>%
  filter(substr(as.character(Timestamp), 1, 4) %in% as.character(year)) %>%
  mutate(Timestamp = format(Timestamp, "%Y-%m"))

sig_nm_uni <- df0630$sig_nm %>% unique() %>% sort()

# kl 산출
topic_vec <- c()
sig_nm_vec <- c()
kl_vec <- c()

i = 1
j = 1

for (j in 1:length(sig_nm_uni)){
  for (i in 1:length(topic)){
    
    df1_s_freq_df.tmp <- df1_s %>% filter(Topic == topic[i]) %>% group_by(Timestamp) %>% summarise(total = sum(Frequency, na.rm = TRUE))
    
    df2_meanprc_df.tmp <- df0630 %>% filter(sig_nm == sig_nm_uni[j])
    
    merged_df.tmp <- merge(df1_s_freq_df.tmp, df2_meanprc_df.tmp, by = "Timestamp")
    
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

write.csv(kl_df, file = "D:/대학원/논문/소논문/부동산_토픽모델링/kl_s_df0706.csv", row.names=FALSE, fileEncoding = 'cp949')

