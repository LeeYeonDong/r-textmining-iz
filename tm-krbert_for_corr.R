library(tidyverse)
library(lubridate)

# 데이터 불러오기
df1 <- read_csv(file = "D:/대학원/논문/소논문/부동산_토픽모델링/topics_over_time0525.csv", 
         locale=locale("ko",encoding="utf-8"))

df2 <- read_csv(file = "D:/대학원/논문/소논문/부동산_토픽모델링/yeondong_rst.csv", 
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
  pivot_longer(cols = c(도심권, 동북권, 서북권, 서남권, 동남권),
               names_to = "sig_nm",
               values_to = "meanprc") %>%
  mutate(
    Timestamp = paste0(Timestamp, "_01"), # "_01"을 추가하여 "yyyy-mm-dd" 형식으로 변환
    Timestamp = ymd(Timestamp) # 날짜 형식으로 변환
  )

df0630 


# 기준
topic <- (df1$Topic %>% unique() %>% sort())[-1]

df1$year <- df1$Timestamp %>% substr(1,4) %>% as.integer()

year <- df1$year %>% unique()

df1 <- df1 %>% filter(!year %in% c(2023, 2024)) %>%
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

df1_freq_df.tmp <- df1 %>% filter(Topic == topic[i]) %>% group_by(Timestamp) %>% summarise(total = sum(Frequency, na.rm = TRUE))

df2_meanprc_df.tmp <- df0630 %>% filter(sig_nm == sig_nm_uni[j])

corr.tmp <- cor(df1_freq_df.tmp$total, df2_meanprc_df.tmp$meanprc)

topic_vec <- append(topic_vec,topic[i])
sig_nm_vec <- append(sig_nm_vec,sig_nm_uni[j])
corr_vec <- append(corr_vec,corr.tmp)

cat("현재", topic[i], "th topic의,", sig_nm_uni[j],"의 correlation을 구하고 있음\n")

}
}

cor_df <- tibble(topic_vec, sig_nm_vec, corr_vec)
cor_df %>% arrange(corr_vec)
cor_df %>% arrange(-corr_vec)

write.csv(cor_df, file = "D:/대학원/논문/소논문/부동산_토픽모델링/cor_df0510.csv", row.names=FALSE, fileEncoding = 'cp949')

  