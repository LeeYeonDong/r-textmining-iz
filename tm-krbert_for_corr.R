library(tidyverse)

# 데이터 불러오기
df1 <- read_csv(file = "D:/대학원/논문/소논문/부동산_토픽모델링/topics_over_time0429.csv", 
         locale=locale("ko",encoding="utf-8"))

df2 <- read_csv(file = "D:/대학원/논문/소논문/부동산_토픽모델링/yeondong_rst.csv", 
                locale=locale("ko",encoding="utf-8"))


# 기준
topic <- (df1$Topic %>% unique() %>% sort())[-1]

df1$Timestamp <- df1$Timestamp %>% substr(1,4) %>% as.integer()

df1 <- df1 %>% filter(!Timestamp %in% c(2023, 2024))

timestamp <- df1$Timestamp %>% unique()

df2 <- df2 %>% filter(year %in% timestamp)

sig_nm_uni <- df2$sig_nm %>% unique() %>% sort()

# corr 산출
topic_vec <- c()
sig_nm_vec <- c()
corr_vec <- c()

for (j in 1:length(sig_nm_uni)){
for (i in 1:length(topic)){

df1_freq_df.tmp <- df1 %>% filter(Topic == topic[i]) %>% group_by(Timestamp) %>% summarise(total = sum(Frequency, na.rm = TRUE))

df2_meanprc_df.tmp <- df2 %>% filter(sig_nm == sig_nm_uni[j]) %>% filter(year %in% df1_freq_df.tmp$Timestamp)

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

  