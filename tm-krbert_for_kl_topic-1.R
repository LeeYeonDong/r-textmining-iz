# 날짜 변수에서 월 단위로 그룹화하여 월별 "제목" 빈도 계산
monthly_title_freq <- df1 %>%
  group_by(Timestamp) %>%
  summarise(TitleCount = n())

# "제목" 변수에 "부동산"이란 단어가 들어간 월별 "제목" 빈도 계산
monthly_budongsan_freq <- df1 %>%
  filter(str_detect(Words, "부동산")) %>%
  group_by(Timestamp) %>%
  summarise(BudongsanCount = n())

# 두 데이터 프레임 결합
combined_freq <- monthly_title_freq %>%
  left_join(monthly_budongsan_freq, by = "Timestamp") %>%
  replace_na(list(BudongsanCount = 0))

# 데이터 프레임을 길게 변환하여 ggplot에 맞게 변환
combined_freq_long <- combined_freq %>%
  pivot_longer(cols = c(TitleCount, BudongsanCount),
               names_to = "Type",
               values_to = "Count")

# 추세 그래프 그리기
combined_freq_long %>% 
  filter(Type == "BudongsanCount") %>% 
ggplot(aes(x = Timestamp, y = Count, color = Type, group = Type)) +
  geom_line(size = 1) +
  labs(title = "Monthly Trends of Title and 'Budongsan' Counts",
       x = "Timestamp",
       y = "Count",
       color = "Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8), legend.position = "bottom")


combined_freq_ratio <- combined_freq %>% 
mutate(ratio = BudongsanCount / TitleCount)

# ratio 추세 그래프 그리기
ggplot(combined_freq_ratio, aes(x = Timestamp, y = ratio, group = 1)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Monthly Trend of Ratio",
       x = "Timestamp",
       y = "Ratio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))


# 부동산 - topic
monthly_title_freq <- df1 %>% 
  filter(Topic != -1) %>% 
  group_by(Timestamp) %>%
  summarise(TitleCount = n())

monthly_title_freq_1 <- df1 %>% 
  filter(Topic == -1) %>% 
  group_by(Timestamp) %>%
  summarise(TitleCount_1 = n())

monthly_budongsan_freq <- df1 %>%
  filter(Topic != -1) %>% 
  filter(str_detect(Words, "부동산")) %>%
  group_by(Timestamp) %>%
  summarise(BudongsanCount = n())

monthly_budongsan_freq_1 <- df1 %>%
  filter(Topic == -1) %>% 
  filter(str_detect(Words, "부동산")) %>%
  group_by(Timestamp) %>%
  summarise(BudongsanCount_1 = n())

monthly_seoul_freq <- df1 %>%
  filter(Topic != -1) %>% 
  filter(str_detect(Words, "서울")) %>%
  group_by(Timestamp) %>%
  summarise(SeoulCount = n())

monthly_seoul_freq_1 <- df1 %>%
  filter(Topic == -1) %>% 
  filter(str_detect(Words, "서울")) %>%
  group_by(Timestamp) %>%
  summarise(SeoulCount_1 = n())


combined_freq_a <- monthly_title_freq %>%
  left_join(monthly_title_freq_1, by = "Timestamp") %>%
  left_join(monthly_budongsan_freq, by = "Timestamp") %>%
  left_join(monthly_budongsan_freq_1, by = "Timestamp") %>%
  left_join(monthly_seoul_freq, by = "Timestamp") %>%
  left_join(monthly_seoul_freq_1, by = "Timestamp") %>%
  replace_na(list(BudongsanCount = 0))


combined_freq_a_long <- combined_freq_a %>%
  pivot_longer(cols = c(TitleCount, TitleCount_1, BudongsanCount, BudongsanCount_1, SeoulCount, SeoulCount_1),
               names_to = "Type",
               values_to = "Count")

combined_freq_a_long %>% 
  #filter(Type != "TitleCount") %>% 
  #filter(Type == c("SeoulCount","SeoulCount_1")) %>% 
  filter(Type == c("BudongsanCount","BudongsanCount_1")) %>% 
ggplot(aes(x = Timestamp, y = Count, color = Type, group = Type)) +
  geom_line(size = 1) +
  labs(title = "Trend by Type over Time",
       x = "Timestamp",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8), legend.position = "bottom")


