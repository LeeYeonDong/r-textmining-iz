# data 준비
뉴스_sports.news_롯데 <- read_csv(file = "C:/대학원/논문/소논문/뉴스_sports.news_롯데.csv", col_names = TRUE, locale=locale('ko',encoding='euc-kr'))

names(뉴스_sports.news_롯데) <- c("언론사_롯데","제목_롯데","날짜_롯데","링크_롯데","좋아_롯데","훈훈_롯데","슬퍼_롯데","화나_롯데","후속_롯데")

뉴스_롯데 <- 뉴스_sports.news_롯데[!is.na(뉴스_sports.news_롯데$날짜_롯데),]

뉴스_롯데$ID_롯데 <- c(1:nrow(뉴스_롯데))

뉴스_롯데 %>% head()

뉴스_롯데$제목_롯데 <- gsub("포토","",뉴스_롯데$제목_롯데)
뉴스_롯데$제목_롯데 <- gsub("오늘","",뉴스_롯데$제목_롯데)
뉴스_롯데$제목_롯데 <- gsub("경기","",뉴스_롯데$제목_롯데)
뉴스_롯데$제목_롯데 <- gsub("사진","",뉴스_롯데$제목_롯데)
뉴스_롯데$제목_롯데 <- gsub("스포츠","",뉴스_롯데$제목_롯데)
뉴스_롯데$제목_롯데 <- gsub("종합","",뉴스_롯데$제목_롯데)
뉴스_롯데$제목_롯데 <- gsub("다시","",뉴스_롯데$제목_롯데)
뉴스_롯데$제목_롯데 <- gsub("치어리더","",뉴스_롯데$제목_롯데)
뉴스_롯데$제목_롯데 <- gsub("\u7f8e","미국",뉴스_롯데$제목_롯데)
뉴스_롯데$제목_롯데 <- gsub("\u65e5","일본",뉴스_롯데$제목_롯데)

## divide reviewtext into separate words
뉴스_롯데 %>% str()
ID제목_롯데 <- 뉴스_롯데 %>% dplyr::select("ID_롯데","제목_롯데") ## MASS와 충돌

# 데이터 분할
ID제목_롯데_dim <- ID제목_롯데 %>% dim()

ID제목_롯데_dim_int <- ID제목_롯데_dim[1] / 10000
ID제목_롯데_dim_int <- ID제목_롯데_dim_int %>% ceiling()
n <- ID제목_롯데_dim_int

ID제목_롯데_sp <- split(ID제목_롯데,rep(1:n, each = 10000))

# 데이터 셋 만들기
ID제목_롯데_tb <- list()
ID제목_롯데_data_set <- list()
ID제목_롯데_token <- c()
ID제목_롯데_한글 <- list()
ID제목_롯데_영어 <- list()
ID제목_롯데 <- tibble()

## 단어기준 토큰화
for (i in 1:n){
  
  cat(i, '번째 데이터 리스트 tokenizer', '중 입니다.\n') 
  
  ID제목_롯데_tb[[i]] <- ID제목_롯데_sp[[i]] %>% tibble()
  
  ID제목_롯데_tb[[i]] <- ID제목_롯데_tb[[i]] %>% 
    unnest_tokens(input = 제목_롯데, output = word_롯데, token = "words", drop = FALSE)
  
  ID제목_롯데_data_set[[i]] <- ID제목_롯데_tb[[i]]  %>% 
    melt() %>% # 식별자id, 측정 변수variable, 측정치value 형태로 데이터를 재구성하는 함수
    as_tibble() %>% 
    select(4,1,2)
  
  names(ID제목_롯데_data_set[[i]]) <- c("ID_롯데","제목_롯데","word_롯데")
  
  ID제목_롯데_한글[[i]] <- ID제목_롯데_data_set[[i]] %>%  
    mutate(한글_롯데 = str_match(word_롯데,'([가-힣]+)')[,2]) %>% ## "한글" variable을 만들고 한글만 저장         
    na.omit() %>% ## ([가-힣]+)/P') 한글만을 선택하는 정규표현식
    mutate(글자수_롯데 = str_length(한글_롯데)) %>%   ## "글자수" variable을 만듭니다 
    filter(str_length(한글_롯데) >= 2) 
  
  ID제목_롯데_영어[[i]] <- ID제목_롯데_data_set[[i]] %>%  
    mutate(영어_롯데 = str_match(word_롯데,'([a-zA-Z]+)')[,2]) %>% ## "한글" variable을 만들고 한글만 저장         
    na.omit() %>% ## 
    mutate(글자수_롯데 = str_length(영어_롯데)) %>%   ## "글자수" variable을 만듭니다 
    filter(str_length(영어_롯데) >= 2) 
  
  ID제목_롯데 <- bind_rows(ID제목_롯데,ID제목_롯데_한글[[i]])
  ID제목_롯데 <- bind_rows(ID제목_롯데,ID제목_롯데_영어[[i]])
}

롯데_tokens <- ID제목_롯데 %>% dplyr::select("ID_롯데","제목_롯데","word_롯데","글자수_롯데")

## count the number of words per article and plot results
롯데_tokens %>% 
  group_by(ID_롯데) %>% 
  summarise(n_tokens = n()) %>%
  mutate(n_tokens_binned = cut(n_tokens, breaks = c(1,seq(0,40,3),Inf))) %>% 
  group_by(n_tokens_binned) %>% 
  summarise(n_article = n()) %>% 
  ggplot(aes(x = n_tokens_binned, y = n_article)) + 
  geom_bar(stat = 'identity',fill = 'blue') + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 5, size = 10)) + 
  geom_text(size = 5, aes(label = n_article), position = position_dodge(width = 1), vjust = -0.5)

# 신경망을 구축 할 때 총 5 개 이상의 빈도를 가진 단어 만 사용. 현재 컬렉션은 60,016개의 고유 한 단어로 내려갑니다.
롯데_tokens %>% 
  group_by(word_롯데) %>% 
  summarize(word_freq = n()) %>% 
  mutate(min_5_freq = case_when(word_freq < 5 ~'token freque롯데y : < 5', TRUE ~ 'token freque롯데y : >= 5')) %>% 
  group_by(min_5_freq) %>% 
  summarise(n_tokens = n()) %>% 
  mutate(ratio_tokens = n_tokens / sum(n_tokens)) 

롯데_tokens_min5 <- 롯데_tokens %>% 
  group_by(word_롯데) %>% 
  mutate(token_freq = n()) %>%  
  filter(token_freq >= 5) %>% 
  group_by(ID_롯데) %>% 
  summarise(롯데TextClean = str_c(word_롯데, collapse = " "))

롯데_tokens_min5$ID_롯데 <- 롯데_tokens_min5$ID_롯데 %>% as.character()

# split reviews and labels into train and test
train_ID_롯데 <- rbinom(n = length(롯데_tokens_min5$ID_롯데), size = 1, prob = 0.1)
train_ID_롯데 %>% table()
train_df_롯데 <- tibble(롯데_tokens_min5$ID_롯데,train_ID_롯데)
names(train_df_롯데) <- c("ID_롯데","train")
train_df_롯데$ID_롯데 <- train_df$ID_롯데 %>% as.character()

x_train_롯데 <- train_df_롯데 %>% 
  left_join(y=롯데_tokens_min5, by= "ID_롯데", match = "all") %>% 
  filter(train == 1) %>% 
  select(롯데TextClean) %>% pull()

x_test_롯데 <- train_df_롯데 %>% 
  left_join(y=롯데_tokens_min5, by= "ID_롯데", match = "all") %>% 
  filter(train == 0) %>% 
  select(롯데TextClean) %>% pull()

y_train_롯데 <- train_df_롯데 %>% 
  left_join(y=롯데_tokens_min5, by= "ID_롯데", match = "all") %>% 
  filter(train == 1) %>% 
  select(train) %>% pull() %>% as.array()

y_test_롯데 <- train_df_롯데 %>% 
  left_join(y=롯데_tokens_min5, by= "ID_롯데", match = "all") %>% 
  filter(train == 0) %>% 
  select(train) %>% pull() %>% as.array()

x_train_롯데 %>% length()
x_test_롯데 %>% length()
y_train_롯데 %>% length()
y_test_롯데 %>% length()