
# data 준비
뉴스_탈북 <- read_csv(file = "C:/대학원/논문/텍스트마이닝상담/탈북_park_df_com_본문제외.csv", col_names = TRUE, locale=locale('ko',encoding='euc-kr'))

뉴스_탈북$ID_park <- c(1:nrow(뉴스_탈북))

뉴스_탈북 %>% View()

## divide reviewtext into separate words
뉴스_탈북 %>% str()

ID제목_park <- 뉴스_탈북 %>% dplyr::select("ID_park","제목_park") ## MASS와 충돌

# 데이터 분할
ID제목_park_dim <- ID제목_park %>% dim()

ID제목_park_dim_int <- ID제목_park_dim[1] / 10000
ID제목_park_dim_int <- ID제목_park_dim_int %>% ceiling()
n <- ID제목_park_dim_int

ID제목_park_sp <- split(ID제목_park,rep(1:n, each = 10000))

# 데이터 셋 만들기
ID제목_park_tb <- list()
ID제목_park_data_set <- list()
ID제목_park_token <- c()
ID제목_park_한글 <- list()
ID제목_park_영어 <- list()
ID제목_park <- tibble()

## 단어기준 토큰화
for (i in 1:n){
  
  cat(i, '번째 데이터 리스트 tokenizer', '중 입니다.\n') 
  
  ID제목_park_tb[[i]] <- ID제목_park_sp[[i]] %>% tibble()
  
  ID제목_park_tb[[i]] <- ID제목_park_tb[[i]] %>% 
    unnest_tokens(input = 제목_park, output = word_park, token = "words", drop = FALSE)
  
  ID제목_park_data_set[[i]] <- ID제목_park_tb[[i]]  %>% 
    melt() %>% # 식별자id, 측정 변수variable, 측정치value 형태로 데이터를 재구성하는 함수
    as_tibble() %>% 
    select(4,1,2)
  
  names(ID제목_park_data_set[[i]]) <- c("ID_park","제목_park","word_park")
  
  ID제목_park_한글[[i]] <- ID제목_park_data_set[[i]] %>%  
    mutate(한글_park = str_match(word_park,'([가-힣]+)')[,2]) %>% ## "한글" variable을 만들고 한글만 저장         
    na.omit() %>% ## ([가-힣]+)/P') 한글만을 선택하는 정규표현식
    mutate(글자수_park = str_length(한글_park)) %>%   ## "글자수" variable을 만듭니다 
    filter(str_length(한글_park) >= 2) 
  
  ID제목_park_영어[[i]] <- ID제목_park_data_set[[i]] %>%  
    mutate(영어_park = str_match(word_park,'([a-zA-Z]+)')[,2]) %>% ## "한글" variable을 만들고 한글만 저장         
    na.omit() %>% ## 
    mutate(글자수_park = str_length(영어_park)) %>%   ## "글자수" variable을 만듭니다 
    filter(str_length(영어_park) >= 2) 
  
  ID제목_park <- bind_rows(ID제목_park,ID제목_park_한글[[i]])
  ID제목_park <- bind_rows(ID제목_park,ID제목_park_영어[[i]])
}

park_tokens <- ID제목_park %>% dplyr::select("ID_park","제목_park","word_park","글자수_park")

park_tokens_min5 <- park_tokens %>% 
  group_by(word_park) %>% 
  mutate(token_freq = n()) %>%  
  filter(token_freq >= 5) %>% 
  group_by(ID_park) %>% 
  summarise(parkTextClean = str_c(word_park, collapse = " "))

park_tokens_min5$ID_park <- park_tokens_min5$ID_park %>% as.character()

# split reviews and labels into train and test
train_ID_park <- rbinom(n = length(park_tokens_min5$ID_park), size = 1, prob = 0.1)
train_ID_park %>% table()
train_df_park <- tibble(park_tokens_min5$ID_park,train_ID_park)
names(train_df_park) <- c("ID_park","train")
train_df_park$ID_park <- train_df$ID_park %>% as.character()

train_ID_park <- rbinom(n = length(park_tokens_min5$ID_park), size = 1, prob = 0.1)
train_ID_park %>% table()
train_df_park <- tibble(park_tokens_min5$ID_park,train_ID_park)
names(train_df_park) <- c("ID_park","train")
train_df_park$ID_park <- train_df$ID_park %>% as.character()

x_train_park <- train_df_park %>% 
  left_join(y=park_tokens_min5, by= "ID_park", match = "all") %>% 
  select(parkTextClean) %>% pull()
x_train_park <- x_train_park[1:ceiling(length(x_train_park)*0.7)]

x_test_park <- x_train_park[1+ceiling(length(x_train_park)*0.7):length(x_train_park)]

y_train_park <- train_df_park %>% 
  left_join(y=park_tokens_min5, by= "ID_park", match = "all") %>% 
  select(train) %>% pull() %>% as.array()
y_train_park <- y_train_park[1:ceiling(length(y_train_park)*0.7)]

y_test_park <- y_train_park[1+ceiling(length(y_train_park)*0.7):length(y_train_park)]

x_train_park %>% length()
x_test_park %>% length()
y_train_park %>% length()
y_test_park %>% length()