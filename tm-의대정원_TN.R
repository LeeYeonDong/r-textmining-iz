# KoNLP useNIADic
# install.packages("multilinguer")
library(multilinguer)
# install_jdk() # rjava 설치
# install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), type = "binary")
#
# install.packages("remotes")
# devtools::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))
library(KoNLP) #최종적으로 "KoNLP" 패키지를 불러옵니다

# devtools::install_github('haven-jeon/NIADic/NIADic', build_vignettes = TRUE)
# Sys.setenv(JAVA_HOME='C:/Program Files/Java/jdk-18.0.1.1')  # 설치한 JAVA version에 따라 달라집니다
buildDictionary(ext_dic = "woorimalsam")  # "woorimalsam" dic을 불러옵니다
useNIADic() 


# 1. https://repo1.maven.org/maven2/org/scala-lang/scala-library/2.11.8/scala-library-2.11.8.jar 여기에 접속하셔서 <scala-library-2.11.8> 파일을 다운
# 2. C:\Users\선생님 노트북 이름\AppData\Local\R\win-library\4.2\KoNLP\java에 가셔서 다운받은 scala-library-2.11.8 파일을 붙여넣기한다
# 3. C:\Program Files\R\R-4.2.2\library 로 간다
# 4. 아래 KoNLP 다운받아 C:\Program Files\R\R-4.2.2\library에 붙여넣은 다음 KoNLP폴더를 압축을 푼다
# 5. R studio를 모두 끄고 다시 실행한다



# textmining
library(ggplot2)
library(tm)
library(qdap)
library(wordcloud2)
library(stringr)
library(rJava)
library(tidytext)
library(dplyr)
library(tidyverse)
library(NLP)
library(base64enc)
library(RmecabKo)
library(reshape2)
library(lubridate)
library(purrr)

# 데이터 불러오기 및 전처리(Preprocessing)
# Load the readxl library
library(readxl)

yt_df_raw <- read_excel("D:/대학원/상담/커뮤니케이션학과/의료분쟁/df_ytb_com0605.xlsx") %>% 
  mutate(본문 = str_replace_all(댓글, "[^가-힣a-zA-Z0-9.,!?\\s]", " ")) %>% 
  as.data.frame() %>% 
  as_tibble() %>%
  filter(날짜 != "없음") %>% select(-댓글) %>%
  distinct(본문, .keep_all = TRUE)


# 글쓴이 변수의 빈도 계산
yt_df_raw %>%
  count(글쓴이) %>%
  filter(n > 150 | n < 2) %>%  # 빈도가 5 이상 50 이하인 경우를 제외
  arrange(desc(n)) %>% 
  ggplot(aes(x = reorder(글쓴이, n), y = n, fill = n)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "blue", high = "red") +  # 빈도에 따라 색상을 설정
  labs(title = "Distribution of Authors",
       x = "Author",
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # x축 텍스트 제거 (너무 많아서 보기가 어려움)
        axis.ticks.x = element_blank(),  # x축 눈금 제거
        panel.grid.minor = element_line(color = "black")) +
  guides(fill = "none")  # 범례 제거

yt_df_raw %>%
  count(글쓴이) %>%
  arrange(desc(n)) %>% 
  write.csv("D:/대학원/상담/커뮤니케이션학과/의료분쟁/yt_df_글쓴이별_댓글작성건수.csv", row.names = FALSE, fileEncoding = 'cp949')

yt_df_raw %>% glimpse()
yt_df_raw$제목 %>% unique() %>% length()
yt_df_raw$날짜 %>% unique() %>% max()
yt_df_raw$날짜 %>% unique() %>% min()

# 날짜 변환
# 현재 날짜
current_date <- as.Date("2024-06-10")

# 상대적 날짜 변환 함수
convert_relative_date <- function(date_str, current_date) {
  date_str <- gsub("\\(.*\\)", "", date_str) # 수정됨 등 제거
  date_str <- trimws(date_str) # 양쪽 공백 제거
  
  if (grepl("일전", date_str)) {
    days <- as.numeric(gsub("일전", "", date_str))
    return(current_date - days(days))
  } else if (grepl("시간전", date_str)) {
    hours <- as.numeric(gsub("시간전", "", date_str))
    return(current_date - dhours(hours))
  } else if (grepl("분전", date_str)) {
    minutes <- as.numeric(gsub("분전", "", date_str))
    return(current_date - dminutes(minutes))
  } else if (grepl("개월전", date_str)) {
    months <- as.numeric(gsub("개월전", "", date_str))
    return(current_date %m-% months(months))
  } else if (grepl("주전", date_str)) {
    weeks <- as.numeric(gsub("주전", "", date_str))
    return(current_date - dweeks(weeks))
  }
  return(NA) # 알 수 없는 형식
}

yt_df_raw %>% select(날짜) %>% unique() %>% as.data.frame()

# 상대적 날짜를 실제 날짜로 변환
yt_df_raw <- yt_df_raw %>%
  filter(날짜 != "없음") %>% # 날짜가 "없음"이 아닌 행만 선택
  rowwise() %>%
  mutate(날짜 = convert_relative_date(날짜, current_date)) %>%
  ungroup() %>%
  mutate(날짜 = format(날짜, "%Y.%m.%d"))

# 결과 확인
print(yt_df_raw)

# yt_df_raw <- yt_df_raw %>% head(1000)

# id 부여
yt_df_raw$id <- c(1:nrow(yt_df_raw))

# # 데이터 프레임을 100행씩 쪼개기
# yt_df <- yt_df_raw %>%
#   mutate(group = (row_number() - 1) %/% 100) %>%
#   group_split(group)
# 
# yt_df[[1]]
# 


# # 명사와 형용사만 추출하는 함수 정의
# extract_nouns_adjectives <- function(txt) {
#   # SimplePos09를 사용하여 형태소 분석 수행
#   pos_result <- SimplePos09(txt)
#   # 명사(N)와 형용사(PA)를 추출
#   word <- unlist(str_extract_all(pos_result, "[가-힣]+/N|[가-힣]+/PA"))
#   # 품사 태그 제거
#   word <- str_remove_all(word, "/[A-Z]+")
#   return(word)
# }

# 조사 패턴 정의
josa_patterns <- c("이", "가", "은", "는", "을", "를", "에", "의", "로", "와", "과", "도", "만", "부터", "까지", "에서", "으로", "라고", "랑", "이랑", "하고")

# remove_josa 함수 정의
remove_josa <- function(text, patterns) {
  for (pattern in patterns) {
    text <- str_remove(text, paste0(pattern, "$"))
  }
  return(text)
}

yt_df_pos <- tibble()

yt_df_pos$글쓴이 %>% unique()

# 텍스트 전처리 함수 정의
preprocess_text <- function(text) {
  text <- str_replace_all(text, "[^가-힣0-9a-zA-Z\\s]", "")  # 특수 문자 제거
  text <- str_squish(text)  # 연속된 공백 제거
  return(text)
}

# 전체 데이터 프레임에 대해 처리 함수 정의
# 명사 형용사만 뽑기
process_data <- function(df) {
  df %>%
    mutate(본문 = preprocess_text(본문)) %>%  # 텍스트 전처리
    rowwise() %>%
    mutate(word = list(SimplePos09(본문))) %>%
    unnest(word) %>%
    rowwise() %>%
    mutate(word = str_split(word, "\\+")) %>%
    unnest(word) %>% 
    filter(str_detect(word, "/N") | str_detect(word, "/PA")) %>% 
    mutate(word = str_remove(word, "/.*")) %>%
    ungroup() %>% 
    filter(str_length(word) >= 2) %>%
    mutate(word = sapply(word, remove_josa, patterns = josa_patterns)) %>%
    mutate(word = as.character(word))  # Ensure 'word' is always a character
}

# 전체 데이터 프레임에 대해 처리 적용
yt_df_pos <- tryCatch({
  process_data(yt_df_raw)
}, error = function(e) {
  message("Error in processing data: ", e)
  return(tibble())  # 빈 데이터프레임 반환
})

# results_yt <- results_yt[!sapply(results_yt, is.null)]
# 
# # 결과를 하나의 데이터프레임으로 병합
# yt_df_pos <- bind_rows(results_yt)
# 
test <- yt_df_pos$id %>% unique() %>% tail(1)

yt_df_pos %>% filter(id == (test-1)) %>% view()

yt_df_pos %>% glimpse()

yt_df_pos %>% 
  select(word) %>%
count(word, sort = TRUE) %>%
  top_n(20, n) %>% 
  wordcloud2(size = 1.0, color = "darkgreen", minRotation = 0, maxRotation = 0)

yt_df_pos %>% 
  select(word) %>%
  count(word, sort = TRUE) %>% 
  write.csv("D:/대학원/상담/커뮤니케이션학과/의료분쟁/yt_df_count_유튜브_수정.csv", row.names = FALSE, fileEncoding = 'cp949')

# tf_idf
# 'id'를 문서 ID로 사용하고 'word'를 단어로 사용합니다.
tf_idf_df <- yt_df_pos %>%
  count(id, word, sort = TRUE) %>%  # 각 단어의 빈도 계산
  filter(n >= 5) %>%   
  bind_tf_idf(term = word, document = id, n = n) %>%
  arrange(desc(tf_idf))

write.csv(tf_idf_df, "D:/대학원/상담/커뮤니케이션학과/의료분쟁/tf_idf_df_유튜브_수정.csv", row.names = FALSE, fileEncoding = 'cp949')


#### 감성분석
##감성사전
sent_dic <- read_csv("D:/대학원/논문/소논문/스포츠기사/SentiWord_Dict.txt")
names(sent_dic) <- c("value")
sent_dic <- sent_dic$value
sent_dic <- strsplit(sent_dic, split="\t")  

word <- c()
polarity <- c()

for (i in 1:length(sent_dic)){ 
  word <- append(word,sent_dic[[i]][1])
  polarity <- append(polarity,sent_dic[[i]][2])
}

sent_dic <- tibble(word,polarity)
sent_dic$polarity <- sent_dic$polarity %>% as.double()

sent_dic %>% filter(word == "승리")

grep("승리",sent_dic$word)

이기 <- grep("이기",sent_dic$word)

sent_dic[7694,2] <- c(2)
sent_dic[7695,2] <- c(2)

# 전체
## 단어에 감정 점수 부여
yt_df_pos_pnn <- yt_df_pos %>% 
  left_join(sent_dic,by="word") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity)) %>% 
  mutate(sentiment = ifelse(polarity > 0, "pos", ifelse(polarity < 0, "neg", "neu"))) 


yt_df_pos_pnn_score <- yt_df_pos_pnn %>% 
  count(sentiment) %>% 
  mutate(ratio = round((n/sum(n)*100),2))

# 빈출 감정단어 파악 30개
yt_df_pos_pnn_top <- yt_df_pos_pnn %>% 
  filter(sentiment != "neu") %>% 
  count(sentiment, word) %>% 
  group_by(sentiment) %>% 
  slice_max(n, n=20)

ggplot(yt_df_pos_pnn_top, aes(x = reorder(word, n), y = n, fill = sentiment)) +
  geom_col() +
  labs(x = "", y = "") + 
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.2, vjust = 0.5) +  # Adjust hjust and vjust
  facet_wrap(~ sentiment, scales = "free") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +  # Expand y-axis to avoid clipping text
  theme_minimal() +
  theme(legend.position = "bottom")

## 긍정 부정 점수가 가장 높은 기사
library(writexl)
yt_df_pos_pnn %>%
  group_by(id) %>%
  summarise(score = sum(polarity, na.rm = TRUE)) %>% arrange(-score) %>% 
  left_join(yt_df_raw, by = "id") %>% 
  distinct(id, 본문, .keep_all = TRUE) %>%
  arrange(desc(score)) %>%
  select(id, 본문, score) %>% 
write_xlsx("D:/대학원/상담/커뮤니케이션학과/의료분쟁/yt_df_pos_pnn_유튜브_수정.xlsx")

yt_df_pos_pnn %>%
write_xlsx("D:/대학원/상담/커뮤니케이션학과/의료분쟁/yt_df_pos_pnn_유튜브_수정_단어별 감정점수.xlsx")



# 단어간 상관분석 - 다른 단어에 비해 상대적으로 자주 함께 사용된 단어 파악
library(widyr)

yt_df_pos_cor <- yt_df_pos_pnn %>% 
  select(id, word) %>% 
  add_count(word) %>% 
  filter(n >= 30) %>% 
  pairwise_cor(item = word,
               feature = id,
               sort = TRUE)

library(tidygraph)
library(ggraph)

# 전체
graph_cor <- yt_df_pos_cor %>% 
  filter(correlation >= 0.5) %>% 
  as_tbl_graph(directed = FALSE) %>% 
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))

set.seed(1029)

ggraph(graph_cor, layout = "fr") +
  
  geom_edge_link(color = "gray50",
                 aes(edge_alpha = correlation,
                     edge_width = correlation),
                 show.legend = FALSE) +
  scale_edge_width(range = c(1,4)) +
  
  geom_node_point(aes(size = centrality,
                      color = group),
                  show.legend = FALSE) +
  scale_size(range = c(5,10)) +
  
  geom_node_text(aes(label = name),
                 repel = TRUE,
                 size = 5) +
  theme_graph()


# visualization : 개별단어
graph_cor_의대 <- yt_df_pos_cor %>% 
  filter(correlation >= 0.1) %>% # correlation 0.1 이상
  filter(item1 == "의대") %>%
  as_tbl_graph(directed = FALSE) %>% 
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))

set.seed(1029)

ggraph(graph_cor_의대, layout = "fr") +
  
  geom_edge_link(color = "gray50",
                 aes(edge_alpha = correlation,
                     edge_width = correlation),
                 show.legend = FALSE) +
  scale_edge_width(range = c(1,4)) +
  
  geom_node_point(aes(size = centrality,
                      color = group),
                  show.legend = FALSE) +
  scale_size(range = c(5,10)) +
  
  geom_node_text(aes(label = name),
                 repel = TRUE,
                 size = 5) +
  theme_graph()

# 
# # 명사 형용사만 추출 for문
# # process_row 함수 정의
# process_row <- function(row) {
#   result <- try({
#     yt_df_tmp <- row %>%
#       select(id, 본문, 날짜) %>%
#       mutate(본문 = preprocess_text(본문))  # 텍스트 전처리
#     
#     yt_df_pos_tmp <- yt_df_tmp %>%
#       rowwise() %>%
#       mutate(word = list(SimplePos09(본문))) %>%
#       unnest(word) %>%
#       rowwise() %>%
#       mutate(word = str_split(word, "\\+")) %>%
#       unnest(word) %>% 
#       filter(str_detect(word, "/N") | str_detect(word, "/PA")) %>% 
#       mutate(word = str_remove(word, "/.*")) %>%
#       ungroup() %>% 
#       filter(str_length(word) >= 2) %>%
#       mutate(word = sapply(word, remove_josa, patterns = josa_patterns)) %>%
#       mutate(word = as.character(word))  # Ensure 'word' is always a character
#     
#     return(yt_df_pos_tmp)
#   }, silent = TRUE)
#   
#   if (inherits(result, "try-error")) {
#     return(NULL)
#   } else {
#     return(result)
#   }
# }
# 
# # 각 행에 대해 process_row 함수 적용
# results <- lapply(seq_len(nrow(yt_df_raw)), function(i) {
#   cat(i, "번째 행 처리 중...\n")
#   result <- process_row(yt_df_raw[i, ])
#   if (is.null(result)) {
#     cat(i, "번째 행 처리 실패\n")
#   } else {
#     cat(i, "번째 행 처리 성공\n")
#   }
#   return(result)
# })
# 
# # 유효한 결과만 병합
# results <- results[!sapply(results, is.null)]
# 
# # 결과를 하나의 데이터프레임으로 병합
# yt_df_pos <- bind_rows(results)
