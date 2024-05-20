# 필요한 패키지 로드
install.packages("pdftools")
library(pdftools)
library(stringr)

# 문제지
# PDF 파일 경로 설정
list_path <- list.files("D:/대학원/논문/Double Negation/rawdata/preprocess")

# "mum_"와 "_cut.pdf" 제거
list_path_제목 <- list_path %>%
  str_remove("^mum_") %>%          # 시작 부분의 "mum_" 제거
  str_remove("^scr_") %>%
  str_remove("_cut\\.pdf$")        # 끝 부분의 "_cut.pdf" 제거

file_path <- paste0("D:/대학원/논문/Double Negation/rawdata/preprocess/", list_path)


# 중괄호 안에 한글이 없고 숫자와 특수기호를 기준으로 텍스트를 분리
# 한 벡터에 숫자 1개만 있는 경우를 빈 문자열로 대체하는 함수
replace_single_numbers <- function(x) {
  if (str_detect(x, "^\\s*\\d+\\s*$")) {
    return("")
  }
  return(x)
}

# 소괄호 안에 단독 알파벳이 있는 줄을 찾는 패턴
pattern <- "^\\s*\\([A-Za-z]\\)\\s*$"

# 전처리
text_tb <- tibble(text_data = NA, 제목 = NA)

for (i in 1:length(list_path)) {
  tryCatch({
    
    cat(i,'번째 pdf 작업.\n')
    
text_data <- file_path[i] %>%
  pdf_text() %>% 
  str_split("\n") %>% 
  unlist() %>% # 각 문제별로 앞의 개행 문자와 공백 제거
  str_remove_all("A-PDF Page Cut DEMO: Purchase from www.A-PDF.com to remove the watermark") %>% 
  str_remove_all("\\[\\d점\\]") %>% 
  str_remove_all("영어") %>% 
  str_remove_all("영역") %>% 
  lapply(replace_single_numbers) %>% 
  unlist()

}, error = function(e) cat("불러올 수 없습니다!\n"))

# 빈 줄과 단독 알파벳 줄을 포함하는 인덱스 찾기
indexes <- which(str_detect(text_data, pattern) | str_detect(text_data, "^\\s*$"))

# 단독 알파벳 줄의 인덱스만 선택
alpha_indexes <- which(str_detect(text_data, pattern))

# 단독 알파벳 줄 주위의 빈 줄을 제거
for (index in alpha_indexes) {
  # 주위의 빈 줄 인덱스
  prev_index <- index - 1
  next_index <- index + 1
  
  # 앞뒤 빈 줄 제거
  if (prev_index > 0 && !is.na(text_data[prev_index]) && str_detect(text_data[prev_index], "^\\s*$")) {
    text_data[prev_index] <- NA
  }
  if (next_index <= length(text_data) && !is.na(text_data[next_index]) && str_detect(text_data[next_index], "^\\s*$")) {
    text_data[next_index] <- NA
  }
  
  # 해당 줄 자체 제거
  if (!is.na(text_data[index])) {
    text_data[index] <- NA
  }
}


# 데이터 분할 함수
split_data_by_empty_lines <- function(data) {
  # 빈 문자열을 기준으로 인덱스 찾기
  split_indices <- which(data == "")
  sections <- list()
  start <- 1
  
  for (i in seq_along(split_indices)) {
    end <- split_indices[i] - 1
    if (start <= end) {
      sections[[length(sections) + 1]] <- data[start:end]
    }
    start <- split_indices[i] + 1
  }
  
  # 마지막 구분자 이후 데이터 처리
  if (start <= length(data)) {
    sections[[length(sections) + 1]] <- data[start:length(data)]
  }
  
  return(sections)
}

# 데이터 분할 적용
text_data <- split_data_by_empty_lines(text_data)

# 리스트 요소를 하나의 문자열로 결합하는 함수
combine_elements <- function(section) {
  # 문자열 요소들을 줄바꿈 문자로 결합
  combined <- paste(section, collapse = "\n")
  return(combined)
}

# 각 리스트 요소를 결합하여 새로운 리스트 생성
text_data <- lapply(text_data, combine_elements) %>% 
  unlist()

  text_tb.tmp <- tibble(text_data)
  text_tb.tmp$제목 <- list_path_제목[i]
  
  text_tb <- bind_rows(text_tb, text_tb.tmp)
}

# 마침표로 끝나는 문장의 개수를 세는 함수 정의
count_sentences <- function(text) {
  sentences <- unlist(strsplit(text, "(?<=\\.)\\s*", perl = TRUE))
  return(sum(str_detect(sentences, "\\.$")))
}

# 데이터 전처리
text_tb_cleaned <- text_tb %>%
  filter(!is.na(text_data)) %>%                     # 행이 결측치이거나 NA인 경우 제거
  mutate(text_data = str_remove_all(text_data, "[가-힣]")) %>%  # 한글 제거
  mutate(text_data = str_remove_all(text_data, "[^\\w\\s.,!?\"']")) %>%  # 특정 문장 기호를 제외한 특수기호 제거
  mutate(text_data = str_replace_all(text_data, "\\s{2,}", " ")) %>%  # 두 칸 이상의 공백을 하나의 공백으로 변환
  mutate(text_data = str_replace_all(text_data, "[\n\t]", "")) %>%  # \n, \t를 제거
  mutate(text_data = str_trim(text_data)) %>%  # 벡터의 앞뒤 공백 제거
  filter(text_data != "") %>%  # 공백만 있는 데이터 프레임 행 제거
  filter_all(any_vars(!is.na(.))) %>%
  mutate(text_data = str_remove_all(text_data, "_{2,}")) %>%  # 두 개 이상의 연속된 밑줄 제거
  mutate(text_data = str_trim(text_data)) %>% # 전체 행이 NA인 경우 제거
  mutate(sentence_count = sapply(text_data, count_sentences)) %>%  # 마침표로 끝나는 문장 개수 세기
  filter(sentence_count >= 3) %>%  # 마침표로 끝나는 문장이 3개 이상인 경우만 남김
  select(-sentence_count) %>%  # 임시로 추가한 열 제거  # 앞뒤 공백 제거  
mutate(text_data = str_remove(text_data, "^\\b[M|W]\\b\\s*")) %>%  # 단독으로 데이터 가장 앞에 "M", "W"가 있는 경우 삭제
  mutate(text_data = str_remove_all(text_data, "\\b\\d+\\.\\s*")) %>%  # "숫자." 패턴 삭제
  mutate(text_data = str_trim(text_data)) %>%  # 앞뒤 공백 제거
distinct(text_data, .keep_all = TRUE)  # text_data 변수를 기준으로 중복 제거, .keep_all = TRUE는 모든 열을 유지

text_tb_cleaned %>% tail(30) %>% view()

library(writexl)
write_xlsx(text_tb_cleaned, "D:/대학원/논문/Double Negation/rawdata/textdata.xlsx")

