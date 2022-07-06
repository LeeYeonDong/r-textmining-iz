# 구어전사텍스트모음 + 문어텍스트모음
install.packages("tidyverse")
library(tidyverse)
구어_dir <- c("C:/대학원/논문/동아대/구어전사텍스트모음")
문어_dir <- c("C:/대학원/논문/동아대/문어텍스트모음")
대화_dir <- c("C:/대학원/논문/동아대/텍스트모음3")

구어_file <- list.files(구어_dir)
문어_file <- list.files(문어_dir)
대화_file <- list.files(대화_dir)


# 준말+문법
준말 <- read_csv(file = "C:/대학원/논문/동아대/준말.csv", col_names = TRUE, locale=locale('ko',encoding='euc-kr'))
names(준말) <- "word" 
준말$구분 <- c("준말")

문법 <- read_csv(file = "C:/대학원/논문/동아대/문법.csv", col_names = TRUE, locale=locale('ko',encoding='euc-kr'))
names(문법) <- "word" 
문법$구분 <- c("문법")

준말문법 <- bind_rows(준말,문법)

# 구어 tokenizer 
구어_token_df <- tibble()

for(i in 1:length(구어_file)){
  cat(i, '번째 구어 데이터 tokenizer 산출', '중 입니다.\n') 
  
구어.tmp <- read_lines(paste0(구어_dir,"/",구어_file[i]))

CT <- grep("CT",구어.tmp)

구어.tmp <- 구어.tmp[CT]
구어.tmp <- 구어.tmp[-c(1:2)]

구어_list <- str_split(구어.tmp,"\t")

구어_token <- c()

for(j in 1:length(구어_list)){
  token.tmp <- 구어_list[[j]][3]
  구어_token <- append(구어_token,token.tmp)
}

파일 <- 구어_file[i]

구어_token.tmp <- tibble(구어_token,파일)
구어_token_df <- bind_rows(구어_token_df,구어_token.tmp)
}

구어_token_df$형태 <- c("구어")

names(구어_token_df) <- c("word","파일","형태")
구어_token_df <- 구어_token_df %>% filter(word != "NA")

a <- 구어_token_df$word

a <- gsub('[:+:]','',a)
a <- gsub('[:__:]','',a)
a <- gsub('[:.:]','',a)
a <- gsub('[:,:]','',a)
a <- gsub('[:?:]','',a)
a <- gsub('[[:digit:]]','',a)
a <- gsub('[[:upper:]]','',a)
a <- gsub('[[:space:]]','',a)
a <- gsub('[:.:]','',a)
a <- gsub('[:…:]','',a)
a <- gsub('//$','',a)
a <- gsub('/$','',a)

구어_token_df$word <- a

구어_df <- 구어_token_df %>% 
  left_join(준말문법,by="word") %>% 
  mutate(구분 = ifelse(is.na(구분), "해당없음", 구분))

구어_df <- 구어_df %>% filter(word != "")
구어_df$파일 %>% unique()


# 문어 tokenizer 
문어_token_df <- tibble()

for(i in 1:length(문어_file)){
  cat(i, '번째 문어 데이터 tokenizer 산출', '중 입니다.\n') 
  
  문어.tmp <- read_lines(paste0(문어_dir,"/",문어_file[i]))
  
  BS <- grep("BS",문어.tmp)
  
  문어.tmp <- 문어.tmp[BS]
  문어.tmp <- 문어.tmp[-c(1:2)]
  
  문어_list <- str_split(문어.tmp,"\t")
  
  문어_token <- c()
  
  for(j in 1:length(문어_list)){
    token.tmp <- 문어_list[[j]][3]
    문어_token <- append(문어_token,token.tmp)
  }
  
  파일 <- 문어_file[i]
  
  문어_token.tmp <- tibble(문어_token,파일)
  문어_token_df <- bind_rows(문어_token_df,문어_token.tmp)
}

문어_token_df$형태 <- c("문어")

names(문어_token_df) <- c("word","파일","형태")
문어_token_df <- 문어_token_df %>% filter(word != "NA")

a <- 문어_token_df$word

a <- gsub('[:+:]','',a)
a <- gsub('[:__:]','',a)
a <- gsub('[:.:]','',a)
a <- gsub('[:,:]','',a)
a <- gsub('[:?:]','',a)
a <- gsub('[[:digit:]]','',a)
a <- gsub('[[:upper:]]','',a)
a <- gsub('[[:space:]]','',a)
a <- gsub('[:.:]','',a)
a <- gsub('[:…:]','',a)
a <- gsub('//$','',a)
a <- gsub('/$','',a)

문어_token_df$word <- a

문어_df <- 문어_token_df %>% 
  left_join(준말문법,by="word") %>% 
  mutate(구분 = ifelse(is.na(구분), "해당없음", 구분))
문어_df <- 문어_df %>% filter(word != "")

# 대화 tokenizer 
대화_token_df1 <- tibble()

for(i in 1:length(대화_file)){
  cat(i, '번째 대화 데이터 tokenizer 산출', '중 입니다.\n') 
  
  대화.tmp <- read_lines(paste0(대화_dir,"/",대화_file[i]))

  CT <- grep("CT",대화.tmp)
  
  대화.tmp <- 대화.tmp[CT]
  대화.tmp <- 대화.tmp[-c(1:2)]
  
  대화_list <- str_split(대화.tmp,"\t")
  
  대화_token <- c()
  
  for(j in 1:length(대화_list)){
    token.tmp <- 대화_list[[j]][3]
    대화_token <- append(대화_token,token.tmp)
  }
  
  파일 <- 대화_file[i]
  
  대화_token.tmp <- tibble(대화_token,파일)
  대화_token_df1 <- bind_rows(대화_token_df1,대화_token.tmp)
}

대화_token_df1$형태 <- c("대화")

names(대화_token_df1) <- c("word","파일","형태")
대화_token_df1 <- 대화_token_df1 %>% filter(word != "NA")

a <- 대화_token_df1$word

a <- gsub('[:+:]','',a)
a <- gsub('[:__:]','',a)
a <- gsub('[:.:]','',a)
a <- gsub('[:,:]','',a)
a <- gsub('[:?:]','',a)
a <- gsub('[[:digit:]]','',a)
a <- gsub('[[:upper:]]','',a)
a <- gsub('[[:space:]]','',a)
a <- gsub('[:.:]','',a)
a <- gsub('[:…:]','',a)
a <- gsub('//$','',a)
a <- gsub('/$','',a)

대화_token_df1$word <- a

대화_df1 <- 대화_token_df1 %>% 
  left_join(준말문법,by="word") %>% 
  mutate(구분 = ifelse(is.na(구분), "해당없음", 구분))
대화_df1 <- 대화_df1 %>% filter(word != "")

#
대화_token_df2 <- tibble()

for(i in 5:length(대화_file)){
  cat(i, '번째 대화 데이터 tokenizer 산출', '중 입니다.\n') 
  
  대화.tmp <- read_lines(paste0(대화_dir,"/",대화_file[i]))
  
  BS <- grep("BS",대화.tmp)
  
  대화.tmp <- 대화.tmp[BS]
  대화.tmp <- 대화.tmp[-c(1:2)]
  
  대화_list <- str_split(대화.tmp,"\t")
  
  대화_token <- c()
  
  for(j in 1:length(대화_list)){
    token.tmp <- 대화_list[[j]][3]
    대화_token <- append(대화_token,token.tmp)
  }
  
  파일 <- 대화_file[i]
  
  대화_token.tmp <- tibble(대화_token,파일)
  대화_token_df2 <- bind_rows(대화_token_df2,대화_token.tmp)
}

대화_token_df2$형태 <- c("대화")

names(대화_token_df2) <- c("word","파일","형태")
대화_token_df2 <- 대화_token_df2 %>% filter(word != "NA")

a <- 대화_token_df2$word

a <- gsub('[:+:]','',a)
a <- gsub('[:__:]','',a)
a <- gsub('[:.:]','',a)
a <- gsub('[:,:]','',a)
a <- gsub('[:?:]','',a)
a <- gsub('[[:digit:]]','',a)
a <- gsub('[[:upper:]]','',a)
a <- gsub('[[:space:]]','',a)
a <- gsub('[:.:]','',a)
a <- gsub('[:…:]','',a)
a <- gsub('//$','',a)
a <- gsub('/$','',a)

대화_token_df2$word <- a

대화_df2 <- 대화_token_df2 %>% 
  left_join(준말문법,by="word") %>% 
  mutate(구분 = ifelse(is.na(구분), "해당없음", 구분))
대화_df2 <- 대화_df2 %>% filter(word != "")

대화_df <- bind_rows(대화_df1,대화_df2)


# 빈도 산출
# 구어
구어_빈도_df <- tibble()

파일수 <- 구어_df$파일 %>% unique()

for(i in 1:length(파일수)){
  cat(i, '번째 구어 데이터 빈도 산출', '중 입니다.\n') 

준말_빈도 <- 구어_df %>% 
  filter(파일 == 파일수[i]) %>% 
  filter(구분 == "준말") %>% 
  nrow()


문법_빈도<- 구어_df %>% 
  filter(파일 == 파일수[i])%>% 
  filter(구분 == "문법") %>% 
  nrow()

파일 <- 파일수[i]

빈도.tmp <- tibble(파일,준말_빈도,문법_빈도)
구어_빈도_df <- bind_rows(구어_빈도_df,빈도.tmp)
}

구어_빈도_df$문체 <- c("구어")

# 문어
문어_빈도_df <- tibble()

파일수 <- 문어_df$파일 %>% unique()

for(i in 1:length(파일수)){
  cat(i, '번째 문어 데이터 빈도 산출', '중 입니다.\n') 
  
  문법_빈도<- 문어_df %>% 
    filter(파일 == 파일수[i])%>% 
    filter(구분 == "문법") %>% 
    nrow()
  
  준말_빈도 <- 문어_df %>% 
    filter(파일 == 파일수[i])%>% 
    filter(구분 == "준말") %>% 
    nrow()
  
  파일 <- 파일수[i]
  
  빈도.tmp <- tibble(파일,준말_빈도,문법_빈도)
  문어_빈도_df <- bind_rows(문어_빈도_df,빈도.tmp)
}

문어_빈도_df$문체 <- c("문어")

# 대화
대화_빈도_df <- tibble()

파일수 <- 대화_df$파일 %>% unique()

for(i in 1:length(파일수)){
  cat(i, '번째 대화 데이터 빈도 산출', '중 입니다.\n') 
  
  준말_빈도 <- 대화_df %>% 
    filter(파일 == 파일수[i]) %>% 
    filter(구분 == "준말") %>% 
    nrow()
  
  
  문법_빈도<- 대화_df %>% 
    filter(파일 == 파일수[i])%>% 
    filter(구분 == "문법") %>% 
    nrow()
  
  파일 <- 파일수[i]
  
  빈도.tmp <- tibble(파일,준말_빈도,문법_빈도)
  대화_빈도_df <- bind_rows(대화_빈도_df,빈도.tmp)
}

대화_빈도_df$문체 <- c("대화")

# 합치기
구어_문어_대화_빈도_df <- bind_rows(구어_빈도_df,문어_빈도_df,대화_빈도_df)

write.csv(구어_문어_대화_빈도_df, file = "C:/대학원/논문/동아대/파일별_준말_문법_빈도.csv", row.names = FALSE)

# 변환
구어_문어_대화_빈도_df$문체 <- gsub("구어","0",구어_문어_대화_빈도_df$문체)
구어_문어_대화_빈도_df$문체 <- gsub("대화","0.5",구어_문어_대화_빈도_df$문체)
구어_문어_대화_빈도_df$문체 <- gsub("문어","1",구어_문어_대화_빈도_df$문체)

구어_문어_대화_빈도_df$문체 <- 구어_문어_대화_빈도_df$문체 %>% factor(levels = c("구어","대화","문어"))


## 로지스틱 회귀분석
reg_result <- glm(formula = 문체 ~ 준말_빈도 + 문법_빈도, family = binomial(link="logit") ,data = 구어_문어_대화_빈도_df)
reg_result %>% summary()


# 오즈비
reg_result %>% coef() %>% exp()
# 해석 : 문법_빈도가 1 증가하면 "구어체일 확률보다 문어체일 확률"(오즈비 : odds ratio)이 1.0945290배 증가한다, 준말_빈도가 1 증가하면 "구어체일 확률보다 문어체일 확률"(오즈비 : odds ratio)이가 0.9873946 배 증가한다
# 쉬운해석 : 문법_빈도가 높을수록 문어체이며, 준말_빈도가 높을수록 구어체이다.

# 공선성
library(car)
reg_result %>% vif()
# 모두 5보다 작다. 공선성에 문제 없다.

# 성능평가
samp <- sample(1:nrow(구어_문어_대화_빈도_df), size = 0.8*nrow(구어_문어_대화_빈도_df), replace = FALSE)
train_data <- 구어_문어_대화_빈도_df[samp,]
test_data <- 구어_문어_대화_빈도_df[-samp,]

reg_pred <- predict(reg_result, test_data,type = "response")
reg_pred %>% summary()

reg_pred_c <- ifelse(reg_pred >= 0.66,"1",
                     ifelse(reg_pred >= 0.33,"0.5","0"))

library(Epi)

reg_roc <- ROC(test=reg_pred, stat=test_data$문체, plot="ROC", AUC=TRUE, main="logistic regression")
reg_roc$AUC


# confusion matrix
install.packages("caret")
library(caret)
confusionMatrix(test_data$문체,as.factor(reg_pred_c))


# 논외
#xgboost
x1x2 <- 구어_문어_빈도_df %>% 
  select(문법_빈도,준말_빈도) %>% 
  data.matrix()

y <- 구어_문어_빈도_df %>% select(문체)
y <- 구어_문어_빈도_df$문체

bst1 <- xgboost(data = x1x2, label = y,
                max_depth = 6, nthread = 2, 
                nrounds = 1000)

rt <- xgb.importance(colnames(x1x2), model = bst1)
xgb.plot.importance(rt) %>% str()

print(rt)

rt <- rt %>% as_tibble()
rt <- rt %>% arrange(-Importance)
rt$Importance <- rt$Importance %>% round(4)

ggplot(rt, aes(x=Importance, y=reorder(Feature,Importance), fill = Importance)) + 
  geom_col() + 
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  scale_fill_gradient(low = "#5D5D5D", high = "#003300") +
  geom_text(aes(label = Importance),hjust = -0.1,size=5) +
  scale_x_continuous(expand = c(0,0), limit=c(0,1)) 

rt


구어_문어_빈도_df <- bind_rows(구어_빈도_df,문어_빈도_df)

구어_문어_빈도_df$문체 <- gsub("구어","0",구어_문어_빈도_df$문체)
구어_문어_빈도_df$문체 <- gsub("문어","1",구어_문어_빈도_df$문체)
구어_문어_빈도_df$문체 <- 구어_문어_빈도_df$문체 %>% as.factor()

reg_result0 <- glm(formula = 문체 ~ 준말_빈도 + 문법_빈도, family = binomial(link="logit") ,data = 구어_문어_빈도_df)
reg_result0 %>% summary()

#0
samp0 <- sample(1:nrow(구어_문어_빈도_df), size = 0.8*nrow(구어_문어_빈도_df), replace = FALSE)
train_data0 <- 구어_문어_빈도_df[samp0,]
test_data0 <- 구어_문어_빈도_df[-samp0,]

reg_pred0 <- predict(reg_result0, test_data0,type = "response")
reg_pred0 %>% summary()

# reg_pred0 <- ifelse(reg_pred0 >= 0.5,1,0)

reg_roc0 <- ROC(test=reg_pred0, stat=test_data0$문체, plot="ROC", AUC=TRUE, main="logistic regression")
reg_roc0$AUC
