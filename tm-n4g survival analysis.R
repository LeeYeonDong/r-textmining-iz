library(tidyverse)
library(survival)
library(GGally)
library(survminer)


# 데이터 불러오기
n4g_ds <- read_csv(file = "D:/대학원/논문/소논문/텍스트마이닝 생존분석/n4g_ds.csv", 
                   locale=locale("ko",encoding="UTF-8"))

n4g_lol <- read_csv(file = "D:/대학원/논문/소논문/텍스트마이닝 생존분석/n4g_lol.csv", 
                   locale=locale("ko",encoding="UTF-8"))

n4g_lou <- read_csv(file = "D:/대학원/논문/소논문/텍스트마이닝 생존분석/n4g_lou.csv", 
                    locale=locale("ko",encoding="UTF-8"))

n4g_mc <- read_csv(file = "D:/대학원/논문/소논문/텍스트마이닝 생존분석/n4g_mc.csv", 
                    locale=locale("ko",encoding="UTF-8"))

# 이름 삽입
# n4g_ds$이름 <- c("1")
# n4g_lol$이름 <- c("2")
# n4g_lou$이름 <- c("3")
# n4g_mc$이름 <- c("4")

n4g_ds$이름 <- c("ds")
n4g_lol$이름 <- c("lol")
n4g_lou$이름 <- c("lou")
n4g_mc$이름 <- c("mc")

# 데이터 프레임 병합
n4g <- bind_rows(n4g_ds,n4g_lol,n4g_lou,n4g_mc)
n4g %>% dim()

# 데이터 확인
n4g %>% str()

# 데이터 전처리
n4g$관심도 <- str_match(n4g$관심도,"\\d+")
n4g$관심도 <- n4g$관심도 %>% as.integer()

n4g$날짜 <- str_match(n4g$날짜,"\\d+")
n4g$날짜 <- n4g$날짜 %>% as.integer()
n4g$날짜 <- max(n4g$날짜) - n4g$날짜 # 첫번째 발매일에 맞추어서 보정 필요

# n4g$이름 <- n4g$이름 %>% as.integer()
n4g$종류 <- n4g$종류 %>% as.factor()

n4g$종류 <- gsub("Opinion",0,n4g$종류)
n4g$종류 <- gsub("News",1,n4g$종류)
n4g$종류 <- gsub("Rumor",2,n4g$종류)
n4g$종류 <- gsub("Article",3,n4g$종류)
n4g$종류 <- gsub("Image",4,n4g$종류)
n4g$종류 <- gsub("Review",6,n4g$종류)
n4g$종류 <- gsub("Interview",7,n4g$종류)
n4g$종류 <- gsub("Podcast",8,n4g$종류)
n4g$종류 <- gsub("Screenshot",9,n4g$종류)
n4g$종류 <- gsub("Trailer",10,n4g$종류)
n4g$종류 <- gsub("Preview",11,n4g$종류)
n4g$종류 <- gsub("Videocast",12,n4g$종류)
n4g$종류 <- gsub("Video",5,n4g$종류)

n4g$종류 %>% unique()
n4g$종류 <- n4g$종류 %>% as.integer()

n4g$id <- c(1:length(n4g$제목))

n4g$관심도 %>% quantile()
n4g$관심도over <- ifelse(n4g$관심도 > 140,1,0)# quantile에 따라 구분구간 조정

n4g_2 <- n4g %>% filter(이름 %in% c("ds","mc"))
n4g_2$이름 %>% unique()

n4g_3 <- n4g %>% filter(이름 %in% c("ds","lol"))
n4g_3$이름 %>% unique()

# data:stanford2과 비교
# stan$id -> n4g$id / stan$time -> n4g$댓글 / stan$mismatch -> n4g$이름 / stan$age -> n4g$날짜 / stan$status -> n4g$관심도 / 

# K-M model (참고)
n4g_2_KM <- survfit(
  Surv(time = n4g_2$댓글,
       event = n4g_2$관심도over) ~ n4g_2$이름, data = n4g_2, type="kaplan-meier") 

n4g_2_KM %>% summary()

n4g_2_KM %>% ggsurvplot(conf.int = TRUE, surv.median.line = "hv", pval = TRUE, pval.method = TRUE)

n4g_3_KM <- survfit(
  Surv(time = n4g_3$댓글,
       event = n4g_3$관심도over) ~ n4g_3$이름, data = n4g_3, type="kaplan-meier") 

n4g_3_KM %>% summary()

n4g_3_KM %>% ggsurvplot(conf.int = TRUE, surv.median.line = "hv", pval = TRUE, pval.method = TRUE)

# Cox Proportional Hazard model
n4g_2_cox1 <- coxph(
  Surv(time = n4g_2$댓글,
       event = n4g_2$관심도over) ~ n4g_2$이름, data = n4g_2)

n4g_2_cox1 %>% summary()


n4g_2_cox2 <- coxph(
  Surv(time = n4g_2$댓글,
       event = n4g_2$관심도over) ~ n4g_2$이름 + n4g_2$날짜, data = n4g_2)

n4g_2_cox2 %>% summary()


n4g_3_cox2 <- coxph(
  Surv(time = n4g_3$댓글,
       event = n4g_3$관심도over) ~ n4g_3$이름 + n4g_3$날짜, data = n4g_3)

n4g_3_cox2 %>% summary()


# plot
# plot(survfit(n4g_cox1),xlab="time", ylab="Survival Rate", conf.int=FALSE, col=1:4)
# legend("topright", legend=c(1,2,3,4), lty = 1, col = 1:4, text.col = 1:4, title = '이름')


# check linearity (for the model that used num X's) using MARTINGALE residuals 
# add a line ax y=residual = 0
# fit a smoother thru the points
# type: the type of residuals to present on Y axis. Allowed values include one of c(“martingale”, “deviance”, “score”, “schoenfeld”, “dfbeta”, “dfbetas”, “scaledsch”, “partial”).
ggcoxdiagnostics(n4g_2_cox1, type = "martingale", linear.predictions = TRUE)
ggcoxdiagnostics(n4g_2_cox1, type = "deviance", linear.predictions = TRUE)

ggcoxdiagnostics(n4g_2_cox2, type = "martingale", linear.predictions = TRUE)
ggcoxdiagnostics(n4g_2_cox2, type = "deviance", linear.predictions = TRUE)

ggcoxdiagnostics(n4g_3_cox2, type = "martingale", linear.predictions = TRUE)
ggcoxdiagnostics(n4g_3_cox2, type = "deviance", linear.predictions = TRUE)

# 비교적 잔차가 고르게 분포한다 = 선형성이 있다


## Checking Proportional Hazard assumption
# test for prop hazards using Schoenfeld test for PH
# H0 : HAZARDS are prop (hazard ratio is constant over time)
# Ha : HAZARDS are Not prop will return test for each X, and for overall model
n4g_2_cox1 %>% cox.zph() 
n4g_2_cox2 %>% cox.zph() 

n4g_3_cox2 %>% cox.zph() 

# cox.zph : Test the Proportional Hazards Assumption of a Cox Regressions

# test IF coef for variable(x) changes over time...if it changes over time -> non-prop hazard (HR changes over time)

# we can see a plot of these as well..(one plot for each parameter) these are plots of "changes in b over time", if we let "b" vary over time recall,... if "b" varies over time, this means that there is NOT PH! the effect is not constatnt over time... it varies!
# pay less attention to the extremes, as line is sensitive here
plot(cox.zph(n4g_2_cox1)[1])
abline(h=0, col=2)
# plot(cox.zph(n4g_2_cox1)[2])
# abline(h=0, col=2)
# zero on the plot means there is no change all right


plot(cox.zph(n4g_2_cox2)[1]) # 이름별
abline(h=0, col=2)
plot(cox.zph(n4g_2_cox2)[2]) # 날짜별
abline(h=0, col=2)
# zero on the plot means there is no change all right
# case[1] : y=0(red line)이 confidense interval에 상당히 포함 되어있지 않으므로 Hazard ratio가 변한다고 볼수 있다
# case[2] : y=0(red line)이 confidense interval에 상당히 포함 되어있으므로 Hazard ratio가 변하지 않는다고 볼 수 있다


plot(cox.zph(n4g_3_cox2)[1]) # 이름별
abline(h=0, col=2)
plot(cox.zph(n4g_3_cox2)[2]) # 날짜별
abline(h=0, col=2)
# zero on the plot means there is no change all right
# case[1] : y=0(red line)이 confidense interval에 상당히 포함 되어있지 않으므로 Hazard ratio가 변한다고 볼수 있다
# case[2] : y=0(red line)이 confidense interval에 상당히 포함 되어있으므로 Hazard ratio가 변하지 않는다고 볼 수 있다