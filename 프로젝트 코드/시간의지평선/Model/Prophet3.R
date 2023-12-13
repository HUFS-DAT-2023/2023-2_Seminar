library(ggplot2)
library(tidyr)
library(forecast)
library(lubridate)
library(dplyr)
library(zoo)
library(utils)
library(reshape2)
library(parallel)
library(prophet)

install.packages('reshape2')
train <- read.csv('train_Prophet1.csv')
train

# 데이터프레임의 첫 번째 열(ID)을 제외하고 나머지 열에 대해 음수 값을 0으로 변환
train[-1] <- lapply(train[-1], function(x) ifelse(x < 0, 0, x))
train


# 데이터 전처리
date_columns <- names(train)[grep("2022|2023", names(train))]
long_train <- train %>%
  select(ID, all_of(date_columns)) %>%
  pivot_longer(cols = all_of(date_columns), names_to = "date", values_to = "y")
long_train$date <- as.Date(gsub("X", "", long_train$date), format = "%Y.%m.%d")
long_train <- long_train %>% rename(ds = date)
head(long_train)

# 공휴일 설정
korean_holidays <- data.frame(
  holiday = c('3'),
  ds = as.Date(c('2023-03-01')),
  lower_window = c(-1),
  upper_window = c(0)
)


# 병렬 처리 설정
numCores <- detectCores() - 1
cl <- makeCluster(numCores)
clusterExport(cl, varlist = c("long_train", "korean_holidays"))
clusterEvalQ(cl, {
  library(prophet)
  library(dplyr)
  library(tidyr)
})


# 병렬 처리를 사용한 예측
result_df <- parLapply(cl, 0:15889, function(id) {
  selected_id_data <- long_train %>% filter(ID == as.character(id))
  m <- prophet(selected_id_data)
  future <- make_future_dataframe(m, periods = 2)
  forecast <- predict(m, future)
  return(data.frame(ID = id, t(forecast$yhat)))
})

result_df
# 클러스터 종료
stopCluster(cl)

# 결과 데이터 프레임 변환
result_df <- do.call(rbind, result_df)

head(result_df)


install.packages('utils')
library(utils)
write.csv(result_df, file = "C:/Users/eun01/OneDrive - 한국외국어대학교/ftest4.csv", row.names = TRUE)
