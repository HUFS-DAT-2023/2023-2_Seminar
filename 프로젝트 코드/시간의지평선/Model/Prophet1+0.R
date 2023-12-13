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
df <- read.csv('ftest2.csv')
df

# 데이터프레임의 첫 번째 열(ID)을 제외하고 나머지 열에 대해 음수 값을 0으로 변환
df[-1] <- lapply(df[-1], function(x) ifelse(x < 0, 0, x))
df


# 결과 확인
print(df)



write.csv(df, file = "C:/Users/eun01/OneDrive - 한국외국어대학교/ftest2-1.csv", row.names = TRUE)
