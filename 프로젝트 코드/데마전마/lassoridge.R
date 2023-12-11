#Ridge_TG
df_TG = read.csv("df_TG.csv")
df_TG
data=df_TG
# glmnet 패키지 로드
library(glmnet)
library(caret)


# 훈련 데이터와 테스트 데이터로 나누기
set.seed(123)  # 재현성을 위한 시드 설정
train_index <- createDataPartition(data$price, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]
summary(train_data)


# 훈련 데이터 스케일링 (표준화)
preprocess_params <- preProcess(train_data[, -2], method = c("center", "scale"))
train_data_scaled <- predict(preprocess_params, train_data)

# 테스트 데이터 스케일링 (표준화)
test_data_scaled <- predict(preprocess_params, test_data)


# 데이터를 glmnet 형식으로 변환
data_matrix <- model.matrix(price ~ . - 1, data = train_data_scaled)
y_vector <- train_data_scaled$price

# 릿지 모델 학습
ridge_model <- glmnet(data_matrix, y_vector, alpha = 0)

# 교차검증
cvfit=cv.glmnet(data_matrix, y_vector)
plot(cvfit)

# 최적의 lambda 값 확인
best_lambda <- ridge_model$lambda.min
cat("Best Lambda:", best_lambda, "\n")

cvfit$lambda.min #0.0002506962

print(ridge_model)
plot(ridge_model)

best_model <- glmnet(data_matrix, y_vector, alpha = 0, lambda = best_lambda)

# cv.glmnet을 통해 모델 학습
cvfit <- cv.glmnet(data_matrix, y_vector, type.measure = "mse")

# 최적 모델의 성능 평가 지표 출력
cvfit$cvm

# 모델 예측값 구하기
predicted_values <- predict(best_model, s = best_lambda, newx = data_matrix)
plot(predicted_values)

# R-squared 계산
rsquared <- cor(predicted_values, y_vector)^2
print(rsquared)
plot(rsquared)


# 최적 모델의 계수 출력
coefficients <- coef(best_model)
print(coefficients)
summary(coefficients)

# 희소 행렬의 값을 데이터 프레임으로 변환
coefficients_df <- data.frame(
  i = as.vector(coefficients@i + 1),
  j = as.vector(coefficients@j + 1),
  x = as.vector(coefficients@x)
)

# 회귀계수의 절대값을 추가
coefficients_df$abs_x <- abs(coefficients_df$x)

# 절대값 기준으로 내림차순 정렬
sorted_coefficients <- coefficients_df[order(-coefficients_df$abs_x), ]

# 결과 출력
print(sorted_coefficients)

# 중요도 평가 (회귀 계수의 절대값 합)
importance <- apply(normalized_coefs, 2, function(x) sum(abs(x)))
print(importance)








# train-test 분리(70:30)
train_indices <- createDataPartition(data$price, p = 0.7, list = FALSE)
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]


# 데이터를 glmnet 형식으로 변환
data_matrix <- model.matrix(price ~ . - 1, data = train_data)
y_vector <- train_data$price
y_vector_test<- test_data$price

# 변수 스케일링 (평균 0, 표준편차 1)
data_matrix <- scale(data_matrix)
y_vector <- scale(y_vector)
y_vector_test <- scale(y_vector_test)

# 릿지 모델 학습
ridge_model <- glmnet(data_matrix, y_vector, alpha = 0)

# 교차검증
cvfit=cv.glmnet(data_matrix, y_vector)
plot(cvfit)

# 최적의 lambda 값 확인
best_lambda <- ridge_model$lambda.min
cat("Best Lambda:", best_lambda, "\n")

cvfit$lambda.min #0.0002506962

# 릿지 모델의 계수 출력
ridge_coefficients <- coef(ridge_model, s = 0.0002400358)
print(ridge_coefficients)

coef(ridge_model)

# 모델을 훈련시킬 때 사용된 변수의 수
num_variables_in_model <- ncol(data_matrix)

# 테스트 데이터의 변수 수를 확인하고 필요에 따라 조정
num_variables_in_test_data <- ncol(test_data)

if (num_variables_in_test_data != num_variables_in_model) {
  # 테스트 데이터의 변수 수를 모델을 훈련시킬 때 사용된 변수의 수와 일치시킴
  # 이 과정에서 필요에 따라 변수를 선택하거나 다른 방법을 사용할 수 있음
  selected_variables <- colnames(data_matrix)[1:num_variables_in_model]
  test_data <- test_data[, selected_variables]
}

# 테스트 데이터를 sparse matrix로 변환
sparse_test_data <- sparse.model.matrix(~ . - 1, data = as.data.frame(test_data))

# 릿지 모델 예측
ridge_predictions <- predict(ridge_model, newx = test_data)

# 예측 성능 평가
mse <- mean((ridge_predictions - y_vector_test)^2)
rsquared <- 1 - mse / var(y_vector_test)

cat("Mean Squared Error (MSE):", mse, "\n")
cat("R-squared:", rsquared, "\n")


best_lambda <- cv_ridge_model$lambda.min

# 최적의 릿지 모델
best_ridge_model <- glmnet(data_matrix, y_vector, alpha = 0, lambda = best_lambda)

# 최적의 릿지 모델을 사용하여 테스트 데이터 예측
ridge_predictions <- predict(best_ridge_model, newx = sparse_test_data)

# 예측 성능 평가
mse <- mean((ridge_predictions - y_vector_test)^2)
rsquared <- 1 - mse / var(y_vector_test)

cat("Best Lambda:", best_lambda, "\n")
cat("Mean Squared Error (MSE):", mse, "\n")
cat("R-squared:", rsquared, "\n")

# 정규화된 회귀 계수 출력
normalized_coefs <- coef(ridge_model, s = 0.7035544 , exact = TRUE, x = data_matrix, y = y_vector)
print(normalized_coefs)

# 중요도 평가 (회귀 계수의 절대값 합)
importance <- apply(normalized_coefs, 2, function(x) sum(abs(x)))
print(importance)

