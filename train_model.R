run_lda <- function(data) {
  lda_results <- 0
  for (i in 1:100) {
    sample <- sample.int(n = nrow(data), size = floor(.75*nrow(data)), replace = F)
    train <- data[sample, ]
    test  <- data[-sample, ]
    lda_fit <- lda(Loan_Status~., data=train)
    lda_predict <- predict(lda_fit, newdata=test)$class
    lda_result <- sum(test$Loan_Status == lda_predict) / length(test$Loan_Status)
    lda_results[i] <- lda_result
  }
  return(mean(lda_results))
}

run_kmeans <- function(data) {
  data$Loan_Status <- NULL
  target <- data$Loan_Status
  kmeans_fit <- kmeans(data, 2, nstart = 1000)
  kmeans_predict <- kmeans_fit$cluster - 1
  kmeans_result <- sum(kmeans_predict == target) / length(kmeans_predict)
  return(kmeans_result)
}