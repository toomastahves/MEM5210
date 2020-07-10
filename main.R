source('clean_data.R')
library(MASS) # lda
library(rpart) # tree

data <- read.csv("data.csv", header = TRUE, stringsAsFactors = FALSE)
data <- clean_data(data)

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