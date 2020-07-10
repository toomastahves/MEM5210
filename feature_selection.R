feature_selection <- function(data) {
  fit_tree <- rpart(Loan_Status~.,data)
  feature_selection = varImp(fit_tree)
  feature_selection$Percent <- feature_selection$Overall / sum(feature_selection$Overall) * 100
  return(feature_selection)
}