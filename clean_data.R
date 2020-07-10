clean_data <- function(data) {
  data <- data[!duplicated(data), ]
  
  data$Customer_ID <- NULL
  data$Loan_ID <- NULL
  data$Months_since_last_delinquent <- NULL
  
  data <- data[data$Current_Loan_Amount != 99999999,]
  
  data <- data[!is.na(data$Current_Loan_Amount),]
  data <- data[!is.na(data$Bankruptcies),]
  data <- data[!is.na(data$Tax_Liens),]
  data <- data[!is.na(data$Number_of_Credit_Problems),]
  data <- data[!is.na(data$Maximum_Open_Credit),]
  data <- data[!is.na(data$Credit_Score),]
  data <- data[!is.na(data$Annual_Income),]
  data <- data[!is.na(data$Monthly_Debt),]
  data <- data[!is.na(data$Years_of_Credit_History),]
  data <- data[!is.na(data$Number_of_Open_Accounts),]
  data <- data[!is.na(data$Current_Credit_Balance),]
  
  data <- data[!is.na(data$Term),]
  data$Term[data$Term == "Long Term"] <- 0
  data$Term[data$Term == "Short Term"] <- 1
  data$Term <- as.numeric(data$Term)
  
  data <- data[!is.na(data$Years_in_current_job),]
  data$Years_in_current_job[data$Years_in_current_job == "< 1 year"] <- 0
  data$Years_in_current_job[data$Years_in_current_job == "1 year"] <- 1
  data$Years_in_current_job[data$Years_in_current_job == "2 years"] <- 2
  data$Years_in_current_job[data$Years_in_current_job == "3 years"] <- 3
  data$Years_in_current_job[data$Years_in_current_job == "4 years"] <- 4
  data$Years_in_current_job[data$Years_in_current_job == "5 years"] <- 5
  data$Years_in_current_job[data$Years_in_current_job == "6 years"] <- 6
  data$Years_in_current_job[data$Years_in_current_job == "7 years"] <- 7
  data$Years_in_current_job[data$Years_in_current_job == "8 years"] <- 8
  data$Years_in_current_job[data$Years_in_current_job == "9 years"] <- 9
  data$Years_in_current_job[data$Years_in_current_job == "10+ years"] <- 10
  data = data[data$Years_in_current_job != "n/a",]
  data$Years_in_current_job <- as.numeric(data$Years_in_current_job)

  data <- data[!is.na(data$Home_Ownership),]
  data$Home_Ownership[data$Home_Ownership == "HaveMortgage"] <- 0
  data$Home_Ownership[data$Home_Ownership == "Home Mortgage"] <- 1
  data$Home_Ownership[data$Home_Ownership == "Own Home"] <- 2
  data$Home_Ownership[data$Home_Ownership == "Rent"] <- 3
  data$Home_Ownership <- as.numeric(data$Home_Ownership)
  
  data <- data[!is.na(data$Purpose),]
  data$Purpose[data$Purpose == "Business Loan"] <- 0
  data$Purpose[data$Purpose == "Buy a Car"] <- 1
  data$Purpose[data$Purpose == "Buy House"] <- 2
  data$Purpose[data$Purpose == "Debt Consolidation"] <- 3
  data$Purpose[data$Purpose == "Educational Expenses"] <- 4
  data$Purpose[data$Purpose == "Home Improvements"] <- 5
  data$Purpose[data$Purpose == "major_purchase"] <- 6
  data$Purpose[data$Purpose == "Medical Bills"] <- 7
  data$Purpose[data$Purpose == "moving"] <- 8
  data$Purpose[data$Purpose == "renewable_energy"] <- 9
  data$Purpose[data$Purpose == "small_business"] <- 10
  data$Purpose[data$Purpose == "Take a Trip"] <- 11
  data$Purpose[data$Purpose == "vacation"] <- 12
  data$Purpose[data$Purpose == "wedding"] <- 13
  data$Purpose[data$Purpose == "other"] <- 14
  data$Purpose[data$Purpose == "Other"] <- 14
  data$Purpose <- as.numeric(data$Purpose)

  data <- data[!is.na(data$Loan_Status),]
  data$Loan_Status[data$Loan_Status == "Charged Off"] <- 0
  data$Loan_Status[data$Loan_Status == "Fully Paid"] <- 1
  data$Loan_Status <- factor(data$Loan_Status)
  
  data <- remove_prior_bias(data)
  #data <- normalize(data)
  data <- remove_insignificant_columns(data)
  
  return(data)
}

normalize <- function(data) {
  data$Current_Loan_Amount <- with(data, (Current_Loan_Amount - min(Current_Loan_Amount)) / (max(Current_Loan_Amount) - min(Current_Loan_Amount)))
  data$Bankruptcies <- with(data, (Bankruptcies - min(Bankruptcies)) / (max(Bankruptcies) - min(Bankruptcies)))
  data$Tax_Liens <- with(data, (Tax_Liens - min(Tax_Liens)) / (max(Tax_Liens) - min(Tax_Liens)))
  data$Number_of_Credit_Problems <- with(data, (Number_of_Credit_Problems - min(Number_of_Credit_Problems)) / (max(Number_of_Credit_Problems) - min(Number_of_Credit_Problems)))
  data$Maximum_Open_Credit <- with(data, (Maximum_Open_Credit - min(Maximum_Open_Credit)) / (max(Maximum_Open_Credit) - min(Maximum_Open_Credit)))
  data$Annual_Income <- with(data, (Annual_Income - min(Annual_Income)) / (max(Annual_Income) - min(Annual_Income)))
  data$Credit_Score <- with(data, (Credit_Score - min(Credit_Score)) / (max(Credit_Score) - min(Credit_Score)))
  data$Years_in_current_job <- with(data, (Years_in_current_job - min(Years_in_current_job)) / (max(Years_in_current_job) - min(Years_in_current_job)))
  data$Home_Ownership <- with(data, (Home_Ownership - min(Home_Ownership)) / (max(Home_Ownership) - min(Home_Ownership)))
  data$Purpose <- with(data, (Purpose - min(Purpose)) / (max(Purpose) - min(Purpose)))
  data$Years_of_Credit_History <- with(data, (Years_of_Credit_History - min(Years_of_Credit_History)) / (max(Years_of_Credit_History) - min(Years_of_Credit_History)))
  data$Monthly_Debt <- with(data, (Monthly_Debt - min(Monthly_Debt)) / (max(Monthly_Debt) - min(Monthly_Debt)))
  data$Number_of_Open_Accounts <- with(data, (Number_of_Open_Accounts - min(Number_of_Open_Accounts)) / (max(Number_of_Open_Accounts) - min(Number_of_Open_Accounts)))
  data$Current_Credit_Balance <- with(data, (Current_Credit_Balance - min(Current_Credit_Balance)) / (max(Current_Credit_Balance) - min(Current_Credit_Balance)))
  
  return(data)
}

remove_prior_bias <- function(data) {
  rows <- sample(nrow(data))
  data <- data[rows, ]
  notpaid <- data[data$Loan_Status == 0,]
  paid <- data[data$Loan_Status == 1,]
  paid <- paid[1:dim(notpaid)[1],]
  data <- rbind(notpaid, paid)
  return(data)
}

remove_insignificant_columns <- function(data) {
  data$Years_in_current_job <- NULL
  data$Purpose <- NULL
  data$Monthly_Debt <- NULL
  data$Years_of_Credit_History <- NULL
  data$Number_of_Open_Accounts <- NULL
  data$Number_of_Credit_Problems <- NULL
  data$Current_Credit_Balance <- NULL
  data$Maximum_Open_Credit <- NULL
  data$Bankruptcies <- NULL
  data$Tax_Liens <- NULL
  
  #data$Current_Loan_Amount <- NULL
  #data$Term <- NULL
  #data$Annual_Income <- NULL
  #data$Home_Ownership <- NULL
  
  return(data)
}