letters <- read.csv("letterdata.csv")
letters_train <- letters[1:16000,]
letters_test <- letters[16001:20000,]
prop.table(table(letters$letter))
prop.table(table(letters_test$letter))
library(kernlab)
letters_linear <- ksvm(letter ~ ., data=letters_train,kernel = "vanilladot")
letters_linear
letters_pred <- predict(letters_linear,letters_test)
table(letters_test$letter,letters_pred)
mean(diag(prop.table(table(letters_test$letter,letters_pred),2)))
accuracy <- function(actual,predicted)
{
  acc <- sum(diag(table(actual,predicted)))/length(actual)
  print(paste("accuracy",acc))
}

sens_mean <- function(actual,predicted)
{
  sens_m <- mean(diag(prop.table(table(actual,predicted),2)))
  print(paste("sensitivity mean:",sens_m))
}

sens_summary <- function(actual,predicted)
{
  sens <- diag(prop.table(table(actual,predicted),2))
  c(Min=min(sens),Mean=mean(sens),Max=max(sens))
}

accuracy(letters_test$letter,letters_pred)
sens_mean(letters_test$letter,letters_pred)

# gaussian rbf kernel
letters_rbf <- ksvm(letter ~ .,data = letters_train, kernel = "rbfdot")
letters_rbf_pred <- predict(letters_rbf,letters_test)
accuracy(letters_test$letter,letters_rbf_pred)
sens_mean(letters_test$letter,letters_rbf_pred)

prediction_mismatch <- function(actual,predicted)
{
  df <- as.data.frame(table(actual,predicted),stringsAsFactors=FALSE)
  names(df)[1:2] <- c("actual","predicted")
  df <- df[df$actual != df$predicted,]
  df <- df[df$Freq > 0,]
  df[order(df$Freq,decreasing = TRUE),]
}

pred_mis <- prediction_mismatch(letters_test$letter,letters_rbf_pred)
sens_summary(letters_test$letter,letters_rbf_pred)
