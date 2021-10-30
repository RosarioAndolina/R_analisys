library(magrittr)
library(ggplot2)
source("/home/rosario/Documenti/R_data/script/OneRFS.R")
source("/home/rosario/Documenti/R_data/script/analisys_tools.R")
credit <- read.csv("credit.csv")
credit$default <- factor(ifelse(credit$default == 1, "no","yes"))
to_factor_selection <- c("installment_rate","residence_history","existing_credits","dependents")
credit[,to_factor_selection] <- as.data.frame(lapply(credit[,to_factor_selection],factor))
rm(to_factor_selection)
correlation_matrix(credit)

set.seed(54321)
train_samples <- get_train_samples(credit,"default")
train_data <- credit[train_samples,]
test_data <- credit[-train_samples,]

# rpart model on all data
library(rpart)

credit_rpart <- rpart(default ~ ., data = credit, method = "class")
cpt <- credit_rpart$cptable
CP <- as.numeric(cpt[,"CP"][which.min(cpt[,"xerror"])])
credit_rpart <- prune(credit_rpart, cp=CP)
# store rpart probs
rpart_probs <- predict(credit_rpart,credit,type="prob")
rpart_probs <- as.data.frame(rpart_probs)
names(rpart_probs) <- c("rpart_no","rpart_yes")
credit_boost <- cbind(credit,rpart_probs)
correlation_matrix(credit_boost)
credit_boost <- credit_boost[,-which(names(credit_boost)=="dependents")]
# naive bayes
library(klaR)
train_data <- credit_boost[train_samples,]
test_data <- credit_boost[-train_samples,]
credit_nb <- NaiveBayes(default ~ .,data = train_data,fL=2)
credit_nb_pred <- predict(credit_nb,test_data)
library(caret)
confusionMatrix(credit_nb_pred$class,test_data$default,positive = "yes")

# feature engigniring
credit_fact <- sapply(credit_boost,is.factor)
credit_fact <- credit_fact[-which(names(credit_fact)=="default")]
csqt <- sapply(credit_boost[,credit_fact],function(x) chisq.test(x,credit_boost$default)$p.value)
csqt[csqt > 0.05]
chebal_job <- cCombine(credit_boost$checking_balance,credit_boost$job)
chisq.test(credit_boost$default,chebal_job)$p.value
credit_boost$chebal_job <- factor(chebal_job)
credit_boost$job <- NULL
chebal_exicre <- cCombine(credit_boost$checking_balance,credit_boost$existing_credits)
chisq.test(credit_boost$default,chebal_exicre)$p.value
credit_boost$chebal_exicre <- factor(chebal_exicre)
chebal_reshis <- cCombine(credit_boost$checking_balance,credit_boost$residence_history)
chisq.test(credit_boost$default,chebal_reshis)$p.value
credit_boost$chebal_reshis <- factor(chebal_reshis)
credit_boost$residence_history <- NULL
insrat_tel <- cCombine(credit_boost$installment_rate,credit_boost$telephone)
chisq.test(credit_boost$default,insrat_tel)$p.value
credit_boost$insrat_tel <- factor(insrat_tel)
credit_boost$telephone <- NULL
pairs.panels(credit_boost[sapply(credit_boost,is.numeric)])
nb_prob <- as.data.frame(predict(credit_nb,credit_boost)$posterior)
names(nb_prob) <- c("nb_no","nb_yes")
credit_boost <- cbind(credit_boost,nb_prob)
# random forest
library(randomForest)
train_data <- credit_boost[train_samples,]
test_data <- credit_boost[-train_samples,]
credit_rf <- randomForest(default ~ .,data = train_data,ntree=500)
credit_rf_pred <- predict(credit_rf,test_data,type = "class")
confusionMatrix(test_data$default,credit_rf_pred,positive = "yes")

