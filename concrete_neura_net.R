concrete <- read.csv("concrete.csv")
pairs_plot(concrete,"strength")
library(psych)
pairs.panels(concrete)
fea_names <- names(concrete)
fea_names <- fea_names[-length(fea_names)]
sapply(fea_names,function(x,data) {hist(data[,x],main=x); x},concrete)
concrete$waterTsuperplastic <- concrete$water*concrete$superplastic
# concrete$superplasticOwater <- concrete$superplastic/concrete$water
# concrete$waterTfineagg <- concrete$water*concrete$fineagg
concrete$age2 <- concrete$age^2
concrete$water2 <- concrete$water^2
concrete_norm <- normalize_all(concrete)
lapply(concrete_norm,summary)
table(concrete$age)
train_samples <- get_train_samples(data.frame(x=factor(concrete$age)),"x",
                                   proportion = 0.75,
                                   maxiter = 50,
                                   table_col = 5)
length(train_samples)
# concrete_train <- concrete_norm[1:773,]
# concrete_test <- concrete_norm[774:1030,]
concrete_train <- concrete_norm[train_samples,]
concrete_test <- concrete_norm[-train_samples,]
obj_idx <- which(names(concrete) == "strength")
library(neuralnet)
system.time(concrete_model <- neuralnet(strength ~ cement + slag + ash + water + superplastic +
                              coarseagg + fineagg + age + waterTsuperplastic +
                              age2 + water2,
                            data = concrete_train,hidden = 6))
#plot(concrete_model)
concrete_pred <- compute(concrete_model,concrete_test[,-obj_idx])
strength_pred <- concrete_pred$net.result
cor(concrete_test$strength,strength_pred)
MAE(concrete_test$strength,strength_pred)/diff(range(concrete_test$strength))
summary(concrete_test$strength)
summary(strength_pred)

plot(concrete_test$strength,strength_pred-concrete_test$strength)
abline(h = 0)
concrete_residual <- data.frame(residual=strength_pred-concrete_test$strength,
                                strength=concrete_test$strength,
                                strength2=concrete_test$strength^2)
res_model <- lm(residual ~ .,data = concrete_residual)
summary(res_model)
res_model$coefficients
new_strength <- (strength_pred - res_model$coefficients[2]*concrete_test$strength -
  res_model$coefficients[3]*concrete_test$strength^2)
cor(new_strength,concrete_test$strength)
MAE(concrete_test$strength,new_strength)/diff(range(concrete_test$strength))
summary(concrete_test$strength)
summary(new_strength)
plot(concrete_test$strength,new_strength)
abline(0,1)
plot(concrete_test$strength,new_strength-concrete_test$strength)
correlation_matrix(concrete)
library(nnet)
concrete_model2 <- nnet(strength ~ .,data = concrete_train,size=6,maxit=2000)
concrete_pred2 <- predict(concrete_model2,concrete_test)
cor(concrete_pred2,concrete_test$strength)
MAE(concrete_test$strength,concrete_pred2)/diff(range(concrete_test$strength))
summary(concrete_test$strength)
summary(concrete_pred2)
plot(concrete_test$strength,concrete_pred2-concrete_test$strength)
new_strength <- (concrete_pred2 - res_model$coefficients[2]*concrete_test$strength -
                   res_model$coefficients[3]*concrete_test$strength^2)
