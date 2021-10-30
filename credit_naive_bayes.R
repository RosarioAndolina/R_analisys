credit <- read.csv("credit.csv")
str(credit)
credit$default <- ifelse(credit$default == 1, "no","yes")
credit$default <- as.factor(credit$default)
table(credit$default)
library(dplyr)
credit <- credit %>%
  mutate(installment_rate=as.factor(installment_rate),
         residence_history=as.factor(residence_history),
         existing_credits=as.factor(existing_credits),
         dependents=as.factor(dependents))

hist(credit$months_loan_duration,freq = FALSE)
lines(density(credit$months_loan_duration))
library(klaR)
tsamp <- get_train_samples(data = credit,class_name = "default",maxiter = 10)
credit.train <- credit[tsamp,]
credit.test <- credit[-tsamp,]
cl_index <- which(names(credit) == "default")
credit_model <- NaiveBayes(default ~ .,data = credit.train,fL=1,usekernel = TRUE)
credit_pred <- predict(credit_model,credit.test[,-cl_index])
str(credit_pred)
cv <- cross_validation(credit.test$default,credit_pred$class)
cv 
# accuracy sensitivity specificity 
# 0.8150000   0.8857143   0.6500000 
library(ROCR)
pred <- prediction(credit_pred$posterior[,2],credit.test$default)
perf <- performance(pred,"acc")
plot(perf)
acc_max_idx <- order(slot(perf,"y.values")[[1]],decreasing = TRUE)
acc_max <- slot(perf,"y.values")[[1]][acc_max_idx[1]]
acc_max_cutoff <- slot(perf,"x.values")[[1]][acc_max_idx[1]]
spec_perf <- performance(pred,"tnr")
plot(spec_perf)
spec <- slot(spec_perf,"y.values")[[1]]
spec_idx <- which(spec < 0.65)
spec <- spec[spec_idx]
spec_cutoff <- slot(spec_perf,"x.values")[[1]][spec_idx]
which.max(spec)
ultimate_pred <- ifelse(credit_pred$posterior[,2] > 0.4,"yes","no")
cross_validation(credit.test$default,ultimate_pred)
ROC <- function(model_pred,true_values)
{
  acc_spec_mean <- function(threshold)
  {
    preds <- ifelse(model_pred > threshold,"no","yes")
    preds <- factor(preds,levels=c("yes","no"))
    cv <- cross_validation(true_values,preds,print=FALSE)
    acc_spec <- mean(cv[c("accuracy","specificity")])
    c(threshold=threshold,acc_spec_mean=acc_spec,cv)
  }
  thres <- seq(0,1,len=500)
  t(sapply(thres,acc_spec_mean))
}
roc_data <- ROC(credit_pred$posterior[,2],credit.test$default)
roc_data <- as.data.frame(roc_data)
library(ggplot2)
ggplot(roc_data,aes(threshold,abs(sensitivity-specificity))) +
  geom_line() +
  theme_bw()
acc_max_idx <- which.max(roc_data$acc_spec_mean)
acc_max_thres <- roc_data$threshold[acc_max_idx]

which.max(roc_data$accuracy) -> acc_max_idx
acc_max_thres <- roc_data$threshold[acc_max_idx]

ggplot(roc_data,aes(1-specificity,sensitivity)) +
  geom_line(aes(col=accuracy)) +
  geom_abline(slope = 1,intercept = 0) +
  theme_bw()

ultimate_pred <- ifelse(credit_pred$posterior[,2] < acc_max_thres,"no","yes")
cross_validation(credit.test$default,ultimate_pred)

ggplot(roc_data,aes(threshold,acc_spec_mean)) +
  geom_line() +
  geom_line(aes(threshold,accuracy),col="blue") +
  geom_line(aes(threshold,sensitivity),col="red") +
  theme_bw()

###### feature engigniering #########
select_if(credit,is.factor) %>%
  names() -> names_factor
names_factor <- names_factor[-which(names_factor == "default")]
idx_grid <- 1:length(names_factor)
names(idx_grid) <- names_factor
grid <- lapply(idx_grid,function(x) expand.grid(names_factor[x],names_factor[-(1:x)],stringsAsFactors = FALSE))
grid$foreign_worker <- NULL
#idx_chisq <- 1:length(grid)
#names(idx_chisq) <- paste(grid$Var1,grid$Var2,sep = "_")
chitest <- function(grid)
{
  idx_chisq <- 1:nrow(grid)
  names(idx_chisq) <- grid$Var2
  chisq <- sapply(idx_chisq,function(x) round(chisq.test(
    table(credit[,grid[x,1]],credit[,grid[x,2]]))$p.value,digits = 3))
  chisq <- chisq[chisq <= 0.01]
  grid <- grid[grid$Var2 == names(chisq),]
  if (nrow(grid) == 0)
    NULL
  else
    grid
}
grid_filtered <- lapply(grid,chitest)
null_grid <- sapply(grid_filtered,is.null)
grid_filtered <- grid_filtered[!null_grid]
#rbind(data.frame(Var1=NULL,Var2=NULL),grid_filtered[[1]])
final_grid <- data.frame(Var1=NULL,Var2=NULL)
for (g in grid_filtered)
{
  final_grid <- rbind(final_grid,g)
}

new_feature_names <- paste(final_grid$Var1,final_grid$Var2,sep = "*")
idx <- 1:nrow(final_grid)
names(idx) <- new_feature_names
credit.new <- credit[,!names(credit) %in% names_factor]
credit.new <- cbind(credit.new,
                    as.data.frame(lapply(idx,
                                         function(x) paste(credit[,final_grid[x,1]],
                                                           credit[,final_grid[x,2]],
                                                           sep = "^"))))
str(credit.new)
credit.new.train <- credit.new[tsamp,]
credit.new.test <- credit.new[-tsamp,]
new_cl_index <- which(names(credit.new) == "default")
credit.new_model <- NaiveBayes(default ~ ., data=credit.new.train,fL=1)
credit.new_pred <- predict(credit.new_model,credit.new.test[,-new_cl_index])
cross_validation(credit.new.test$default,credit.new_pred$class)
# accuracy sensitivity specificity 
# 0.7900000   0.7714286   0.8333333 

roc_data <- as.data.frame(ROC(credit.new_pred$posterior[,2],credit.new.test$default))
library(ggplot2)
ggplot(roc_data,aes(1-specificity,sensitivity)) +
  geom_line(col="blue") +
  geom_abline(slope = 1,intercept = 0) +
  theme_bw()

ggplot(roc_data,aes(threshold,specificity)) +
  geom_line(aes(col=accuracy)) +
  scale_color_gradientn(colours = c("#7813e3","#07abea","#43ee0f","#f2e921","#d80000")) +
  theme_bw()
    
acc_max_idx <- which.max(roc_data$accuracy)
acc_max <- roc_data$accuracy[acc_max_idx]
thres_max <- roc_data$threshold[acc_max_idx]
ultimate_pred <- ifelse(credit.new_pred$posterior[,2] > thres_max, "yes","no")
cross_validation(credit.new.test$default,ultimate_pred)
# accuracy sensitivity specificity 
# 0.8600000   0.9000000   0.7666667 
max_acc_spec_mean_idx <- which.max(roc_data$acc_spec_mean)
thres_acc_spec <- roc_data$threshold[max_acc_spec_mean_idx]
ultimate_pred <- ifelse(credit.new_pred$posterior[,2] > thres_acc_spec, "yes","no")
cross_validation(credit.new.test$default,ultimate_pred)
# accuracy sensitivity specificity 
# 0.8550000   0.8714286   0.8166667
sens_diff_min_idx <- which.min(abs(roc_data$sensitivity-roc_data$specificity))
thres_sens_diff_min <- roc_data$threshold[sens_diff_min_idx]
ultimate_pred <- ifelse(credit.new_pred$posterior[,2] < thres_sens_diff_min,"no","yes")
cross_validation(credit.new.test$default,ultimate_pred)
####
ultimate_pred <- ifelse(credit.new_pred$posterior[,2] < 0.91,"no","yes")
cross_validation(credit.new.test$default,ultimate_pred)
