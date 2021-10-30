wine <- read.csv("whitewines.csv")
str(wine)
table(wine$quality)
system.time(train_samples <- get_train_samples(wine,
                                               "quality",
                                               table_col = 4,
                                               maxiter = 50))
prop.table(table(wine$quality[train_samples]))

wine.train <- wine[train_samples,]
wine.test <- wine[-train_samples,]
nrow(wine.train)
nrow(wine.test)
library("rpart")
wine_model <- rpart(quality ~ ., data=wine.train)
wine_model
library(rpart.plot)
rpart.plot(wine_model,digits = 3)
wine_pred <- predict(wine_model,wine.test,type = "vector")
cor(wine.test$quality,wine_pred)
MAE(wine.test$quality,wine_pred)
library(ggplot2)
ggplot(data.frame(actual=wine.test$quality,predicted=wine_pred),aes(actual,predicted)) +
  geom_count() +
  geom_smooth(method = "lm", formula = y ~ splines::bs(x,4)) +
  theme_bw()
plotcp(wine_model)
wine_model <- rpart(quality ~ .,data=wine.train,control = rpart.control(cp = 0.01))
wine_pred <- predict(wine_model,wine.test,type = "vector")
cor(wine.test$quality,wine_pred)
MAE(wine.test$quality,wine_pred)
plotcp(wine_model)
model_pruned <- prune(wine_model,cp = 0.01)
wine_pred_pruned <- predict(model_pruned,wine.test,type = "vector")
cor(wine.test$quality,wine_pred_pruned)
wine_model$cptable[,"CP"][which.min(wine_model$cptable[,"xerror"])]
correlation_matrix(wine)

# model tree
library(RWeka)
wine_m5p <- M5P(quality ~ .,data=wine.train)
summary(wine_m5p)
wine_m5p_pred <- predict(wine_m5p,wine.test)
summary(wine_m5p_pred)
cor(wine.test$quality,wine_m5p_pred)
MAE(wine.test$quality,wine_m5p_pred)

## simple nnet
library(nnet)
library(parallel)
nnet_models <- function(x)
{
  mod <- nnet(quality ~ .,data=wine.n.train,size=8,linout=TRUE,maxit=1000)
  list(model=mod,val=mod$value)
}
get_best_model <- function()
{
  ncores <- detectCores()
  cl <- makeCluster(ncores,type = "FORK")
  clusterEvalQ(cl,library(nnet))
  clusterExport(cl,c("wine.train","nnet_models"))
  all_models <- parLapply(cl,1:10,nnet_models)
  stopCluster(cl)
  best_model <- which.min(sapply(1:10,function(x) all_models[[x]]$val))
  all_models[[best_model]]$model
}

test_model <- function(model)
{
  pred <- predict(model,wine.n.test)
  print(summary(pred))
  c <- cor(wine.n.test$quality,pred)
  m <- MAE(wine.n.test$quality,pred)
  c(correlation=c,MAE=m)
}

wine_nnet <- get_best_model()

wine_nnet_pred <- predict(wine_nnet,wine.test)
summary(wine_nnet_pred)
cor(wine.test$quality,wine_nnet_pred)
MAE(wine.test$quality,wine_nnet_pred)

test_model(get_best_model())
wine.n <- normalize_all(wine)
wine.n.train <- wine.n[train_samples,]
wine.n.test <- wine.n[-train_samples,]
wine.n_model <- get_best_model()
test_model(wine.n_model)
plot(wine.n.test$quality,predict(wine.n_model,wine.n.test))
plot(wine.test$quality,wine_nnet_pred)
