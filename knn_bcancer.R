library(ggplot2)
library(dplyr)
library(gmodels)
library(class)

bcancer <- read.csv("wisc_bc_data.csv")
# bcancer[,-1] %>%
#   normalize_all() -> bcancer.n
bcancer[,-1] %>%
  mutate(fradim_w=fractal_dimension_worst*area_worst,
         fradim_m=fractal_dimension_mean*area_mean,
         fradim_s=fractal_dimension_se*area_se,
         smooth_m=smoothness_mean*area_mean,
         smooth_w=smoothness_worst*area_worst,
         sym_m=symmetry_mean*area_mean,
         sym_s=symmetry_se*area_se) -> bcancer.new
bcancer.new[,-c(grep("fractal",names(bcancer.new)),
                grep("smoothness",names(bcancer.new)))] %>%
  normalize_all() -> bcancer.n

bcancer.unclass <- bcancer.n[470:569,]
bcancer.n <- bcancer.n[1:469,]

#set.seed(123)
kdata <- data.frame()
for (f in seq(0.65,0.9,by=0.05))
{
  train_fraction <- f
  nrow_train <- round(nrow(bcancer.n)*train_fraction)
  nrow_test <- nrow(bcancer.n)-nrow_train
  K <- round(sqrt(nrow_train))
  K.seq <- seq(K-floor(K/2),K+ceiling(K/2),by = 1)
  #K.seq <- seq(K-floor(K/2),2*K,by = 2)
  for (kk in K.seq)
  {
    accuracy <- c()
    false_neg <- c()
    false_pos <- c()
    numerrors <- c()
    for (i in 1:15)
    {
      bcancer.s <- bcancer.n[sample(1:nrow(bcancer.n)),]
      bcancer.train <- bcancer.n[1:nrow_train,]
      bcancer.test <- bcancer.n[((nrow_train)+1):nrow(bcancer.n),]
      bcancer.pred <- knn(train = bcancer.train[,-1],
                          test = bcancer.test[,-1],
                          cl = bcancer.train$diagnosis,
                          k = kk)
      t <- table(bcancer.test$diagnosis,bcancer.pred)
      accuracy <- c(accuracy,sum(diag(t))/nrow_test)
      false_neg <- c(false_neg,t["M","B"]/nrow_test)
      false_pos <- c(false_pos,t["B","M"]/nrow_test)
      numerrors <- c(numerrors,(nrow_test-sum(diag(t)))/nrow_test)
    }
    kdata <- rbind(kdata,data.frame(k=kk,
                                    accuracy_med=median(accuracy),
                                    false_neg_med=median(false_neg),
                                    false_pos_med=median(false_pos),
                                    numerrors_med=median(numerrors),
                                    tfrac=as.character(train_fraction)))
  }
  print(paste("TRAIN FRACTION",train_fraction))
  print(summary(kdata))
}
kdata <- kdata %>%
  mutate(tfrac=factor(tfrac,
                      levels = rev(sortedLevels_by(data = kdata,
                                                     to.sort = "tfrac",
                                                     sort.by = "accuracy_med"))))
ggplot(kdata,aes(k,accuracy_med,col=tfrac)) +
  # scale_y_continuous(limits = c(0.94,1)) +
  # scale_x_continuous(limits = c(10,35)) +
geom_line() +
labs(title="Accuracy") +
  #geom_vline(aes(col=tfrac),xintercept = kdata$kbest, data=kdata) +
theme_bw()

ggpairs_by(kdata,"k",aes(col=tfrac),flip = TRUE) + 
  geom_smooth(aes(col=tfrac),method = "lm",se=FALSE)

rm(list=c("accuracy","false_neg","i","kk","bcancer.s"))

# let's look the best
train_fraction <- 0.9
nrow_train <- round(nrow(bcancer.n)*train_fraction)
nrow_test <- nrow(bcancer.unclass)
K <- 11
bcancer.s <- bcancer.n[sample(1:nrow(bcancer.n)),]
bcancer.train <- bcancer.n[1:nrow_train,]
bcancer.test <- bcancer.unclass
bcancer.pred <- knn(train = bcancer.train[,-1],
                    test = bcancer.test[,-1],
                    cl = bcancer.train$diagnosis,
                    k = K)
rm("bcancer.s")
CrossTable(bcancer.test$diagnosis,bcancer.pred,chisq = TRUE,prop.chisq = FALSE)
t <- table(bcancer.test$diagnosis,bcancer.pred)
print(paste("accuracy:",sum(diag(t))/nrow_test))

source('~/Documenti/R_data/script/naivebayes_numeric.R')
model <- nbnum(train = bcancer.train[,-1],
               class = bcancer.train$diagnosis)
pred <- predict(model,test = bcancer.test[,-1],type = "class")
CrossTable(bcancer.test$diagnosis,pred,chisq = TRUE,prop.chisq = FALSE)
accuracy <- sum(diag(table(bcancer.test$diagnosis,pred)))/length(pred)
print(paste("accuracy:",accuracy))