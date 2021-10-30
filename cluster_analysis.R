library(magrittr)
source("/home/rosario/Documenti/R_data/script/analisys_tools.R")
teen <- read.csv("snsdata.csv")
teen$age <- ifelse(teen$age >= 13 & teen$age < 20, teen$age,NA)

################ missing value imputation
# age
t_age <- tapply(teen$age,teen$gradyear,function(x) sample(x[!is.na(x)],length(x[is.na(x)])))
str(t_age)
years <- as.numeric(names(table(teen$gradyear)))
for (y in years)
{
  teen$age[is.na(teen$age) & teen$gradyear == y] <- t_age[[as.character(y)]]
}
summary(teen$age)
rm(t_age)
# gender na imputation with naive bayes
library(klaR)
train <- teen[!is.na(teen$gender),]
train[,5:ncol(teen)] <- as.data.frame(lapply(train[,5:ncol(teen)],function(x) ifelse(x==0,"no","yes")))
test <- teen[is.na(teen$gender),]
test <- test[,-which(names(test)=="gender")]
test[,4:ncol(test)] <- as.data.frame(lapply(test[,4:ncol(test)],function(x) ifelse(x==0,"no","yes")))
model <- NaiveBayes(gender ~ .,data=train,fL=1)
gender <- predict(model,test,)
table(gender$class)
length(gender$class)
teen$gender[is.na(teen$gender)] <- gender$class
table(teen$gender)
rm(list = c("train","test","model","gender"))
detach("package:klaR",unload = TRUE)

################ kmeans model
teen$female <- ifelse(teen$gender=="F",1,0)
gender <- teen$gender
teen$gender <- NULL
interests <- teen[,4:(ncol(teen)-1)]
interests_z <- as.data.frame(lapply(interests,scale))
ncluster <- 6
set.seed(2345)
teen_clusters <- kmeans(interests_z,ncluster)
prop.table(teen_clusters$size)
teen_clusters$centers
max_centers_coord <- apply(teen_clusters$centers,1,function(x) names(interests_z)[order(x^2,decreasing = TRUE)])
cluster_assoc <-apply(teen_clusters$centers,2,function(x) which.max(abs(x)))
cluster_interests <- lapply(1:ncluster,function(x) names(which(cluster_assoc == x)))
cluster_labels <- c("brain","princesses","athletes","basket cases","saints","criminals")
names(cluster_interests) <- cluster_labels

# save interests for rules analysis
interests_df <- apply(interests,1,function(x) paste(names(interests)[which(x > 0)],collapse = ",")) %>%
  as.data.frame(stringsAsFactors=FALSE)
names(interests_df) <- "interests"
interests_df <- interests_df[which(nchar(interests_df$interests) != 0),]
cat(interests_df,file = "teen_interests.csv",sep = "\n")
rm(interests_df)
# add cluster in teen df
teen$cluster <- cluster_labels[teen_clusters$cluster]
head(teen[,c("cluster","age","gradyear","friends","female")])
# demographic characteristics
tapply(teen$age,teen$cluster,mean)
tapply(teen$female,teen$cluster,mean)
tapply(teen$friends,teen$cluster,mean)
tapply(teen$friends,list(teen$female,teen$cluster),quantile,c(0.25))
prop.table(table(teen$female,teen$cluster),2)

# rules classification with the ripper algorithm
library(RWeka)
train_samples <- get_train_samples(teen,"cluster")
prop.table(table(teen$cluster[train_samples]))
prop.table(table(teen$cluster))
train_data <- teen[train_samples,]
train_data$female <- NULL
train_data$gender <- gender[train_samples]
train_data$cluster <- factor(train_data$cluster)
train_data[,4:(ncol(train_data)-2)] <- as.data.frame(lapply(train_data[,4:(ncol(train_data)-2)],function(x) ifelse(x==0,"no","yes")))
test_data <- teen[-train_samples,]
test_data$female <- NULL
test_data$gender <- gender[-train_samples]
test_data$cluster <- factor(test_data$cluster)
test_data[,4:(ncol(test_data)-2)] <- as.data.frame(lapply(test_data[,4:(ncol(test_data)-2)],function(x) ifelse(x==0,"no","yes")))
model <- JRip(cluster ~ .,data = train_data)
summary(model)
model
pred <- predict(model,test_data)
str(pred)
acc <- sum(diag(table(test_data$cluster,pred)))/length(pred) # 0.915
