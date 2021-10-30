library("tm")
sms_raw <- read.csv("sms_spam.csv",stringsAsFactors = FALSE)
sms_raw$type <- factor(sms_raw$type)
sms_corpus <- VCorpus(x = VectorSource(sms_raw$text))
sms_corpus_clean <- tm_map(sms_corpus,content_transformer(tolower))

has_long_num <- function(x)
{
  gsub("[[:digit:]]{5}"," haslongnum ",x)
}

sms_corpus_clean <- tm_map(sms_corpus_clean, 
                           content_transformer(has_long_num))

sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)
sms_corpus_clean <- tm_map(sms_corpus_clean,removeWords,stopwords())
#sms_corpus_clean <- tm_map(sms_corpus_clean, 
#                           content_transformer(function(x) {gsub("'","",x)}))


replacePunctuation <- function(x)
{
  gsub("[[:punct:]]+", " ",x)
}

sms_corpus_clean <- tm_map(sms_corpus_clean, content_transformer(replacePunctuation))
sms_corpus_clean <- tm_map(sms_corpus_clean,removeWords,stopwords())
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)
sms_corpus_clean <- tm_map(sms_corpus_clean,removeWords, letters)
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)
dump(list = c("sms_corpus_clean"),file = "sms_corpus.RData")

N_grams <- function(x,n=2,sep = "_",output = "string")
{
  n <- ifelse(n < length(x),n,length(x))
  if (length(x) == 1)
    token <- scan(text = x,what = character(),quiet = TRUE)
  else
    token <- x
  ptoken <- token
  out <- token
  i <- 1
  while (i <= (n-1))
  {
    ptoken <- paste(ptoken[-length(ptoken)],token[-(1:i)],sep = sep)
    out <- c(out,ptoken)
    i <- i + 1
  }
  if (output == "string")
    paste(out,collapse = " ")
  else if (output == "vector")
    out
}

sms_corpus_clean<- tm_map(sms_corpus_clean,content_transformer(N_grams))
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)
sms_freq_world <- findFreqTerms(sms_dtm,5)
sms_dtm <- sms_dtm[,sms_freq_world]

convert_counts <- function(x)
{
  x <- ifelse(x > 0, "Yes","No")
}

sms_dtm <- as.data.frame(apply(sms_dtm,2,convert_counts))
sms_dtm <- as.data.frame(lapply(sms_dtm, factor,levels=c("Yes","No")))
msg_length <- sapply(sms_raw$text,nchar,USE.NAMES = FALSE)
sms_dtm$msglen <- cut(msg_length,breaks = c(0,125,250,max(msg_length)))

ntrain <- round(nrow(sms_dtm)*0.75)
sms_dtm_train <- sms_dtm[1:ntrain,]
sms_dtm_test <- sms_dtm[(ntrain+1):nrow(sms_dtm),]
sms_train_labels <- sms_raw$type[1:ntrain]
sms_test_labels <- sms_raw$type[(ntrain+1):nrow(sms_dtm)]

library(wordcloud)
wordcloud(sms_corpus_clean[sms_raw$type == "spam"],
          min.freq = 20,
          random.order = FALSE)

library(e1071)
sms_classifier <- naiveBayes(sms_dtm_train,sms_train_labels,laplace = 0)
sms_pred <- predict(sms_classifier,sms_dtm_test)
library(gmodels)
cmatrix <- CrossTable(sms_pred,sms_test_labels,prop.chisq = FALSE,dnn = c("predicted","actual"))
modelstat <- list(accuracy=sum(diag(cmatrix$t))/length(sms_pred))
modelstat <- c(modelstat, as.list(diag(cmatrix$prop.col)))
names(modelstat)[-1] <- c("sensitivity","specificity")
modelstat
