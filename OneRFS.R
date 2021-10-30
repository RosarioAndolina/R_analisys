# 1RFS (one rule features selections)
get_majority.1RFS <- function(x)
{
  t <- table(x)
  majority <- which.max(t)
  names(t)[majority]
}

one_rule_prediction <- function(feature,Class)
{
  if (is.numeric(feature))
    stop("numeric features are not implemented")
  if (is.character(feature))
    feature <- factor(feature)
  rule <- tapply(Class,feature,get_majority.1RFS)
  prediction <- feature
  levels(prediction) <- as.vector(rule)
  prediction <- factor(as.character(prediction),levels=names(table(Class)))
  prediction
}
get_error_rate <- function(feature,Class)
{
  if (is.numeric(feature))
    stop("numeric features are not implemented")
  if (is.character(feature))
    feature <- factor(feature)
  rule <- tapply(Class,feature,get_majority.1RFS)
  prediction <- feature
  levels(prediction) <- as.vector(rule)
  prediction <- factor(as.character(prediction),levels=names(table(Class)))
  err_rate <- 1-sum(diag(table(prediction,Class)))/length(Class)
  err_rate
}
