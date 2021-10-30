all_char <- intToUtf8(c(9,33:126))
all_char <- as.factor(strsplit(all_char,"")[[1]])
all_levels <- levels(all_char)
bcancer <- read.csv("wisc_bc_data.csv")
nf <- names(bcancer)
cosine <- function(x,y)
{
  sum(x*y)/(sqrt(sum(x**2))*sqrt(sum(y**2)))
}

mod <- function(x)
{
  sqrt(sum(x**2))
}

dot_prod <- function(x,y)
{
  sum(x*y)
}

get_components <- function(x, levels)
{
  splitted <- factor(strsplit(x,"")[[1]],levels = levels)
  splitted <- splitted[! is.na(splitted)]
  as.numeric(table(splitted))
}

get_components.knt <- function(x, levels)
{
  fx <- factor(x,levels = levels)
  fx <- fx[! is.na(fx)]
  as.numeric(table(fx))
}

ngram_char <- function(x)
{
  xsplit <- strsplit(x,"")[[1]]
  len <- length(xsplit)
  ng <- paste(xsplit[-len],xsplit[-1],sep = "")
  c(xsplit,ng)
}

names(nf) <- nf

get_levels <- function(x)
{
  all_lev <- c()
  for (s in x)
    all_lev <- c(all_lev,s)
  levels(factor(all_lev))
}
# k-nearest-terms ##############
knt <- function(terms, ref_index=1)
{
  splitted <- sapply(terms,ngram_char)
  # get all levels
  all_lev <- get_levels(splitted)
  tcm <- t(sapply(splitted,get_components.knt,levels = all_lev))
  colnames(tcm) <- all_lev
  reference <- tcm[ref_index,]
  termscos <- normalize(apply(tcm,1,cosine,reference))
  termsmod <- normalize(apply(tcm,1,mod))
  termsdprod <- normalize(apply(tcm,1,dot_prod,reference))
  termslen <- normalize(sapply(terms,nchar))
  output <- list(terms=terms,
                 termscos=termscos,
                 termsmod=termsmod,
                 termsdprod=termsdprod,
                 termslen=termslen,
                 all_lev=all_lev,
                 reference=reference)
  #output <- lapply(output,function(x) {ifelse(is.numeric(x), normalize(x),x)})
  output
}


guess.knt <- function(model,word,k=ceiling(sqrt(length(model$terms))))
{
  if (k > length(model$terms))
    stop(paste("choose k less than or equal to",length(model$terms)))
  wcomp <- get_components(word,levels = model$all_lev)
  #terms cosine
  tc <- cosine(wcomp,model$reference)
  tc <- normalize(tc)
  #terms mod
  tm <- mod(wcomp)
  tm <- normalize(tm)
  #terms dot prod
  tdp <- dot_prod(wcomp,model$reference)
  tdp <- normalize(tdp)
  #terms len
  tlen <- nchar(word)
  tlen <- normalize(tlen)
  wdist_index <- order(sqrt((tc - model$termscos)**2 + 
                            (tm - model$termsmod)**2) +
                            (tdp - model$termsdprod)**2 +
                            (tlen - model$termslen)**2)
  first_k <- model$terms[wdist_index[1:k]]
  first_k_split <- sapply(first_k,ngram_char)
  wsplit <- ngram_char(word)
  new_lev <- get_levels(first_k_split)
  first_k_comp <- lapply(first_k_split,get_components.knt,levels=new_lev)
  wcomp <- get_components.knt(wsplit,levels=new_lev)
  guess_vect <- sapply(first_k_comp,cosine,wcomp)
  guess_vect <- guess_vect[guess_vect > 0.5]
  sort(guess_vect, decreasing = TRUE)
}
# 
# tcm <- t(sapply(nf,get_components,levels=all_levels))
# 
# reference <- get_components(nf[1],levels = all_levels)
# 
# termscos <- apply(tcm,1,cosine,reference)
# termsmod <- apply(tcm,1,mod)
# 
# tc <- get_components("geo",levels = all_levels)
# tm <- mod(tc)
# tc <- cosine(tc,reference)
wlen <- sapply(nf,nchar)
median(wlen)
rindex <- sample(1:length(nf),1)
model <- knt(nf,20)
guess.knt(model,"compactness_wort",15)

terms_data <- data.frame(terms=model$terms,
                         termscos=model$termscos,
                         termsmod=model$termsmod,
                         termsdprod=model$termsdprod,
                         termslen=model$termslen,stringsAsFactors = FALSE)


rownames(terms_data) <- NULL
library(ggplot2)
ggplot(terms_data,aes(termslen,termsdprod)) +
  geom_text(aes(label=terms))+
  theme_bw()
