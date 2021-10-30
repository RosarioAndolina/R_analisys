#################################################################
#                    ggpairs_by
#################################################################
ggpairs_by <- function(data,feature,mapping=NULL,subtitle=NULL,flip=FALSE)
{
  require(dplyr)
  require(ggplot2)
  get_facet_ncol <- function(x)
  {
    ncol=round(sqrt(x))
    nrow=round(x/ncol)
    if (ncol <= nrow)
      ncol+1
    else
      ncol
  }
  # check if feature is numeric or character
  findex <- 0
  if (is.character(feature))
  {
    i <- 1
    for (n in names(data))
    {
      if (n == feature)
      {
        findex <- i
        break
      }
      i <- i+1
    }
  }
  if (is.numeric(feature))
  {
    findex <- feature
    feature <- names(data)[findex]
  }
  # select factors
  data %>%
    select_if(is.factor) -> data.factor
  
  # prepare data for plot
  data[,-findex] %>%
    select_if(is.numeric) %>%
    cbind(data[,findex],.) -> data
  findex <- 1
  colnames(data)[findex] <- feature

  names(data)[-findex] -> lutable
  newdata <- data.frame()
  for (i in 1:length(lutable))
  {
    newdata <- rbind(newdata,
      cbind(x=data[,lutable[i]],
            y=data[,findex],
            xlabel=lutable[i],
            data.factor,stringsAsFactors = FALSE))
  }
  
  # plot
  facet_ncol <- get_facet_ncol(nlevels(factor(newdata$xlabel)))
  xlab <- ""
  ylab <- ""
  if (is.factor(data[,findex]))
  {
    xlab <- toupper(feature)
    graph <- ggplot(newdata,aes(y,x)) + 
      geom_boxplot(mapping=mapping)
  }
  else
  {
    if (flip==TRUE)
    {
      xlab <- toupper(feature)
      graph <- ggplot(newdata,aes(y,x))
    }
    else
    {
      ylab <- toupper(feature)
      graph <- ggplot(newdata,aes(x,y))
    }
    graph <- graph +
      geom_point(mapping=mapping)
  }
    graph +
      labs(x=xlab,
           y=ylab,
           title=paste("Pairs plot by",toupper(feature)),
           subtitle=subtitle) +
      facet_wrap(~ xlabel,scales = "free",ncol = facet_ncol) +
      theme_bw()
}

#################################################################
#                    sortedLevels_by
#################################################################
sortedLevels_by <- function(data,to.sort,sort.by,func=mean,...)
{
  require(dplyr)
  if (is.null(to.sort) & is.null(sort.by))
    stop("to.sort and sort.by args are both required")
  if (!is.character(to.sort) & !is.character(sort.by))
    stop(to.sort," and ",sort.by," must be character")
  if (! is.factor(data[,to.sort]))
    stop(to.sort," must be factor")
  if (! is.numeric(data[,sort.by]))
    stop(sort.by," must be numeric")
  if (is.function(sort.by))
    stop(sort.by,"can't be a function")
  if (is.function(to.sort))
    stop(to.sort,"can't be a function")
  
  data[,sort.by] %>%
    tapply(data[,to.sort],func,...) %>%
    sort() %>%
    names()
}

#################################################################
#                    allFactToNumeric
#################################################################
allFacToNumeric <- function(data,sort.by=NULL,FUNC=mean,...)
{
  fact_names <- data %>% select_if(is.factor) %>% names()

  for (fname in fact_names)
  {
    if (is.null(sort.by))
      data[,fname] <- as.numeric(factor(data[,fname]))
    else
    {
      lev <- sortedLevels_by(data,fname,sort.by,FUNC,...)
      data[,fname] <- as.numeric(factor(data[,fname],
                                        levels = lev))
    }
  }
  data
}

#################################################################
#                    normalize
#################################################################
normalize <- function(x)
{
  (x-min(x))/diff(range(x))
}

#################################################################
#                    normalize_all
#################################################################
normalize_all <- function(data,FUNC=normalize,...)
{
  require(magrittr)
  data %>%
    lapply(function(x) {if (is.numeric(x)) FUNC(x,...) else x}) %>%
    as.data.frame()
}

#################################################################
#                    get_random
#################################################################
get_random <- function(n,x,adjust=1)
{
  if (is.numeric(x) & !is.factor(x))
  {
    d <- density(x,adjust=adjust)
    rnorm(n,sample(x,size = n,replace = TRUE),d$bw)
  }
  else
  {
    probs <- sort(prop.table(table(x)))
    nprobs <- length(probs)
    if (nprobs == 1)
      stop("variable x must have more than one category")
    prob_breaks <- c(0,probs[1],probs[2:(nprobs-1)]+probs[1],1)
    print(prob_breaks)
    rsamples <- runif(n)
    names(probs)[findInterval(rsamples,prob_breaks,rightmost.closed = TRUE,all.inside = TRUE)]
  }
}

#################################################################
#                    cross_validation
#################################################################
cross_validation <- function(actual,predicted,print=TRUE)
{
  if (print)
  {
    require(gmodels)
    ct <- CrossTable(actual,predicted,prop.c = FALSE,
                     prop.t = FALSE,
                     prop.chisq = FALSE,dnn = c("actual","predicted"))
  }
  else
  {
    ct <- list(t=table(actual,predicted),
               prop.row=prop.table(table(actual,predicted),1))
  }
  acc <- sum(diag(ct$t))/length(predicted)
  sens <- diag(ct$prop.row)
  out <- c(acc,sens)
  #names(out) <- c("accuracy","sensitivity","specificity")
  names(out)[1] <- "accuracy"
  out
}

#################################################################
#                    get_train_samples
#################################################################
get_train_samples <- function(data,class_name,proportion=0.8,nsamples=1,maxiter=50,table_col=1)
{
  if (maxiter < nsamples)
    maxiter <- 2*nsamples
  cl_index <- which(names(data) == class_name)
  ntrain <- round(nrow(data)*proportion)
  iterator <- 1:maxiter
  names(iterator) <- as.character(1:maxiter)
  samples <- lapply(iterator,function(x) sample(1:nrow(data),ntrain))
  samp_prop <- sapply(samples,function(x) abs(prop.table(table(data[x,cl_index]))[[table_col]]-
                                              prop.table(table(data[,cl_index]))[[table_col]]))
  samples <- samples[order(samp_prop)]
  if (nsamples > 1)
    samples[1:nsamples]
  else
    samples[[1]]
}

#################################################################
#                    correlation_matrix
#################################################################
correlation_matrix <- function(data)
{
  require(ggplot2)
  numeric_names <- names(data)[sapply(data,is.numeric)]
  if (length(numeric_names) > 1)
  {
    corr_grid <- expand.grid(numeric_names,numeric_names,stringsAsFactors = FALSE)
    corr_grid$correlation <- apply(corr_grid,1,function(x) cor(data[,x[1]],data[,x[2]]))
    pcorr <- ggplot(corr_grid,aes(Var1,Var2)) +
      geom_raster(aes(fill=correlation)) +
      geom_text(aes(label=round(correlation,2),size=0.5),col="white") +
      labs(title="Correlation Matrix")+
      scale_fill_gradientn(colours = c("#3B5998","white","#CC2A36")) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = -45,vjust = 1, hjust = 0))
    plot(pcorr)
  }
  factor_names <- names(data)[sapply(data,is.factor)]
  if (length(factor_names) > 1)
  {
    chisq_grid <- expand.grid(factor_names,factor_names,stringsAsFactors = FALSE)
    idx <- 1:nrow(chisq_grid)
    pval <- sapply(idx,function(x) chisq.test(table(data[,chisq_grid$Var1[x]],
                                                    data[,chisq_grid$Var2[x]]))$p.value)
    chisq_grid$pvalue <- pval
    chisq_grid <- chisq_grid[chisq_grid$pval <= 0.05,]
    pchitest <- ggplot(chisq_grid,aes(Var1,Var2)) +
      geom_raster(aes(fill=pvalue)) +
      geom_text(aes(label=round(pvalue,3),size=0.5),col="white")+
      labs(title="Chisq Test p-value Matrix") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = -45,vjust = 1,hjust = 0))
    if (length(numeric_names != 0))
      invisible(readline(prompt = "Please press <enter> for the next plot"))
    plot(pchitest)
  }
}

#################################################################
#                         capitalize
#################################################################
capitalize <- function(x)
{
  x <- tolower(x)
  first <- substr(x,1,1)
  last <- substr(x,2,nchar(x))
  paste(toupper(first),last,sep = "",collapse = "")
}

#################################################################
#                         pairs_plot
#################################################################
pairs_plot <- function(data,objective)
{
  require(ggplot2)
  if (!is.character(objective))
    stop(paste("objective must be character"))
  if (length(which(names(data) == objective)) == 0)
    stop(paste("feature",objective,"doesn't exist"))
  #numeric
  fnames <- names(data[,sapply(data,is.numeric)])
  if (length(fnames) != 0)
  {
    grid <- expand.grid(objective,fnames,stringsAsFactors = FALSE)
    grid <- grid[grid$Var1 != grid$Var2,]
    if (is.numeric(data[,objective]))
    {
      plot_data <- do.call("rbind",
                           apply(grid,1,function(x) data.frame(y=data[,x[1]],
                                                               x=data[,x[2]],
                                                               name=x[2])))
      p <- ggplot(plot_data,aes(x,y)) +
        geom_point(aes(col=name)) +
        facet_wrap(~name,scales="free") +
        theme_bw() +
        labs(y=capitalize(objective),x="",
             title=paste(capitalize(objective),"vs. Numeric Pairs"))
      plot(p)
    }
    else if (is.factor(data[,objective]))
    {
      plot_data <- do.call("rbind",
                           apply(grid,1,function(x) data.frame(x=data[,x[1]],
                                                               y=data[,x[2]],
                                                               name=x[2])))
      p <- ggplot(plot_data,aes(x,y)) +
        geom_boxplot(aes(col=x,fill=x,alpha=0.5)) +
        geom_jitter(aes(col=x),width = 0.3,size=1) +
        facet_wrap(~name,scales="free") +
        theme_bw() +
        labs(x=capitalize(objective),y="",
             title=paste(capitalize(objective),"vs. Numeric Pairs"))
      plot(p)
    }
  }# end numeric
  # factors
  fnames <- names(data[,sapply(data,is.factor)])
  if (length(fnames) != 0)
  {
    grid <- expand.grid(objective,fnames,stringsAsFactors = FALSE)
    grid <- grid[grid$Var1 != grid$Var2,]
    if (is.numeric(data[,objective]))
    {
      plot_data <- do.call("rbind",
                           apply(grid,1,function(x) data.frame(y=data[,x[1]],
                                                               x=data[,x[2]],
                                                               name=x[2])))
      p <- ggplot(plot_data,aes(x,y)) +
        geom_boxplot(aes(col=x,fill=x,alpha=0.5)) +
        geom_jitter(aes(col=x),width = 0.2,size=1) +
        facet_wrap(~name,scales="free") +
        theme_bw() +
        labs(y=capitalize(objective),x="",
             title=paste(capitalize(objective),"vs. Factors Pairs"))
      invisible(readline(prompt = "Please press [enter] for the next plot"))
      plot(p)
    }
    else if (is.factor(data[,objective]))
    {
      gen_data_frame <- function(x)
      {
        pt <- prop.table(table(data[,x[1]],data[,x[2]]),2)
        df <- as.data.frame(pt,stringsAsFactors=FALSE)
        df$name <- x[2]
        df
      }
      plot_data <- do.call("rbind",
                           apply(grid,1,gen_data_frame))
      p <- ggplot(plot_data,aes(Var1,Freq)) +
        geom_col(aes(col=Var2,fill=Var2,alpha=0.5),position = "dodge") +
        facet_wrap(~name,scales="free") +
        theme_bw() +
        labs(x=capitalize(objective),y="",
             title=paste(capitalize(objective),"vs. Factors Pairs"))
      invisible(readline(prompt = "Please press [enter] for the next plot"))
      plot(p)
    }
  }# end factors
}

#################################################################
#                     MEAN ABSOLUTE ERROR
#################################################################
MAE <- function(actual,predicted)
{
  mean(abs(actual-predicted))
}

#################################################################
#                     NUMERIC TO CATEGORICAL
#################################################################
numToCateg <- function(feature,grouping)
{
  if (!is.numeric(feature))
    stop("The feature must be numeric")
  if (!is.factor(grouping))
  {
    if (is.character(grouping))
      grouping <- factor(grouping)
    else
      stop("grouping can't be factorized")
  }
  source("/home/rosario/Documenti/R_data/script/OneRFS.R",echo = FALSE)
  optimal_breaks <- sqrt(length(feature))
  start_breaks <- floor(optimal_breaks-sqrt(optimal_breaks))
  stop_breaks <- ceiling(optimal_breaks+sqrt(optimal_breaks))
  cut_index <- start_breaks:stop_breaks
  err_rate <- sapply(cut_index,function(x){ get_error_rate(cut(feature,x,include.lowest = TRUE),grouping)})
  best_cut <- which.min(err_rate)
  cut(feature,cut_index[best_cut],include.lowest = TRUE)
}

#################################################################
#                         N GRAMS
#################################################################
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

#################################################################
#                         DUMMY CODING
#################################################################
dummy_coding <- function(df,f_names)
{
  # df          a data frame
  # f_names     features names to be dummy-coded (character)
  require(magrittr)
  if (!is.data.frame(df))
    stop("df is required to be a data.frame")
  if (!is.character(f_names))
    stop("f_names is required to be a character vector")
  dummify <- function(x,x_name)
  {
    if (!is.factor(x))
      stop(paste("feature",x_name,"is required to be a factor"))
    x_levels <- levels(x)
    nl <- length(x_levels)
    dummy_var <- x_levels[1:(nl-1)]
    dummy_var <- gsub("[[:space:]]","_",dummy_var)
    idx <- 1:(nl-1)
    names(idx) <- dummy_var
    dummy_df <- as.data.frame(lapply(idx,function(i) ifelse(x == dummy_var[i],1,0)))
    names(dummy_df) <- paste(x_name,"=",dummy_var,sep = "")
    dummy_df
  }
  dummy_frame <- lapply(f_names,function(x) dummify(df[,x],x)) %>%
    do.call(what = cbind,.)
  cbind(df[,-which(names(df) == f_names)],dummy_frame)
}


expand_grid_no_rep <- function(x,y)
{
  require(magrittr)
  lx <- length(x)
  ly <- length(y)
  if (lx > ly)
  {
    x_temp <- x
    lx_temp <- lx
    x <- y
    y <- x_temp
    lx <- ly
    ly <- lx_temp
  }
  data.frame(Var1 = rep(x,c(ly:(ly-lx+1))),
             Var2 = lapply(1:lx,function(i) y[i:ly]) %>% do.call(c,.))
}


#################################################################
#                      COMBINE CATEGORICAL
#################################################################
cCombine <- function(x,y,sep="*")
{
  if (is.numeric(x))
    stop("feature x can't be numeric")
  if (is.numeric(y))
    stop("feature y can't be numeric")
  x <- gsub(" ","_",x)
  y <- gsub(" ","_",y)
  paste(x,y,sep = sep)
}

#################################################################
#                     COMPUTE TIME DURATION
#################################################################
compute_time_duration <- function(x,y)
{
  x <- as.numeric(strsplit(x,split = ":")[[1]])
  y <- as.numeric(strsplit(y,split = ":")[[1]])
  result <- y-x
  porto <- 0
  for (i in 3:1)
  {
    if (result[i] == 0 && porto != 0)
      stop("x > y")
    if (porto != 0)
    {
      result[i] <- result[i] + porto
      porto <- 0
    }
    if (result[i] < 0)
    {
      result[i] <- result[i] + 60
      porto <- -1
    }
  }
  sprintf("%02d:%02d:%02d",result[1],result[2],result[3])
}