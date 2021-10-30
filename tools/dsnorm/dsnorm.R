####### dsnorm ###########
# skewed normal probality density function
#______________________________________________________________________
dsnorm <- function(x,mu=0,sigma=1,shape=1)
{
  x <- (x-mu)/sigma
  2*dnorm(x)*pnorm(shape*x)/sigma
}

########### psnorm #################
# skewed normal cdf
#______________________________________________________________________
psnorm <- function(q,mu=0,sigma=1,shape=1,lower.tail=TRUE)
{
  if (lower.tail)
  {
    sapply(q,function(x) {integrate(f = dsnorm,
                   lower = -Inf, 
                   upper = x,
                   mu=mu, sigma=sigma, shape=shape)$value})
  }
  else
  {
    sapply(q,function(x) {integrate(f = dsnorm,
                   lower = x, 
                   upper = Inf,
                   mu=mu, sigma=sigma, shape=shape)$value})
  }
}

############### qsnorm #################
# quantile function
#______________________________________________________________________
qsnorm <- function(p,mu=0,sigma=1,shape=1,lower.tail = TRUE)
{
  absdist <- function(q,prob)
  {
    abs(prob - psnorm(q,mu,sigma,shape,lower.tail))
  }
  sapply(p,function(x) {optimize(f = absdist,
                  interval = c(mu-5*sigma,mu+6*sigma),
                  tol = 1.e-6,prob=x)$minimum})
}

############ rsnorm ###################
# random generator for skewed normal pdf
#______________________________________________________________________
rsnorm <- function(n,mu=0,sigma=1,shape=1)
{
  qsnorm(runif(n),mu=mu,sigma=sigma,shape=shape)
}

########### log_likelihood ##############
#______________________________________________________________________
log_likelihood <- function(par,x,FUNC)
{
  #scale <- 1.e-2
  -sum(log(FUNC(x,par)))
}

############# hist_fit ##############
# make histogram and fit it
#______________________________________________________________________
fit_hist <- function(x,spar,FUNC,linecol="red",...)
{
  h <- hist(x,freq = FALSE,...)
  fit <- optim(par = spar,fn = log_likelihood,x=x,FUNC=FUNC)
  xline <- seq(floor(min(x)),ceiling(max(x)),len=100)
  yline <- FUNC(xline,fit$par)
  lines(xline,yline,col=linecol)
  fit$par
}



# cl <- "B"
# bcancer.tocheck <- filter(bcancer.train,diagnosis == cl)[26:30]
# for (f in names(bcancer.tocheck[,-1]))
# {
#   fit_hist(x = bcancer.tocheck[,f],
#            main=paste("Histogram of ",f," (",cl,")"),
#            xlab=f)
# }
# 
# extimation.dnorm <- data.frame()
