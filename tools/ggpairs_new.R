ggpairs.prepare <- function(data)
{
  # get factors index
  findex <- c()
  for (i in 1:ncol(data))
  {
    if (is.factor(data[,i]))
      findex <- c(findex,i)
  }
  
  cnames <- colnames(data)
  grid <- expand.grid(cnames,cnames,stringsAsFactors = FALSE)
  colnames(grid) <- c("x","y")
  diagonal <- subset(grid, x == y)
  grid <- subset(grid, x != y)
  
  subset.grid <- function(fx,fy)
  {
    grid[sapply(grid$x, function(x) fx(data[,x])) &
           sapply(grid$y, function(x) fy(data[,x])),]
  }
  
  make.data.from.grid <- function(g)
  {
    gdata <- data.frame()
    if (nrow(g) != 0)
    {
      for (i in 1:nrow(g))
      {
        gdata <- rbind(gdata,
                         cbind(data.frame(x=data[,g[i,"x"]],
                                          y=data[,g[i,"y"]]),
                               data.frame(labelx=g[i,"x"],
                                          labely=g[i,"y"],
                                          stringsAsFactors = FALSE),
                               data[,cnames[findex]]))
      }

      colnames(gdata)[5:ncol(gdata)] <- cnames[findex]

      gdata$labelx <- factor(gdata$labelx,levels=levels(factor(cnames)))
      gdata$labely <- factor(gdata$labely,levels=levels(factor(cnames)))
    }
    gdata
  }
  
  nngrid <- subset.grid(is.numeric,is.numeric)
  fngrid <- subset.grid(is.factor,is.numeric)
  nfgrid <- subset.grid(is.numeric,is.factor)
  ffgrid <- subset.grid(is.factor,is.factor)
  
  nndata <- make.data.from.grid(nngrid)
  fndata <- make.data.from.grid(fngrid)
  nfdata <- make.data.from.grid(nfgrid)
  ffdata <- make.data.from.grid(ffgrid)
  # numeric diagonal
  ndiag <- diagonal[sapply(diagonal$x, function(x) is.numeric(data[,x])),]
  ndata <- make.data.from.grid(ndiag)
  

  # factor diagonal
  fdiag <- diagonal[sapply(diagonal$x, function(x) is.factor(data[,x])),]
  fdata <- make.data.from.grid(fdiag)
  #fdata <- fdata[,-2]
  
  list(nn=nndata,nf=nfdata,fn=fndata,ff=ffdata,n=ndata,f=fdata)
  # # numeric-numeric grid
  # nngrid <- grid[sapply(grid$x, function(x) is.numeric(data[,x])) &
  #                  sapply(grid$y, function(x) is.numeric(data[,x])),]
  # # numeric-factor grid
  # nfgrid <- grid[sapply(grid$x, function(x) is.numeric(data[,x])) &
  #                  sapply(grid$y, function(x) is.factor(data[,x])),]
  # # factor-numeric grid
  # fngrid <- grid[sapply(grid$x, function(x) is.factor(data[,x])) &
  #                  sapply(grid$y, function(x) is.numeric(data[,x])),]
  # # factor-factor grid
  # ffgrid <- grid[sapply(grid$x, function(x) is.factor(data[,x])) &
  #                  sapply(grid$y, function(x) is.numeric(data[,x])),]
  # # numeric diagonal
  # ndiag <- diagonal[sapply(diagonal$x, function(x) is.numeric(data[,x])),]
  # 
  # # factor diagonal
  # fdiag <- diagonal[sapply(diagonal$x, function(x) is.factor(data[,x])),]
  # 
  # # prepare all data
  # AllData <- list()
  # 
  # # numeric-numeric
  # if (nrow(nngrid) != 0)
  # {
  #   nndata <- data.frame()
  #   for (i in 1:nrow(nngrid))
  #   {
  #     nndata <- rbind(nndata,
  #                      cbind(data.frame(x=data[,nngrid[i,"x"]],
  #                                       y=data[,nngrid[i,"y"]]),
  #                            data.frame(labelx=nngrid[i,"x"],
  #                                       labely=nngrid[i,"y"],
  #                                       stringsAsFactors = FALSE),
  #                            data[,cnames[findex]]))
  #   }
  #   
  #   colnames(nndata)[5:ncol(nndata)] <- cnames[findex]
  # 
  #   nndata$labelx <- factor(nndata$labelx,levels=cnames[-findex])
  #   nndata$labely <- factor(nndata$labely,levels=cnames[-findex])
  # }
  # 
  # # factor-numeric
  # if (nrow(fngrid) != 0)
  # {
  #   fndata <- data.frame()
  #   for (i in 1:nrow(fngrid))
  #   {
  #     fndata <- rbind(fndata,
  #                     cbind(data.frame(x=data[,fngrid[i,"x"]],
  #                                      y=data[,fngrid[i,"y"]]),
  #                           data.frame(labelx=fngrid[i,"x"],
  #                                      labely=fngrid[i,"y"],
  #                                      stringsAsFactors = FALSE),
  #                           data[,cnames[findex]]))
  #   }
  #   
  #   colnames(fndata)[5:ncol(fndata)] <- cnames[findex]
  #   
  #   fndata$labelx <- factor(fndata$labelx,levels=cnames[-findex])
  #   fndata$labely <- factor(fndata$labely,levels=cnames[-findex])
  # }
  # 
  # # factor-numeric
  # if (nrow(fngrid) != 0)
  # {
  #   fndata <- data.frame()
  #   for (i in 1:nrow(fngrid))
  #   {
  #     fndata <- rbind(fndata,
  #                     cbind(data.frame(x=data[,fngrid[i,"x"]],
  #                                      y=data[,fngrid[i,"y"]]),
  #                           data.frame(labelx=fngrid[i,"x"],
  #                                      labely=fngrid[i,"y"],
  #                                      stringsAsFactors = FALSE),
  #                           data[,cnames[findex]]))
  #   }
  #   
  #   colnames(fndata)[5:ncol(fndata)] <- cnames[findex]
  #   
  #   fndata$labelx <- factor(fndata$labelx,levels=cnames[-findex])
  #   fndata$labely <- factor(fndata$labely,levels=cnames[-findex])
  # }
  # 
  # # make densities
  # densities <- data.frame()
  # for (i in 1:nrow(density.grid))
  # {
  #   ddata <- data[,density.grid[i,"x"]]
  #   d <- density(ddata,n=128,adjust=1,na.rm=TRUE)
  #   r <- diff(range(ddata))
  #   d$y <- (d$y/max(d$y))*r + min(ddata)
  #   densities <- rbind(densities,
  #                      cbind(data.frame(x = d$x, y = d$y),
  #                            data.frame(labelx=density.grid[i,"x"],
  #                                       labely=density.grid[i,"y"],
  #                                       stringsAsFactors = FALSE)))
  # }
  # #colnames(densities)[5:ncol(densities)] <- cnames[findex]
  # densities$labelx <- factor(densities$labelx,levels=cnames[-findex])
  # densities$labely <- factor(densities$labely,levels=cnames[-findex])
  # 
  # #AllData <- list(point=newdata,density=densities)
  # AllData
}

#_______________________________________________________________________
ggpairs <- function(data,mapping = NULL,...,
                    na.rm = FALSE, 
                    smooth = FALSE, 
                    smooth_mapping = NULL,
                    smooth_method = "auto",
                    smooth_formula = y ~ x,
                    smooth_se = TRUE)
{
  # require(ggplot2)
  df <- ggpairs.prepare(data)
  gp <- ggplot(df$point,aes(x,y)) + facet_grid(labely ~ labelx, scales = "free")
  gpoint <- geom_point(mapping = mapping,...,na.rm=na.rm)
  # geom_density(aes(x=x,y=..scaled..*diff(range(x))+min(x),fill=Species,alpha=0.5),
  #              data=df$density,
  #              #position = "identity",
  #              adjust = 2) +
  gline <- geom_line(aes(x,y),data=df$density,na.rm=na.rm)
  gt <- theme_bw()
  ggp <- gp + gpoint + gline + gt
  if (smooth == TRUE)
  {
    gsmooth <- geom_smooth(mapping = smooth_mapping,
                           method = smooth_method, 
                           formula = smooth_formula,
                           se = smooth_se,
                           na.rm = na.rm)
    ggp <- ggp + gsmooth
  }
  
  plot(ggp)
}
plotta <- function(data,fill=NULL)
{
  df <- ggpairs.prepare(data)
  p <- ggplot(data.frame(labelx=colnames(data),
                         labely=colnames(data))) +
    theme_bw() + facet_grid(labely ~ labelx, scales = "free")
  if (nrow(df$ff) != 0)
    p <- p + geom_count(aes(x,y,size=..n..,group=fill,col=fill), data=df$ff)
  if (nrow(df$nf) != 0)
    p <- p + geom_jitter(aes(x,y,col=fill), data=df$nf,height = 0.3)
  if (nrow(df$f) != 0)
    p <- p + geom_count(aes(x,y,size=..n..,group=fill,col=fill), data=df$f)
  if (nrow(df$nn) != 0)
    p <- p + geom_point(aes(x,y,col=fill),data=df$nn)
  if (nrow(df$n) != 0)
    p <- p + geom_boxplot(aes(x,y,fill=fill,alpha=0.5),data=df$n,position = "identity")
  if (nrow(df$n) != 0)
    p <- p + geom_boxplot(aes(x,y,fill=fill),data=df$fn)
  if (nrow(df$ff) != 0 | nrow(df$f) != 0)
    p <- p + scale_size_area(max_size = 5)
  
  plot(p)
}
