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
  grid <- expand.grid(cnames[-findex],cnames[-findex],stringsAsFactors = FALSE)
  colnames(grid) <- c("x","y")
  density.grid <- subset(grid, x == y)
  grid <- subset(grid,x != y)
  
  newdata <- data.frame()
  for (i in 1:nrow(grid))
  {
    newdata <- rbind(newdata,
                     cbind(data.frame(x=data[,grid[i,"x"]],
                                      y=data[,grid[i,"y"]]),
                           data.frame(labelx=grid[i,"x"],
                                      labely=grid[i,"y"]),
                           data[,cnames[findex]]))
  }
  colnames(newdata)[5:ncol(newdata)] <- cnames[findex]
  
  #newdata$labelx <- factor(newdata$labelx,levels=cnames[-findex])
  #newdata$labely <- factor(newdata$labely,levels=cnames[-findex])
  
  # make densities
  densities <- data.frame()
  for (i in 1:nrow(density.grid))
  {
    ddata <- data[,density.grid[i,"x"]]
    d <- density(ddata,n=128,adjust=1,na.rm=TRUE)
    r <- diff(range(ddata))
    d$y <- (d$y/max(d$y))*r + min(ddata)
    densities <- rbind(densities,
                       cbind(data.frame(x = d$x, y = d$y),
                             data.frame(labelx=density.grid[i,"x"],
                                        labely=density.grid[i,"y"],
                                        stringsAsFactors = FALSE)))
  }
  #colnames(densities)[5:ncol(densities)] <- cnames[findex]
  densities$labelx <- factor(densities$labelx,levels=cnames[-findex])
  densities$labely <- factor(densities$labely,levels=cnames[-findex])
  
  AllData <- list(point=newdata,density=densities)
  AllData
}

ggpairs <- function(data,mapping = NULL,...,
                       na.rm = FALSE, 
                       smooth = FALSE, 
                       smooth_mapping = NULL,
                       smooth_method = "auto",
                       smooth_formula = y ~ x,
                       smooth_se = TRUE)
{
  require(ggplot2)
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