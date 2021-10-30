#!/usr/bin/R

hist2d.make_range <- function(data)
{
	c(floor(min(data)),ceiling(max(data)))
}

hist2d.check_range <- function(data,range,label)
{
	if (is.null(range))
	{
		range <- hist2d.make_range(data)
	}
	else if (length(range) != 2)
	{
		err.msg <- sprintf("RangeError: %srange has length != 2\n",label)
		stop(err.msg)
	}
	else if (range[1] > range[2])
	{
		warning(sprintf("%srange[1] > %srange[2]\nrange sorted\n",label,label))
		range <- sort(range)
	}
	range
}

hist2d.autobin <- function(n) ceiling(2*n**0.333)

hist2d <- function(x,y,nbins=NULL,nxbins=nbins,nybins=nbins,xbreaks=NULL,ybreaks=NULL,xrange=NULL,yrange=NULL,density=FALSE)
{	
	if (is.null(xbreaks))
	{
		if (is.null(nxbins))
			nxbins <- hist2d.autobin(length(x))
		xrange <- hist2d.check_range(x,xrange,"x")
		xbreaks <- seq(xrange[1],xrange[2],len=nxbins+1)
	}
	else
	{
		nxbins <- length(xbreaks)-1
		if (! is.null(xrange))
		{
			warning("xrange will be recalcualted on xbreaks")
			xrange <- range(xbreaks)
		}
	}
	
	if (is.null(ybreaks))
	{
		if (is.null(nybins))
			nybins <- hist2d.autobin(length(y))
		yrange <- hist2d.check_range(y,yrange,"y")
		ybreaks <- seq(yrange[1],yrange[2],len=nybins+1)
	}
	else
	{
		nybins <- length(ybreaks)-1
		if (! is.null(yrange))
		{
			warning("yrange will be recalcualted on ybreaks")
			yrange <- range(ybreaks)
		}
	}
	
	xmids <- xbreaks[-(nxbins+1)]+diff(xbreaks)/2
	ymids <- ybreaks[-(nybins+1)]+diff(ybreaks)/2
	
	intx <- factor(findInterval(x,xbreaks),levels=1:nxbins)
	inty <- factor(findInterval(y,ybreaks),levels=1:nybins)
	#print(table(intx,inty))
	freq <- as.data.frame(table(intx,inty),stringsAsFactors=FALSE)
	freq$intx <- as.numeric(freq$intx)
	freq$inty <- as.numeric(freq$inty)
	h2d <- expand.grid(xmids,ymids)
	h2d$counts <- freq$Freq
	if (density == TRUE)
	{
		total_counts <- sum(h2d$counts)
		h2d$counts <- h2d$counts/total_counts
		colnames(h2d)[3] <- "freq"
	}
	colnames(h2d)[1:2] <- c("xmids","ymids")
	rm(list=c("freq","intx","inty"))
	h2d
}
