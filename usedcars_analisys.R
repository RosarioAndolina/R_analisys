library(ggplot2)
library(dplyr)
library(gmodels)

setwd("/home/rosario/Documenti/R_data")

usedcars <- read.csv("usedcars.csv",stringsAsFactors = FALSE)

# let's look at the summaries of numeric features

usedcars %>%
  select_if(is.numeric) %>%
  lapply(summary)

# price qqplot

mean.price <- mean(usedcars$price)
sd.price <- sd(usedcars$price)
q.line <- data.frame(x=qnorm(ppoints(100)),
                     y=qnorm(ppoints(100),mean.price,sd.price))

ggplot(usedcars) + geom_qq(aes(sample=price),distribution = qnorm) + 
  theme_bw() +
  geom_line(aes(x,y),data=q.line) +
  labs(title="Used Cars Price QQ plot\nNormal distribution",
       caption="usedcars Data")

# mileage qqplot

mean.mileage <- mean(usedcars$mileage)
sd.mileage <- sd(usedcars$mileage)
mean.log.mileage <- mean(log(usedcars$mileage))
sd.log.mileage <- sd(log(usedcars$mileage))
q.line <- data.frame(x=qnorm(ppoints(100)),
                     y=qnorm(ppoints(100),mean.mileage,sd.mileage))
q.line.ln <- data.frame(x=qnorm(ppoints(100)),
                        y=qnorm(ppoints(100),mean.log.mileage,sd.log.mileage))

q.line.df <- rbind(cbind(q.line,distr="normal"),cbind(q.line.ln,distr="log-normal"))
rm(list = c("q.line","q.line.ln"))
# normal qqplot
ggplot(usedcars) + geom_qq(aes(sample=mileage),distribution = qnorm) + 
  theme_bw() +
  geom_line(aes(x,y,col=distr),data=filter(q.line.df,distr=="normal")) +
  labs(title="Used Cars mileage QQ plot\nNormal distribution",
       caption="usedcars Data")

# log-normal qqplot

ggplot(usedcars) + geom_qq(aes(sample=log(mileage)),distribution = qnorm) + 
  theme_bw() +
  geom_line(aes(x,y,col=distr),data=filter(q.line.df,distr=="log-normal")) +
  labs(title="Used Cars mileage QQ plot\nLog-Normal distribution",
       y="log(mileage)",
       caption="usedcars Data")

# mileage histogram vs. normal dist & log-normal

mileage.low.edge <- usedcars$mileage %>% min() %>% floor()
mileage.hight.edge <- usedcars$mileage %>% max() %>% ceiling()
mileage.seq <- seq(mileage.low.edge,mileage.hight.edge,len=100)

ggplot(usedcars,aes(mileage)) +
  geom_histogram(aes(y=..density..),
                 fill="#35961b",
                 bins = round(sqrt(nrow(usedcars)))) + 
  geom_line(aes(x,y,col="normal"),
            data=data.frame(x=mileage.seq,
                            y=dnorm(mileage.seq,mean.mileage,sd.mileage)),
            size=1.5) +
  geom_line(aes(x,y,col="log-normal"),
            data=data.frame(x=mileage.seq,
                            y=dlnorm(mileage.seq,mean.log.mileage,sd.log.mileage)),
            size=1.5) +
  theme_bw() +
  labs(title="Mileage Histogram",caption="usedcars Data")+
  scale_colour_discrete(name="Distribution")

# price histogram
price.seq <- seq(floor(min(usedcars$price)),ceiling(max(usedcars$price)))
ggplot(usedcars,aes(price)) + 
  geom_histogram(aes(y=..density..),
                 fill="#35961b",
                 bins=round(sqrt(nrow(usedcars)))) +
  geom_line(aes(x,y),
             data=data.frame(x=price.seq,
                             y=dnorm(price.seq,mean.price,sd.price)),
             col="blue") +
  theme_bw() + 
  labs(title="Price Histogram vs. Normal Distribution",
       caption="Usedcars Data")

# correlation matrix

usedcars %>%
  select_if(is.numeric) %>%
  cor()

#correletion matrix grid
cmgrid <- expand.grid(names(usedcars.numeric),names(usedcars.numeric),stringsAsFactors = FALSE)
cmgrid$cor <- 0
for (i in 1:nrow(cmgrid)) 
{
  cmgrid$cor[i] <- cor(usedcars.numeric[,cmgrid$Var1[i]],usedcars.numeric[,cmgrid$Var2[i]])
}
rm("i")

# plot cor matrix
cmgrid %>%
  mutate(Var1=factor(Var1,levels = colnames(usedcars.numeric)),
         Var2=factor(Var2,levels = rev(colnames(usedcars.numeric)))) %>%

ggplot(aes(Var1,Var2)) + geom_raster(aes(fill=cor)) + theme_bw()+
  labs(title="Correlation Matrix",x="Features",y="Features",
       caption="Usedcars Data") +
  theme(axis.text.x = element_text(hjust = 0.5,vjust = 0.5,angle = 45)) +
  geom_text(aes(label=round(cor,digits = 2)),col="white") +
  scale_fill_gradient2(low = "red",breaks=seq(-1,1,len=5))
  
############# cross table ################
table(usedcars$color) %>%
  Filter(function(x) x > median(.),.) %>%
  names() -> color.selected

mutate(usedcars,mostused=(color %in% color.selected)) -> usedcars 
# most used color vs. model
CrossTable(x = usedcars$model,
           y=usedcars$mostused,chisq = TRUE)
# most used color vs. transmission
CrossTable(x=usedcars$transmission,
           y=usedcars$mostused,chisq = TRUE)

# most used color vs. most recent
usedcars <- mutate(usedcars,mostrecent=ifelse(year >= 2009,TRUE,FALSE))
CrossTable(x=usedcars$mostrecent,
           y=usedcars$mostused,chisq = TRUE)

# most recent vs. model
CrossTable(x=usedcars$model,
           y=usedcars$mostrecent,chisq = TRUE)

############# scatter plot ##################

ggplot(usedcars,aes(mileage,price,col=mostused)) + geom_point()+
  theme_bw()+
  labs(title="Price vs Mileage for colours usage",caption="Usedcars Data") +
  scale_colour_discrete(name="Color\nUsage",labels=c("less used","most used")) +
  geom_smooth(method = "lm",se=FALSE)

########### convert char in numeric ###############

usedcars %>%
  mutate(model=as.numeric(factor(model)),
         transmission=as.numeric(factor(transmission)),
         mostused=as.numeric(factor(mostused)),
         mostrecent=as.numeric(factor(mostrecent))) %>%
  select(-5) -> usedcars.num

pairs(select(usedcars.num,price,transmission,mostused,mostrecent))

############### regression model ################

fit <- lm(price ~ year + model + mileage,data=usedcars.num)
fit
summary(fit)
cor(usedcars$price,predict(fit,usedcars.num))

# better performance

fit <- lm(price ~ year + model + mileage + transmission + mostused + mostrecent,
          data=usedcars.num)
fit
summary(fit)
cor(usedcars$price,predict(fit,usedcars.num))


usedcars.num %>%
  mutate(model2=model**2) -> usedcars.num

fit <- lm(price ~ year + model + model2 + mileage + transmission + mostused + mostrecent,
          data=usedcars.num)
fit
summary(fit)
cor(usedcars$price,predict(fit,usedcars.num))

########### best conversion char in numeric ###############
model.levels <- names(sort(table(usedcars$model)))
trans.levels <- names(sort(table(usedcars$transmission)))
color.levels <- names(sort(table(usedcars$color)))
mostused.levels <- names(sort(table(usedcars$mostused)))

usedcars %>%
  mutate(model=as.numeric(factor(model,levels = model.levels)),
         transmission=as.numeric(factor(transmission,levels = trans.levels)),
         color=as.numeric(factor(color,levels = color.levels)),
         mostused=as.numeric(factor(mostused,levels = mostused.levels)),
         mostrecent=as.numeric(factor(mostrecent))) -> usedcars.num

usedcars.num %>%
  mutate(model2=model**2,
         year2=year**2) -> usedcars.num

fit <- lm(price ~ year + year2 + model + model2 + mileage + transmission + color + mostrecent,
          data=usedcars.num)
fit
summary(fit)
cor(usedcars$price,predict(fit,usedcars.num))

##### better #####

usedcars$price %>%
  tapply(usedcars$color,mean) %>%
  sort() %>%
  names() -> color.levels.mean

usedcars$price %>%
  tapply(usedcars$transmission,mean) %>%
  sort() %>%
  names() -> transmission.levels.mean

usedcars.num %>%
  mutate(color=as.numeric(factor(usedcars$color,levels = color.levels.mean)),
         transmission=as.numeric(factor(usedcars$transmission,
                                        levels = transmission.levels.mean))) -> usedcars.num
with(usedcars.num,cor(color,price))
with(usedcars.num,cor(transmission,price))

fit <- lm(price ~ year + year2 + model + mileage + transmission + color + mostrecent + mostused,
          data=usedcars.num)

fit
summary(fit)
cor(usedcars$price,predict(fit,usedcars.num))

#correletion matrix grid
cmgrid <- expand.grid(names(usedcars.num),names(usedcars.num),stringsAsFactors = FALSE)
cmgrid$cor <- 0
for (i in 1:nrow(cmgrid)) 
{
  cmgrid$cor[i] <- cor(usedcars.num[,cmgrid$Var1[i]],usedcars.num[,cmgrid$Var2[i]])
}
rm("i")

# plot cor matrix
cmgrid %>%
  mutate(Var1=factor(Var1,levels = colnames(usedcars.num)),
         Var2=factor(Var2,levels = rev(colnames(usedcars.num)))) %>%
  
  ggplot(aes(Var1,Var2)) + geom_raster(aes(fill=cor)) + theme_bw()+
  labs(title="Correlation Matrix",x="Features",y="Features",
       caption="Usedcars Data") +
  theme(axis.text.x = element_text(hjust = 0.5,vjust = 0.5,angle = 45)) +
  geom_text(aes(label=round(cor,digits = 2)),col="white") +
  scale_fill_gradient2(low = "red",breaks=seq(-1,1,len=5))