# load necessary libraries and files
library(e1071)
require(ggplot2)
library(magrittr)
library(sandwich)
library(lmtest)
require(data.table)

setwd("~/workspace/r")
csv<-read.table('homework2_data.csv',header=TRUE, sep=',', colClasses =c("Date", "numeric", "numeric", "numeric","numeric","numeric","numeric"))

g_p <- csv$gold_last_month/csv$plat_last_month
log_g_p <- log(g_p)
mkt_rf <- c(tail(csv$mkt_rf, -1), NA)
vix <- csv$vix
trading_dt <- csv$trading_dt

# ============TASK ONE============
mean(g_p); sd(g_p); skewness(g_p)
mean(log_g_p); sd(log_g_p); skewness(log_g_p)
cor(g_p, vix, use="complete.obs")


# ============TASK TWO============

qplot(trading_dt, log_g_p) + geom_line() + ggtitle('log(G/P) ratio over time')

# ============TASK THREE============

lm(mkt_rf ~ log_g_p) %>% summary
print(paste(sd(log_g_p),'is the standard deviation of the log gold platinum ratio'))
print(paste(round(sd(log_g_p)*.014655*100,2),
            'is the percent change in market for a 1 standard deviation change'))

# ============TASK FOUR============
lm(mkt_rf[trading_dt<as.Date("1990-01-01")] ~ log_g_p[trading_dt<as.Date("1990-01-01")]) %>% summary
lm(mkt_rf[trading_dt>=as.Date("1990-01-01")] ~ log_g_p[trading_dt>=as.Date("1990-01-01")]) %>% summary


# ============EXTRA ONE============
l=lm(mkt_rf ~ log_g_p) 
lmtest::coeftest(l, vcov. = NeweyWest(l,3,prewhite = FALSE))


# ============EXTRA TWO============
keep=data.table()
N=120
date_range <- seq(N,length(mkt_rf)-2)
for(i in date_range){
  a=lm(mkt_rf[seq(i-N+1,i)] ~ log_g_p[seq(i-N+1,i)]) %>% summary
  
  b=data.table(dt=trading_dt[i],a$coefficients[c(2,4,6)] %>% t)
  colnames(b)=c('dt','coef','se','t')
  
  next_y=mkt_rf[i+1]
  predicted_x=coef(a)[1]+coef(a)[2]*log_g_p[i+1]
  error=abs(predicted_x-next_y)
  b <- cbind(b, error)

  keep=rbind(keep,b)
}
keep[,lower:=coef-se]
keep[,upper:=coef+se]

# Make sure it doesn't look too big
options(repr.plot.width=8, repr.plot.height=4.5)
ggplot(keep,aes(x=as.Date(dt),coef)) + geom_line(col='blue') + 
  geom_line(aes(x=as.Date(dt),upper,col='red')) + 
  geom_line(aes(x=as.Date(dt),lower,col='red')) +
  ggthemes::theme_tufte() + ggtitle('Coefficients')

SSE=sum(keep$error^2)
SST=sum((mkt_rf[date_range]-mean(mkt_rf[date_range]))^2)
print(paste('out of sample r-squared:',1-SSE/SST))

# ============EXTRA THREE============

new_data<-read.table('FF25*25data-cleaned.csv',header=TRUE, sep=',')
log_g_p_with_dt <- data.frame(format(trading_dt, "%Y%m"), log_g_p)
colnames(log_g_p_with_dt) <- c("X","log_g_p")
new_data <-  merge(new_data, log_g_p_with_dt, by="X")

name <- c()
ret <- c()
beta <- c()

for(i in names(new_data)){
  if (i != "X" && i != "log_g_p") {
    return <- c(tail(new_data[[i]], -1), NA)
    model <- lm(return ~ new_data[["log_g_p"]])
    beta <- c(beta, summary(model)$coef[2,"Estimate"])
    ret <- c(ret, mean(new_data[[i]]))
    name <- c(name, i)
    
  }
}

res = data.frame(name, ret, beta)
ggplot(res, aes(x=beta, y=ret, label=name)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_text(aes(label=name),hjust=0, vjust=0) +
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region
