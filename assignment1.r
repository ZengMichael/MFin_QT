# load necessary libraries and files
require(feather)
require(data.table)
require(RcppRoll)
require(dplyr)
require(ggplot2)
require(lubridate)

setwd('F:\\study\\MFIN7037\\Assignment_1') # please set directory here
csv <- fread('homework_class_1.csv')


# define functions
end_of_month <- function(x){
     x <- as.Date(x)
     day(x) <- lubridate::days_in_month(x)        
     x %>% as.character
}
csv[,dt:=end_of_month(dt)]

apply_quantiles <- function(x,include_in_quantiles=NULL,bins=10){
    if(is.null(include_in_quantiles)){
        include_in_quantiles <- rep(TRUE,length(x))
        }
    quantiles <- quantile(ifelse(include_in_quantiles,x,NA),
        probs=seq(0,1,length.out=bins+1),na.rm=TRUE)
    quantiles['0%'] <- min(x,na.rm=TRUE)-1 ; quantiles['100%'] <- max(x,na.rm=TRUE)+1
    return(cut(x,breaks=quantiles,labels=FALSE))
}



# ============TASK ONE============
task_one <- csv[,list(ID, ret_t2_t12)] %>% na.omit
cor(task_one$ID, task_one$ret_t2_t12)
quantile(task_one$ID, c(.01, .05, .50, .95, .99))
quantile(task_one$ret_t2_t12, c(.01, .05, .50, .95, .99))
rm(task_one)

# ============TASK TWO============

# ------Vanilla------

# set bin
task_two <- csv[abs(prc)>5 & !is.na(prc_lag13) & !is.na(mcap_lag1) & !is.na(ret_lag2)]
task_two <- task_two[, count:=.N, by = permno]
task_two <- task_two[count >= 8]
task_two <- task_two[,mom_bin:=apply_quantiles(ret_t2_t12, bins=5)]
task_two <- task_two[,ID_bin:=apply_quantiles(ID_lag1, bins=5), by=mom_bin]
portfolio <- task_two[,list(portfolio_vw=sum(ret_this_month*mcap_lag1)/sum(mcap_lag1)),
    by=list(dt,mom_bin,ID_bin)]

# strategy one
portfolio1 <- merge(portfolio[mom_bin==5 & ID_bin==5], portfolio[mom_bin==1 & ID_bin==5],
    suffixes=c('_long','_short'),by='dt')
portfolio1[,strategy_vw:=portfolio_vw_long-portfolio_vw_short]
portfolio1 <- portfolio1 %>% arrange(dt) %>% setDT
portfolio1 <- na.omit(portfolio1)
portfolio1[,cumulative_return:=cumprod(1+strategy_vw)-1]
portfolio1[,log_cumulative_return:=ifelse(cumulative_return>0,log(cumulative_return),NA)]   
ggplot(portfolio1,aes(x=as.Date(dt),y=log_cumulative_return)) + geom_line() + ggtitle('Strategy One')

# strategy two
portfolio2 <- merge(portfolio[mom_bin==5 & ID_bin==1], portfolio[mom_bin==1 & ID_bin==1],
    suffixes=c('_long','_short'),by='dt')
portfolio2[,strategy_vw:=portfolio_vw_long-portfolio_vw_short]
portfolio2 <- portfolio2 %>% arrange(dt) %>% setDT
portfolio2[,cumulative_return:=cumprod(1+strategy_vw)-1]
portfolio2[,log_cumulative_return:=ifelse(cumulative_return>0,log(cumulative_return),NA)] 
ggplot(portfolio2,aes(x=as.Date(dt),y=log_cumulative_return)) + geom_line() + ggtitle('Strategy Two')

# mean, standard deviation, sharpe ratio
print('strategy one mean')
mean(portfolio1$strategy_vw,na.rm=TRUE)
print('strategy one sd')
sd(portfolio1$strategy_vw,na.rm=TRUE)
print('strategy one sharpe')
mean(portfolio1$strategy_vw,na.rm=TRUE)/sd(portfolio1$strategy_vw,na.rm=TRUE)*sqrt(12) # ignore risk free rate
print('strategy two mean')
mean(portfolio2$strategy_vw,na.rm=TRUE)
print('strategy two sd')
sd(portfolio2$strategy_vw,na.rm=TRUE)
print('strategy two sharpe')
mean(portfolio2$strategy_vw,na.rm=TRUE)/sd(portfolio2$strategy_vw,na.rm=TRUE)*sqrt(12) # ignore risk free rate

# add ff three factor
marketfactor <- read_feather('ff_three_factor') %>% setDT
monthly_market <- marketfactor[,list(mkt_rf=prod(1+mkt_rf)-1,
    hml=prod(1+HML)-1,
    smb=prod(1+SMB)-1,
    rf=prod(1+RF)-1
),by=list(dt=end_of_month(dt))]

portfolio1 <- merge(portfolio1,monthly_market,by='dt')
lm(data=portfolio1,strategy_vw ~ mkt_rf + hml +smb) %>% summary

portfolio2 <- merge(portfolio2,monthly_market,by='dt')
lm(data=portfolio2,strategy_vw ~ mkt_rf + hml +smb) %>% summary

# ------Event Study------

# create lead and lag values
task_two[,lead0:=ret_use]
for(i in seq(1,25)) {
    task_two[,eval(paste('lead',i,sep='')):=shift(lead0,type='lead',n=i,fill=NA),by=list(permno)]
    }
for(i in seq(1,4)) {
    task_two[,eval(paste('lag',i,sep='')):=shift(lead0,type='lag',n=i,fill=NA),by=list(permno)]
    }

# reshape
cols=paste('lead',sep='',seq(0,25))
melted <- task_two[!is.na(mcap_lag1) & !is.na(mom_bin) & !is.na(ID_bin),
    c('dt','permno','mcap_lag1','mom_bin','ID_bin',cols),with=FALSE] %>%
    melt.data.table(id.vars=c('permno','dt','mcap_lag1','mom_bin','ID_bin'))
melted[mom_bin == 5 & ID_bin == 5, type:='portfolio1_long']
melted[mom_bin == 1 & ID_bin == 5, type:='portfolio1_short']
melted[mom_bin == 5 & ID_bin == 1, type:='portfolio2_long']
melted[mom_bin == 1 & ID_bin == 1, type:='portfolio2_short']
melted <- melted[!is.na(type)]

# portfolio construction
melted <- melted[!is.na(value),list(value=sum(value*mcap_lag1)/sum(mcap_lag1)),
    by=list(dt,type,variable)]
melted[,cumulative:=cumprod(1+value)-1,by=list(dt,type)]
melted <- melted[,list(cumulative=mean(cumulative)),by=list(type,variable)]
strat1 <- melted[type %in% c('portfolio1_long','portfolio1_short'),] %>%
    dcast.data.table(variable ~ type) %>%
    mutate(spread=portfolio1_long-portfolio1_short) %>%
    mutate(horizon=gsub(variable,pattern='lead',replacement='') %>% as.integer)
ggplot(strat1,aes(x=horizon,y=spread)) + geom_line() + ggtitle('Strategy One')
strat2 <- melted[type %in% c('portfolio2_long','portfolio2_short'),] %>%
    dcast.data.table(variable ~ type) %>%
    mutate(spread=portfolio2_long-portfolio2_short) %>%
    mutate(horizon=gsub(variable,pattern='lead',replacement='') %>% as.integer)
ggplot(strat2,aes(x=horizon,y=spread)) + geom_line() + ggtitle('Strategy Two')

# ============TASK THREE============

# ------1------
# check whether pure momentum strategy is better in term of sharpe ratio
portfolio_alt <- task_two[,list(portfolio_vw=sum(ret_this_month*mcap_lag1)/sum(mcap_lag1)),
    by=list(dt,mom_bin)]
portfolio3 <- merge(portfolio_alt[mom_bin==5], portfolio_alt[mom_bin==1],suffixes=c('_long','_short'),by='dt')
portfolio3[,strategy_vw:=portfolio_vw_long-portfolio_vw_short]
portfolio3 <- portfolio3 %>% arrange(dt) %>% setDT
portfolio3[,cumulative_return:=cumprod(1+strategy_vw)-1]
portfolio3[,log_cumulative_return:=ifelse(cumulative_return>0,log(cumulative_return),NA)]
mean(portfolio3$strategy_vw,na.rm=TRUE)/sd(portfolio3$strategy_vw,na.rm=TRUE)*sqrt(12)
rm(portfolio_alt)
rm(portfolio3)

# ------3------
# run a regression with ff four factor
marketfactor_ff4 <- read_feather('ff_four_factor') %>% setDT
monthly_market_ff4 <- marketfactor_ff4[,list(mkt_rf=prod(1+mkt_rf)-1,
    hml=prod(1+HML)-1,
    smb=prod(1+SMB)-1,
    rf=prod(1+RF)-1,
    mom=prod(1+Mom)-1
),by=list(dt=end_of_month(dt))]

portfolio2_ff4 <- merge(portfolio[mom_bin==5 & ID_bin==1], portfolio[mom_bin==1 & ID_bin==1],
    suffixes=c('_long','_short'),by='dt')
portfolio2_ff4[,strategy_vw:=portfolio_vw_long-portfolio_vw_short]
portfolio2_ff4 <- portfolio2_ff4 %>% arrange(dt) %>% setDT
portfolio2_ff4[,cumulative_return:=cumprod(1+strategy_vw)-1]
portfolio2_ff4[,log_cumulative_return:=ifelse(cumulative_return>0,log(cumulative_return),NA)] 
portfolio2_ff4 <- merge(portfolio2_ff4,monthly_market_ff4,by='dt')
lm(data=portfolio2_ff4,strategy_vw ~ mkt_rf + hml + smb + mom) %>% summary
