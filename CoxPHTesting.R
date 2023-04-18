# Cox PH paper - heatwave clustering
library(survival)
library(data.table)
library(lubridate)
library(zoo)
library(gmodels)
library(survminer)

################################################################
################################################################
# first combine data sets from the temp folder
u1<-read.csv(file="G:/My Drive/Personal/Career/BOM/Data/Adelaide_MaxT.csv")
u2<-read.csv(file="G:/My Drive/Personal/Career/BOM/Data/Brisbane_MaxT.csv")
u3<-read.csv(file="G:/My Drive/Personal/Career/BOM/Data/Melbourne_MaxT.csv")
u4<-read.csv(file="G:/My Drive/Personal/Career/BOM/Data/Sydney_MaxT.csv")
r1<-read.csv(file="G:/My Drive/Personal/Career/BOM/Data/Amberley_MaxT.csv")
r2<-read.csv(file="G:/My Drive/Personal/Career/BOM/Data/MtGambier_MaxT.csv")
r3<-read.csv(file="G:/My Drive/Personal/Career/BOM/Data/Sale_MaxT.csv")
r4<-read.csv(file="G:/My Drive/Personal/Career/BOM/Data/Nowra_MaxT.csv")
u1$date <- as.Date(with(u1, paste(Year, Month, Day, sep="-")), "%Y-%m-%d")
u2$date <- as.Date(with(u2, paste(Year, Month, Day, sep="-")), "%Y-%m-%d")
u3$date <- as.Date(with(u3, paste(Year, Month, Day, sep="-")), "%Y-%m-%d")
u4$date <- as.Date(with(u4, paste(Year, Month, Day, sep="-")), "%Y-%m-%d")
r1$date <- as.Date(with(r1, paste(Year, Month, Day, sep="-")), "%Y-%m-%d")
r2$date <- as.Date(with(r2, paste(Year, Month, Day, sep="-")), "%Y-%m-%d")
r3$date <- as.Date(with(r3, paste(Year, Month, Day, sep="-")), "%Y-%m-%d")
r4$date <- as.Date(with(r4, paste(Year, Month, Day, sep="-")), "%Y-%m-%d")
u1<-as.data.table(u1)
u2<-as.data.table(u2)
u3<-as.data.table(u3)
u4<-as.data.table(u4)
r1<-as.data.table(r1)
r2<-as.data.table(r2)
r3<-as.data.table(r3)
r4<-as.data.table(r4)
u1<-u1[,c(9,6)]
u2<-u2[,c(9,6)]
u3<-u3[,c(9,6)]
u4<-u4[,c(9,6)]
r1<-r1[,c(9,6)]
r2<-r2[,c(9,6)]
r3<-r3[,c(9,6)]
r4<-r4[,c(9,6)]
setnames(u1,c('date','Maximum.temperature..Degree.C.'),c('date','u1'))
setnames(u2,c('date','Maximum.temperature..Degree.C.'),c('date','u2'))
setnames(u3,c('date','Maximum.temperature..Degree.C.'),c('date','u3'))
setnames(u4,c('date','Maximum.temperature..Degree.C.'),c('date','u4'))
setnames(r1,c('date','Maximum.temperature..Degree.C.'),c('date','r1'))
setnames(r2,c('date','Maximum.temperature..Degree.C.'),c('date','r2'))
setnames(r3,c('date','Maximum.temperature..Degree.C.'),c('date','r3'))
setnames(r4,c('date','Maximum.temperature..Degree.C.'),c('date','r4'))
x1<-merge.data.table(u1,u2,by="date",all=TRUE)
x2<-merge.data.table(x1,u3,by="date",all=TRUE)
x3<-merge.data.table(x2,u4,by="date",all=TRUE)
x4<-merge.data.table(x3,r1,by="date",all=TRUE)
x5<-merge.data.table(x4,r2,by="date",all=TRUE)
x6<-merge.data.table(x5,r3,by="date",all=TRUE)
x7<-merge.data.table(x6,r4,by="date",all=TRUE)
dat<-x7[date>"1955-12-31",]
summary(dat)

# carry forward data for missing obs
dat$u1<-na.locf(dat$u1, na.rm=FALSE, fromLast = TRUE)
dat$u2<-na.locf(dat$u2, na.rm=FALSE, fromLast = TRUE)
dat$u3<-na.locf(dat$u3, na.rm=FALSE, fromLast = TRUE)
dat$u4<-na.locf(dat$u4, na.rm=FALSE, fromLast = TRUE)
dat$r1<-na.locf(dat$r1, na.rm=FALSE, fromLast = TRUE)
dat$r2<-na.locf(dat$r2, na.rm=FALSE, fromLast = TRUE)
dat$r3<-na.locf(dat$r3, na.rm=FALSE, fromLast = TRUE)
dat$r4<-na.locf(dat$r4, na.rm=FALSE, fromLast = TRUE)

# Nominate exceedance thresholds (using a percentile)
p90.u<-quantile(dat[,c(u1,u2,u3,u4)],0.95,na.rm = TRUE) #95th perc
p90.u
p90.r<-quantile(dat[,c(r1,r2,r3,r4)],0.95,na.rm = TRUE) #95th perc
p90.r

qu1<-quantile(dat[date<"1980-01-01",]$u1,0.95,na.rm = TRUE) #95th perc
qu2<-quantile(dat[date<"1980-01-01",]$u2,0.95,na.rm = TRUE) #95th perc
qu3<-quantile(dat[date<"1980-01-01",]$u3,0.95,na.rm = TRUE) #95th perc
qu4<-quantile(dat[date<"1980-01-01",]$u4,0.95,na.rm = TRUE) #95th perc
qr1<-quantile(dat[date<"1980-01-01",]$r1,0.95,na.rm = TRUE) #95th perc
qr2<-quantile(dat[date<"1980-01-01",]$r2,0.95,na.rm = TRUE) #95th perc
qr3<-quantile(dat[date<"1980-01-01",]$r3,0.95,na.rm = TRUE) #95th perc
qr4<-quantile(dat[date<"1980-01-01",]$r4,0.95,na.rm = TRUE) #95th perc

#####################################################
## Heatwave calcs by location
dat<-dat[,hot.u1:=ifelse((u1>qu1),1,0)]
dat<-dat[,hot.u2:=ifelse((u2>qu2),1,0)]
dat<-dat[,hot.u3:=ifelse((u3>qu3),1,0)]
dat<-dat[,hot.u4:=ifelse((u4>qu4),1,0)]
dat<-dat[,hot.r1:=ifelse((r1>qr1),1,0)]
dat<-dat[,hot.r2:=ifelse((r2>qr2),1,0)]
dat<-dat[,hot.r3:=ifelse((r3>qr3),1,0)]
dat<-dat[,hot.r4:=ifelse((r4>qr4),1,0)]

# 3 day
dat <- dat %>%
  dplyr::mutate(heatw3.u1 = zoo::rollsumr(hot.u1, k = 3, fill = NA, align = "right"),
                heatw3.u2 = zoo::rollsumr(hot.u2, k = 3, fill = NA, align = "right"),
                heatw3.u3 = zoo::rollsumr(hot.u3, k = 3, fill = NA, align = "right"),
                heatw3.u4 = zoo::rollsumr(hot.u4, k = 3, fill = NA, align = "right"),
                heatw3.r1 = zoo::rollsumr(hot.r1, k = 3, fill = NA, align = "right"),
                heatw3.r2 = zoo::rollsumr(hot.r2, k = 3, fill = NA, align = "right"),
                heatw3.r3 = zoo::rollsumr(hot.r3, k = 3, fill = NA, align = "right"),
                heatw3.r4 = zoo::rollsumr(hot.r4, k = 3, fill = NA, align = "right"))

# Classify 3-day heatwaves
dat<-dat[,hw3d.u1:=ifelse(heatw3.u1>2,1,0)]
dat<-dat[,hw3d.u2:=ifelse(heatw3.u2>2,1,0)]
dat<-dat[,hw3d.u3:=ifelse(heatw3.u3>2,1,0)]
dat<-dat[,hw3d.u4:=ifelse(heatw3.u4>2,1,0)]
dat<-dat[,hw3d.r1:=ifelse(heatw3.r1>2,1,0)]
dat<-dat[,hw3d.r2:=ifelse(heatw3.r2>2,1,0)]
dat<-dat[,hw3d.r3:=ifelse(heatw3.r3>2,1,0)]
dat<-dat[,hw3d.r4:=ifelse(heatw3.r4>2,1,0)]

sum(dat$hw3d.u1,na.rm=TRUE)
sum(dat$hw3d.u2,na.rm=TRUE)
sum(dat$hw3d.u3,na.rm=TRUE)
sum(dat$hw3d.u4,na.rm=TRUE)
sum(dat$hw3d.r1,na.rm=TRUE)
sum(dat$hw3d.r2,na.rm=TRUE)
sum(dat$hw3d.r3,na.rm=TRUE)
sum(dat$hw3d.r4,na.rm=TRUE)

# 4 day
dat <- dat %>%
  dplyr::mutate(heatw4.u1 = zoo::rollsumr(hot.u1, k = 4, fill = NA, align = "right"),
                heatw4.u2 = zoo::rollsumr(hot.u2, k = 4, fill = NA, align = "right"),
                heatw4.u3 = zoo::rollsumr(hot.u3, k = 4, fill = NA, align = "right"),
                heatw4.u4 = zoo::rollsumr(hot.u4, k = 4, fill = NA, align = "right"),
                heatw4.r1 = zoo::rollsumr(hot.r1, k = 4, fill = NA, align = "right"),
                heatw4.r2 = zoo::rollsumr(hot.r2, k = 4, fill = NA, align = "right"),
                heatw4.r3 = zoo::rollsumr(hot.r3, k = 4, fill = NA, align = "right"),
                heatw4.r4 = zoo::rollsumr(hot.r4, k = 4, fill = NA, align = "right"))

# Classify 4-day heatwaves
dat<-dat[,hw4d.u1:=ifelse(heatw4.u1>3,1,0)]
dat<-dat[,hw4d.u2:=ifelse(heatw4.u2>3,1,0)]
dat<-dat[,hw4d.u3:=ifelse(heatw4.u3>3,1,0)]
dat<-dat[,hw4d.u4:=ifelse(heatw4.u4>3,1,0)]
dat<-dat[,hw4d.r1:=ifelse(heatw4.r1>3,1,0)]
dat<-dat[,hw4d.r2:=ifelse(heatw4.r2>3,1,0)]
dat<-dat[,hw4d.r3:=ifelse(heatw4.r3>3,1,0)]
dat<-dat[,hw4d.r4:=ifelse(heatw4.r4>3,1,0)]

sum(dat$hw4d.u1,na.rm=TRUE)
sum(dat$hw4d.u2,na.rm=TRUE)
sum(dat$hw4d.u3,na.rm=TRUE)
sum(dat$hw4d.u4,na.rm=TRUE)
sum(dat$hw4d.r1,na.rm=TRUE)
sum(dat$hw4d.r2,na.rm=TRUE)
sum(dat$hw4d.r3,na.rm=TRUE)
sum(dat$hw4d.r4,na.rm=TRUE)

# 5 day
dat <- dat %>%
  dplyr::mutate(heatw5.u1 = zoo::rollsumr(hot.u1, k = 5, fill = NA, align = "right"),
                heatw5.u2 = zoo::rollsumr(hot.u2, k = 5, fill = NA, align = "right"),
                heatw5.u3 = zoo::rollsumr(hot.u3, k = 5, fill = NA, align = "right"),
                heatw5.u4 = zoo::rollsumr(hot.u4, k = 5, fill = NA, align = "right"),
                heatw5.r1 = zoo::rollsumr(hot.r1, k = 5, fill = NA, align = "right"),
                heatw5.r2 = zoo::rollsumr(hot.r2, k = 5, fill = NA, align = "right"),
                heatw5.r3 = zoo::rollsumr(hot.r3, k = 5, fill = NA, align = "right"),
                heatw5.r4 = zoo::rollsumr(hot.r4, k = 5, fill = NA, align = "right"))

# Classify 5-day heatwaves
dat<-dat[,hw5d.u1:=ifelse(heatw5.u1>4,1,0)]
dat<-dat[,hw5d.u2:=ifelse(heatw5.u2>4,1,0)]
dat<-dat[,hw5d.u3:=ifelse(heatw5.u3>4,1,0)]
dat<-dat[,hw5d.u4:=ifelse(heatw5.u4>4,1,0)]
dat<-dat[,hw5d.r1:=ifelse(heatw5.r1>4,1,0)]
dat<-dat[,hw5d.r2:=ifelse(heatw5.r2>4,1,0)]
dat<-dat[,hw5d.r3:=ifelse(heatw5.r3>4,1,0)]
dat<-dat[,hw5d.r4:=ifelse(heatw5.r4>4,1,0)]

sum(dat$hw5d.u1,na.rm=TRUE)
sum(dat$hw5d.u2,na.rm=TRUE)
sum(dat$hw5d.u3,na.rm=TRUE)
sum(dat$hw5d.u4,na.rm=TRUE)
sum(dat$hw5d.r1,na.rm=TRUE)
sum(dat$hw5d.r2,na.rm=TRUE)
sum(dat$hw5d.r3,na.rm=TRUE)
sum(dat$hw5d.r4,na.rm=TRUE)

dat<-dat[,hot3.u:=ifelse((hw3d.u1==1|hw3d.u2==1|hw3d.u3==1|hw3d.u4==1),1,0)]
dat<-dat[,hot3.r:=ifelse((hw3d.r1==1|hw3d.r2==1|hw3d.r3==1|hw3d.r4==1),1,0)]
sum(dat$hot3.u, na.rm = TRUE) # urban>95th perc
sum(dat$hot3.r, na.rm = TRUE) # rural>95th perc

dat<-dat[,hot4.u:=ifelse((hw4d.u1==1|hw4d.u2==1|hw4d.u3==1|hw4d.u4==1),1,0)]
dat<-dat[,hot4.r:=ifelse((hw4d.r1==1|hw4d.r2==1|hw4d.r3==1|hw4d.r4==1),1,0)]
sum(dat$hot4.u, na.rm = TRUE) # urban>95th perc
sum(dat$hot4.r, na.rm = TRUE) # rural>95th perc

dat<-dat[,hot5.u:=ifelse((hw5d.u1==1|hw5d.u2==1|hw5d.u3==1|hw5d.u4==1),1,0)]
dat<-dat[,hot5.r:=ifelse((hw5d.r1==1|hw5d.r2==1|hw5d.r3==1|hw5d.r4==1),1,0)]
sum(dat$hot5.u, na.rm = TRUE) # urban>95th perc
sum(dat$hot5.r, na.rm = TRUE) # rural>95th perc

# Remove all temp data, just keep heatwave flags
dat<-dat[,-c(2:65)]

# load enso data
enso.raw<-read.csv(file="G:/My Drive/Personal/Career/BOM/Data/enso_anom_mthly.csv")
enso.raw$date <- as.Date(with(enso.raw, paste(Year, Month, "01", sep="-")), "%Y-%m-%d")
dat.tot<-merge.data.table(dat,enso.raw,by="date",all=TRUE)
dat.tot<-dat.tot[date>"1955-12-31",]
dat.tot$Year<-year(dat.tot$date)
dat.tot$Month<-month(dat.tot$date)
dat.tot$TOTAL<-na.locf(dat.tot$TOTAL, na.rm=FALSE, fromLast = FALSE)
dat.tot$ClimAdju<-na.locf(dat.tot$ClimAdju, na.rm=FALSE, fromLast = FALSE)
dat.tot$stANOM<-na.locf(dat.tot$stANOM, na.rm=FALSE, fromLast = FALSE)
dat<-dat.tot
dat$index <- 1:nrow(dat)
str(dat)

# load iod data
iod.raw<-read.csv(file="G:/My Drive/Personal/Career/BOM/Data/iod.csv")
iod.raw$date <- as.Date(with(iod.raw, paste(Year, Month, "01", sep="-")), "%Y-%m-%d")
dat.tot<-merge.data.table(dat,iod.raw,by="date",all=TRUE)
dat.tot<-dat.tot[date>"1955-12-31",]
dat.tot$Year<-year(dat.tot$date)
dat.tot$Month<-month(dat.tot$date)
dat.tot$iod<-na.locf(dat.tot$iod, na.rm=FALSE, fromLast = FALSE)
dat<-dat.tot
dat$index <- 1:nrow(dat)
str(dat)

# reformat data for SVA
dat<-dat[,daynum:=yday(date)] # set "time" as days since start of each year
dat<-dat[,time:=ifelse(daynum>300,daynum-300,daynum+65)] # set "time" as days since start of each year
str(dat$time)

#########################################################
# split data into two: urban vs rural###
dat.u<-dat[,-c(3,5,7)]
dat.r<-dat[,-c(2,4,6)]
dat.u<-dat.u[,urb:=1]
dat.r<-dat.r[,urb:=0]
dat.sva<-rbind(dat.u,dat.r,fill=TRUE)
dat.sva$date<-ymd(dat.sva$date)

df<-dat.sva[order(date)] # order by date
df$index <- 1:nrow(df) # attach an index to each row
df<-df[,epoch:=ifelse(date<"1980-01-01",0,1)] # set epoch pre 1980 / post 1980
df<-df[,enso:=ifelse(stANOM>0.25,1,ifelse(stANOM<(-0.25),-1,0))] # set enso + = el nino, - = la nina
df<-df[,iodipole:=ifelse(iod>0.25,1,ifelse(iod<0,-1,0))] # set iod + = dry, - = wet
df<-df[,pos.enso.iod:=ifelse((iod>0 & stANOM>0),"both_pos",
                             ifelse((iod<0 & stANOM<0),"both_neg","neutral"))]
df$stANOM.iod<-df$stANOM*df$iod
df$enso<-as.factor(df$enso)
df$iodipole<-as.factor(df$iodipole)
df$pos.enso.iod<-as.factor(df$pos.enso.iod)

df$hot.3d<-ifelse(is.na(df$hot3.u)==FALSE,df$hot3.u,df$hot3.r) # assign heatwave
df$hot.4d<-ifelse(is.na(df$hot4.u)==FALSE,df$hot4.u,df$hot4.r) # assign heatwave
df$hot.5d<-ifelse(is.na(df$hot5.u)==FALSE,df$hot5.u,df$hot5.r) # assign heatwave

#df<-df[hot.r==1 | hot.u==1,]
summary(df)

#########################################################################
# FREQUENCY TABLES
f.epoch<-df[,.(mean(hot3.u,na.rm=T),mean(hot4.u,na.rm=T),mean(hot5.u,na.rm=T),
               mean(hot3.r,na.rm=T),mean(hot4.r,na.rm=T),mean(hot5.r,na.rm=T)),by=.(epoch)]
f.epoch

f.pos.both<-df[,.(mean(hot3.u,na.rm=T),mean(hot4.u,na.rm=T),mean(hot5.u,na.rm=T),
               mean(hot3.r,na.rm=T),mean(hot4.r,na.rm=T),mean(hot5.r,na.rm=T)),by=.(pos.enso.iod)]
f.pos.both

f.iod<-df[,.(mean(hot3.u,na.rm=T),mean(hot4.u,na.rm=T),mean(hot5.u,na.rm=T),
                 mean(hot3.r,na.rm=T),mean(hot4.r,na.rm=T),mean(hot5.r,na.rm=T)),by=.(iodipole)]
f.iod

f.enso<-df[,.(mean(hot3.u,na.rm=T),mean(hot4.u,na.rm=T),mean(hot5.u,na.rm=T),
                 mean(hot3.r,na.rm=T),mean(hot4.r,na.rm=T),mean(hot5.r,na.rm=T)),by=.(enso)]
f.enso


#########################################################################
# curtail data to record heatwaves only
dfcurt3<-df[(hot3.u==1 | hot3.r==1),]
dfcurt4<-df[(hot4.u==1 | hot4.r==1),]
dfcurt5<-df[(hot5.u==1 | hot5.r==1),]

# Cox Prop Hazards Model
# 3 DAY HEATWAVES
cox3d <- coxph(Surv(time,hot.3d)~urb+stANOM+iod+epoch,data=df,na.action=na.exclude)
print(summary(cox3d))
survdiff(Surv(time,hot.3d)~urb,data=df)

cox3d <- coxph(Surv(time,hot.3d)~urb+stANOM+iod+stANOM.iod,data=df,na.action=na.exclude)
print(summary(cox3d))
survdiff(Surv(time,hot.3d)~urb,data=df)
survdiff(Surv(time,hot.3d)~epoch,data=df)

##########################################################
# with(surv.3d, head(data.frame(time, cum_haz=-log(surv)), n=4))
# plot(surv.3d, fun="cumhaz", col=c("blue", "red"),
#      main=expression(paste("Urban vs Rural 3-day heatwaves")),
#      ylab="cumulative hazard", lwd=2)
# legend(x="topleft", lwd=2, col=c("blue", "red"),
#        legend=c("Urban", "Rural"))
###########################################################

surv.3d <- survfit(Surv(time,hot.3d)~urb,data=dfcurt3) #+stANOM+epoch
summary(surv.3d)$table
# survival plot
ggsurvplot(surv.3d,pval=TRUE,conf.int=TRUE,risk.table=TRUE,
           risk.table.col="strata",linetype="strata",
           surv.median.line="hv",ggtheme=theme_bw(),
           size=0.5,palette=c("#E7B800","#2E9FDF"),fun="pct",
           legend.title = "Location",legend.labs = c("Rural","Urban"),
           censor.shape=".",xlim=c(0,200)) #surv1
# hazard plot
ggsurvplot(surv.3d,pval=TRUE,conf.int=TRUE,risk.table=TRUE,
           risk.table.col="strata",linetype="strata",
           surv.median.line="hv",ggtheme=theme_bw(),
           size=0.5,palette=c("#E7B800","#2E9FDF"),fun="cumhaz",
           legend.title = "Location",legend.labs = c("Rural","Urban"),
           censor.shape=".")

surv.3d <- survfit(Surv(time,hot.3d)~epoch,data=dfcurt3) #+urb+epoch
summary(surv.3d)$table
# survival plot
ggsurvplot(surv.3d,pval=TRUE,conf.int=TRUE,risk.table=TRUE,
           risk.table.col="strata",linetype="strata",
           surv.median.line="hv",ggtheme=theme_bw(),
           size=0.5,palette=c("#E7B800","#2E9FDF"),fun="pct",
           legend.title = "Epoch",legend.labs = c("Pre-1980","Post-1980"),
           censor.shape=".",xlim=c(0,200)) #surv2
# hazard plot
ggsurvplot(surv.3d,pval=TRUE,conf.int=TRUE,risk.table=TRUE,
           risk.table.col="strata",linetype="strata",
           surv.median.line="hv",ggtheme=theme_bw(),
           size=0.5,palette=c("#E7B800","#2E9FDF"),fun="cumhaz",
           legend.title = "Epoch",legend.labs = c("Pre-1980","Post-1980"),
           censor.shape=".")

surv.3d <- survfit(Surv(time,hot.3d)~enso,data=dfcurt3) #+urb+epoch
summary(surv.3d)$table
# survival plot
ggsurvplot(surv.3d,pval=TRUE,conf.int=TRUE,risk.table=TRUE,
           risk.table.col="strata",linetype="strata",
           surv.median.line="hv",ggtheme=theme_bw(),
           size=0.5,palette=c("#E7B800","black","#2E9FDF"),fun="pct",
           legend.title = "ENSO",legend.labs = c("La Nina","Neutral","El Nino"),
           censor.shape=".",xlim=c(0,200))#surv3
# hazard plot
ggsurvplot(surv.3d,pval=TRUE,conf.int=TRUE,risk.table=TRUE,
           risk.table.col="strata",linetype="strata",
           surv.median.line="hv",ggtheme=theme_bw(),
           size=0.5,palette=c("#E7B800","#2E9FDF"),fun="cumhaz",
           legend.title = "ENSO",legend.labs = c("La Nina","El Nino"),
           censor.shape=".")

surv.3d <- survfit(Surv(time,hot.3d)~iodipole,data=dfcurt3) #+urb+epoch
summary(surv.3d)$table
# survival plot
ggsurvplot(surv.3d,pval=TRUE,conf.int=TRUE,risk.table=TRUE,
           risk.table.col="strata",linetype="strata",
           surv.median.line="hv",ggtheme=theme_bw(),
           size=0.5,palette=c("#E7B800","black","#2E9FDF"),fun="pct",
           legend.title = "IOD",legend.labs = c("neg IOD","neutral","pos IOD"),
           censor.shape=".",xlim=c(0,200)) #surv4
# hazard plot
ggsurvplot(surv.3d,pval=TRUE,conf.int=TRUE,risk.table=TRUE,
           risk.table.col="strata",linetype="strata",
           surv.median.line="hv",ggtheme=theme_bw(),
           size=0.5,palette=c("#E7B800","black","#2E9FDF"),fun="cumhaz",
           legend.title = "IOD",legend.labs = c("neg IOD","neutral","pos IOD"),
           censor.shape=".")

surv.3d <- survfit(Surv(time,hot.3d)~enso.iod,data=dfcurt3) #+urb+epoch
summary(surv.3d)$table
# survival plot
ggsurvplot(surv.3d,pval=TRUE,conf.int=TRUE,risk.table=TRUE,
           risk.table.col="strata",linetype="strata",
           surv.median.line="hv",ggtheme=theme_bw(),
           size=0.5,palette=c("#E7B800","#2E9FDF","black"),fun="pct",
           legend.title = "ENSO & IOD",legend.labs = c("neg IOD & ENSO","pos IOD & ENSO","neutral"),
           censor.shape=".",xlim=c(0,200)) #surv5
# hazard plot
ggsurvplot(surv.3d,pval=TRUE,conf.int=TRUE,risk.table=TRUE,
           risk.table.col="strata",linetype="strata",
           surv.median.line="hv",ggtheme=theme_bw(),
           size=0.5,palette=c("#E7B800","#2E9FDF","black"),fun="cumhaz",
           legend.title = "ENSO & IOD",legend.labs = c("neg IOD & ENSO","pos IOD & ENSO","neutral"),
           censor.shape=".")


# 4 DAY HEATWAVES
cox4d <- coxph(Surv(time,hot.4d)~urb+stANOM+iod+stANOM.iod,data=df,na.action=na.exclude)
print(summary(cox4d))
survdiff(Surv(time,hot.4d)~urb,data=df)

surv.4d <- survfit(Surv(time,hot.4d)~urb,data=dfcurt4) #+stANOM+epoch
summary(surv.4d)$table
# survival plot
ggsurvplot(surv.4d,pval=TRUE,conf.int=TRUE,risk.table=TRUE,
           risk.table.col="strata",linetype="strata",
           surv.median.line="hv",ggtheme=theme_bw(),
           size=0.5,palette=c("#E7B800","#2E9FDF"),fun="pct",
           legend.title = "Location",legend.labs = c("Rural","Urban"),
           censor.shape=".")
# hazard plot
ggsurvplot(surv.4d,pval=TRUE,conf.int=TRUE,risk.table=TRUE,
           risk.table.col="strata",linetype="strata",
           surv.median.line="hv",ggtheme=theme_bw(),
           size=0.5,palette=c("#E7B800","#2E9FDF"),fun="cumhaz",
           legend.title = "ENSO",legend.labs = c("La Nina","El Nino"),
           censor.shape=".")


surv.4d <- survfit(Surv(time,hot.4d)~epoch,data=dfcurt4) #+urb+epoch
summary(surv.4d)$table
# survival plot
ggsurvplot(surv.4d,pval=TRUE,conf.int=TRUE,risk.table=TRUE,
           risk.table.col="strata",linetype="strata",
           surv.median.line="hv",ggtheme=theme_bw(),
           size=0.5,palette=c("#E7B800","#2E9FDF"),fun="pct",
           legend.title = "Epoch",legend.labs = c("Pre-1980","Post-1980"),
           censor.shape=".")
# hazard plot
ggsurvplot(surv.4d,pval=TRUE,conf.int=TRUE,risk.table=TRUE,
           risk.table.col="strata",linetype="strata",
           surv.median.line="hv",ggtheme=theme_bw(),
           size=0.5,palette=c("#E7B800","#2E9FDF"),fun="cumhaz",
           legend.title = "ENSO",legend.labs = c("La Nina","El Nino"),
           censor.shape=".")

surv.4d <- survfit(Surv(time,hot.4d)~iodipole,data=dfcurt4) #+urb+epoch
summary(surv.4d)$table
# survival plot
ggsurvplot(surv.4d,pval=TRUE,conf.int=TRUE,risk.table=TRUE,
           risk.table.col="strata",linetype="strata",
           surv.median.line="hv",ggtheme=theme_bw(),
           size=0.5,palette=c("#E7B800","black","#2E9FDF"),fun="pct",
           legend.title = "IOD",legend.labs = c("neg IOD","neutral","pos IOD"),
           censor.shape=".",xlim=c(0,200)) #surv9

surv.4d <- survfit(Surv(time,hot.4d)~enso,data=dfcurt4) #+urb+epoch
summary(surv.4d)$table
# survival plot
ggsurvplot(surv.4d,pval=TRUE,conf.int=TRUE,risk.table=TRUE,
           risk.table.col="strata",linetype="strata",
           surv.median.line="hv",ggtheme=theme_bw(),
           size=0.5,palette=c("#E7B800","#2E9FDF"),fun="pct",
           legend.title = "ENSO",legend.labs = c("La Nina","El Nino"),
           censor.shape=".")
# hazard plot
ggsurvplot(surv.4d,pval=TRUE,conf.int=TRUE,risk.table=TRUE,
           risk.table.col="strata",linetype="strata",
           surv.median.line="hv",ggtheme=theme_bw(),
           size=0.5,palette=c("#E7B800","#2E9FDF"),fun="cumhaz",
           legend.title = "ENSO",legend.labs = c("La Nina","El Nino"),
           censor.shape=".")

# 5 DAY HEATWAVES
cox5d <- coxph(Surv(time,hot.5d)~urb+stANOM+iod+stANOM.iod,data=df,na.action=na.exclude)
print(summary(cox5d))
survdiff(Surv(time,hot.5d)~urb,data=df)

surv.5d <- survfit(Surv(time,hot.5d)~urb,data=dfcurt5) #+stANOM+epoch
summary(surv.5d)$table
#survival plot
ggsurvplot(surv.5d,pval=TRUE,conf.int=TRUE,risk.table=TRUE,
           risk.table.col="strata",linetype="strata",
           surv.median.line="hv",ggtheme=theme_bw(),
           size=0.5,palette=c("#E7B800","#2E9FDF"),fun="pct",
           legend.title = "Location",legend.labs = c("Rural","Urban"),
           censor.shape=".") #surv6

#hazard plot
ggsurvplot(surv.5d,pval=TRUE,conf.int=TRUE,risk.table=TRUE,
           risk.table.col="strata",linetype="strata",
           surv.median.line="hv",ggtheme=theme_bw(),
           size=0.5,palette=c("#E7B800","#2E9FDF"),fun="cumhaz",
           legend.title = "Epoch",legend.labs = c("Rural","Urban"),
           censor.shape=".")


surv.5d <- survfit(Surv(time,hot.5d)~epoch,data=dfcurt5) #+urb+epoch
summary(surv.5d)$table
#survival plot
ggsurvplot(surv.5d,pval=TRUE,conf.int=TRUE,risk.table=TRUE,
           risk.table.col="strata",linetype="strata",
           surv.median.line="hv",ggtheme=theme_bw(),
           size=0.5,palette=c("#E7B800","#2E9FDF"),fun="pct",
           legend.title = "Epoch",legend.labs = c("Pre-1980","Post-1980"),
           censor.shape=".") #surv7

#hazard plot
ggsurvplot(surv.5d,pval=TRUE,conf.int=TRUE,risk.table=TRUE,
           risk.table.col="strata",linetype="strata",
           surv.median.line="hv",ggtheme=theme_bw(),
           size=0.5,palette=c("#E7B800","#2E9FDF"),fun="cumhaz",
           legend.title = "Epoch",legend.labs = c("Pre-1980","Post-1980"),
           censor.shape=".")


surv.5d <- survfit(Surv(time,hot.5d)~enso,data=dfcurt5) #+urb+epoch
summary(surv.5d)$table
#survival plot
ggsurvplot(surv.5d,pval=TRUE,conf.int=TRUE,risk.table=TRUE,
           risk.table.col="strata",linetype="strata",
           surv.median.line="hv",ggtheme=theme_bw(),
           size=0.5,palette=c("#E7B800","black","#2E9FDF"),fun="pct",
           legend.title = "ENSO",legend.labs = c("La Nina","Neutral","El Nino"),
           censor.shape=".") #surv8

#hazard plot
ggsurvplot(surv.5d,pval=TRUE,conf.int=TRUE,risk.table=TRUE,
           risk.table.col="strata",linetype="strata",
           surv.median.line="hv",ggtheme=theme_bw(),
           size=0.5,palette=c("#E7B800","#2E9FDF"),fun="cumhaz",
           legend.title = "ENSO",legend.labs = c("La Nina","El Nino"),
           censor.shape=".")

surv.5d <- survfit(Surv(time,hot.5d)~iodipole,data=dfcurt5) #+urb+epoch
summary(surv.5d)$table
# survival plot
ggsurvplot(surv.5d,pval=TRUE,conf.int=TRUE,risk.table=TRUE,
           risk.table.col="strata",linetype="strata",
           surv.median.line="hv",ggtheme=theme_bw(),
           size=0.5,palette=c("#E7B800","black","#2E9FDF"),fun="pct",
           legend.title = "IOD",legend.labs = c("neg IOD","neutral","pos IOD"),
           censor.shape=".",xlim=c(0,200)) #surv9
# hazard plot
ggsurvplot(surv.5d,pval=TRUE,conf.int=TRUE,risk.table=TRUE,
           risk.table.col="strata",linetype="strata",
           surv.median.line="hv",ggtheme=theme_bw(),
           size=0.5,palette=c("#E7B800","#2E9FDF"),fun="cumhaz",
           legend.title = "IOD",legend.labs = c("neg IOD","pos IOD"),
           censor.shape=".")

surv.5d <- survfit(Surv(time,hot.5d)~pos.enso.iod,data=dfcurt5) #+urb+epoch
summary(surv.5d)$table
# survival plot
ggsurvplot(surv.5d,pval=TRUE,conf.int=TRUE,risk.table=TRUE,
           risk.table.col="strata",linetype="strata",
           surv.median.line="hv",ggtheme=theme_bw(),
           size=0.5,palette=c("#E7B800","#2E9FDF","black"),fun="pct",
           legend.title = "ENSO & IOD",legend.labs = c("neg IOD & ENSO","pos IOD & ENSO","neutral"),
           censor.shape=".",xlim=c(0,200)) #surv10
# hazard plot
ggsurvplot(surv.5d,pval=TRUE,conf.int=TRUE,risk.table=TRUE,
           risk.table.col="strata",linetype="strata",
           surv.median.line="hv",ggtheme=theme_bw(),
           size=0.5,palette=c("#E7B800","#2E9FDF","black"),fun="cumhaz",
           legend.title = "ENSO & IOD",legend.labs = c("neg IOD & ENSO","pos IOD & ENSO","neutral"),
           censor.shape=".")

### FOR TABLES
library(broom)
out <- tidy(cox3d)
out
out<-as.data.table(out)
write.csv(out,'G:/My Drive/Personal/Career/BOM/Papers/CoxPH/cox3d.csv')
exp(coef(cox3d)) # exponentiated coefficients
exp(confint(cox3d)) # 95% CI for exponentiated coefficients

out <- tidy(cox4d)
out
out<-as.data.table(out)
write.csv(out,'G:/My Drive/Personal/Career/BOM/Papers/CoxPH/cox4d.csv')
exp(coef(cox4d)) # exponentiated coefficients
exp(confint(cox4d)) # 95% CI for exponentiated coefficients

out <- tidy(cox5d)
out
out<-as.data.table(out)
write.csv(out,'G:/My Drive/Personal/Career/BOM/Papers/CoxPH/cox5d.csv')
exp(coef(cox5d)) # exponentiated coefficients
exp(confint(cox5d)) # 95% CI for exponentiated coefficients

##### MAJOR PLOT
df.master<-df[date>"1999-12-31",c("date","hot3.u","hot3.r","stANOM","iod","urb","daynum","Year","Month")]
df.wide<-df.master[,.(sum(hot3.u,na.rm=T),sum(hot3.r,na.rm=T),mean(stANOM,na.rm=T),mean(iod,na.rm=T)),by=.(Year,Month)]
df.wide<-setNames(df.wide,c("Year","Month","Urban","Rural","ENSO","IOD"))
df.wide$date<-as.Date(with(df.wide, paste(Year, Month, "01", sep="-")), "%Y-%m-%d")
df.wide<-df.wide[,-c(1:2)]
df.long<-melt.data.table(df.wide,id.vars = c("date"))
df.long<-na.omit(df.long)

df.yrs<-df[date>"1999-12-31",.(date,Year,Month,pos.enso.iod)]#)
df.yrs<-df.yrs[pos.enso.iod=="both_pos"]

shade <- data.table("year" = c(2003,2003,2004,2009,2014,2019), 
                      "from" = c("2002-11-01","2003-09-01","2004-09-01","2009-05-01","2014-09-01","2019-11-01"), 
                      "to" = c("2003-03-31","2003-11-30","2005-05-30","2010-06-01","2016-06-01","2020-04-20"))  # Create data.table
shade$from<-as.Date(shade$from)
shade$to<-as.Date(shade$to)

# closest to fixed below
ggplot()+ 
  geom_col(data=subset(df.long,variable=="Urban"),aes(date,value))+
  geom_col(data=subset(df.long,variable=="Rural"),aes(date,value))+
  geom_line(data=subset(df.long,variable=="ENSO"),aes(date,value))+
  geom_line(data=subset(df.long,variable=="IOD"),aes(date,value))+
  geom_ribbon(data=subset(df.long,variable=="ENSO"),aes(x=date,ymin=0,ymax=value),fill="green")+
  geom_ribbon(data=subset(df.long,variable=="IOD"),aes(x=date,ymin=0,ymax=value),fill="blue")+
  geom_rect(data = shade, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf),fill = alpha("#F8766D", .3)) +
  facet_grid(variable~.,scales="free_y")
  #geom_ribbon(data=subset(df.long[(df.long$variable=="ONI"&df.long$value>0),],variable=="ONI"),outline.type="both",aes(x=date,ymin=0,ymax=pmax(0,value)),fill="red") +
  #geom_ribbon(data=subset(df.long[(df.long$variable=="ONI"&df.long$value<0),],variable=="ONI"),outline.type="both",aes(x=date,ymin=pmin(0,value),ymax=0),fill="green") +
  #geom_path()+
  #geom_ribbon(data=subset(df.long[(df.long$variable=="IOD"&df.long$value>0),],variable=="IOD"),aes(x=date,ymin=0,ymax=pmax(0,value)),fill="lightblue") +
  #geom_ribbon(data=subset(df.long[(df.long$variable=="IOD"&df.long$value<0),],variable=="IOD"),aes(x=date,ymin=pmin(0,value),ymax=0),fill="blue") +
  #geom_path()+

ggplot(data=df.long,aes(x=date,y=value)) +
  geom_line(aes(color=variable)) +
  facet_grid(variable~.) +
  theme(legend.position="bottom") +
  labs(x="Date")

# Forest plots
# First add interaction term for graphical forest plots
df$enso.iod<-df$stANOM*df$iod
m1 <- coxph(Surv(time, hot.3d)~urb+stANOM+iod+enso.iod,data=df,na.action=na.exclude)
print(summary(m1))
ggforest(m1, data = df, main="3-day heatwave HR by parameter (Urban, stANOM, IOD, epoch)", fontsize=0.75)
m2 <- coxph(Surv(time, hot.4d)~urb+stANOM+iod+enso.iod,data=df,na.action=na.exclude)
print(summary(m2))
ggforest(m2, data = df, main="4-day heatwave HR by parameter (Urban, stANOM, IOD, epoch)", fontsize=0.75)
m3 <- coxph(Surv(time, hot.5d)~urb+stANOM+iod+enso.iod,data=df,na.action=na.exclude)
print(summary(m3))
ggforest(m3, data = df, main="5-day heatwave HR by parameter (Urban, stANOM, IOD, epoch)", fontsize=0.75)

#### DIAGNOSTICS
# LL test diagnostics - add in interaction term and test
cox3d <- coxph(Surv(time,hot.3d)~urb+stANOM+iod,data=df,na.action=na.exclude)#+iod+epoch
cox3d_1 <- coxph(Surv(time,hot.3d)~urb+stANOM+iod+enso.iod,data=df,na.action=na.exclude)#+iod+epoch
anova(cox3d, cox3d_1)
cox3d_1$loglik
chi <- 2 * cox3d_1$loglik[2] - 2 * cox3d$loglik[2]
pvalue <- 1 - pchisq(chi, df = 1) # df = 3 - 2
pvalue # difference is significant

######################################################
## Schoenfeld residuals
## First convert to monthly data set
# split data into two: urban vs rural###
dat.u<-dat[,.(sum(hot3.u),sum(hot3.r),mean(stANOM),mean(iod)),by=.(Year,Month)]
dat.r<-dat[,.(sum(hot3.u),sum(hot3.r),mean(stANOM),mean(iod)),by=.(Year,Month)]
dat.u<-dat.u[,urb:=1]
dat.r<-dat.r[,urb:=0]
dat.sva<-rbind(dat.u,dat.r,fill=TRUE)
dat.sva$date<-as.Date(with(dat.sva, paste(Year, Month, "01", sep="-")), "%Y-%m-%d")
dat.sva<-setNames(dat.sva,c("Year","Month","hot3.u","hot3.r","stANOM","iod","urb","date"))
df<-dat.sva[order(date)] # order by date
str(df)
df<-df[,epoch:=ifelse(date<"1980-01-01",0,1)] # set epoch pre 1980 / post 1980
df<-df[,enso:=ifelse(stANOM>0,1,0)] # set enso + = el nino, - = la nina
df<-df[,iodipole:=ifelse(iod>0,1,0)] # set iod + = dry, - = wet
df<-df[,pos.enso.iod:=ifelse((iod>0 & stANOM>0),"both_pos",
                             ifelse((iod<0 & stANOM<0),"both_neg","neutral"))]
df$pos.enso.iod<-as.factor(df$pos.enso.iod)
df$hot.3d<-ifelse(is.na(df$hot3.u)==FALSE,df$hot3.u,df$hot3.r) # assign heatwave
# df$hot.4d<-ifelse(is.na(df$hot4.u)==FALSE,df$hot4.u,df$hot4.r) # assign heatwave
# df$hot.5d<-ifelse(is.na(df$hot5.u)==FALSE,df$hot5.u,df$hot5.r) # assign heatwave
#df<-df[hot.r==1 | hot.u==1,]
summary(df)

#########################################################################

# curtail data to record heatwaves only
dfcurt3<-df[(hot3.u==1 | hot3.r==1),]
# dfcurt4<-df[(hot4.u==1 | hot4.r==1),]
# dfcurt5<-df[(hot5.u==1 | hot5.r==1),]

# df$time<-(as.Date(df$date)-as.Date("1956-01-01"))
df.master$time<-yday(df.master$date)
df.mas<-df.master[date>"1990-01-01",]
cox3d <- coxph(Surv(time,hot3.r)~stANOM+iod+stANOM:iod,data=df.mas,na.action=na.exclude)#+iod+epoch
schoen3d = cox.zph(cox3d)
schoen3d
ggcoxzph(schoen3d,xlim=c(0,0.18),point.col="black",point.size=0.75,
         font.main=10,font.submain=10,font.x=10,font.y=8,font.tickslab=8)

### TABLES
freq.hw<-df[,c(sum(hot3.u,na.rm=TRUE),sum(hot3.r,na.rm=TRUE),
               sum(hot4.u,na.rm=TRUE),sum(hot4.r,na.rm=TRUE),
               sum(hot5.u,na.rm=TRUE),sum(hot5.r,na.rm=TRUE)),
            by=.(epoch,enso)]
freq.hw
write.csv(freq.hw,"G:/My Drive/Personal/Career/BOM/Data/tabresults.csv")

plot(dfcurt3$time,dfcurt3$stANOM,cex=0.25)
plot(dfcurt3$time,dfcurt3$iod,cex=0.25)

#######################################################
# binary covars only
cox3d <- coxph(Surv(time,hot.3d)~urb+enso+iodipole+enso:iodipole,data=df,na.action=na.exclude)
print(summary(cox3d))
cox4d <- coxph(Surv(time,hot.4d)~urb+enso+iodipole+enso:iodipole,data=df,na.action=na.exclude)
print(summary(cox4d))
cox5d <- coxph(Surv(time,hot.5d)~urb+enso+iodipole+enso:iodipole,data=df,na.action=na.exclude)
print(summary(cox5d))

### FOR TABLES
library(broom)
out <- tidy(cox3d)
out
out<-as.data.table(out)
write.csv(out,'G:/My Drive/Personal/Career/BOM/Papers/CoxPH/cox3dbin.csv')
exp(coef(cox3d)) # exponentiated coefficients
exp(confint(cox3d)) # 95% CI for exponentiated coefficients

out <- tidy(cox4d)
out
out<-as.data.table(out)
write.csv(out,'G:/My Drive/Personal/Career/BOM/Papers/CoxPH/cox4dbin.csv')
exp(coef(cox4d)) # exponentiated coefficients
exp(confint(cox4d)) # 95% CI for exponentiated coefficients

out <- tidy(cox5d)
out
out<-as.data.table(out)
write.csv(out,'G:/My Drive/Personal/Career/BOM/Papers/CoxPH/cox5dbin.csv')
exp(coef(cox5d)) # exponentiated coefficients
exp(confint(cox5d)) # 95% CI for exponentiated coefficients

