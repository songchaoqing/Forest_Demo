
# load library
library (survival); library(survminer); library(MASS)

# load data
S_BCI <- read.csv('Survival_Data.csv')
outpath = 'C:/Users/Desktop/'

# status--1 for dead; 0 for alive
# time--survival time
# SM--soil moisture (%)
# VPD--vapor pressure deficit (kPa)
# Temp-air temperature (C)
# SR--solar radiation (W/m2)
# DBH--diameter at the last census (mm)
# RGR_yr--relative growth rate between previous and last census (year-1)
# WD--wood density (g cm-3)
# hom--height of measurement
# treeID and stemID---tree and stem ID

# class
m1 <- (S_BCI$DBH>0)*1
m1 <- m1 + ((S_BCI$DBH)>=150)*1
m1 <- m1 + ((S_BCI$DBH)>400)*1
S_BCI$DBHnew <- m1

m2 <- (S_BCI$RGR_yr>-5.25)*1
m2 <- m2 + ((S_BCI$RGR_yr)>=1)*1
m2 <- m2 + ((S_BCI$RGR_yr)>10)*1
S_BCI$RGR_yrnew <- m2

m3 <- (S_BCI$WD>0)*1
m3 <- m3 + ((S_BCI$WD)>=0.3)*1
m3 <- m3 + ((S_BCI$WD)>0.6)*1
S_BCI$WDnew <- m3

m4 <- (S_BCI$SM>0)*1
m4 <- m4 + ((S_BCI$SM)>47.42)*1
S_BCI$SMnew <- m4

m5 <- (S_BCI$VPD>0)*1
m5 <- m5 + ((S_BCI$VPD)>1.03)*1
S_BCI$VPDnew <- m5

m6 <- (S_BCI$Temp>0)*1
m6 <- m6 + ((S_BCI$Temp)>25.91)*1
S_BCI$Tempnew <- m6

m7 <- (S_BCI$SR>0)*1
m7 <- m7 + ((S_BCI$SR)>254.23)*1
S_BCI$SRnew <- m7

########################
# survival: Kaplan-Meier
########################

# species
fit <- survfit(Surv(time, status) ~ species, data=S_BCI)
dev.new(width=3,height=3)
png(file=paste(outpath,"Surv.png",sep="/"),width=7000,height=7000,units='px',res=600)
ggsurvplot(fit,data=S_BCI,pval=TRUE,conf.int=TRUE,surv.median.line="hv",xlab='Year')
dev.off()

fit1 <- survfit(Surv(time, status) ~ DBHnew, data=S_BCI);    print(fit1)
fit2 <- survfit(Surv(time, status) ~ RGR_yrnew, data=S_BCI); print(fit2)
fit3 <- survfit(Surv(time, status) ~ WDnew, data=S_BCI);     print(fit3)
fit4 <- survfit(Surv(time, status) ~ SMnew, data=S_BCI);     print(fit4)
fit5 <- survfit(Surv(time, status) ~ VPDnew, data=S_BCI);    print(fit5)
fit6 <- survfit(Surv(time, status) ~ Tempnew, data=S_BCI);   print(fit6)
fit7 <- survfit(Surv(time, status) ~ SRnew, data=S_BCI);     print(fit7)

surv_diff1 <- survdiff(Surv(time, status) ~ DBHnew, data=S_BCI);    surv_diff1
surv_diff2 <- survdiff(Surv(time, status) ~ RGR_yrnew, data=S_BCI); surv_diff2
surv_diff3 <- survdiff(Surv(time, status) ~ WDnew, data=S_BCI);     surv_diff3
surv_diff4 <- survdiff(Surv(time, status) ~ SMnew, data=S_BCI);     surv_diff4
surv_diff5 <- survdiff(Surv(time, status) ~ VPDnew, data=S_BCI);    surv_diff5
surv_diff6 <- survdiff(Surv(time, status) ~ Tempnew, data=S_BCI);   surv_diff6
surv_diff7 <- survdiff(Surv(time, status) ~ SRnew, data=S_BCI);     surv_diff7


f1 <- ggsurvplot(fit1,data=S_BCI,pval=TRUE,conf.int=TRUE,surv.median.line="hv",xlab='Year',
                 legend.title="DBH",legend.labs = c('all',"<15", "15-40",'>40'),title='e',
                 palette=c('grey',rgb(237/255,0/255,0/255),rgb(66/255,181/255,64/255),rgb(0/255,70/255,139/255)),
                 add.all=TRUE,ggtheme = theme_bw()+theme(plot.title=element_text(vjust=-7,hjust=-0.1,size=17),
                                                         text=element_text(size=13),panel.grid=element_blank(),
                                                         axis.text.x=element_text(size=14),
                                                         axis.text.y=element_text(size=14),
                                                         panel.border=element_rect(fill=NA,size=0.8)))
f1
f2 <- ggsurvplot(fit2,data=S_BCI,pval=TRUE,conf.int=TRUE,surv.median.line="hv",xlab='Year',
                 legend.title="RGR",legend.labs = c('all',"<1", "1-10",'>10'),title='f',
                 palette=c('grey',rgb(66/255,181/255,64/255),rgb(0/255,70/255,139/255),rgb(237/255,0/255,0/255)),
                 add.all=TRUE,ggtheme = theme_bw()+theme(plot.title=element_text(vjust=-7,hjust=-0.1,size=17),
                                                         text=element_text(size=13),panel.grid=element_blank(),
                                                         axis.text.x=element_text(size=14),
                                                         axis.text.y=element_text(size=14),
                                                         panel.border=element_rect(fill=NA,size=0.8)))
f2
f3 <- ggsurvplot(fit3,data=S_BCI,pval=TRUE,conf.int=TRUE,surv.median.line="hv",xlab='Year',
                 legend.title="WD",legend.labs = c('all',"<0.3", "0.3-0.6",'>0.6'),title='g',
                 palette=c('grey',rgb(237/255,0/255,0/255),rgb(66/255,181/255,64/255),rgb(0/255,70/255,139/255)),
                 add.all=TRUE,ggtheme = theme_bw()+theme(plot.title=element_text(vjust=-7,hjust=-0.1,size=17),
                                                         text=element_text(size=13),panel.grid=element_blank(),
                                                         axis.text.x=element_text(size=14),
                                                         axis.text.y=element_text(size=14),
                                                         panel.border=element_rect(fill=NA,size=0.8)))
f3
f4 <- ggsurvplot(fit4,data=S_BCI,pval=TRUE,conf.int=TRUE,surv.median.line="hv",xlab='Year',
                 legend.title="SM",legend.labs = c('all',"Low SM","High SM"),title='a',
                 palette=c('grey',rgb(237/255,0/255,0/255),rgb(66/255,181/255,64/255)),
                 add.all=TRUE,ggtheme = theme_bw()+theme(plot.title=element_text(vjust=-7,hjust=-0.1,size=17),
                                                         text=element_text(size=13),panel.grid=element_blank(),
                                                         axis.text.x=element_text(size=14),
                                                         axis.text.y=element_text(size=14),
                                                         panel.border=element_rect(fill=NA,size=0.8)))
f4
f5 <- ggsurvplot(fit5,data=S_BCI,pval=TRUE,conf.int=TRUE,surv.median.line="hv",xlab='Year',
                 legend.title="VPD",legend.labs = c('all',"Low VPD","High VPD"),title='b',
                 palette=c('grey',rgb(66/255,181/255,64/255),rgb(237/255,0/255,0/255)),
                 add.all=TRUE,ggtheme = theme_bw()+theme(plot.title=element_text(vjust=-7,hjust=-0.1,size=17),
                                                         text=element_text(size=13),panel.grid=element_blank(),
                                                         axis.text.x=element_text(size=14),
                                                         axis.text.y=element_text(size=14),
                                                         panel.border=element_rect(fill=NA,size=0.8)))
f5
f6 <- ggsurvplot(fit6,data=S_BCI,pval=TRUE,conf.int=TRUE,surv.median.line="hv",xlab='Year',
                 legend.title="Temp",legend.labs = c('all',"Low Temp","High Temp"),title='c',
                 palette=c('grey',rgb(66/255,181/255,64/255),rgb(237/255,0/255,0/255)),
                 add.all=TRUE,ggtheme = theme_bw()+theme(plot.title=element_text(vjust=-7,hjust=-0.1,size=17),
                                                         text=element_text(size=13),panel.grid=element_blank(),
                                                         axis.text.x=element_text(size=14),
                                                         axis.text.y=element_text(size=14),
                                                         panel.border=element_rect(fill=NA,size=0.8)))
f6
f7 <- ggsurvplot(fit7,data=S_BCI,pval=TRUE,conf.int=TRUE,surv.median.line="hv",xlab='Year',
                 legend.title="SR",legend.labs = c('all',"Low SR","High SR"),title='d',
                 palette=c('grey',rgb(66/255,181/255,64/255),rgb(237/255,0/255,0/255)),
                 add.all=TRUE,ggtheme = theme_bw()+theme(plot.title=element_text(vjust=-7,hjust=-0.1,size=17),
                                                         text=element_text(size=13),panel.grid=element_blank(),
                                                         axis.text.x=element_text(size=14),
                                                         axis.text.y=element_text(size=14),
                                                         panel.border=element_rect(fill=NA,size=0.8)))
f7

f <- list(f4,f1,f5,f2,f6,f3,f7)
dev.new(width=8,height=5)
png(file=paste(outpath,"Surv_KM.png",sep="/"),width=9400,height=4800,units='px',res=600)
arrange_ggsurvplots(f,print=TRUE,nrow=2,ncol=4)
dev.off()

################################
# accelerated failure time model
################################

lm.reg=lm(status~DBH+RGR_yr+WD+SM+VPD+Temp+SR,data=S_BCI)
vif(lm.reg); summary(lm.reg); stepAIC(lm.reg)

aftfit1 <- survreg(Surv(time,status) ~ DBH+RGR_yr+WD+SM+VPD+SR,data=S_BCI,dist='weibull')
summary(aftfit1); stepAIC(aftfit1)

aftfit2 <- survreg(Surv(time,status) ~ DBH+RGR_yr+WD+SM+VPD+SR,data=S_BCI,dist='exponential')
summary(aftfit2); stepAIC(aftfit2)

aftfit3 <- survreg(Surv(time,status) ~ DBH+RGR_yr+WD+SM+VPD+SR,data=S_BCI,dist='loglogistic')
summary(aftfit3); stepAIC(aftfit3)

aftfit4 <- survreg(Surv(time,status) ~ DBH+RGR_yr+WD+SM+VPD+SR,data=S_BCI,dist='lognormal')
summary(aftfit4); stepAIC(aftfit4)
