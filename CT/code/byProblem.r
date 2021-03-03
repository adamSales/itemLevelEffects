library(ggplot2)
library(dplyr)
library(lme4)

Y <- read.csv('~/Box Sync/CT/data/RANDstudyData/posttest_item_scores_by_fieldid.csv')
Y <- Y[order(Y$YEAR),]
Y <- Y[!duplicated(Y$field_id),] # keep first year for each student
Y <- Y[!is.na(Y$field_id),] # I'm not sure what the deal is with this one case, but it's in year 3
Y[is.na(Y)] <- 0

Ylong <- NULL
for(p in 1:32) Ylong <- rbind(Ylong,cbind(field_id=Y[['field_id']],resp=Y[[paste0('posttest_a',p)]],prob=p))

pload('~/Box Sync/CT/data/RANDstudyData/HSdata.RData')
dat <- subset(dat,year%in%c(1,2)) # only years 1 & 2
#dat <- subset(dat,field_id%in%Y$field_id) # deletes 1 student (why?)
dat <- dat[order(dat$year),]
dat <- dat[!duplicated(dat$field_id),]

datLong <- merge(dat,Ylong,by='field_id')

datWide <- merge(dat,Y,by='field_id')

### model problem-level treatment effects


## probEff <- function(i){
##  form <- as.formula(paste0('posttest_a',i,'~treatment*year+xirt+xirt:year+pair+pair:year+(1|classid2)+(year|schoolid2)-1'))
##  glmer(form,family=binomial,data=datWide)
## }
## itemMods <- lapply(1:32,probEff)


## itemEffects1 <- sapply(itemMods,function(mod) fixef(mod)['treatment']+fixef(mod)['treatment:year'])
## itemEffects2 <- sapply(itemMods,function(mod) fixef(mod)['treatment']+2*fixef(mod)['treatment:year'])
## itemDiff <- sapply(itemMods,function(mod) summary(mod)$coef['treatment:year',])

## ################ unadjusted percentages:
## perc <- lapply(1:32,
##                function(i) c(c1=mean(dat[[paste0('posttest_a',i)]][dat$treatment==0 & dat$year==1]),
##                              t1=mean(dat[[paste0('posttest_a',i)]][dat$treatment==1 & dat$year==1]),
##                              c2=mean(dat[[paste0('posttest_a',i)]][dat$treatment==0 & dat$year==2]),
##                              t2=mean(dat[[paste0('posttest_a',i)]][dat$treatment==1 & dat$year==2])))

## perc <- do.call('c',perc)
## perc <- data.frame(item=rep(1:32,each=4),treatment=rep(c('ctl','trt','ctl','trt'),32),year=factor(rep(c(1,1,2,2),32)),perc=perc)





## rasch model:
library(lme4)
rasch1 <- glmer(resp~treatment+xirt+(treatment|prob)+(1|field_id)+(1|classid2)+(1|schoolid2),
                family=binomial,data=datLong,subset=year==1)
save(rasch1,'rasch1.RData')
rasch2 <- glmer(resp~treatment+xirt+(treatment|prob)+(1|field_id)+(1|classid2)+(1|schoolid2),
                family=binomial,data=datLong,subset=year==2)
save(rasch2,'rasch2.RData')

### test for different effects
yr1test <- anova(rasch1,
                 update(rasch1,.~.-(treatment|prob)+(1|prob),REML=FALSE))

yr2test <- anova(rasch2,
                 update(rasch2,.~.-(treatment|prob)+(1|prob),REML=FALSE))

## r1 <- glmer(resp~treatment+xirt+(1|prob)+(1|field_id)+(1|classid2)+(1|schoolid2),
##                 family=binomial,data=datLong,subset=year==1)


effects <- function(mod)
    ranef(mod)$prob$treatment+fixef(mod)['treatment']

seEff <- function(mod){
    pv <- attr(ranef(mod,condVar=TRUE)$prob,'postVar')
    sqrt(summary(mod)$coef['treatment',2]^2+pv[2,2,])
}

eff <- data.frame(year=rep(c(1,2),each=32),
                  item=rep(1:32,2),
                  effect=c(effects(rasch1),effects(rasch2)),
                  seEff=c(seEff(rasch1),seEff(rasch2)))





rctYes <- read.csv('rctYesOut.csv')
desBased <- subset(rctYes,table_id==9&!is.na(sglevel),
                   select=c(outcome,sglevel,impact,se_impact))
desBased <- desBased%>%arrange(sglevel)%>%rename(year=sglevel,item=outcome,effect=impact,
                                                 seEff=se_impact)%>%mutate(est='desBased')

eff$est <- 'rasch1'

eff$effnum <- order(eff$effect[eff$est=='rasch1'&eff$year==2])

rasch1.1 <- rasch1
rasch1.2 <- rasch2
save(rasch1.1,file='rasch1.1.RData')
save(rasch1.2,file='rasch1.2.RData')

datLong$year2 <- datLong$year-1
## rasch2 <- glmer(resp~(treatment+xirt)*year2+
##                     (treatment*year2|prob)+
##                         (1|field_id)+
##                             (1|classid2)+
##                                 (1|schoolid2),
##                 family=binomial,data=datLong)
## save(rasch2,file='rasch2.RData')

load('rasch2.RData')

re2 <- ranef(rasch2,condVar=TRUE)
pv2 <- attr(re2$prob,'postVar')

r2eff <- data.frame(
    year=rep(c(1,2),each=32),
    item=rep(1:32,2),
    effect=fixef(rasch2)['treatment']+
        c(re2$prob$treatment,
          fixef(rasch2)['treatment:year2']+re2$prob$treatment+re2$prob[['treatment:year2']]),
    seEff=sqrt(summary(rasch2)$coef['treatment',2]^2+
                   c(pv2[2,2,],
                     summary(rasch2)$coef['treatment:year2',2]^2+
                         2*vcov(rasch2)['treatment','treatment:year2']+
                             pv2[2,2,]+pv2[4,4,]+2*pv2[2,4,])),
    est='rasch2')

eff <- rbind(eff,r2eff)

eff2 <- tidyr::spread(eff[,-4],est,effect)
effse2 <- tidyr::spread(eff[,-3],est,seEff)

### cor btw years
vc <- VarCorr(rasch2)$prob
v1 <- vc[2,2]
v2 <- vc[4,4]
c12 <- vc[2,4]

ccc <- v1+c12
r <- ccc/sqrt(v1*(v1+v2+2*c12))

desc <- read.csv('problemDesc.csv')

desc$type <- with(desc,
                  ifelse(solve,'solve',
                         ifelse(fit.model.to.data,'fit.model.to.data',
                                ifelse(graph,'graph',
                                       ifelse(calculate,'calculate','other')))))

datLong$type <- desc$type[match(datLong$prob,desc$Problem)]

rasch1.2.2 <- update(rasch1.2,.~.+type)
save(rasch1.2.2,file='byType2.RData')


eff$type <- desc$type[match(eff$item,desc$Problem)]

eff%>%group_by(type,est)%>%summarize(avg=mean(effect))%>%spread(est,avg)

pd <- filter(eff,est=='rasch1')%>%arrange(type)%>%group_by(year)%>%mutate(newnum=1:n())
ggplot(pd,aes(newnum,effect,color=year,group=year))+geom_point()+geom_line()+geom_hline(yintercept=0,linetype='dotted')

pd <- filter(eff,est=='rasch1')%>%
    mutate(effect2=ifelse(year==1,
               effect-mean(pd$effect[pd$year==1]),
               effect-mean(pd$effect[pd$year==2])))%>%
        group_by(type)%>%mutate(avg=mean(effect2))%>%ungroup()%>%arrange(avg)%>%
            group_by(year)%>%mutate(newnum=1:n(),Year=factor(year))





ggplot(pd,aes(newnum,effect,color=Year,group=Year))+geom_point()+geom_line()+geom_hline(yintercept=0,linetype='dashed')+
    scale_x_continuous(name="Problem Type",
                       breaks=sapply(unique(pd$type),function(tt) min(pd$newnum[pd$type==tt])),
                       labels=unique(pd$type))+
        ylab('Effect')+
            geom_vline(xintercept=sapply(unique(pd$type)[-1],
                           function(tt) min(pd$newnum[pd$type==tt]))-0.5,linetype='dotted')


pdf('effectsByProblem.pdf')
plot(ranef(rasch1)$prob$treatment+fixef(rasch1)['treatment'],type='b',col='blue',ylim=c(-0.6,.6),xlab='Posttest Item',ylab='Treatment Effect (log-odds ratio)')
lines(ranef(rasch2)$prob$treatment+fixef(rasch2)['treatment'],type='b',col='red')
legend('bottomleft',legend=c('year 1','year 2'),col=c('blue','red'),lwd=1)
dev.off()




datLong$Z1 <- datLong$treatment*(datLong$year==1)
datLong$Z2 <- datLong$treatment*(datLong$year==2)

rasch3 <- glmer(resp~xirt*year+Z1+Z2
                +(Z1+Z2|prob)+(1|field_id)+(1|classid2)+(year|schoolid2),
                family=binomial,data=datLong)

rasch <- glmer(resp~as.factor(prob)+treatment*year+race+spec+frl+esl+xirt+(1|field_id)+(1|teachid2)+(1|schoolid2),
 family=binomial,data=datLong)


