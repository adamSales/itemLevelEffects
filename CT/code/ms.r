library(ggplot2)
library(dplyr)
library(lme4)

Y <- read.csv('../data/RANDstudyData/posttest_item_scores_by_fieldid.csv')
Y <- Y[order(Y$YEAR),]
Y <- Y[!duplicated(Y$field_id),] # keep first year for each student
Y <- Y[!is.na(Y$field_id),] # I'm not sure what the deal is with this one case, but it's in year 3
Y[is.na(Y)] <- 0

Ylong <- NULL
for(p in 1:32) Ylong <- rbind(Ylong,cbind(field_id=Y[['field_id']],resp=Y[[paste0('posttest_a',p)]],prob=p))

load('../data/RANDstudyData/MSdata.RData')
dat <- subset(dat,year%in%c(1,2)) # only years 1 & 2
#dat <- subset(dat,field_id%in%Y$field_id) # deletes 1 student (why?)
dat <- dat[order(dat$year),]
dat <- dat[!duplicated(dat$field_id),]

datLong <- merge(dat,Ylong,by='field_id')

datWide <- merge(dat,Y,by='field_id')


rasch1.1 <- glmer(resp~treatment+xirt+(treatment|prob)+(1|field_id)+(1|classid2)+(1|schoolid2),
                family=binomial,data=datLong,subset=year==1)
save(rasch1.1,file='rasch1.1ms.RData')
rasch1.2 <- glmer(resp~treatment+xirt+(treatment|prob)+(1|field_id)+(1|classid2)+(1|schoolid2),
                family=binomial,data=datLong,subset=year==2)
save(rasch1.2,file='rasch1.2ms.RData')

### test for different effects
yr1test <- anova(rasch1.1,
                 update(rasch1.1,.~.-(treatment|prob)+(1|prob)))

yr2test <- anova(rasch1.2,
                 update(rasch1.2,.~.-(treatment|prob)+(1|prob)))


datLong$year2 <- datLong$year-1
rasch2 <- glmer(resp~(treatment+xirt)*year2+
                    (treatment*year2|prob)+
                        (1|field_id)+
                            (1|classid2)+
                                (1|schoolid2),
                family=binomial,data=datLong)
save(rasch2,file='rasch2ms.RData')

save(list=ls(),file='msStuff.RData')
