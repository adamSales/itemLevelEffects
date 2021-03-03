library(rstan)


rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stanDat <- list()

Y <- read.csv('~/Box Sync/CT/data/RANDstudyData/posttest_item_scores_by_fieldid.csv')
Y <- Y[!duplicated(Y$field_id),] # keep first year for each student
Y <- Y[!is.na(Y$field_id),] # I'm not sure what the deal is with this one case, but it's in year 3
Y[is.na(Y)] <- 0

Ylong <- NULL
for(p in 1:32) Ylong <- rbind(Ylong,cbind(field_id=Y[['field_id']],resp=Y[[paste0('posttest_a',p)]],prob=p))

load('~/Box Sync/CT/data/RANDstudyData/HSdata.RData')
dat <- subset(dat,year%in%c(1,2)) # only years 1 & 2
#dat <- subset(dat,field_id%in%Y$field_id) # deletes 1 student (why?)
dat <- dat[order(dat$year),]
dat <- dat[!duplicated(dat$field_id),]

datLong <- merge(dat,Ylong,by='field_id')

### compare trt and control across years:
dat$trtYear <- with(dat,factor(ifelse(year==1,
                      ifelse(treatment==1,'T1','C1'),
                      ifelse(treatment==1,'T2','C2'))))

acrossYears <- lmer(Y~trtYear+xirt+xirt:year
                    #+grade+race+sex+frl+xirt+esl+spec+gradeMIS+raceMIS+sexMIS+frlMIS+xirtMIS+specMIS
                    +pair+pair:year+(1|classid2)+(year|schoolid2)-1,data=dat)


plot(c(1,1.01,2,2.01),fixef(acrossYears)[1:4],col=c('red','blue','red','blue'),ylim=c(-.7,.1))
segments(c(1,1.01),fixef(acrossYears)[1:2],c(2,2.01),fixef(acrossYears)[3:4],col=c('red','blue'))
ccc <- summary(acrossYears)$coef
Hmisc::errbar(c(1,1.01,2,2.01),fixef(acrossYears)[1:4],fixef(acrossYears)[1:4]+ccc[1:4,2],fixef(acrossYears)[1:4]-ccc[1:4,2],col=c('red','blue','red','blue'),add=TRUE,errbar.col=c('red','blue','red','blue'))


dat <- cbind(dat,Y[as.character(dat$field_id),])

### estimate each item separately:
itemMods <- list()
for(i in 1:32){
 form <- as.formula(paste0('posttest_a',i,'~treatment*year+xirt+xirt:year+pair+pair:year+(1|classid2)+(year|schoolid2)-1'))
 itemMods[[i]] <- glmer(form,family=binomial,data=dat)
}

itemEffects1 <- sapply(itemMods,function(mod) fixef(mod)['treatment']+fixef(mod)['treatment:year'])
itemEffects2 <- sapply(itemMods,function(mod) fixef(mod)['treatment']+2*fixef(mod)['treatment:year'])
itemDiff <- sapply(itemMods,function(mod) summary(mod)$coef['treatment:year',])
## rasch model:
library(lme4)
rasch <- glmer(resp~as.factor(prob)+treatment*year+race+spec+frl+esl+xirt+(1|field_id)+(1|teachid2)+(1|schoolid2),
 family=binomial,data=datLong)

items <- paste0('posttest_a',1:32)
rownames(Y) <- Y$field_id
Y <- Y[as.character(dat$field_id),items]
stopifnot(all(rownames(Y)==as.character(dat$field_id)))


Y <- as.matrix(Y)

stopifnot(nrow(dat)==nrow(Y))

stanDat$Y <- Y

id <- 1:nrow(dat)

dat <- droplevels(dat)

stanDat$n_student <- length(id)
stanDat$n_item <- length(items)

stanDat$teacher <- as.numeric(dat$teachid2)
stanDat$school <- as.numeric(dat$schoolid2)
stanDat$n_teacher <- max(stanDat$teacher)
stanDat$n_school <- max(stanDat$school)
stopifnot(with(stanDat,n_teacher==length(unique(teacher)) & n_school==length(unique(school))))

stanDat$year <- dat$year
stanDat$pretest <- dat$xirt

stanDat$blackMulti <- ifelse(dat$race=='blackMulti',1,0)
stanDat$hispAIAN <- ifelse(dat$race=='hispAIAN',1,0)
stanDat$gifted <- ifelse(dat$spec=='gifted',1,0)
stanDat$specialEd <- ifelse(dat$spec=='speced',1,0)
stanDat$frl <- ifelse(dat$frl=='1',1,0)
stanDat$lep <- ifelse(dat$esl=='1',1,0)

stanDat$Z <- dat$treatment

library(R2jags)
mod

mod <- stan(file = 'byProblem.stan',data=stanDat)


rownames(Y) <- Y$field_id
dat <- cbind(dat,Y[as.character(dat$field_id),])

corr2T <- sapply(unique(dat$schoolid2[dat$treatment==1]),
                 function(scl) mean(dat$posttest_a2[dat$schoolid2==scl & dat$year==1]))
corr2C <- sapply(unique(dat$schoolid2[dat$treatment==0]),
                 function(scl) mean(dat$posttest_a2[dat$schoolid2==scl & dat$year==1]))



load('~/Box Sync/CT/data/problemLevelUsageData/probLevelData.RData')
x$unit <- tolower(x$unit)


probUnit <- read.csv('problemUnits.csv',stringsAsFactors=FALSE,header=FALSE)


## make matrix
prob2Unit <- matrix(0,32,length(unique(x$unit)[!grepl('test',unique(x$unit))]))
colnames(prob2Unit) <- unique(x$unit)[!grepl('test',unique(x$unit))]


for(i in 1:nrow(probUnit)){
    units <- unlist(probUnit[i,])
    units <- units[units!='']
    units[grep('orderop',units)] <- "order-of-operations"
    prob2Unit[i,units] <- 1
}

## students who worked on units ostensibly related to factoring quadratics:
quadFacStud <- unique(x$field_id[x$unit%in%c("quadratics-factoring_es", "quadeqnswithfactoring", "polynomial-multiplying-factoring", "quadratics-solving_es")])

dat$quadFac <- dat$field_id%in%quadFacStud

## did students work more on these units in year 2 than year 1?
mean(dat$quadFac[dat$treatment==1 & dat$year==1])
mean(dat$quadFac[dat$treatment==1 & dat$year==2])

## how did it go for them on the posttest?
mean(dat$posttest_a2[dat$treatment==1 & dat$year==1 & dat$quadFac])
mean(dat$posttest_a2[dat$treatment==1 & dat$year==2 & dat$quadFac])


load('~/Box Sync/CT/data/problemLevelUsageData/probLevelData.RData')
x$unit <- tolower(x$unit)
x$section <- tolower(x$section)
x <- subset(x,field_id%in%dat$field_id)
x <- x[-grep('test',x$section),]
x$year <- dat$year[match(x$field_id,dat$field_id)]

pdf('yearDiff.pdf')
for(nnn in c('nhints1','nerrs1','total_t1')) boxplot(log(x[[nnn]]+1)~x$year,main=paste(nnn,'by year'))
dev.off()

jpeg('hintsDiff.jpg')
boxplot(log(x$nhints1+1)~x$year,main='Log(# Hints per Problem+1)',xlab='Study Year')
dev.off()

jpeg('errDiff.jpg')
boxplot(log(x$nerrs1+1)~x$year,main='Log(# Errors per Problem+1)',xlab='Study Year')
dev.off()

jpeg('timeDiff.jpg')
boxplot(log(x$total_t1)~x$year,main='Log(Time per Problem)',xlab='Study Year')
dev.off()




x$section <- factor(x$section)
sec1 <- table(x$section[x$year==1])/length(unique(x$field_id[x$year==1]))
sec2 <- table(x$section[x$year==2])/length(unique(x$field_id[x$year==2]))

secDiff <- sec1-sec2

head(secDiff[order(abs(secDiff),decreasing=TRUE)],20)

secDiff2 <- secDiff[-grep('unit-conv',names(secDiff))]

head(secDiff[order(abs(secDiff2),decreasing=TRUE)],20)
