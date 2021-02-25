library(tidyverse)
library(lme4)

tn <- read_csv('data/TerraNova.csv')
tn2 <- read_csv('data/TerraNova students 11-4-2015 with user id.csv')

tn2%>%filter(condition=='Treatment')%>%
    group_by(school_id)%>%
    summarize(n=n(),user=n_distinct(student_user_id),userNA=sum(is.na(student_user_id)),sri=n_distinct(student_sri_id),sriNA=sum(is.na(student_sri_id)))%>%
    print(n=Inf)


items <- read_csv('data/efficacy_score_item.csv')

########################################
### is this right?
########################################
items$correct[is.na(items$correct)] <- 0


dim(items)
n_distinct(items$student_sri_id,items$terranova_problem_id)

tn2%>%group_by(student_sri_id)%>%
    summarize(cond=n_distinct(condition),school=n_distinct(school_id),teach=n_distinct(teacher_sri_id),sped=n_distinct(sped))%>%
    ungroup()%>%
    select(-student_sri_id)%>%
    map(table)

### OK student_sri_id seems to work out well
tn2 <- tn2%>%filter(!is.na(student_sri_id))%>%group_by(student_sri_id)%>%slice(1)%>%ungroup() #keep first row corresponding to each student_sri_id


items <- tn2%>%
    #select(Cohort:SPED)%>%
    #rename(student_sri_id=StudentID)%>%
    full_join(items)

## items <- items%>%
##     group_by(pair)%>%
##     mutate(ntrtgrp=n_distinct(condition))%>%
##     filter(ntrtgrp>1)%>%
##     ungroup()

### wait--gotta make sure the data is right...
## How do student_user_id and student_sri_id correspond?
## check1 <- items%>%
##     filter(condition=='Treatment')%>%
##     group_by(student_sri_id)%>%
##     summarize(nrows=n(),nprobs=n_distinct(terranova_problem_id),nstudid=n_distinct(student_user_id))

## sum(check1$nrows!=check1$nprobs)
## which(check1$nrows!=check1$nprobs)
## table(check1$nstudid)

mod0 <- glmer(correct~as.factor(pair)+sped+condition+(1|terranova_problem_id)+(1|student_sri_id) +(1|teacher_sri_id)+(1|school_id),
              data=items,
              family=binomial)
#              subset=Cohort==2)

mod0a <- glmer(correct~(1|pair)+sped+condition+(1|terranova_problem_id)+(1|student_sri_id) +(1|teacher_sri_id)+(1|school_id),
              data=items,
              family=binomial)

mod1 <- glmer(correct~as.factor(pair)+sped+condition+(condition|terranova_problem_id)+(1|student_sri_id) +(1|teacher_sri_id)+(1|school_id),
              data=items,
              family=binomial)
save(mod0,mod0a,mod1,file='tryMods.RData')

getEffs <- function(mod){
    re <- ranef(mod,condVar=TRUE)

    SEs <- sqrt(
        apply(attr(re$terranova_problem_id,'postVar'),3,function(x) x[2,2])+
        summary(mod)$coef['conditionTreatment',2]^2)

    data.frame(
        prob=factor(rownames(re$terranova_problem_id),levels=c(1:36,paste0(37,c('A','B','C')))),
        effect=fixef(mod)['conditionTreatment']+re$terranova_problem_id$conditionTreatment,
        se=SEs)%>%
        mutate(
            min1se=effect-SEs,
            max1se=effect+SEs,
            min2se=effect-2*SEs,
            max2se=effect+2*SEs
        )
}

ggplot(pdat,aes(prob,effect))+
        geom_point()+
        geom_errorbar(aes(ymin=min1se,ymax=max1se),width=0,size=2)+
        geom_errorbar(aes(ymin=min2se,ymax=max2se),width=0,size=1)+
        geom_hline(yintercept=0)+
        labs(x='Item',y='Effect')


mod2 <- update(mod1,subset=terranova_problem_id%in%as.character(1:32))

save(mod0,mod0a,mod1,mod2, file='tryMods.RData')


#### incorporate skill builder info
probCount <- read_csv('data/problem_count.csv')

## duplicated columns
for(dup in grep('_[12]',names(probCount)[-1])){
    print(names(probCount)[c(dup,dup+1)])
    print(all.equal(probCount[[dup]],probCount[[dup+1]]))
}

## all true--delete dups
probCount <- probCount[,-grep('_[12]',names(probCount))]

corCount <- read_csv('data/correct_count.csv')

## duplicated columns
for(dup in grep('_[12]',names(corCount))){
    print(names(corCount)[c(dup-1,dup)])
    print(all.equal(corCount[[dup]],corCount[[dup-1]]))
}
## all true--delete dups
corCount <- corCount[,-grep('_[12]',names(corCount))]

setdiff(items$terranova_problem_id,names(corCount))
setdiff(names(corCount),items$terranova_problem_id)

corCount$`13` <- 0
probCount$`13` <- 0

corCount <- pivot_longer(corCount,!contains("student"),names_to="terranova_problem_id",values_to="correctCount")
probCount <- pivot_longer(probCount,!contains("student"),names_to="terranova_problem_id",values_to="probCount")

probCount%>%
    group_by(student_sri_id,terranova_problem_id)%>%
    summarize(count=n_distinct(probCount))%>%
    xtabs(~count,data=.)

probCount%>%
    group_by(student_sri_id,terranova_problem_id)%>%
    mutate(count=n_distinct(probCount))%>%
    filter(count==4)

#### hmmmm for now gonna assume that multiple "student_user_id"s corresponding to an individual "student_sri_id" means the same student logged on to ASSISTments using multiple IDs, and sum probCount and correctCount

probCount <- probCount%>%
    group_by(student_sri_id,terranova_problem_id)%>%
    summarize(probCount=sum(probCount,na.rm=TRUE))%>%
    ungroup()

corCount <- corCount%>%
    group_by(student_sri_id,terranova_problem_id)%>%
    summarize(correctCount=sum(correctCount,na.rm=TRUE))%>%
    ungroup()


items%>%filter(condition=='Treatment')%>%summarize(n=n(),data=n_distinct(student_sri_id,terranova_problem_id))
corCount%>%summarize(n=n(),data=n_distinct(student_sri_id,terranova_problem_id))
probCount%>%summarize(n=n(),data=n_distinct(student_sri_id,terranova_problem_id))

items <- left_join(items,corCount)%>%left_join(probCount)
items%>%filter(condition=='Treatment')%>%summarize(n=n(),data=n_distinct(student_sri_id,terranova_problem_id))

items$correctCount[is.na(items$correctCount)] <- 0
items$probCount[is.na(items$probCount)] <- 0

sum(items$correctCount>items$probCount) # should be 0
items$correctCount[items$condition=='Control'] <- 0
items$probCount[items$condition=='Control'] <- 0

probLevel <- items%>%
    group_by(terranova_problem_id)%>%
    summarize(
        workedProbs=sum(probCount),
        correctProbs=sum(correctCount)
    )%>%
    ungroup()%>%
    mutate(
        workedProbsS=scale(workedProbs),
        correctProbsS=scale(correctProbs)
    )
items <- left_join(items,probLevel)

mod1 <- glmer(correct~(1|pair)+sped+condition+(condition|terranova_problem_id)+(1|student_sri_id) +(1|teacher_sri_id)+(1|school_id),
              data=items,
              family=binomial)
summary(mod1)
save(mod1,items,file='newModels.RData')

effs1 <-
    probLevel%>%
    rename(prob=terranova_problem_id)%>%
    mutate(prob=factor(prob,levels=c(1:36,paste0(37,c('A','B','C')))),
           type=ifelse(prob%in%c(33:36,paste0(37,c('A','B','C'))),'open-ended','multiple choice'))%>%
    full_join(getEffs(mod1))




ggplot(effs1,aes(workedProbs,effect,color=type,fill=type))+
    geom_point()+
    #geom_errorbar(aes(ymin=min1se,ymax=max1se),width=0,size=2)+
    #geom_errorbar(aes(ymin=min2se,ymax=max2se),width=0,size=1)+
    geom_hline(yintercept=0)+
    labs(x='Worked Problems',y='Effect')+
    scale_x_sqrt()+
    geom_smooth(se=FALSE)

ggplot(effs1,aes(correctProbs,effect,color=type,fill=type))+
    geom_point()+
    #geom_errorbar(aes(ymin=min1se,ymax=max1se),width=0,size=2)+
    #geom_errorbar(aes(ymin=min2se,ymax=max2se),width=0,size=1)+
    geom_hline(yintercept=0)+
    labs(x='Correct Problems',y='Effect')+
    scale_x_sqrt()+
    geom_smooth(se=FALSE)

### normal
normal <- items%>%
    nest(dat=-terranova_problem_id)%>%
    mutate(fit=map(dat,~glmer(correct~(1|pair)+sped+condition+(1|teacher_sri_id)+(1|school_id),data=.x,family=binomial)),
           fit=map(fit,tidy),
           fit=map(fit,~filter(.,startsWith(term,'condition'))))%>%
    unnest(fit)


### do they agree?
normal%>%
    select(-effect)%>%
    rename(prob=terranova_problem_id)%>%
    mutate(prob=factor(prob,levels=c(1:36,paste0(37,c('A','B','C')))))%>%
    full_join(effs1)%>%
    ggplot(aes(estimate,effect,color=type))+geom_point()+
    geom_hline(yintercept=0)+geom_abline(intercept=0,slope=1)+
    labs(title='Effect Estimates',x='Separate Regressions',y='Together')

### SEs
normal%>%
    select(-effect)%>%
    rename(prob=terranova_problem_id)%>%
    mutate(prob=factor(prob,levels=c(1:36,paste0(37,c('A','B','C')))))%>%
    full_join(effs1)%>%
    ggplot(aes(std.error,se,color=type))+geom_point()+
   geom_abline(intercept=0,slope=1)+
    labs(title='Standard Errors',x='Separate Regressions',y='Together')+xlim(0,0.4)+ylim(0,0.4)




mod2 <- update(mod1,.~.+workedProbs+condition:workedProbs)
summary(mod2)
save(mod1,mod2,items,file='newModels.RData')

mod3 <- update(mod1,.~.+correctProbs+condition:correctProbs)
summary(mod3)
save(mod1,mod2,mod3,items,file='newModels.RData')

mod4 <- update(mod1,.~.+correctCount+condition:correctCount)
summary(mod4)
save(mod1,mod2,mod3,mod4,items,file='newModels.RData')

mod5 <- update(mod1,.~.+probCount+condition:probCount)
summary(mod5)
save(mod1,mod2,mod3,mod4,mod5,items,file='newModels.RData')

mod0a <- glmer(correct~(1|pair)+sped+condition+(1|terranova_problem_id)+(1|student_sri_id) +(1|teacher_sri_id)+(1|school_id),
              data=items,
              family=binomial)
summary(mod0a)
save(mod0a,mod1,mod2,mod3,mod4,mod5,items,file='newModels.RData')


items%>%
    filter(condition=='Treatment',workedProbs>1000)%>%
    ggplot(aes(correct==1,probCount))+
    geom_violin()+facet_wrap(~terranova_problem_id)+
    scale_y_sqrt()

items%>%
    group_by(terranova_problem_id,condition)%>%
    summarize(nas=mean(is.na(correct)))%>%
    pivot_wider( id_cols=terranova_problem_id,names_from=condition,values_from=nas)
