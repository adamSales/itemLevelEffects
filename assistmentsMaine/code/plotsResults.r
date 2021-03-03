require(tidyverse)
require(broom)
require(broom.mixed)
require(lme4)

###### data exploration

items%>%
    group_by(condition, terranova_problem_id)%>%
    summarize(correct=mean(correct))%>%
    ggplot(aes(condition,correct))+
    geom_col(position='dodge')+facet_wrap(~terranova_problem_id)

items%>%
    filter(condition=='Treatment',workedProbs>1000)%>%
    ggplot(aes(correct==1,probCount))+
    geom_violin()+facet_wrap(~terranova_problem_id)+
    scale_y_sqrt()

items%>%
    group_by(terranova_problem_id,condition)%>%
    summarize(nas=mean(is.na(correct)))%>%
    pivot_wider( id_cols=terranova_problem_id,names_from=condition,values_from=nas)


probLevel%>%
    mutate(prob=factor(terranova_problem_id,levels=c(1:36,paste0(37,c('A','B','C')))))%>%
    ggplot(aes(prob,workedProbs))+geom_point()+
    scale_y_sqrt()

effs1 <- getEffs(mod1)

### merge w problem-level skill builder data
effs1 <-
    probLevel%>%
    rename(prob=terranova_problem_id)%>%
    mutate(prob=factor(prob,levels=c(1:36,paste0(37,c('A','B','C')))),
           type=ifelse(prob%in%c(33:36,paste0(37,c('A','B','C'))),'open-ended','multiple choice'))%>%
    full_join(getEffs(mod1))

### effects per problem
ggplot(effs1,aes(prob,effect))+
        geom_point()+
        geom_errorbar(aes(ymin=min1se,ymax=max1se),width=0,size=2)+
        geom_errorbar(aes(ymin=min2se,ymax=max2se),width=0,size=1)+
        geom_hline(yintercept=0)+
        labs(x='Item',y='Effect')

### effects per problem by worked/correct probs
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


### does big glmer agree w seprarate models for each problem?
### effect estimates:
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

normalOLS%>%
    rename(prob=terranova_problem_id)%>%
    mutate(prob=factor(prob,levels=c(1:36,paste0(37,c('A','B','C')))))%>%
    full_join(effs1)%>%
    ggplot(aes(estimate,effect,color=type))+geom_point()+
    geom_hline(yintercept=0)+geom_vline(xintercept=0)+
    geom_smooth(method='lm',se=FALSE)+
    labs(title='Effect Estimates',x='Separate OLS Regressions',y='Together')

### anova: test for between-problem effects
