require(tidyverse)
require(broom)
require(broom.mixed)
require(lme4)
require(rstanarm)


mod1 <- glmer(correct~(1|pair)+sped+condition*type+(condition|terranova_problem_id)+(1|student_sri_id) +(1|teacher_sri_id)+(1|school_id),
              data=items,
              family=binomial)
summary(mod1)
save(mod1,items,file='newModels.RData')

### separate regressions
normal <- items%>%
    nest(dat=-terranova_problem_id)%>%
    mutate(fit=map(dat,~glmer(correct~(1|pair)+sped+condition+(1|teacher_sri_id)+(1|school_id),data=.x,family=binomial)),
           fit=map(fit,tidy),
           fit=map(fit,~filter(.,startsWith(term,'condition'))))%>%
    unnest(fit)

save(items,mod1,normal,file='newModels.RData')

### separate OLS regressions
normalOLS <- items%>%
    nest(dat=-terranova_problem_id)%>%
    mutate(fit=map(dat,~lm(correct~as.factor(pair)+sped+condition,data=.x)),
           fit=map(fit,tidy),
           fit=map(fit,~filter(.,startsWith(term,'condition'))))%>%
    unnest(fit)

save(items,mod1,normal,normalOLS,file='newModels.RData')

mod2 <- update(mod1,.~.+workedProbs+condition:workedProbs)
summary(mod2)
save(items,mod1,mod2,normal,normalOLS,file='newModels.RData')

mod3 <- update(mod1,.~.+correctProbs+condition:correctProbs)
summary(mod3)
save(items,mod1,mod2,mod3,normal,normalOLS,file='newModels.RData')


mod4 <- update(mod1,.~.+correctCount+condition:correctCount)
summary(mod4)
save(mod1,mod2,mod3,mod4,items,file='newModels.RData')

mod5 <- update(mod1,.~.+probCount+condition:probCount+condition:type:probCount)
summary(mod5)
save(mod1,mod2,mod3,mod4,mod5,items,file='newModels.RData')

mod0a <- glmer(correct~(1|pair)+sped+condition+(1|terranova_problem_id)+(1|student_sri_id) +(1|teacher_sri_id)+(1|school_id),
              data=items,
              family=binomial)
summary(mod0a)
save(list=c(grep('mod',ls(),value=TRUE),grep('normal',ls(),value=TRUE),'items'),file='newModels.RData')

mod0b <- glmer(correct~(1|pair)+sped+condition*type+(1|terranova_problem_id)+(1|student_sri_id) +(1|teacher_sri_id)+(1|school_id),
              data=items,
              family=binomial)
summary(mod0b)
save(list=c(grep('mod',ls(),value=TRUE),grep('normal',ls(),value=TRUE),'items'),file='newModels.RData')


### what about just the treatment group?
modTrt <- glmer(correct~sped+probCount+(1|terranova_problem_id)+(1|student_sri_id) +(1|teacher_sri_id)+(1|school_id),
                data=items,
                subset=condition=='Treatment'&type=='multiple choice',
                family=binomial(logit))
save(list=c(grep('mod',ls(),value=TRUE),grep('normal',ls(),value=TRUE),'items'),file='newModels.RData')
