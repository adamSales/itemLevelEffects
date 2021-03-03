require(tidyverse)
require(broom)
require(broom.mixed)
require(lme4)
require(rstanarm)

options(mc.cores = 4)

mod1 <- stan_glmer(correct~(1|pair)+sped+condition*type+(condition|terranova_problem_id)+(1|student_sri_id) +(1|teacher_sri_id)+(1|school_id),
              data=items,
              family=binomial)
summary(mod1)
save(mod1,items,file='newModelsStan.RData')


mod2 <- update(mod1,.~.+workedProbs+condition:workedProbs)
summary(mod2)
save(items,mod1,mod2,file='newModelsStan.RData')

mod3 <- update(mod1,.~.+correctProbs+condition:correctProbs)
summary(mod3)
save(items,mod1,mod2,mod3,file='newModelsStan.RData')


### what about just the treatment group?
modTrt <- stan_glmer(correct~sped+probCount+(1|terranova_problem_id)+(1|student_sri_id) +(1|teacher_sri_id)+(1|school_id),
                data=items,
                subset=condition=='Treatment'&type=='multiple choice',
                family=binomial(logit))
save(list=c(grep('mod',ls(),value=TRUE),'items'),file='newModelsStan.RData')
