library(tidyverse)
library(lme4)

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


ggplot(pdat,aes(prob,effect))+
        geom_point()+
        geom_errorbar(aes(ymin=min1se,ymax=max1se),width=0,size=2)+
        geom_errorbar(aes(ymin=min2se,ymax=max2se),width=0,size=1)+
        geom_hline(yintercept=0)+
        labs(x='Item',y='Effect')


mod2 <- update(mod1,subset=terranova_problem_id%in%as.character(1:32))

save(mod0,mod0a,mod1,mod2, file='tryMods.RData')











