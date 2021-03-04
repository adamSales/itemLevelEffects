require(xtable)
require(tidyverse)
require(lme4)

load('CT/rasch2.RData')
rasch2hs <- rasch2
load('CT/rasch2ms.RData')
rasch2ms <- rasch2

reMS <- ranef(rasch2ms,condVar=TRUE)
reHS <- ranef(rasch2hs,condVar=TRUE)

effMS <- getEffCT(rasch2ms,reMS)%>%mutate(level='MS')
effHS <- getEffCT(rasch2hs,reHS)%>%mutate(level='HS')

eff <- bind_rows(effMS,effHS)%>%mutate(level=factor(level,levels=c('MS','HS')))

### add in problem type
desc <- read.csv('CT/problemDesc2.csv')

## desc$type <- with(desc,
##                   ifelse(solve,'solve',
##                          ifelse(fit.model.to.data,'fit model\nto data',
##                                 ifelse(graph,'graph',
##                                        ifelse(calculate,'calculate','other')))))

eff$type <- desc$type[match(eff$item,desc$Problem)]

jpeg('probDiff.jpg',width=6,height=6, units='in',res=100)
par(mfrow=c(2,2))
probDiff(reMS,level='MS',year=1)
probDiff(reMS,level='MS',year=2)
probDiff(reHS,level='HS',year=1)
probDiff(reHS,level='HS',year=2)
dev.off()

eff%>%
    mutate(signif=ifelse(effect-2*seEff>0,'Positive',ifelse(effect+2*seEff<0,'Negative','Undetermined')))%>%
    ggplot(aes(item+ifelse(year==2,.1,-.1), effect,ymin=effect-2*seEff,ymax=effect+2*seEff,color=factor(year)))+
    geom_point()+geom_line()+
    geom_errorbar(width=0)+geom_hline(yintercept=0)+scale_x_continuous(breaks=1:32)+
facet_wrap(~level,nrow=2)+labs(x='Item',y='Treatment Effect (Logit Scale)',color='Year')
ggsave('ctEffects.jpg',width=6.5,height=4)

eff%>%group_by(level,year,type)%>%mutate(newnum=1:n())%>%ungroup()%>%
    ggplot(aes(newnum,effect,color=factor(year),label=ifelse(year==2,item,'')))+
    geom_point()+
    geom_text(nudge_y=.1,color='black')+
    geom_line()+geom_hline(yintercept=0,linetype='dotted')+
    facet_grid(level~type)+
     theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
    labs(y='Treatment Effect (Logit Scale)',color='Year')

ggsave('ctEffectsType.jpg',width=6.5,height=4)

desc%>%
    select(Problem,`Main Skill`=Main.skill,Notes=notes,Type=type)%>%
    mutate(Type=gsub('\n',' ',Type))%>%
    xtable(caption="Skills required for the 32 items of the Algebra Proficiency
  Exam, the posttest for the CTA1 Evaluation",label="tab:ctSkills")%>%
print(file='CTposttest.tex',include.rownames=FALSE)


desc%>%
    group_by(type)%>%
    summarize(Items=paste(sort(Problem),collapse=', '))%>%
    mutate(
        Example=c('Which of these points is on the graph of [function]',
                  'Find the length of the base of the right triangle shown below',
                  'Which of the lines below is the graph of [linear equation]?',
                  'Which of these shows a correct factorization of [quadratic equation]?',
                  'Solve the following system of equations',
                  'Which of these expressions is equivalent to the one below?')
    )%>%
    rename(Objective=type)%>%
    xtable(caption="Objectives required for the 32 items of the Algebra Proficiency
  Exam, the posttest for the CTA1 Evaluation",label="tab:ctSkills",align=c('c','p{2.5in}','p{1in}','p{2.5in}'))%>%
print(file='CTposttest.tex',include.rownames=FALSE,floating.environment="table*")
