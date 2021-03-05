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
ggplot(ungroup(effs1),aes(as.numeric(prob),effect))+
    geom_point()+
    geom_line()+
#        geom_errorbar(aes(ymin=min1se,ymax=max1se),width=0,size=2)+
        geom_errorbar(aes(ymin=min2se,ymax=max2se),width=0,size=1)+
    geom_hline(yintercept=0)+
    scale_x_continuous(breaks=1:39,labels=levels(effs1$prob))+
    labs(x='Item',y='Effect')

ggsave('../assEffects.jpg',width=6.75,height=2)


tn <- read_csv('assistmentsMaine/Terranova Data/Terranova Data/WPI TerraNova and Data/Cristina and Andrew Test Taging.csv')

names(tn)[1] <- 'Problem'
names(tn)[2] <- 'cristina'
names(tn)[3] <- 'cristina2'
names(tn)[4] <- 'andrew'

tn$grade <- tn$cristina%>%strsplit('.',fixed=TRUE)%>%map_chr(~.[1])
tn$grade[tn$Problem%in%c(23,35)] <- 7 ## in next column over
tn$skill <- tn$cristina%>%strsplit('.',fixed=TRUE)%>%map_chr(~.[2])

tn$gradeA <- tn$andrew%>%strsplit('.',fixed=TRUE)%>%map_chr(~.[1])
tn$gradeA[tn$gradeA=='?'] <- NA
tn$skillA <- tn$andrew%>%strsplit('.',fixed=TRUE)%>%map_chr(~.[2])

skillz <- c(
    EE="Expressions & Equations",
    F="Functions",
    G="Geometry",
    MD="Measurement & Data",
    NBT="Number & Operations in Base 10",
    NF="Number & Operations--Fractions",
    NS="The Number System",
    OA="Operations & Algebraic Thinking",
    RP="Ratios & Proportional Relationships",
    SP="Statistics & Probability")

tn$skillz <- skillz[tn$skill]

getProbs <- function(sk){
    probs <-
        with(tn,
             sort(na.omit(unique(c(Problem[skill==sk],Problem[skillA==sk])))))
    int <-
        with(tn,
             sort(na.omit(intersect(Problem[skill==sk],Problem[skillA==sk]))))
    probs%>%
        map_chr(~paste0(.,ifelse(.%in%int,'','*')))%>%
        paste(collapse=',')
}


map_dfr(names(skillz),
               ~with(tn,tibble(
    Standard=skillz[.],
    Grades=paste(sort(unique(na.omit(c(grade[skill==.],gradeA[skillA==.])))),
                 collapse='&'),
    Items=getProbs(.)
    )))%>%
    xtable(caption="Common Core State Standards and grade levesl for the 37 TerraNova items, as identified by Andrew Burnett and Cristina Heffernan. Items classified discordantly are marked with a *.",label="tab:assistmentsSkills",align=c('c','p{2.5in}','p{1in}','p{2.5in}'))%>%
print(file='terranovva.tex',include.rownames=FALSE,floating.environment="table*")


tn <- read.csv('Terranova Data/Terranova Data/WPI TerraNova and Data/fromWordDoc.csv')
tn <- na.omit(tn)
names(tn)[1] <- 'prob'
tn$prob <- as.character(tn$prob)
tn <- tn[c(1:nrow(tn),37,37),]
tn$prob[37:39] <- paste0('37',c('A','B','C'))

effs1 <- full_join(effs1,tn)

effs1%>%
    filter(type=='multiple choice')%>%
    mutate(#CCSS=ifelse(type=='open-ended','Open\nEnded',CCSS),
           CCSS=fct_lump_min(CCSS,3))%>%
    group_by(CCSS)%>%
    mutate(newnum=1:n(),CCSS=gsub(' ','\n',CCSS))%>%ungroup()%>%
    ggplot(aes(newnum,effect,label=prob))+
    geom_point()+
    geom_text(nudge_y=.1,color='black')+
    geom_line()+geom_hline(yintercept=0,linetype='dotted')+
    facet_wrap(~CCSS,nrow=1,scales='free_x')+
     theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
    labs(y='Treatment Effect (Logit Scale)')

ggsave('../assEffectsType.jpg',width=6.5,height=2)

### effects per problem by worked/correct probs
ggplot(filter(effs1,type=='multiple choice'),aes(workedProbs,effect,label=prob))+#,color=type,fill=type))+
    geom_text()+
    #geom_errorbar(aes(ymin=min1se,ymax=max1se),width=0,size=2)+
    #geom_errorbar(aes(ymin=min2se,ymax=max2se),width=0,size=1)+
    #geom_hline(yintercept=0)+
    labs(x='Worked Problems',y='Effect')+
    scale_x_sqrt(breaks=c(0,500,1000,2000,5000,10000,20000,40000,60000),labels=c(0,500,'1K','2K','5K','10K','20K','40K','60K'))+
    geom_smooth(se=TRUE)
ggsave('../assEffectsWorkedProbs.jpg',width=6.5,height=2)


ggplot(filter(effs1,type=='multiple choice'),aes(correctProbs,effect,label=prob))+#color=type,fill=type))+
    geom_text()+
    #geom_errorbar(aes(ymin=min1se,ymax=max1se),width=0,size=2)+
    #geom_errorbar(aes(ymin=min2se,ymax=max2se),width=0,size=1)+
    #geom_hline(yintercept=0)+
    labs(x='Correct Problems',y='Effect')+
    scale_x_sqrt(breaks=c(0,500,1000,2000,5000,10000,20000,40000),labels=c(0,500,'1K','2K','5K','10K','20K','40K'))+
    geom_smooth(se=TRUE)
ggsave('../assEffectsCorrectProbs.jpg',width=6.5,height=2)


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


### se for open ended
seoe <- sqrt(
    vcov(mod1)['conditionTreatment','conditionTreatment']+
    vcov(mod1)['conditionTreatment:typeopen-ended','conditionTreatment:typeopen-ended']+
    2*vcov(mod1)['conditionTreatment','conditionTreatment:typeopen-ended']
)


re <- ranef(mod1,condVar=TRUE)
effs <- re$terranova_problem_id$conditionTreatment
names(effs) <- rownames(re$terranova_problem_id)
V <- apply(attr(re$terranova_problem_id,'postVar'),3,function(x) x[2,2])

names(V) <- names(effs)

V <- V[order(effs)]
effs <- effs[order(effs)]

ps <- outer(names(effs),names(effs),function(n1,n2) 2*pnorm(-abs(effs[n1]-effs[n2])/sqrt(V[n1]+V[n2])))

    stars <- ifelse(ps<0.001,'p<0.001',ifelse(ps<0.01,'p<0.01',ifelse(ps<0.05,'p<0.05',NA)))#ifelse(ps<0.1,'.',''))))
rownames(stars) <- colnames(stars) <- names(effs)
 colnames(stars) <- rev(names(effs))

    omar <- par()$mar
    on.exit(par(mar=omar))
    if(key) par(mar=c(2, 2, 2, 4.1)) else par(mar=c(2,2,2,0)) # adapt margins

    plot(stars,asp=TRUE, axis.col=list(at=seq(1,39,2),labels=colnames(stars)[seq(1,39,2)],pos=0,cex.axis=.75,gap.axis=0,mgp=c(1,0,1),lwd.ticks=0),
         axis.row=list(at=1:39,labels=colnames(stars),pos=0,cex.axis=0.5,gap.axis=0,mgp=c(1,0.5,1),las=2))#,main=paste(level,'Year',year),
#         key=if(key) list(side=4,las=1) else NULL)
    axis(1,at=seq(2,39,2),labels=colnames(stars)[seq(2,32,2)],cex.axis=.75,gap.axis=0,tick=FALSE,mgp=c(1,1,1))
