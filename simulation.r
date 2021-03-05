library(tidyverse)
library(lme4)

makeData <- function(n=100,nexp=20)
    data.frame(
        Y = rnorm(n*nexp),
        expr = rep(letters[1:nexp],each=n),
        Z = rep(c(0,1),n*nexp/2)
    )

sim1 <- function(n=100,nexp=20){
    dat <- makeData(n=n,nexp=nexp)
    mod <- lmer(Y~Z+(Z|expr),data=dat)
    re <- ranef(mod,condVar=TRUE)
    effs <- fixef(mod)['Z']+re$expr$Z
    ses <- sqrt(vcov(mod)['Z','Z']+
                apply(attr(re$expr,'postVar'),3,function(x) x[2,2])
                )

### t-tests
    ttests <- dat%>%
        group_by(expr)%>%
        summarize(p=t.test(Y[Z==1],Y[Z==0])$p.value)

    c(pp=sum(abs(effs)>2*ses),np=sum(ttests$p<0.05))
}

system.time(sim40 <- replicate(1000,sim1(n=500,nexp=40)))
sim <- map(seq(5,35,5),~replicate(1000,sim1(n=500,nexp=.)))
sim[[8]] <- sim40

save(sim,file='simulation.RData')

tibble(n=rep(seq(5,40,5),2),rate=c(map_dbl(sim,~mean(.['pp',]>0)),map_dbl(sim,~mean(.['np',]>0))),type=rep(c('Together','Separate'),each=8))%>%
    ggplot(aes(n,rate,color=type))+geom_point()+geom_line()+labs(x="# Experiments",y="Familywise Error Rate",color=NULL)+
    geom_hline(yintercept=0.05,linetype="dotted")+ylim(0,1)+theme(legend.position='top')+geom_function(fun=~1-0.95^.x)

ggsave('simulationResults.pdf',height=3,width=3,units='in')


mean(sim)
prop.test(sum(sim),length(sim),p=0.05)
