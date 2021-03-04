getEffCT <- function(mod,re2){
    if(missing(re2))
        re2 <- ranef(mod,condVar=TRUE)
   pv2 <- attr(re2$prob,'postVar')

   r2eff <- data.frame(
       year=rep(c(1,2),each=32),
       item=rep(1:32,2),
       effect=fixef(mod)['treatment']+
           c(re2$prob$treatment,
             fixef(mod)['treatment:year2']+re2$prob$treatment+re2$prob[['treatment:year2']]),
       seEff=sqrt(summary(mod)$coef['treatment',2]^2+
                              c(pv2[2,2,],
                                summary(mod)$coef['treatment:year2',2]^2+
                                            2*vcov(mod)['treatment','treatment:year2']+
                                            pv2[2,2,]+pv2[4,4,]+2*pv2[2,4,])),
       est='mod')%>%
       mutate(itemOrd=factor(item,levels=order(effect[year==1])))

   r2eff
}


probDiff <- function(re,year=1,level,key=FALSE){
    effs <- re$prob$treatment
    if(year==2) effs <- effs+re$prob$`treatment:year2`

    names(effs) <- rownames(re$prob)

    V <- apply(attr(re$prob,'postVar'),3,function(x) x[2,2])
    if(year==2) V <- V+apply(attr(re$prob,'postVar'),3,function(x) x[4,4]+2*x[2,4])

    names(V) <- names(effs)

    V <- V[order(effs)]
    effs <- effs[order(effs)]

    ps <- outer(names(effs),names(effs),function(n1,n2) 2*pnorm(-abs(effs[n1]-effs[n2])/sqrt(V[n1]+V[n2])))

    stars <- ifelse(ps<0.001,'p<0.001',ifelse(ps<0.01,'p<0.01',ifelse(ps<0.05,'p<0.05',NA)))#ifelse(ps<0.1,'.',''))))
    rownames(stars) <- colnames(stars) <- names(effs)

    omar <- par()$mar
    on.exit(par(mar=omar))
    if(key) par(mar=c(2, 2, 2, 4.1)) else par(mar=c(2,2,2,0)) # adapt margins

    plot(stars,asp=TRUE, axis.col=list(at=seq(1,31,2),labels=colnames(stars)[seq(1,31,2)],pos=0,cex.axis=.75,gap.axis=0,mgp=c(1,0,1),lwd.ticks=0),
         axis.row=list(at=1:32,labels=colnames(stars),pos=0,cex.axis=0.5,gap.axis=0,mgp=c(1,0.5,1),las=2),main=paste(level,'Year',year),
         key=if(key) list(side=4,las=1) else NULL)
    axis(1,at=seq(2,32,2),labels=colnames(stars)[seq(2,32,2)],cex.axis=.75,gap.axis=0,tick=FALSE,mgp=c(1,1,1))
}
