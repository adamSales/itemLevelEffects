getEffCT <- function(mod){
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
