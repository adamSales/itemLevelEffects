getEffs <- function(mod){
    re <- ranef(mod,condVar=TRUE)

    vcv <- vcov(mod)

### find open-ended questions
    openEnded <- unique(as.character(mod@frame$terranova_problem_id[mod@frame$type=='open-ended']))

    Vs <-# sqrt(
        apply(attr(re$terranova_problem_id,'postVar'),3,function(x) x[2,2])+
        vcv['conditionTreatment','conditionTreatment']

    Vs[rownames(re$terranova_problem_id)%in%openEnded] <-
        Vs[rownames(re$terranova_problem_id)%in%openEnded]+
        vcv["conditionTreatment:typeopen-ended","conditionTreatment:typeopen-ended"]+
        2*vcv["conditionTreatment:typeopen-ended","conditionTreatment"]

    effect <- fixef(mod)['conditionTreatment']+re$terranova_problem_id$conditionTreatment+
         +ifelse(rownames(re$terranova_problem_id)%in%openEnded,fixef(mod)["conditionTreatment:typeopen-ended"],0)

    data.frame(
        prob=factor(rownames(re$terranova_problem_id),levels=c(1:36,paste0(37,c('A','B','C')))),
        effect=effect,
        se=sqrt(Vs))%>%
        mutate(
            min1se=effect-se,
            max1se=effect+se,
            min2se=effect-2*se,
            max2se=effect+2*se
        )
}
