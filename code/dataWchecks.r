tn <- read_csv('data/TerraNova.csv')
tn2 <- read_csv('data/TerraNova students 11-4-2015 with user id.csv')

tn2%>%filter(condition=='Treatment')%>%
    group_by(school_id)%>%
    summarize(n=n(),user=n_distinct(student_user_id),userNA=sum(is.na(student_user_id)),sri=n_distinct(student_sri_id),sriNA=sum(is.na(student_sri_id)))%>%
    print(n=Inf)


items <- read_csv('data/efficacy_score_item.csv')

########################################
### is this right?
########################################
items$correct[is.na(items$correct)] <- 0


dim(items)
n_distinct(items$student_sri_id,items$terranova_problem_id)

tn2%>%group_by(student_sri_id)%>%
    summarize(cond=n_distinct(condition),school=n_distinct(school_id),teach=n_distinct(teacher_sri_id),sped=n_distinct(sped))%>%
    ungroup()%>%
    select(-student_sri_id)%>%
    map(table)

### OK student_sri_id seems to work out well
tn2 <- tn2%>%filter(!is.na(student_sri_id))%>%group_by(student_sri_id)%>%slice(1)%>%ungroup() #keep first row corresponding to each student_sri_id


items <- tn2%>%
    #select(Cohort:SPED)%>%
    #rename(student_sri_id=StudentID)%>%
    full_join(items)

## items <- items%>%
##     group_by(pair)%>%
##     mutate(ntrtgrp=n_distinct(condition))%>%
##     filter(ntrtgrp>1)%>%
##     ungroup()

### wait--gotta make sure the data is right...
## How do student_user_id and student_sri_id correspond?
## check1 <- items%>%
##     filter(condition=='Treatment')%>%
##     group_by(student_sri_id)%>%
##     summarize(nrows=n(),nprobs=n_distinct(terranova_problem_id),nstudid=n_distinct(student_user_id))

## sum(check1$nrows!=check1$nprobs)
## which(check1$nrows!=check1$nprobs)
## table(check1$nstudid)

#### incorporate skill builder info
probCount <- read_csv('data/problem_count.csv')

## duplicated columns
for(dup in grep('_[12]',names(probCount)[-1])){
    print(names(probCount)[c(dup,dup+1)])
    print(all.equal(probCount[[dup]],probCount[[dup+1]]))
}

## all true--delete dups
probCount <- probCount[,-grep('_[12]',names(probCount))]

corCount <- read_csv('data/correct_count.csv')

## duplicated columns
for(dup in grep('_[12]',names(corCount))){
    print(names(corCount)[c(dup-1,dup)])
    print(all.equal(corCount[[dup]],corCount[[dup-1]]))
}
## all true--delete dups
corCount <- corCount[,-grep('_[12]',names(corCount))]

setdiff(items$terranova_problem_id,names(corCount))
setdiff(names(corCount),items$terranova_problem_id)

corCount$`13` <- 0
probCount$`13` <- 0

corCount <- pivot_longer(corCount,!contains("student"),names_to="terranova_problem_id",values_to="correctCount")
probCount <- pivot_longer(probCount,!contains("student"),names_to="terranova_problem_id",values_to="probCount")

probCount%>%
    group_by(student_sri_id,terranova_problem_id)%>%
    summarize(count=n_distinct(probCount))%>%
    xtabs(~count,data=.)

probCount%>%
    group_by(student_sri_id,terranova_problem_id)%>%
    mutate(count=n_distinct(probCount))%>%
    filter(count==4)

#### hmmmm for now gonna assume that multiple "student_user_id"s corresponding to an individual "student_sri_id" means the same student logged on to ASSISTments using multiple IDs, and sum probCount and correctCount

probCount <- probCount%>%
    group_by(student_sri_id,terranova_problem_id)%>%
    summarize(probCount=sum(probCount,na.rm=TRUE))%>%
    ungroup()

corCount <- corCount%>%
    group_by(student_sri_id,terranova_problem_id)%>%
    summarize(correctCount=sum(correctCount,na.rm=TRUE))%>%
    ungroup()


items%>%filter(condition=='Treatment')%>%summarize(n=n(),data=n_distinct(student_sri_id,terranova_problem_id))
corCount%>%summarize(n=n(),data=n_distinct(student_sri_id,terranova_problem_id))
probCount%>%summarize(n=n(),data=n_distinct(student_sri_id,terranova_problem_id))

items <- left_join(items,corCount)%>%left_join(probCount)
items%>%filter(condition=='Treatment')%>%summarize(n=n(),data=n_distinct(student_sri_id,terranova_problem_id))

items$correctCount[is.na(items$correctCount)] <- 0
items$probCount[is.na(items$probCount)] <- 0

sum(items$correctCount>items$probCount) # should be 0
items$correctCount[items$condition=='Control'] <- 0
items$probCount[items$condition=='Control'] <- 0

probLevel <- items%>%
    group_by(terranova_problem_id)%>%
    summarize(
        workedProbs=sum(probCount),
        correctProbs=sum(correctCount)
    )%>%
    ungroup()%>%
    mutate(
        workedProbsS=scale(workedProbs),
        correctProbsS=scale(correctProbs)
    )
items <- left_join(items,probLevel)%>%
    mutate(type=ifelse(terranova_problem_id%in%c(33:36,paste0(37,c('A','B','C'))),'open-ended','multiple choice'))


