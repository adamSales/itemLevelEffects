require(tidyverse)
require(broom)
require(broom.mixed)
require(lme4)


tn <- read_csv('data/TerraNova.csv')
tn2 <- read_csv('data/TerraNova students 11-4-2015 with user id.csv')
items <- read_csv('data/efficacy_score_item.csv')

#### incorporate skill builder info
probCount <- read_csv('data/problem_count.csv')
corCount <- read_csv('data/correct_count.csv')

########################################
### is this right?
########################################
items$correct[is.na(items$correct)] <- 0


### OK student_sri_id seems to work out well
tn2 <- tn2%>%filter(!is.na(student_sri_id))%>%group_by(student_sri_id)%>%slice(1)%>%ungroup() #keep first row corresponding to each student_sri_id


items <- tn2%>%
    full_join(items)

## delete dup items from prob/cor count
probCount <- probCount[,-grep('_[12]',names(probCount))]
corCount <- corCount[,-grep('_[12]',names(corCount))]

### this column doesn't exist for some reason
corCount$`13` <- 0
probCount$`13` <- 0

### make them "long"
corCount <- pivot_longer(corCount,!contains("student"),names_to="terranova_problem_id",values_to="correctCount")
probCount <- pivot_longer(probCount,!contains("student"),names_to="terranova_problem_id",values_to="probCount")


#### hmmmm for now gonna assume that multiple "student_user_id"s corresponding to an individual "student_sri_id" means the same student logged on to ASSISTments using multiple IDs, and sum probCount and correctCount
probCount <- probCount%>%
    group_by(student_sri_id,terranova_problem_id)%>%
    summarize(probCount=sum(probCount,na.rm=TRUE))%>%
    ungroup()

corCount <- corCount%>%
    group_by(student_sri_id,terranova_problem_id)%>%
    summarize(correctCount=sum(correctCount,na.rm=TRUE))%>%
    ungroup()


items <- left_join(items,corCount)%>%left_join(probCount)

items$correctCount[is.na(items$correctCount)] <- 0
items$probCount[is.na(items$probCount)] <- 0

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


