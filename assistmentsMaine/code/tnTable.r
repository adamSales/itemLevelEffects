library(xtable)
library(tidyverse)

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


tn <- read.csv('assistmentsMaine/Terranova Data/Terranova Data/WPI TerraNova and Data/fromWordDoc.csv')
tn <- na.omit(tn)
names(tn)[1] <- 'Item'

tn%>%group_by(CCSS)%>%summarize(Items=paste(unique(Item),collapse=','))%>%
    mutate(
        CCSS=ifelse(CCSS=='Make Sense/Perservere',"Make sense of problems and persevere in solving them (MP)",CCSS),
        CCSS=ifelse(CCSS=='Reason Abstract/Quantative',"Reason abstractly and quantitatively (MP)",CCSS),
        Items=gsub('37','37a,37b,37c',Items)
        )%>%
        xtable(caption="Common Core State Standards (CCSS) for the 37 TerraNova items, as identified by the ASSISTments team. Standards are from grade 7 except where indicated--grade 8 (8G) or Mathematical Practice (MP)",
               label="tab:assistmentsSkills",align=c('c','p{3in}','p{3in}'))%>%
    print(file='terranovva.tex',include.rownames=FALSE,floating.environment="table*")
