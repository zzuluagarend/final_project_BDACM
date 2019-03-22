rm(list = ls())
library(pacman)
library(tidyverse)
p_load(ggplot2, dplyr, foreign, psych, lsr, nlme, multcomp)

data <- read.csv("pilot.csv")
data <- as_tibble(data)
data <- dplyr::select(data, -(startDate), -(endTime), -(trial_type))
data <- dplyr::select(data, submission_id, q1, answer1, q2, answer2, q3, answer3, q4, answer4,
                q5, answer5, q6, answer6, q8, answer8, q9, answer9, q10,
                answer10, q11, answer11, q12, answer12, timeSpent, age, gender, education,
                comments, languages)
data<- dplyr::select(data, submission_id, gender, age, timeSpent, everything())
# data2<- select(data1, starts_with("q"))

data<-rename(data, "01"=answer1, "02"=answer2, "03"=answer3, "04"=answer4, "05"=answer5,
              "06"=answer6, "08"=answer8, "09"=answer9, "10"=answer10,
              "11"=answer11, "12"=answer12, id=submission_id
              )

# tidy up

data<- data %>% tidyr::gather('01', '02', '03', '04', '05', '06', '08', '09', '10', '11',
                         '12', key='question', value = 'response'
                        )

# data<- data %>% tidyr::spread(key = 'question', value = 'response')

# sorting the data frame/tibble
# data <- arrange(data, id)

# Eliminating questions labels and unnecessary data - preliminars 
# data <- dplyr::select(data, -(q1:q12), -(gender), -(age), -(education:languages))

# graphic
data2<-data %>% dplyr::group_by(question) %>% dplyr::summarize(mean.response= mean(response, na.rm = TRUE))

ggplot(data2, aes(question, mean.response)) + geom_point() + geom_line()


# ANOVA repeated measures 1

model1 <- aov(response~question + Error(id/question), data=data)
summary(model1)

SScomp <- (mean(dv[myfactor=="f1"]) -
               + mean(dv[myfactor=="f2"]))^2
dfcomp <- 1
n <- 5 #5 subjects per group
SSerr <- 4.4 #read off the ANOVA table
dferr <- 8 # read off the ANOVA table
MSerr <- SSerr / dferr
Fcomp <- (n * SScomp/2)/MSerr
pcomp <- 1-pf(Fcomp, dfcomp, dferr)
Fcomp


# ANOVA repeated measures 2
model2<-lme(response~question, random = ~1|id/question, data=data, na.action=na.omit
            )
summary(model2)
anova(model2)

# In case question not factor
data$question<-factor(data$question)

# multiple comparisons Tukey correction
glht(model2, linfct=mcp(question="Tukey"))

# ANOVA repeated measures 3

