library(pacman)
p_load(tidyverse, psych, lsr, lme4, brms, sjPlot,
       stringr, ggplot2, lsmeans, HDInterval)

rm(list = ls())

ioed <- read.csv("results_ioed_test_zadkiel.csv")    #("pilotexp2.csv")
ioed <- as_tibble(ioed) %>% 
  select(submission_id, q1, answer1, q2, answer2, q3, answer3, q4, 
  answer4, q5, answer5, q6, answer6, q7, answer7, q8, answer8, q9, answer9, q10,
  answer10, q11, answer11, q12, answer12, item_HD, explanation_high, post_high, 
  RT1, item_LD, explanation_low, post_low, RT2, timeSpent, age, gender, 
  education, comments, languages)

ioed <- ioed %>% 
  filter(submission_id != 71) %>% 
  filter(submission_id != 72) %>% 
  filter(submission_id != 73) %>% 
  filter(submission_id != 74) %>% 
  filter(submission_id != 75) %>% 
  filter(submission_id != 76) %>% 
  filter(submission_id != 96) %>% 
  filter(submission_id != 106)
  
ioed <- ioed %>% select( -(age:languages))

pre_rateHD1 <- NA
pre_rateLD1 <- NA
pre_rateHD2 <- NA
pre_rateLD2 <- NA

for (i in 1:nrow(ioed)) {
  for (j in seq(2, 24, by = 2)) {
    if (ioed[i,j] == "How Healthcare system in your country works") {
      pre_rateHD1[i] = ioed[i,j+1]
      ioed<- ioed %>% mutate(item_HD1 = "How Healthcare system in your country works")
    }
  }
}
      
for (i in 1:nrow(ioed)) {
  for (j in seq(2, 24, by = 2)) {
    if (ioed[i,j] =="How a zipper works") {
      pre_rateHD2[i] = ioed[i,j+1]
      ioed<- ioed %>% mutate(item_HD2 = "How a zipper works")
    }
  }
}
      
for (i in 1:nrow(ioed)) {
  for (j in seq(2, 24, by = 2)) {
    if (ioed[i,j] == "How an incinerator works") {
      pre_rateLD1[i] = ioed[i,j+1]
      ioed<- ioed %>% mutate(item_LD1 = "How an incinerator works")
    }
  }
}
   
for (i in 1:nrow(ioed)) {
  for (j in seq(2, 24, by = 2)) {
    if (ioed[i,j] == "How piano keys make sounds") {
      pre_rateLD2[i] = ioed[i,j+1]
      ioed<- ioed %>% mutate(item_LD2 = "How piano keys make sounds")
    }
  }
}


ioed <- ioed %>% 
  mutate(pre_rateHD1 = as.numeric(as.character(unlist(pre_rateHD1))),
         pre_rateLD1 = as.numeric(as.character(unlist(pre_rateLD1))),
         pre_rateHD2 = as.numeric(as.character(unlist(pre_rateHD2))),
         pre_rateLD2 = as.numeric(as.character(unlist(pre_rateLD2)))
        )

ioed <- ioed %>% 
  dplyr::select(submission_id, item_HD1, pre_rateHD1, item_LD1, 
                pre_rateLD1, item_HD2, pre_rateHD2, item_LD2, 
                pre_rateLD2, item_HD:timeSpent, -explanation_high, 
                -explanation_low)

ioed <- ioed %>%
# ioed1 <- ioed %>%
  dplyr::mutate(submission_id = as.factor(submission_id),
                itempre_HD = NA,
                pre_high = NA,
                itempre_LD = NA,
                pre_low = NA)


for (i in 1:nrow(ioed)) {
  for (j in c(2,6)) {
    if (ioed[i,j] != ioed[i,10]) {
      ioed[i,j] = NA
      ioed[i,j+1] = NA
    }
  }
}  

for (i in 1:nrow(ioed)) { 
  for (j in c(4,8)) {
    if (ioed[i,j] != ioed[i,13]) {
      ioed[i,j] = NA
      ioed[i,j+1] = NA
    }
  }
}

for (i in 1:nrow(ioed)) { 
  for (j in c(2,6)) {
    if (!is.na(ioed[i,j])) {
      ioed[i,17] = ioed[i,j]
      ioed[i,18] = ioed[i,j+1]
    }
  }
}


for (i in 1:nrow(ioed)) { 
  for (j in c(4,8)) {
    if (!is.na(ioed[i,j])) {
      ioed[i,19] = ioed[i,j]
      ioed[i,20] = ioed[i,j+1]
    }
  }
}

ioed1 <- ioed
# ioed <- ioed1

ioed <- ioed %>% 
  dplyr::select(submission_id, item_HD:timeSpent, pre_high, pre_low)


ioed <- ioed %>% 
  mutate(ioed_high = pre_high-post_high,
         ioed_low = pre_low-post_low)

ioed <- ioed %>% 
  gather(ioed_high, ioed_low,
         key='desirability', value = 'magnitude') %>%
  mutate(desirability = as.factor(desirability)) %>%  
  gather(RT1, RT2, key = 'order', value = 'rt')
  # distinct(rt, .keep_all = TRUE)

ioed <- ioed %>%
  slice(1:(nrow(ioed)/4), (((nrow(ioed)/4)*3)+1):(nrow(ioed))) %>% 
  mutate(item_HD = as.character(item_HD),
         item_LD = as.character(item_LD))

ioed2 <- ioed %>% 
  gather(item_HD, item_LD, key = "condition", value = "item") %>% 
  mutate(item = as.factor(item))

ioed2 <- ioed2 %>% 
  slice(1:(nrow(ioed2)/4), (((nrow(ioed2)/4)*3)+1):(nrow(ioed2))) %>% 
  mutate(item = as.factor(item))
  
  
model1 <- glm(data=ioed2, 
              formula=magnitude~rt + desirability + rt*desirability)

model2 <- glm(data=ioed2, 
              formula=magnitude~rt + desirability + rt*desirability +
                (1 + item))

summary(model1)
summary(model2)

model_bay <- brm(data=ioed2, 
                 formula=magnitude~rt + desirability + rt*desirability)

summary(model_bay)


int_plot <- plot_model(model1, type = "pred", terms = c("rt", "desirability"))
int_plot
plot_model(model1, type = "int")
plot_model(model1, type = "eff")

plot_model(model1, type = "pred", terms = c("desirability", "rt"))
