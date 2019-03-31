library(pacman)
p_load(tidyverse, psych, lsr, lme4, brms, sjPlot,
       stringr, ggplot2, lsmeans, HDInterval, stringr)

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
      ioed<- ioed %>% mutate(item_HD1 = 
                               "How Healthcare system in your country works")
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

ioed <- ioed %>% 
  dplyr::select(submission_id, item_HD:timeSpent, pre_high, pre_low)

#creating a checkpoint
ioed1 <- ioed

#calculating the magnitude of illusion
ioed <- ioed %>% 
  mutate(ioed_high = pre_high-post_high,
         ioed_low = pre_low-post_low)

ioed <- ioed %>% 
  gather(ioed_high, ioed_low,
         key='desirability', value = 'magnitude') %>%
  mutate(desirability = as.factor(desirability)) %>%  
  gather(RT1, RT2, key = 'order', value = 'rt')
  # distinct(rt, .keep_all = TRUE)

#deleting the duplicates
ioed <- ioed %>%
  slice(1:(nrow(ioed)/4), (((nrow(ioed)/4)*3)+1):(nrow(ioed))) %>% 
  mutate(item_HD = as.character(item_HD),
         item_LD = as.character(item_LD))
#gather by low or high desirability
ioed2 <- ioed %>% 
  gather(item_HD, item_LD, key = "condition", value = "item") %>% 
  mutate(item = as.factor(item))

#deleting duplicates
#ioed2 contains the magnitude of illusion (difference bethween the first and the second measurements)
ioed2 <- ioed2 %>% 
  slice(1:(nrow(ioed2)/4), (((nrow(ioed2)/4)*3)+1):(nrow(ioed2))) %>% 
  mutate(item = as.factor(item))
  
#ioed3 contains raw measurements of understanding to explore the ioed raw effect (if the second measurements were actually lower than the first ones)
#gathered by high and low desirability
ioed3 <- ioed1 %>% 
  gather(pre_high, post_high, key = "high", value = "score_high") %>% 
  gather(pre_low, post_low, key = "low", value = "score_low") %>%  
  filter(substring(high, 1, 3) == substring(low, 1, 3))

#ioed4 contains ... same as above
#gathered by pre and post measurements
ioed4 <- ioed1 %>% 
  gather(pre_high, pre_low, key = "pre", value = "score_pre") %>% 
  gather(post_high, post_low, key = "post", value = "score_post")

#tidying duplicate entries
#setting time and illusion variable as factors
ioed4 <- ioed4 %>%  
  filter(str_match(ioed4$pre, ".*[:punct:](.*)")[,2] == 
           str_match(ioed4$post, ".*[:punct:](.*)")[,2]) %>% 
  gather(score_pre, score_post, key = "time", value = "value") %>% 
  mutate(time = as.factor(time),
         illusion = as.factor(c(replicate(nrow(ioed4)/4, "high"), 
                                replicate(nrow(ioed4)/4, "low"), 
                                replicate(nrow(ioed4)/4, "high"), 
                                replicate(nrow(ioed4)/4, "low"))))

#linear model for testing the raw illusion of explanatory depth
#summary explores the interaction of time and illusion

model_ilu <- glm(formula = value ~ time*illusion, data = ioed4) 
summary(model_ilu)
plot_model(model_ilu, type = "int")
plot_model(model_ilu, type = "eff")

model1 <- glm(data=ioed2, 
              formula=magnitude~rt + desirability + rt*desirability)

model2 <- glm(data=ioed2, 
              formula=magnitude~rt + desirability + rt*desirability +
                (1 + item))

summary(model1)
summary(model2)

plot_model(model2, type = "int")
plot_model(model2, type = "eff")

BF_model1 <- BayesFactor::lmBF(data=as.data.frame(ioed2), 
                               formula=magnitude~rt + desirability)

post_samples_mod1 <- BayesFactor::posterior(model = BF_model1, iterations = 5000)

get_prior(data=ioed2, 
          formula=magnitude~rt + desirability + rt*desirability +
            (1 + item))

priors <- c(set_prior("normal(0, .10)", class = "b", 
                       coef = "desirabilityioed_low"),
            set_prior("normal(0.6, .10)", class = "Intercept"),
            set_prior("uniform(0,1500000)", class = "b", coef = "rt"))

model_bay <- brm(data=ioed2, 
                 formula=magnitude~rt + desirability + rt*desirability +
                   (1 + item),
                 prior = priors,
                 iter = 10000,
                 warmup = floor(2000))

posterior_samples(model_bay)

summary(model_bay)

int_plot <- plot_model(model1, type = "pred", terms = c("rt", "desirability"))
int_plot
plot_model(model1, type = "int")
plot_model(model1, type = "eff")

plot_model(model1, type = "pred", terms = c("desirability", "rt"))

#RESULTS
#The results don't show any significant raw effect of illusion of explanatory depth
#The difference between the low desirability items and the high desirability items has a tendency towards significance
#With the low desirability items, we found tendency towards a negative effect of illusion, meaning the participants' initial rating of their knowledge estimation was lower than the post rating (after the explanation they thought they know better than what they thought beforehand), which is in accordance to the existing literature (Gaviria et al. 2017
#
#The results of the model comparing reaction time (rt) and the magnitude of illusion show that the reaction time has no effect on the magnitude of illusion.