rm(list = ls())
library(pacman)
p_load(dplyr, foreign, psych, lsr, nlme, MBESS, gmodels,
     multcomp, ez, stringr, htmlwidgets, ggplot2, lsmeans)

ioed <- read.csv("pilotexp2.csv")
ioed <- as_tibble(ioed)
ioed <- dplyr::select(ioed, submission_id, q1, answer1, q2, answer2, q3, answer3, q4, 
                      answer4, q5, answer5, q6, answer6, q7, answer7, q8, answer8, q9, 
                      answer9, q10, answer10, q11, answer11, q12, answer12, item_HD, 
                      explanation_high, post_high, RT1, item_LD, explanation_low, 
                      post_low, RT2, timeSpent, age, gender, education, comments, 
                      languages)

ioed1 <- dplyr::select(ioed, -(age:languages))

pre_rateHD1 <- NA
pre_rateLD1 <- NA
pre_rateHD2 <- NA
pre_rateLD2 <- NA

for (i in 1:nrow(ioed1)) {
  for (j in seq(2, 24, by = 2)) {
    if (ioed1[i,j] == "How Healthcare system in your country works") {
      pre_rateHD1[i] = ioed1[i,j+1]
      ioed1<- mutate(ioed1, 
                     item_HD1 = "How Healthcare system in your country works"
      )
    }
  }
}
      
for (i in 1:nrow(ioed1)) {
  for (j in seq(2, 24, by = 2)) {
    if (ioed1[i,j] =="How online privacy policies work") {
      pre_rateHD2[i] = ioed1[i,j+1]
      ioed1<- mutate(ioed1,
                     item_HD2 = "How online privacy policies work"
                     )
    }
  }
}
      
for (i in 1:nrow(ioed1)) {
  for (j in seq(2, 24, by = 2)) {
    if (ioed1[i,j] == "How an incinerator works") {
      pre_rateLD1[i] = ioed1[i,j+1]
      ioed1<- mutate(ioed1,
                     item_LD1 = "How an incinerator works"
                     )
    }
  }
}
   
for (i in 1:nrow(ioed1)) {
  for (j in seq(2, 24, by = 2)) {
    if (ioed1[i,j] == "How a photocopier makes copies") {
      pre_rateLD2[i] = ioed1[i,j+1]
      ioed1<- mutate(ioed1,
                     item_LD2 = "How a photocopier makes copies"
                     )
    }
  }
}


ioed1 <- mutate(ioed1,
                pre_rateHD1 = as.numeric(as.character(unlist(pre_rateHD1))),
                pre_rateLD1 = as.numeric(as.character(unlist(pre_rateLD1))),
                pre_rateHD2 = as.numeric(as.character(unlist(pre_rateHD2))),
                pre_rateLD2 = as.numeric(as.character(unlist(pre_rateLD2)))
                )

ioed2 <- ioed1 %>% 
  dplyr::select(submission_id, item_HD1, pre_rateHD1, item_LD1, 
                pre_rateLD1, item_HD2, pre_rateHD2, item_LD2, pre_rateLD2,
                item_HD:timeSpent, -explanation_high, -explanation_low)


# High desirable items 
ioed2$item_HD<-str_replace(ioed2$item_HD, c("How high 1"), 
                           c("How Healthcare system in your country works")
                          )
ioed2$item_HD<-str_replace(ioed2$item_HD, c("How How high 2"), 
                           c("How online privacy policies work")
                          )

# Low desirable items 
ioed2$item_LD<-str_replace(ioed2$item_LD, c("How low 1"), 
                           c("How an incinerator works")
)
ioed2$item_LD<-str_replace(ioed2$item_LD, c("How low 2"), 
                           c("How a photocopier makes copies")
)

ioed3 <- ioed2 %>% 
  dplyr::mutate(itempre_HD = NA,
                pre_high = NA,
                itempre_LD = NA,
                pre_low = NA,
                within_factor = NA,
                between_factor = NA)

for (i in 1:nrow(ioed3)) {
  for (j in c(2,6)) {
    if (ioed3[i,j] != ioed3[i,10]) {
      ioed3[i,j] = NA
      ioed3[i,j+1] = NA
    }
  }
}  

for (i in 1:nrow(ioed3)) { 
  for (j in c(4,8)) {
    if (ioed3[i,j] != ioed3[i,13]) {
      ioed3[i,j] = NA
      ioed3[i,j+1] = NA
    }
  }
}

for (i in 1:nrow(ioed3)) { 
  for (j in c(2,6)) {
    if (!is.na(ioed3[i,j])) {
      ioed3[i,17] = ioed3[i,j]
      ioed3[i,18] = ioed3[i,j+1]
    }
  }
}


for (i in 1:nrow(ioed3)) { 
  for (j in c(4,8)) {
    if (!is.na(ioed2[i,j])) {
      ioed3[i,19] = ioed3[i,j]
      ioed3[i,20] = ioed3[i,j+1]
    }
  }
}

ioed3 <- ioed3 %>% 
  dplyr::select(submission_id, itempre_HD:pre_low, -(item_HD1:pre_rateLD2), 
                item_HD:between_factor)


ioed4 <- ioed3 %>% 
  mutate(ioed_high = pre_high-post_high,
         ioed_low = pre_low-post_low)


ioed4 <- ioed4 %>% 
  tidyr::gather(ioed_high, ioed_low,
                key='desirability', value = 'magnitude') 

ioed4 <- ioed4 %>% 
  mutate(desirability = as.factor(desirability))

model1 <- lm(data= ioed3, formula=magnitude ~ desirability)
summary(model1)

int_plot <- plot_model(model1, type = "eff")

# ioed3<- ioed2 %>% tidyr::gather(pre_high, post_high, pre_low, post_low,
#                                 key='question', value = 'response'
#                                 )
# 
# for (i in 1:nrow(ioed3)) { 
#   for (j in 11) {
#     if (1 == stringr::str_count(ioed3[i, 11], "pre")) {
#       ioed3[i,j-2] = "pre"
#     } else {
#       ioed3[i,j-2] = "post"
#     }
#   }
# }
# 
# for (i in 1:nrow(ioed3)) { 
#   for (j in 11) {
#     if (1 == stringr::str_count(ioed3[i, 11], "high")) {
#       ioed3[i,j-1] = "high_desirability"
#     } else {
#       ioed3[i,j-1] = "low_desirability"
#     }
#   }
# }

# Repeated measures ANOVA
expaov <- ezANOVA(ioed3,
           dv = response,
           wid = submission_id,
           within = within_factor,
           between = between_factor,
           detailed = TRUE,
           return_aov = TRUE
           )

expaov$ANOVA$MSE <- expaov$ANOVA$SSd/expaov$ANOVA$DFd
expaov$ANOVA$etasquared <- expaov$ANOVA$SSn/(expaov$ANOVA$SSn+expaov$ANOVA$DFd)


# .2 anova with lme and multple comparisons

model1<-lme(response~question,
            random = ~ 1 | submission_id/question, 
            data=ioed3, na.action=na.omit
)
summary(model1)
anova(model1)

ioed3$question<-as.factor(ioed3$question)

glht(model1, linfct = mcp(question="Tukey"))
lsmeans(model1, ~question) %>% pairs

model2<-lme(response~between_factor+within_factor,
            random = ~ 1 | submission_id/question, 
            data=ioed3, na.action=na.omit
)

lsmeans(model2, ~between_factor) %>% pairs

# 5. Plot

total_pre <- c(ioed3$pre_low, ioed3$pre_high)
total_post <- c(ioed3$post_low, ioed3$post_high)
pre_sd <- sd(total_pre)
post_sd <- sd(total_post)

ioed3 <- mutate(ioed3, withinfactor = as.character(NA))

for (i in 1:nrow(ioed3)) { 
  for (j in 9) {
    if (ioed3[i,j]=="pre") {
      ioed3[i,j+4]=0
    }
    else if (ioed3[i,j]=="post") {
      ioed3[i,j+4]=1
    }
  }
}

plotaov<-  ezPlot(data = ioed3,
                 dv = response,
                 wid = submission_id,
                 within = withinfactor,
                 between = between_factor,
                 x = withinfactor,
                 do_bars = TRUE,
                 do_lines = TRUE,
                 split = between_factor,
                 print_code = TRUE
                 )

# Confidence intervals

plotaov$SE <- sqrt(plotaov$SD^2/plotaov$N)
# plotaov$CI95 <- qt(1-0.025, expaov$ANOVA$DFd)
plotaov$CI95 <- qt(1-0.025, plotaov$N-1)
plotaov$limsup <- plotaov$Mean + plotaov$CI95*plotaov$SE
plotaov$liminf <- plotaov$Mean - plotaov$CI95*plotaov$SE

# gmodels::ci(ioed3$pre_high, confidence = 0.95, alpha = 1 - 0.95)

ggplot(data = plotaov, mapping = aes(y=Mean, x=withinfactor, linetype=between_factor))+
  geom_point()+geom_line(mapping = aes(linetype = between_factor, x=I(as.numeric(withinfactor))))+
  geom_errorbar(mapping = aes(ymin = liminf, ymax = limsup),
                linetype = 1, show.legend = FALSE, width = 0.25, alpha = .5) +
  scale_linetype_discrete(name = "Desirability",
                          labels = c("high", "low")) +
  xlab("Judgement timing") +
  scale_x_discrete(breaks=c("0", "1"),
                   labels=c("pre", "post"))
