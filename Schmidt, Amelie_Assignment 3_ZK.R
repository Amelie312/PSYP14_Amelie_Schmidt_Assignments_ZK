#Assignment 3
library(psych) 
library(car) 
library(lmtest) 	
library(boot) 
library(tidyverse) 
library(cAIC4) 
library(r2glmm) 
library(lme4)
library(lmerTest) 
library(MuMIn) 
library(optimx)

data_3 = read.csv ('https://tinyurl.com/ha-dataset3')
data_4 = read.csv('https://tinyurl.com/ha-dataset4')
View(data_3)
View(data_4)

data_3 %>% 
  describe()

#note: sex typo in ID 182 femlae 
# correct typo ID 182
data_3[data_3 == "femlae"] <- "female"

#pain
data_3%>% 
  ggplot() + 
  aes(x = pain) + 
  geom_histogram(bins = 50)

#sex 
data_3 %>% 
  ggplot()+
  aes(x = sex, y = pain)+
  geom_boxplot()

t.test(pain ~ sex, data = data_3)
#t test non significant, no differences between gender

#age
data_3 %>% 
  ggplot()+
  aes(x = age)+
  geom_bar()+
  geom_histogram()

data_3 %>% 
  ggplot()+
  aes(x = age, y = pain)+
  geom_point()
data_3 %>% 
  mutate(rownum = row.names(data_3)) %>%
  ggplot()+
  aes(x = age, y = pain, label = rownum)+
  geom_point() +
  geom_label()

#STAI_trait
data_3 %>% 
  ggplot()+
  aes(x = STAI_trait)+
  geom_bar()+
  geom_histogram()

data_3 %>% 
  ggplot()+
  aes(x = STAI_trait, y = pain)+
  geom_point()

data_3 %>% 
  mutate(rownum = row.names(data_3)) %>%
  ggplot()+
  aes(x = STAI_trait, y = pain, label = rownum)+
  geom_point() +
  geom_label()

#pain_cat
data_3 %>% 
  ggplot()+
  aes(x = pain_cat)+
  geom_bar()+
  geom_histogram()

data_3 %>% 
  ggplot()+
  aes(x = pain_cat, y = pain)+
  geom_point()

#cortisol_serum
data_3 %>% 
  ggplot()+
  aes(x = cortisol_serum)+
  geom_bar()+
  geom_histogram()

data_3 %>% 
  ggplot()+
  aes(x =cortisol_serum, y = pain)+
  geom_point()
#cortisol_saliva
data_3 %>% 
  ggplot()+
  aes(x = cortisol_saliva)+
  geom_bar()+
  geom_histogram()

data_3%>% 
  ggplot()+
  aes(x =cortisol_saliva, y = pain)+
  geom_point()
#mindfulness
data_3 %>% 
  ggplot()+
  aes(x = mindfulness)+
  geom_bar()+
  geom_histogram()

data_3 %>% 
  ggplot()+
  aes(x = mindfulness, y = pain)+
  geom_point()

#weight
data_3%>% 
  ggplot()+
  aes(x = weight)+
  geom_bar()+
  geom_histogram()

data_3 %>% 
  ggplot()+
  aes(x = weight, y = pain)+
  geom_point()
#IQ
data_3 %>% 
  ggplot()+
  aes(x = IQ)+
  geom_bar()+
  geom_histogram()

data_3 %>% 
  ggplot()+
  aes(x = IQ, y = pain)+
  geom_point()

#household_income
data_3 %>% 
  ggplot()+
  aes(x = household_income)+
  geom_bar()+
  geom_histogram()

data_3 %>% 
  ggplot()+
  aes(x = household_income, y = pain)+
  geom_point()


#Dataset 4
data_4 %>% 
  describe()
#note: ID 80 mindfulness >6 even though scale ranges from 1-6 --> exclude
data_4 %>% 
  ggplot()+
  aes(x = mindfulness)+
  geom_bar()+
  geom_histogram()

data_4 %>% 
  ggplot()+
  aes(x = mindfulness, y = pain)+
  geom_point()

data_4_corr <- data_4%>% 
  slice(-80)

#pain
data_4%>% 
  ggplot() + 
  aes(x = pain) + 
  geom_histogram(bins = 50)

#sex 
data_4 %>% 
  ggplot()+
  aes(x = sex, y = pain)+
  geom_boxplot()

t.test(pain ~ sex, data = data_4)
#t test non significant, no differences between gender

#age
data_4 %>% 
  ggplot()+
  aes(x = age)+
  geom_bar()+
  geom_histogram()

data_4 %>% 
  ggplot()+
  aes(x = age, y = pain)+
  geom_point()
data_4 %>% 
  mutate(rownum = row.names(data_4)) %>%
  ggplot()+
  aes(x = age, y = pain, label = rownum)+
  geom_point() +
  geom_label()

#STAI_trait
data_4 %>% 
  ggplot()+
  aes(x = STAI_trait)+
  geom_bar()+
  geom_histogram()

data_4 %>% 
  ggplot()+
  aes(x = STAI_trait, y = pain)+
  geom_point()

data_4 %>% 
  mutate(rownum = row.names(data_4)) %>%
  ggplot()+
  aes(x = STAI_trait, y = pain, label = rownum)+
  geom_point() +
  geom_label()

#pain_cat
data_4 %>% 
  ggplot()+
  aes(x = pain_cat)+
  geom_bar()+
  geom_histogram()

data_4 %>% 
  ggplot()+
  aes(x = pain_cat, y = pain)+
  geom_point()

#cortisol_serum
data_4 %>% 
  ggplot()+
  aes(x = cortisol_serum)+
  geom_bar()+
  geom_histogram()

data_4 %>% 
  ggplot()+
  aes(x =cortisol_serum, y = pain)+
  geom_point()
#cortisol_saliva
data_4 %>% 
  ggplot()+
  aes(x = cortisol_saliva)+
  geom_bar()+
  geom_histogram()

data_4%>% 
  ggplot()+
  aes(x =cortisol_saliva, y = pain)+
  geom_point()
#mindfulness
data_4 %>% 
  ggplot()+
  aes(x = mindfulness)+
  geom_bar()+
  geom_histogram()

data_4 %>% 
  ggplot()+
  aes(x = mindfulness, y = pain)+
  geom_point()

#weight
data_4%>% 
  ggplot()+
  aes(x = weight)+
  geom_bar()+
  geom_histogram()

data_4 %>% 
  ggplot()+
  aes(x = weight, y = pain)+
  geom_point()
#IQ
data_4 %>% 
  ggplot()+
  aes(x = IQ)+
  geom_bar()+
  geom_histogram()

data_4 %>% 
  ggplot()+
  aes(x = IQ, y = pain)+
  geom_point()

#household_income
data_4 %>% 
  ggplot()+
  aes(x = household_income)+
  geom_bar()+
  geom_histogram()

data_4 %>% 
  ggplot()+
  aes(x = household_income, y = pain)+
  geom_point()

#custom functions
coef_table = function(model) {
  require (lm.beta)
  mod_sum = summary (model)
  mod_sum_p_values = as.character ( round (mod_sum$coefficients[,
                                                                4], 3))
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values !=
                     "1"] = substr (mod_sum_p_values[mod_sum_p_values != "0" &
                                                       mod_sum_p_values != "1"], 2, nchar (mod_sum_p_values[mod_sum_p_values !=
                                                                                                              "0" & mod_sum_p_values != "1"]))
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  mod_sum_table = cbind ( as.data.frame ( round ( cbind ( coef (model),
                                                          confint (model), c (0, lm.beta (model)$standardized.coefficients[ c (2: length (model$coefficients))])),
                                                  2)), mod_sum_p_values)
  names (mod_sum_table) = c ("b", "95%CI lb", "95%CI ub", "Std.Beta",
                             "p-value")
  mod_sum_table["(Intercept)", "Std.Beta"] = "0"
  return (mod_sum_table)
}
stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object,"y"))
  sdx <- apply(getME(object,"X"), 2, sd)
  sc <- fixef(object)*sdx/sdy
  se.fixef <- coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))
}

data_3 %>%
  mutate(hospital = factor(hospital))
data_4_corr %>%
  mutate(hospital = factor(hospital))


mod_3_rand_int_ <- lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1|hospital), data = data_3)

lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1|hospital), data = data_3)
summary(mod_3_rand_int_)
confint(mod_3_rand_int_)
stdCoef.merMod(mod_3_rand_int_)

#most influential predictor: cortisol_serum


r2beta(mod_3_rand_int_, method = "nsj", data = data_3)
r.squaredGLMM(mod_3_rand_int_)

# Predictions
predictions_int = predict(mod_3_rand_int_, newdata = data_4_corr, allow.new.levels = TRUE)
pain_prediction_int = cbind(data_4_corr, predictions_int)
pain_prediction_int

#RSS
RSS_int = sum((data_4_corr[, "pain"] - predictions_int)^2)
RSS_int

#TSS
mod_mean <- lm(pain ~ 1, data = data_4_corr)

TSS_int = sum((data_4_corr$pain - predict(mod_mean))^2)
TSS_int

R2 = 1-(RSS_int/TSS_int)
R2

#most influential predictor: cortisol_serum
#random slope model
mod_3_slope = lmer(pain ~ cortisol_serum + (cortisol_serum|hospital), data = data_3)
summary(mod_3_slope)

data_3 = data_3 %>%
  mutate(pred_int = predict(mod_3_rand_int_),
         pred_slope = predict(mod_3_slope))

data_3 %>%
  ggplot() +
  aes(y = pain, x = cortisol_serum, group = hospital)+
  geom_point(aes(color = hospital), size = 4, col = "#6E6E6E") +
  geom_line(color='black', aes(y=pred_slope, x=cortisol_serum))+
  facet_wrap( ~ hospital, ncol = 2)




