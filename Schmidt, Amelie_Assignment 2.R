#Assignment 2
library(psych)
library(car) 
library(lmtest) 	
library(tidyverse) 

home_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")
View(home_sample_1)


#Descriptives

home_sample_1 %>% 
  describe()

#pain
home_sample_1 %>% 
  ggplot() + 
  aes(x = pain) + 
  geom_histogram(bins = 50)

#sex 
home_sample_1 %>% 
  ggplot()+
  aes(x = sex, fill = pain)+
  geom_boxplot()

#age
home_sample_1 %>% 
  ggplot()+
  aes(x = age)+
  geom_bar()+
  geom_histogram()

home_sample_1 %>% 
  ggplot()+
  aes(x = age, y = pain)+
  geom_point()
home_sample_1 %>% 
  mutate(rownum = row.names(data_sample_1)) %>%
  ggplot()+
  aes(x = age, y = pain, label = rownum)+
  geom_point() +
  geom_label()

#STAI_trait
home_sample_1 %>% 
  ggplot()+
  aes(x = STAI_trait)+
  geom_bar()+
  geom_histogram()

home_sample_1 %>% 
  ggplot()+
  aes(x = STAI_trait, y = pain)+
  geom_point()

home_sample_1 %>% 
  mutate(rownum = row.names(data_sample_1)) %>%
  ggplot()+
  aes(x = STAI_trait, y = pain, label = rownum)+
  geom_point() +
  geom_label()

#pain_cat
home_sample_1 %>% 
  ggplot()+
  aes(x = pain_cat)+
  geom_bar()+
  geom_histogram()

home_sample_1 %>% 
  ggplot()+
  aes(x = pain_cat, y = pain)+
  geom_point()

#cortisol_serum
home_sample_1 %>% 
  ggplot()+
  aes(x = cortisol_serum)+
  geom_bar()+
  geom_histogram()

home_sample_1 %>% 
  ggplot()+
  aes(x =cortisol_serum, y = pain)+
  geom_point()
#cortisol_saliva
home_sample_1 %>% 
  ggplot()+
  aes(x = cortisol_saliva)+
  geom_bar()+
  geom_histogram()

home_sample_1 %>% 
  ggplot()+
  aes(x =cortisol_saliva, y = pain)+
  geom_point()
#mindfulness
home_sample_1 %>% 
  ggplot()+
  aes(x = mindfulness)+
  geom_bar()+
  geom_histogram()

home_sample_1 %>% 
  ggplot()+
  aes(x = mindfulness, y = pain)+
  geom_point()

#weight
home_sample_1 %>% 
  ggplot()+
  aes(x = weight)+
  geom_bar()+
  geom_histogram()

home_sample_1 %>% 
  ggplot()+
  aes(x = weight, y = pain)+
  geom_point()
#IQ
home_sample_1 %>% 
  ggplot()+
  aes(x = IQ)+
  geom_bar()+
  geom_histogram()

home_sample_1 %>% 
  ggplot()+
  aes(x = IQ, y = pain)+
  geom_point()

#household_income
home_sample_1 %>% 
  ggplot()+
  aes(x = household_income)+
  geom_bar()+
  geom_histogram()

home_sample_1 %>% 
  ggplot()+
  aes(x = household_income, y = pain)+
  geom_point()
#note: Age exclude row number 93 value 444 --> not realistic 
#note: STAI_trait exclude row number 150 value 3.90 since scale of State Trait Anxiety Inventory ranges from 20 to 80
# exclude rows 93 and 150 (age 444, STAI 3.90)
home_corr <- home_sample_1 %>% 
  slice(-c(93,150))

home_corr %>% 
  summary()

#pain
home_corr %>% 
  ggplot() + 
  aes(x = pain) + 
  geom_histogram(bins = 50)

#sex 
home_corr %>% 
  ggplot()+
  aes(x = sex, fill = pain)+
  geom_boxplot()

#age
home_corr %>% 
  ggplot()+
  aes(x = age)+
  geom_bar()+
  geom_histogram()

home_corr %>% 
  ggplot()+
  aes(x = age, y = pain)+
  geom_point()
home_corr %>% 
  mutate(rownum = row.names(data_sample_1)) %>%
  ggplot()+
  aes(x = age, y = pain, label = rownum)+
  geom_point() +
  geom_label()

#STAI_trait
home_corr %>% 
  ggplot()+
  aes(x = STAI_trait)+
  geom_bar()+
  geom_histogram()

home_corr %>% 
  ggplot()+
  aes(x = STAI_trait, y = pain)+
  geom_point()

home_corr %>% 
  mutate(rownum = row.names(data_sample_1)) %>%
  ggplot()+
  aes(x = STAI_trait, y = pain, label = rownum)+
  geom_point() +
  geom_label()

#pain_cat
home_corr %>% 
  ggplot()+
  aes(x = pain_cat)+
  geom_bar()+
  geom_histogram()

home_corr %>% 
  ggplot()+
  aes(x = pain_cat, y = pain)+
  geom_point()

#cortisol_serum
home_corr %>% 
  ggplot()+
  aes(x = cortisol_serum)+
  geom_bar()+
  geom_histogram()

home_corr %>% 
  ggplot()+
  aes(x =cortisol_serum, y = pain)+
  geom_point()
#cortisol_saliva
home_corr %>% 
  ggplot()+
  aes(x = cortisol_saliva)+
  geom_bar()+
  geom_histogram()

home_corr %>% 
  ggplot()+
  aes(x =cortisol_saliva, y = pain)+
  geom_point()
#mindfulness
home_corr %>% 
  ggplot()+
  aes(x = mindfulness)+
  geom_bar()+
  geom_histogram()

home_corr %>% 
  ggplot()+
  aes(x = mindfulness, y = pain)+
  geom_point()

#weight
home_corr %>% 
  ggplot()+
  aes(x = weight)+
  geom_bar()+
  geom_histogram()

home_corr %>% 
  ggplot()+
  aes(x = weight, y = pain)+
  geom_point()
#IQ
home_corr %>% 
  ggplot()+
  aes(x = IQ)+
  geom_bar()+
  geom_histogram()

home_corr%>% 
  ggplot()+
  aes(x = IQ, y = pain)+
  geom_point()

#household_income
home_corr %>% 
  ggplot()+
  aes(x = household_income)+
  geom_bar()+
  geom_histogram()

home_corr %>% 
  ggplot()+
  aes(x = household_income, y = pain)+
  geom_point()

#cross tab
home_corr %>% 
  select(pain, age, STAI_trait, pain_cat, mindfulness, cortisol_serum, cortisol_saliva, IQ, weight,household_income) %>% 
  cor()
#cortisol saliva and serum correlate highly


#Step 2: Influential outliers
#custom function for coefficient tables
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


#Initial model
mod_initial <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = home_corr)
summary(mod_initial)


lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = home_corr)
coef_table(mod_initial)


#residual leverage
mod_initial %>% 	
  plot(which = 5)	

#cook's distance
mod_initial %>% 	
  plot(which = 4)
#cook's distance --> cook's distance threshold 4/N
4/158
#3, 102, 113
home_corr %>% 
  slice(3, 102, 113)

#decision: keep them to be closer to reality, no indication, that the values are mistaken

#Backward regression
home_backward <- step(mod_initial, direction = "backward")
#final model predictors included: sex, household_income, mindfulness, age, pain_cat, cortisol_serum
#final model predictors excluded: STAI, weight, IQ - AIC 44.38

mod_home_backward <- lm(pain ~ sex + household_income + mindfulness + age + pain_cat + cortisol_serum, data = home_corr)
summary(mod_home_backward)

coef_table(mod_home_backward)

#residual leverage
mod_home_backward %>% 	
  plot(which = 5)	

#cook's distance
mod_home_backward %>% 	
  plot(which = 4)
#cook's distance --> cook's distance threshold 4/N
4/158
#102, 113, 147
home_corr %>% 
  slice(102, 113, 147)

#Assumption test
#Normality: QQ plot, histogram of residuals, skew & kurtosis 
mod_home_backward%>% 
  plot(which=2)

residuals_mod_home_backward = enframe (residuals(mod_home_backward))
residuals_mod_home_backward %>% 
  ggplot ()+
  aes(x= value)+
  geom_histogram()

describe(residuals(mod_home_backward))
#Normality alright: skew -0.11, kurtosis -0.4

#Linearity
mod_home_backward %>% 
  residualPlots()

#homoscedasticity
mod_home_backward%>% 
  plot(which=3)

mod_home_backward %>% 
  ncvTest()

mod_home_backward %>% 
  bptest()


#multicollinearity
mod_home_backward %>% 
  vif()

#Model theory
mod_home_theory <- lm (pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = home_corr)

lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = home_corr)
summary(mod_home_theory)
coef_table(mod_home_theory)
#assumptions mod_home_theory tested in assignment 1

#Model comparison:
AIC(mod_initial)
AIC(mod_home_backward)
AIC(mod_home_theory)
#Anova initial and backward model
anova(mod_initial, mod_home_backward)

#Predictions
home_sample_2 = read.csv("https://tinyurl.com/ha-dataset2")
view(home_sample_2)
summary(home_sample_2)
describe(home_sample_2)
# mindfulness: one value is 7.17 even though the scale ranges from 1 to 6
# exclude ID 8
home_2_corr <- home_sample_2 %>% 
  slice(-8)
# dataset only used for testing the models, therefore no in-depth descriptives, outliers depict real data

#Prediction
predictions_initial = predict(mod_initial, newdata = home_2_corr)
pain_initial_prediction = cbind(home_2_corr, predictions_initial)
pain_initial_prediction
pain_initial_prediction %>% 
  select(pain) %>% 
  describe()

predictions_backward = predict(mod_home_backward, newdata = home_2_corr)
pain_backward_prediction = cbind(home_2_corr, predictions_backward)
pain_backward_prediction
pain_backward_prediction %>% 
  select(pain) %>% 
  describe()

predictions_theory = predict(mod_home_theory, newdata = home_2_corr)
pain_theory_prediction = cbind(home_2_corr, predictions_theory)
pain_theory_prediction
pain_theory_prediction %>% 
  select(pain) %>% 
  describe()

RSS_test_initial = sum((home_2_corr[, "pain"] - predictions_initial)^2)
RSS_test_back= sum((home_2_corr[, "pain"] - predictions_backward)^2)
RSS_test_theory= sum((home_2_corr[, "pain"] - predictions_theory)^2)

RSS_test_initial
RSS_test_back
RSS_test_theory
