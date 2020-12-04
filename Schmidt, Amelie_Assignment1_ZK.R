#Assignment 1
library(psych) 
library(car) 
library(tidyverse)
library(gridExtra)
library(lmtest)

data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")
View(data_sample_1)

#custom function for creating coefficient tables
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

#Describtives
data_sample_1 %>% 
  describe()

#pain
data_sample_1 %>% 
  ggplot() + 
  aes(x = pain) + 
  geom_histogram(bins = 50)

#sex 
data_sample_1 %>% 
  ggplot()+
  aes(x = sex, y = pain)+
  geom_boxplot()
#age
data_sample_1 %>% 
  ggplot()+
  aes(x = age)+
  geom_bar()+
  geom_histogram()

data_sample_1 %>% 
  ggplot()+
  aes(x = age, y = pain)+
  geom_point()
data_sample_1 %>% 
  mutate(rownum = row.names(data_sample_1)) %>%
  ggplot()+
  aes(x = age, y = pain, label = rownum)+
  geom_point() +
  geom_label()

#STAI_trait
data_sample_1 %>% 
  ggplot()+
  aes(x = STAI_trait)+
  geom_bar()+
  geom_histogram()

data_sample_1 %>% 
  ggplot()+
  aes(x = STAI_trait, y = pain)+
  geom_point()

data_sample_1 %>% 
  mutate(rownum = row.names(data_sample_1)) %>%
  ggplot()+
  aes(x = STAI_trait, y = pain, label = rownum)+
  geom_point() +
  geom_label()

#pain_cat
data_sample_1 %>% 
  ggplot()+
  aes(x = pain_cat)+
  geom_bar()+
  geom_histogram()

data_sample_1 %>% 
  ggplot()+
  aes(x = pain_cat, y = pain)+
  geom_point()

#cortisol_serum
data_sample_1 %>% 
  ggplot()+
  aes(x = cortisol_serum)+
  geom_bar()+
  geom_histogram()

data_sample_1 %>% 
  ggplot()+
  aes(x =cortisol_serum, y = pain)+
  geom_point()
#cortisol_salvia
data_sample_1 %>% 
  ggplot()+
  aes(x = cortisol_saliva)+
  geom_bar()+
  geom_histogram()

data_sample_1 %>% 
  ggplot()+
  aes(x =cortisol_saliva, y = pain)+
  geom_point()
#mindfulness
data_sample_1 %>% 
  ggplot()+
  aes(x = mindfulness)+
  geom_bar()+
  geom_histogram()

data_sample_1 %>% 
  ggplot()+
  aes(x = mindfulness, y = pain)+
  geom_point()

#note: Age exclude row number 93 value 444 --> not realistic 
#note: STAI_trait exclude row number 150 value 3.90 since scale of State Trait Anxiety Inventory ranges from 20 to 80
#Conclusion: exclude rows 93 and 150
data_corr <- data_sample_1 %>% 
  slice(-c(93,150))

data_corr %>% 
  summary()
#pain
data_corr %>% 
  ggplot() + 
  aes(x = pain) + 
  geom_histogram(bins = 50)

#sex
data_corr %>% 
  ggplot()+
  aes(x = sex, y = pain)+
  geom_boxplot()
#age
data_corr %>% 
  ggplot()+
  aes(x = age)+
  geom_bar()+
  geom_histogram()

data_corr %>% 
  ggplot()+
  aes(x = age, y = pain)+
  geom_point()

#STAI_trait
data_corr %>% 
  ggplot()+
  aes(x = STAI_trait)+
  geom_bar()+
  geom_histogram()

data_corr %>% 
  ggplot()+
  aes(x = STAI_trait, y = pain)+
  geom_point()

#pain_cat
data_corr %>% 
  ggplot()+
  aes(x = pain_cat)+
  geom_bar()+
  geom_histogram()

data_corr %>% 
  ggplot()+
  aes(x = pain_cat, y = pain)+
  geom_point()

#cortisol_serum
data_corr %>% 
  ggplot()+
  aes(x = cortisol_serum)+
  geom_bar()+
  geom_histogram()

data_corr %>% 
  ggplot()+
  aes(x =cortisol_serum, y = pain)+
  geom_point()
#cortisol_saliva
data_corr %>% 
  ggplot()+
  aes(x = cortisol_saliva)+
  geom_bar()+
  geom_histogram()

data_corr %>% 
  ggplot()+
  aes(x =cortisol_saliva, y = pain)+
  geom_point()
#mindfulness
data_corr %>% 
  ggplot()+
  aes(x = mindfulness)+
  geom_bar()+
  geom_histogram()

data_corr %>% 
  ggplot()+
  aes(x = mindfulness, y = pain)+
  geom_point()


#Step 2: Influential outliers
#Model 1: Age and sex
mod1 <- lm(pain ~ sex + age, data = data_corr)
data_corr %>%
  ggplot() +
  aes(x = age, y = pain) +
  geom_point() +
  geom_smooth(method = "lm", se = F)
data_corr %>%
  ggplot() +
  aes(x = sex, y = pain) +
  geom_point() +
  geom_smooth(method = "lm", se = F)
summary(mod1)
#residual leverage plot
lm(pain ~ age + sex, data = data_corr)
mod1 %>% 	
  plot(which = 5)	

#cook's distance cook's distance threshold 4/N
4/158
mod1 %>% 	
  plot(which = 4)
#99, 127,140
data_corr %>% 
  slice(99, 127, 140)
#decision: keep them to be closer to reality

#Model 2: All predictors
mod2 <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = data_corr)
lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = data_corr)
summary(mod2)
#residual leverage plot
mod2 %>% 	
  plot(which = 5)	

#cook's distance --> cook's distance threshold 4/N
4/158
mod2 %>% 	
  plot(which = 4)

data_corr %>% 
  slice(68, 99, 113)
#decision: keep them to be closer to reality

#Check assumptions Model 2
#Normality: QQ plot, histogram of residuals, skew & kurtosis 
mod2%>% 
  plot(which=2)

residuals_mod2 = enframe (residuals(mod2))
residuals_mod2 %>% 
  ggplot ()+
  aes(x= value)+
  geom_histogram()

describe(residuals(mod2))

#Linearity
mod2 %>% 
  residualPlots()

#homoscedasticity
mod2%>% 
  plot(which=3)

mod2 %>% 
  ncvTest()

mod2 %>% 
  bptest()

#multicollinearity
mod2 %>% 
  vif()
#cross tab
data_corr %>% 
  select(pain, age, STAI_trait, pain_cat, mindfulness, cortisol_serum, cortisol_saliva, IQ, weight,household_income) %>% 
  cor()
#cortisol saliva and serum correlate highly, exclude cortisol_saliva from model, since cortisol_serum is reliably related to stress


#Model 3 without cortisol_saliva
mod3 <- lm (pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_corr)

lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_corr)
summary(mod3)
#residual leverage plot
mod3 %>% 	
  plot(which = 5)	

#cook's distance --> cook's distance threshold 4/N
4/158
mod3 %>% 	
  plot(which = 4)

data_corr %>% 
  slice(68, 99, 113)

#Check assumptions Model 3
#Normality: QQ plot, histogram of residuals, skew & kurtosis 
mod3%>% 
  plot(which=2)

residuals_mod3 = enframe (residuals(mod3))
residuals_mod3 %>% 
  ggplot ()+
  aes(x= value)+
  geom_histogram()

describe(residuals(mod3))

#Linearity
mod3 %>% 
  residualPlots()

#homoscedasticity
mod3%>% 
  plot(which=3)

mod3 %>% 
  ncvTest()

mod3 %>% 
  bptest()

#multicollinearity
mod3 %>% 
  vif()

#Comparison models
summary(mod1)
summary(mod3)

coef_table(mod1)
coef_table(mod3)
confint(mod1)
confint(mod3)
#Model fit
AIC(mod1)
AIC(mod3)
#Models are significantly different, Model 3 significantly better 

#Anova
anova(mod1, mod3)
#Anova: F(4, 151) = 31.985, p < .001
