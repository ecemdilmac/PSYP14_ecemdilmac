# Home Assignment- Ecem Dilmac


#Packages

library(tidyverse)
library(psych)
library(lm.beta)
library(gridExtra)
library(car)
library(lmtest)
library(sandwich)
library(boot)
library(lmboot)
library(cAIC4)
library(r2glmm)
library(lme4)
library(lmerTest)
library(MuMIn)
library(optimx)
library(jtools)

#Custom Functions

coef_table = function(model){	
  require(lm.beta)	
  mod_sum = summary(model)	
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3))		
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"]))		
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"		
  
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(mod), confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])), 2)), mod_sum_p_values)		
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")		
  mod_sum_table["(Intercept)","Std.Beta"] = "0"		
  return(mod_sum_table)	
}	

stdCoef.merMod <- function(object) {	
  sdy <- sd(getME(object,"y"))	
  sdx <- apply(getME(object,"X"), 2, sd)	
  sc <- fixef(object)*sdx/sdy	
  se.fixef <- coef(summary(object))[,"Std. Error"]	
  se <- se.fixef*sdx/sdy	
  return(data.frame(stdcoef=sc, stdse=se))	
}	


#Assignment 1

#Model 1

data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")

View(data_sample_1)

describe(data_sample_1)

data_sample_1 %>%
  summary()

data_sample_1_new <- data_sample_1 %>%
  slice(-c(93,150))

View(data_sample_1_new)


data_sample_1_new %>%
  ggplot() +
  aes(x=age, y=pain) +
  geom_point()

data_sample_1_new %>%
  ggplot() +
  aes(x=sex, y=pain)+
  geom_point()

mod_pain1= lm(pain ~ age + sex, data= data_sample_1_new)

#Normality Assumption 

mod_pain1 %>% plot(which=2)

residuals_mod_pain1=enframe(residuals(mod_pain1))
residuals_mod_pain1 %>% ggplot() + aes(x=value) + geom_histogram()

describe(residuals(mod_pain1))

#Linearity Assumption

mod_pain1 %>% residualPlots()

#Homoscedasticity

mod_pain1 %>% plot (which=3)

mod_pain1 %>% ncvTest()
mod_pain1 %>% bptest()

#No Multicollinearity

mod_pain1 %>% vif()

#All assumptions are met for Model 1

sm=summary(mod_pain1)
sm

AIC(mod_pain1)

confint(mod_pain1)

lm.beta(mod_pain1)

mod_pain1 %>% plot(which=4)

data_sample_1_new %>% slice(c(100,128,141))


# Model 2

View(data_sample_1_new)

mod_pain2= lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data=data_sample_1_new)

#Normality Assumption 

mod_pain2 %>% plot(which=2)

residuals_mod_pain2=enframe(residuals(mod_pain2))
residuals_mod_pain2 %>% ggplot() + aes(x=value) + geom_histogram()

describe(residuals(mod_pain2))

#Linearity Assumption

mod_pain2 %>% residualPlots()

#Homoscedasticity

mod_pain2 %>% plot (which=3)

mod_pain2 %>% ncvTest()
mod_pain2 %>% bptest()

#No Multicollinearity

mod_pain2 %>% vif()

#There is multicollinearity so one of the cortisol measures should be removes. In line with theory I will remove "cortisol_saliva" since serum cortisol is more reliable for stress prediction. So we also run all the 

#New Model 2

mod_pain2_new= lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data= data_sample_1_new )

#Normality Assumption 

mod_pain2_new %>% plot(which=2)

residuals_mod_pain2_new=enframe(residuals(mod_pain2_new))
residuals_mod_pain2_new %>% ggplot() + aes(x=value) + geom_histogram()

describe(residuals(mod_pain2_new))

#Linearity Assumption

mod_pain2_new %>% residualPlots()

#Homoscedasticity

mod_pain2_new %>% plot (which=3)

mod_pain2_new %>% ncvTest()
mod_pain2_new %>% bptest()

#No Multicollinearity

mod_pain2_new %>% vif()

#All assumptions met for New Model 2

sm2_new= summary (mod_pain2_new) 
sm2_new

AIC(mod_pain2_new)

confint(mod_pain2_new)

lm.beta(mod_pain2_new)

mod_pain2_new %>% plot(which=4) 

#Regression Equation
Y= 1.84 + (-0.04)*age + 0.27*sex + (-0.02)*STAI_trait + 0.12*pain_cat + (-0.28)*minfulness+ 0.56*cortisol_serum

#Model Comparison

anova(mod_pain1, mod_pain2_new)
#ANOVA F test is significant (p<.001), so one model should be significantly better.
#According to R2 adjusted values, new model 2 explained %47 of the variance in pain, while model 1 explained 5%. 
#AIC scores showed that the difference of variance explained by the new model 2 is significantly better than model 1. 
#We can choose the significantly better model which is new model 2.

#Coefficient Table

#For Model 1
sm_p_values = as.character(round(sm$coefficients[,4], 3))	
sm_p_values[sm_p_values != "0" & sm_p_values != "1"] = substr(sm_p_values[sm_p_values != "0" & sm_p_values != "1"], 2, nchar(sm_p_values[sm_p_values != "0" & sm_p_values != "1"]))	
sm_p_values[sm_p_values == "0"] = "<.001"	


sm_table = cbind(as.data.frame(round(cbind(coef(mod_pain1), confint(mod_pain1), c(0, lm.beta(mod_pain1)$standardized.coefficients[c(2,3)])), 2)), sm_p_values)	
names(sm_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")	
sm_table["(Intercept)","Std.Beta"] = "0"	


sm_table = coef_table(mod_pain1)	
sm_table	

#For New Model 2

confint(mod_pain2_new)
lm.beta(mod_pain2_new)
sm2_new




#Assignment 2

mod_pain3= lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income , data= data_sample_1_new )

#Normality Assumption 

mod_pain3 %>% plot(which=2)

residuals_mod_pain3=enframe(residuals(mod_pain3))
residuals_mod_pain3 %>% ggplot() + aes(x=value) + geom_histogram()

describe(residuals(mod_pain3))

#Linearity Assumption

mod_pain3 %>% residualPlots()

#Homoscedasticity

mod_pain3 %>% plot (which=3)

mod_pain3 %>% ncvTest()
mod_pain3 %>% bptest()

#No Multicollinearity

mod_pain3 %>% vif()

#All assumptions are met for Model 3

#Backwards Regression

mod_back_pain= step(mod_pain3, direction = "backward")

#Backward Model

backward_mod_pain= lm(pain ~ age + sex + pain_cat + mindfulness + cortisol_serum + household_income , data= data_sample_1_new )

sm_b= summary (backward_mod_pain) 
sm_b

AIC(backward_mod_pain)

confint(backward_mod_pain)

lm.beta(backward_mod_pain)

backward_mod_pain %>% plot(which=4) 

#Regression Equation of the backwards model

Y= 1.95 + (-0.04)*Age + 0.28*sex + 0.11*pain_cat + (-0.26)*mindfulness + 0.52*cortisol_serum + (-0.00)*household_income

#Theory-based Model

theory_based_mod_pain= lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data= data_sample_1_new )

#Comparison of Backward Model and Theory-based Model

AIC(backward_mod_pain)
AIC(theory_based_mod_pain)

anova(backward_mod_pain,theory_based_mod_pain)

#New data

data_sample_2 = read.csv("https://tinyurl.com/ha-dataset2")
View(data_sample_2)

describe(data_sample_2)

data_sample_2 %>%
  summary()

#Prediction With New Data

RAD= sum(abs(data_sample_1_new$pain - predict(theory_based_mod_pain)))
RAD

RSS= sum((data_sample_1_new$pain - predict(theory_based_mod_pain))^2)
RSS

RAD_b= sum(abs(data_sample_1_new$pain - predict(backward_mod_pain)))
RAD_b

RSS_b= sum((data_sample_1_new$pain - predict(backward_mod_pain))^2)
RSS_b

rad= data_sample_1_new$pain-predict(theory_based_mod_pain)
rad

rad_b= data_sample_1_new$pain-predict(backward_mod_pain)
rad_b

comparison  = t.test(rad, rad_b)
comparison

#Statistics of Backwards Model

confint(backward_mod_pain)
lm.beta(backward_mod_pain)
sm_backwards = summary(backward_mod_pain)
sm_backwards





#Assignment 3

data_sample_3 = read.csv("https://tinyurl.com/ha-dataset3")
data_sample_4 = read.csv("https://tinyurl.com/ha-dataset4")

View(data_sample_3)
View(data_sample_4)

data_sample_3= data_sample_3 %>%
  mutate(hospital= factor(hospital))

data_sample_4 %>%
  mutate(hospital= factor(hospital))

View(data_sample_3)
View(data_sample_4)

#Visualization

data_sample_3 %>%
  ggplot()+
  aes(y = pain, x = cortisol_serum)+
  geom_point(aes(color = hospital), size = 4)+
  geom_smooth(method = "lm", se = F)

int_plot_3 = data_sample_3%>%
  ggplot()+
  aes(y = pain, x = cortisol_serum, color = hospital)+
  geom_point(size = 4)+
  geom_smooth(method = "lm", se = F, fullrange=TRUE)
int_plot_3


#Random Intercept Model

int_plot_3+
  xlim(-1, 50)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)


mod_rnd_int= lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1|hospital), data= data_sample_3 )
mod_rnd_int

AIC(mod_pain2_new)
cAIC(mod_rnd_int)

sm_rnd_int= summary(mod_rnd_int)
sm_rnd_int

confint(mod_rnd_int)
stdCoef.merMod(mod_rnd_int)


sum(residuals(mod_pain2_new)^2)
sum(residuals(mod_rnd_int)^2)

#Marginal R squared

r2beta(mod_rnd_int, method = "nsj", data = data_sample_3)

#Marginal and Conditional R squared

r.squaredGLMM(mod_rnd_int)

#Prediction With New Data


RSS_4= sum((data_sample_4$pain - predict(mod_rnd_int))^2)
RSS_4

mod_rnd_int_mean <-lm(pain~1, data = data_sample_4 )

TSS_4=sum((data_sample_4$pain- predict(mod_rnd_int_mean))^2)
TSS_4

R2_4 = 1-(RSS_4/TSS_4)
R2_4

#New Model on Data 3

mod_rnd_slope= lmer(pain ~ cortisol_serum + (cortisol_serum|hospital), data= data_sample_3 )
mod_rnd_slope

sm_s= summary(mod_rnd_slope)
cAIC(mod_rnd_slope)

#Visualization of Regression Lines

data_sample_3 = data_sample_3%>%
  mutate(pred_slope =predict(mod_rnd_slope))

data_sample_3%>%
  ggplot()+
  aes(y = pain, x = cortisol_serum, group = hospital)+
  geom_point(aes(color = hospital), size = 2)+
  geom_line(color='red',aes(y=pred_slope, x=cortisol_serum))+
  facet_wrap(~hospital, ncol = 2)+
  theme_apa(legend.pos = "right",
            legend.use.title = FALSE,
            legend.font.size = 12,
            x.font.size = 12,
            y.font.size = 12,
            facet.title.size = 12,
            remove.y.gridlines = TRUE,
            remove.x.gridlines = TRUE
  )


