#loading all necessary packages
library(psych)
library(magrittr)
library(tidyverse)
library(lm.beta)
library(ggplot2)
library(gridExtra)
library(car) # for residualPlots, vif, pairs.panels, ncvTest	
library(lmtest) # bptest	
library(sandwich) # for coeftest vcovHC estimator	
library(boot) # for bootstrapping	
library(lmboot) # for wild bootsrapping	
library(apaTables)
library(lme4) # for lmer() mixed models	
library(lmerTest)
library(sjPlot)
library(cAIC4)


#########ASSIGNMENT 1

data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")

view(data_sample_1)

describe(data_sample_1)
#the minimum of IQ is 53 which seems unlikely but is theoretically possible

#the max for pain is at 55 while the theoretical max is at 10, so now I'll look up the participant who had that value
data_sample_1 %>% 
  filter(pain == 55)
# the most probable answer is that the participant had a score of 5 so I'll change it to 5


data_sample_1[88, 2] = 5
  #but I want to save it in a new data.frame but for some reason that doesnt work :(
  
#see if there are any new strange things with this variable now
  describe(data_sample_1$pain)
  #now the abnormalities


#state and trait is also off for their min and max: should be 20 to 80 but it is 4.2 to 51 wtfff
  data_sample_1 %>% 
    filter(STAI_trait == 4.2)
  #again its only one participant, I'm assuming the true value is 42 and adjust it
data_sample_1[34, 5] = 42

view(data_sample_1)


###mutating sex into a factor
data_sample_1 <- data_sample_1 %>% 
  mutate(sex=factor(sex))

levels(data_sample_1$sex)

view(data_sample_1)

#now checking again if there is anything else that is off
describe(data_sample_1$STAI_trait)

#checking for preassumptions to build the regression model
#building the regression model 1
mod_1 <- lm(pain ~ age+ sex, data = data_sample_1)

mod_1

#checking for extreme cases with high leverage (look at Cooks difference)
mod_1 %>% 
  plot(which= 5)

mod_1 %>% 
  plot(which= 4)

#we're not excluding any outliers because they are all <1

#general graphic plotting of the data
hist_age <- data_sample_1 %>% 
  ggplot()+
  aes(x= age)+
  geom_histogram()

hist_STAI_trait <- data_sample_1 %>% 
  ggplot()+
  aes(x= STAI_trait)+
  geom_histogram()

hist_pain_cat <- data_sample_1 %>% 
  ggplot()+
  aes(x= pain_cat)+
  geom_histogram()

hist_mindfulness <- data_sample_1 %>% 
  ggplot()+
  aes(x= mindfulness)+
  geom_histogram()

hist_cort_serum <- data_sample_1 %>% 
  ggplot()+
  aes(x= cortisol_serum)+
  geom_histogram()

hist_cort_saliva <- data_sample_1 %>% 
  ggplot()+
  aes(x= cortisol_saliva)+
  geom_histogram()

hist_pain <- data_sample_1 %>% 
  ggplot()+
  aes(x= pain)+
  geom_histogram()

hist_all <- grid.arrange(hist_age, 
             hist_cort_saliva, 
             hist_cort_serum, 
             hist_mindfulness, 
             hist_pain,
             hist_pain_cat,
             hist_STAI_trait)


#making scatterplots
scat_age <- data_sample_1 %>% 	
  mutate(rownum = row.names(data_sample_1)) %>%  	
  ggplot() +	
  aes(x = age, y = pain, label = rownum) +	
  geom_point() +	
  geom_text()	+
  geom_smooth(method = "lm")

scat_sex <- data_sample_1 %>% 	
  mutate(rownum = row.names(data_sample_1)) %>%  	
  ggplot() +	
  aes(x = sex, y = pain, label = rownum) +	
  geom_point() +	
  geom_text()

scat_pain_cat <- data_sample_1 %>% 	
  mutate(rownum = row.names(data_sample_1)) %>%  	
  ggplot() +	
  aes(x = pain_cat, y = pain, label = rownum) +	
  geom_point() +	
  geom_text()	+
  geom_smooth(method = "lm")

scat_cort_saliva <- data_sample_1 %>% 	
  mutate(rownum = row.names(data_sample_1)) %>%  	
  ggplot() +	
  aes(x = cortisol_saliva, y = pain, label = rownum) +	
  geom_point() +	
  geom_text()	+
  geom_smooth(method = "lm")

scat_mindfulness <- data_sample_1 %>% 	
  mutate(rownum = row.names(data_sample_1)) %>%  	
  ggplot() +	
  aes(x = mindfulness, y = pain, label = rownum) +	
  geom_point() +	
  geom_text()	+
  geom_smooth(method = "lm")



#skewness and kurtosis
describe(residuals(mod_1))# is ok because their between -1 and 1

#normality of residuals qq-plot
mod_1 %>% 
  plot(which = 2)

#histogram
residuals_mod_1 <- enframe(residuals(mod_1))	
residuals_mod_1 %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()	

#linearity 
yhat1 <- fitted.values( object = mod_1 )
plot( x = yhat1, y = data_sample_1$pain, xlab = "Fitted Values", ylab = "Observed Values")
plot(x = mod_1, which = 1)
residualPlots( model = mod_1 )

#homogenity of variance
plot(x = mod_1, which = 3)
ncvTest(mod_1) #not significant

#collinearity
vif(mod_1)


mod_1
summary(mod_1)
confint(mod_1)
lm.beta(mod_1)
tab_model(mod_1, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE)


#building regression model 2
mod_2 <- lm(pain~ age+ sex+ STAI_trait+ pain_cat+ mindfulness+ cortisol_serum + cortisol_saliva, data= data_sample_1)


#checking all the preassumptions for model 2

mod_2 %>% 
  plot(which= 5)

mod_2 %>% 
  plot(which= 4)

#skewness and kurtosis
describe(residuals(mod_2))

#normality of residuals qq-plot
mod_2 %>% 
  plot(which = 2)

#histogram
residuals_mod_2 <- enframe(residuals(mod_2))	
residuals_mod_2 %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()	

#linearity 
yhat1 <- fitted.values( object = mod_1 )
plot( x = yhat1, y = data_sample_1$pain, xlab = "Fitted Values", ylab = "Observed Values")
plot(x = mod_2, which = 1)
residualPlots( model = mod_2)

#homogenity of variance
plot(x = mod_2, which = 3)
ncvTest(mod_2)#not significant

#collinearity
vif(mod_2)

data_sample_1 %>% select(age, sex, pain, STAI_trait, pain_cat, mindfulness, cortisol_serum) %>%
  pairs.panels(col = "red", lm = T)

#excluding cortisol saliva because it has a high collinearity
mod_2_final <- lm(pain~ age+ sex+ STAI_trait+ pain_cat+ mindfulness+ cortisol_serum, data= data_sample_1)

mod_2_final

summary(mod_2_final)

#comparing the two models
summary(mod_1)$adj.r.squared
summary(mod_2_final)$adj.r.squared

AIC(mod_1)
AIC(mod_2_final)


anova(mod_1, mod_2)

tab_model(mod_2_final, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE)

#the second model seems to be much better than the first model because it has a lower AIC and the difference is more than two points

#########ASSIGNMENT 2

#make some new descriptives for the new variables that we'll include now
hist_IQ <-data_sample_1 %>% 	
  ggplot() +	
  aes(x = IQ) +	
  geom_histogram()

hist_income <- data_sample_1 %>% 	
  ggplot() +	
  aes(x = household_income) +	
  geom_histogram()


#first creating a third model with all the predictors
mod_3 <- lm(pain ~ age+ sex+ STAI_trait+ pain_cat+ mindfulness+ cortisol_serum+ weight+ IQ+ household_income, data= data_sample_1)

mod_3

summary(mod_3)
AIC(mod_3)

#do model diagnostic on the full model
# Cooks Distance
plot(x = mod_3, which = 4)# figure margins too large
plot(x = mod_3, which = 5)# figure margins too large

#Normality
plot(x = mod_3, which = 2)# figure margins too large

#Linearity
yhat_mod_3 <- fitted.values( object = mod_3 )
plot( x = yhat_mod_3, y = data_sample_1$pain, xlab = "Fitted Values", ylab = "Observed Values")
plot(x = mod_3, which = 1)# figure margins too large
residualPlots( model = mod_3 )# this doesn't give me an output AGAIN

#homogeneity of variance
plot(x = mod_3, which = 3)# figure margins too large
ncvTest(mod_3)#not significant

#collinearity 
vif(mod_3)# is similar to the example so not problematic

#making a backwards regression
mod_3_back <- step(mod_3, direction = "backward")

#renaming the last model of the first assignment theory-based-model
theory_based_model <- mod_2_final

#new model with only the predictors that are left from the backward regression
backward_model <- lm(pain ~ age+ mindfulness+ cortisol_serum+ pain_cat, data = data_sample_1)

backward_model
summary(backward_model)
confint(backward_model)
lm.beta(backward_model)

tab_model(backward_model, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE)


#compare theory based model and backward model
AIC(backward_model)
AIC(theory_based_model)

summary(backward_model)$adj.r.squared	
summary(theory_based_model)$adj.r.squared
anova(theory_based_model, backward_model)# not significant

#check the new data first
home_sample_2 = read.csv("https://tinyurl.com/87v6emky")

view(home_sample_2)

describe(home_sample_2)

summary(home_sample_2)

home_sample_2 %>% 	
  ggplot() +	
  aes(x = age) +	
  geom_histogram()

home_sample_2 %>% 	
  ggplot() +	
  aes(x = STAI_trait) +	
  geom_histogram( bins = 50)

home_sample_2 %>% 	
  ggplot() +	
  aes(x = pain_cat) +	
  geom_histogram( bins = 50)

home_sample_2 %>% 	
  ggplot() +	
  aes(x = mindfulness) +	
  geom_histogram( bins = 50)

home_sample_2 %>% 	
  ggplot() +	
  aes(x = cortisol_serum) +	
  geom_histogram( bins = 50)

home_sample_2 %>% 	
  ggplot() +	
  aes(x = cortisol_saliva) +	
  geom_histogram( bins = 50)

home_sample_2 %>% 	
  ggplot() +	
  aes(x = pain) +	
  geom_histogram()

#making predictions for the new data
pred_test_back <- predict(backward_model, home_sample_2)
pred_test_theory <- predict(theory_based_model, home_sample_2)

# now we calculate the sum of squared residuals 	
pred_test_back = sum((home_sample_2[,"pain"] - pred_test_back)^2)	
pred_test_theory = sum((home_sample_2[,"pain"] - pred_test_theory)^2)	
pred_test_back	
pred_test_theory

#########ASSIGNMENT 3

file_3 = read.csv("https://tinyurl.com/b385chpu")
file_4 = read.csv("https://tinyurl.com/4f8thztv")

view(file_3)
view(file_4)

describe(file_3)
str(file_3)

#changing woman to female
file_3[25,3] = "female"

#changing the income to N/A (ID2)
file_3[2,12] = "N/A"

view(file_3)

describe(file_4)
summary(file_4)

##data set 3
##making sex into a factor
file_3 <- file_3 %>% 
  mutate(sex = factor(sex))

levels(file_3$sex)

#making hospital a factor
file_3 <- file_3 %>% mutate(hospital = factor(hospital))

levels(file_3$hospital)

describe(file_3)
str(file_3)
view(file_3)
summary(file_3)


#random intercept model

mod_4 <- lmer(pain ~ age+ sex+ STAI_trait+ pain_cat+ mindfulness+ cortisol_serum +(1|hospital), data=file_3)# and all the ones from model one

mod_4

summary(mod_4) 
confint(mod_4)
tab_model(mod_4, theory_based_model, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE)

#random intercept slope model
mod_manyplots <- lmer(pain ~ cortisol_serum +(cortisol_serum|hospital) , data=file_3)


# compute the variance explained by the fixed effect predictors using marginal R^2
# and the variance explained by the fixed and random effect terms combined using conditional R2

# marginal R2
# If the 95% CI does not contain 0, it means that the fixed effect term(s) explain a significant portion of the variation of the
# outcome compared to the mean (the null model).

# marginal and conditional R2
# conditional R --> variance explained by the fixed and random effect terms combined 

# variance for residuals
sum(residuals(mod_4)^2)

#making predictions for the new data set 4
pain_pred_mod_4 <- predict(mod_4, file_4, allow.new.levels = TRUE)


RSS = sum((file_4$pain - pain_pred_mod_4)^2)
RSS

#TSS
mod_mean <- lm(pain ~ 1, data = file_4)
mod_mean
TSS=sum((file_4$pain - predict(mod_mean))^2)
TSS

R<-1-(RSS/TSS)
R

#Random intercept slope model on data set 3 with only the most influential factor from the last model (cortisol_serum)
Random_int_slope = lmer(pain ~ cortisol_serum + (cortisol_serum|hospital), data = file_3)
Random_int_slope
Random_int_slope_opt = lmer(pain ~ cortisol_serum + (cortisol_serum|hospital), control = lmerControl(optimizer = "Nelder_Mead"), data = file_3)
Random_int_slope_opt


#comparison between the final model and the original model (model coef (standardized ones) and confintervals)



#visualizing the different hospitals pain, cort_serum, hospital
hospital_plot <- file_3 %>% ggplot()+aes(y=pain, x=cortisol_serum, color=hospital)+geom_point(size=3)+geom_smooth(method="lm", se=F, fullrange=TRUE)

hospital_plot 


predict_file_3_new <- predict(Random_int_slope_opt, file_3, allow.levels = TRUE)

predict_file_3_new
                              
file_3 %>% 		
  ggplot()+		
  aes(y = pain, x = cortisol_serum, group = hospital)+		
  geom_point(aes(color = hospital), size = 1) +		
  geom_line(color='red', aes(y=predict_file_3_new, x=cortisol_serum))+		
  facet_wrap( ~ hospital, ncol = 2)

cAIC(Random_int_slope_opt)
cAIC(mod_4)
