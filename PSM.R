
setwd("/Users/jnlinao/iCloud/UCI_21-22/Winter'22/BANA_277/Homework/IX")
data <- read.csv('star_digital.csv')





str(data)
head(data)
summary(data)

table(data$test)
table(data$purchase)





#Create feature: sum of all impressions 
data2 <- data
data2$tot.imp <- data$imp_1 + data$imp_2 + data$imp_3 + 
  data$imp_4 + data$imp_5 + data$imp_6

#Welch t-test: Difference of total impressions between control and treatment groups
t.test_tot.imp <- t.test(data2$tot.imp ~ data2$test)
t.test_tot.imp

#Welch t-test: Difference of conversion rate between control and treatment groups 
t.test_cr <- t.test(data2$purchase ~ data2$test)
t.test_cr

#Logistic Regression 
## Pivot Table
xtabs(~purchase + test, data = data2)  

##convert to categorical 
data2$purchase <- factor(data2$purchase)
data2$test <- factor(data2$test)

##run logit
logit.1 <- glm(purchase ~ test + imp_1 + imp_2, data = data2, family = "binomial")
35073, 34930, 34660
exp(0.058878)
exp(-4.066414)
exp(0.128374)
exp(0.281768)
##evaluate
summary(logit.1)
coef(logit.1)
exp(coef(logit.1)) #exponentiate  coefficients to interpret as odds-ratios


t.test_tot.imp

#Create new df
star <- data

star$Timp <- star$imp_1 + star$imp_2 + star$imp_3 + 
  star$imp_4 + star$imp_5 + star$imp_6 
star$Timp_t <- star$test * star$Timp


#Logistic Regression

##convert to categorical 
str(data3)
star$purchase <- factor(star$purchase)

##run logit
logit.2 <- glm(purchase ~ Timp + Timp_t, 
               data = star, family = "binomial")

logit.3 <- glm(purchase ~ test_tot.imp_inter, 
               data = data3, family = "binomial")

##evaluate
summary(logit.2)
coef(logit.2)
exp(coef(logit.2))

##evaluate
summary(logit.3)
coef(logit.3)
exp(coef(logit.3)) #exponentiate  coefficients to interpret as odds-ratios



data4$purchase <- factor(data4$purchase)
data4$tot.imp_1.5 <- data4$imp_1 + data4$imp_2 + data4$imp_3 + 
  data4$imp_4 + data4$imp_5

star

logit.4 <- glm(purchase~imp_1+imp_2+imp_3+imp_4+imp_5,data=data4, family = 'binomial')
summary(logit.4)
exp(coef(logit.4))

logit.5 <- glm(purchase~imp_6,data=data4, family = 'binomial')
summary(logit.5)
exp(coef(logit.5))

logit.6 <- glm(purchase~tot.imp_1.5,data=data4, family = 'binomial')
summary(logit.6)
exp(coef(logit.6))

logit.7 <- glm(purchase~tot.imp_1.5 + imp_6+,data=data4, family = 'binomial')
summary(logit.7)
exp(coef(logit.7))




star$timp.15 <- star$imp_1 + star$imp_2 + star$imp_3 + 
  star$imp_4 + star$imp_5
star$timp.15_t <- star$timp.15*star$test

star$timp.6_t <- star$imp_6*star$test

logit.8 <- glm(purchase~timp.15+timp.15_t+star$imp_6 + star$timp.6_t,data=star, family = 'binomial')
summary(logit.8)
exp(coef(logit.8))


rm(list=ls())


