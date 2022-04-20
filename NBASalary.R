# Load libraries
library(tidyverse);library(car);library(emmeans);library(MASS);library(reshape);library(reshape2);library(faraway);library(caret)

# Read csv files
salary_data <- read.csv("NBA_season1718_salary.csv"); stats_data <- read.csv("Seasons_Stats.csv")
dim(salary_data);dim(stats_data)

# Full join both data sets
data0 <- salary_data %>% 
  full_join(.,stats_data, by=unique("Player")) %>% 
  subset(Year == 2017) %>% 
  group_by(Player) %>% 
  summarise(season17_18 = sum(season17_18), Age = mean(Age),
            G = mean(G),GS = mean(GS),MP = mean(MP),PER = mean(PER),TS. = mean(TS.),
            X3PAr =mean(X3PAr),FTr = mean(FTr),ORB. = mean(ORB.),DRB. = mean(DRB.),
            TRB. = mean(TRB.),AST. = mean(AST.),STL. = mean(STL.),BLK. = mean(BLK.),
            TOV.  = mean(TOV.),USG. = mean(USG.) ,OWS = mean(OWS) ,DWS = mean(DWS) ,WS = mean(WS),
            WS.48 = mean(WS.48) ,OBPM = mean(OBPM) ,DBPM = mean(DBPM) ,BPM = mean(BPM) ,VORP = mean(VORP),
            FG = mean(FG) ,FGA = mean(FGA) ,FG. = mean(FG.),X3P = mean(X3P) ,X3PA = mean(X3PA) ,
            X2P = mean(X2P) ,X2PA = mean(X2PA) ,X2P. = mean(X2P.) ,eFG. = mean(eFG.),FT = mean(FT),
            FTA = mean(FTA),FT. = mean(FT.),ORB = mean(ORB),DRB = mean(DRB),TRB = mean(TRB),
            AST = mean(AST),STL = mean(STL),BLK = mean(BLK),TOV = mean(TOV),PF = mean(PF),PTS = mean(PTS)); dim(data0)

# Omit Player column and set data frame
fdata <- data0 %>% 
  dplyr::select(-Player) %>% 
  na.omit(); dim(fdata); 403/486
final_data <- data.frame(fdata)

# Explore response variable
mean(final_data$season17_18)
range(final_data$season17_18)

# Explore explanatory variables
range(final_data$G)
range(final_data$GS)
range(final_data$MP)
range(final_data$PER)
range(final_data$TS.)
range(final_data$X3PAr)
range(final_data$FTr) # incorrect data that are greater than 1
range(final_data$ORB.)
range(final_data$DRB.)
range(final_data$TRB.)
range(final_data$AST.)
range(final_data$STL.)
range(final_data$BLK.)
range(final_data$TOV.)
range(final_data$USG.)
range(final_data$OWS)
range(final_data$DWS)
range(final_data$WS)
range(final_data$WS.48)
range(final_data$OBPM)
range(final_data$DBPM)
range(final_data$BPM)
range(final_data$VORP)
range(final_data$FG)
range(final_data$FGA)
range(final_data$FG.)
range(final_data$X3P)
range(final_data$X3PA)
range(final_data$X2P)
range(final_data$X2PA)
range(final_data$X2P.)
range(final_data$eFG.)
range(final_data$FT)
range(final_data$FTA)
range(final_data$FT.)
range(final_data$ORB)
range(final_data$DRB)
range(final_data$TRB)
range(final_data$AST)
range(final_data$STL)
range(final_data$BLK)
range(final_data$TOV)
range(final_data$PF)
range(final_data$PTS)


# Observe data with statistically significant predictors (lm19) using Box Plot
d <- melt(final_data, id="season17_18")
ggplot(d,aes(x=variable,y=value,color=variable)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(size=10, angle=45))
dt <- final_data[,c(1,2,3,5,9,19,36)]
dt1 <- final_data[,c(1,2,3)]
dt2 <- final_data[,c(1,5)]
dt3 <- final_data[,c(1,36)]
dt4 <- final_data[,c(1,9,19)]
e <- melt(dt,id="season17_18")
e1 <- melt(dt1,id="season17_18")
e2 <- melt(dt2,id="season17_18")
e3 <- melt(dt3,id="season17_18")
e4 <- melt(dt4,id="season17_18")
ggplot(e,aes(x=variable,y=value)) + geom_boxplot()
ggplot(e1,aes(x=variable,y=value)) + geom_boxplot()
ggplot(e2,aes(x=variable,y=value)) + geom_boxplot()
ggplot(e3,aes(x=variable,y=value)) + geom_boxplot()
ggplot(e4,aes(x=variable,y=value)) + geom_boxplot()

# Fit the regression, summarize and check assumptions
lm1 <- lm(season17_18 ~ ., data = final_data); summary(lm1);performance::check_model(lm1)

# Check if response needs to be transformed using Box-Cox transformation
par(mfrow=c(2,3));qqPlot(final_data$season17_18);qqPlot((final_data$season17_18)^0.25);hist(final_data$season17_18);hist((final_data$season17_18)^0.25);plot(final_data$season17_18,resid(lm2),data=final_data);plot((final_data$season17_18)^0.25,resid(lm2),data=final_data)
ptf<- powerTransform(season17_18~.,data = final_data)
summary(ptf)

# Fit regression with transformed predictors (Full model)
lm2 <- lm((season17_18)^0.25~Age+G+GS+MP+PER+TS.+sqrt(X3PAr)+log(FTr+4)+log(ORB.+1)+DRB.+log(TRB.+1)+log(AST.+1)+STL.+sqrt(BLK.)
          + log(TOV.+ 20) + USG. + log(OWS+ 20) + sqrt(DWS+ 20) + sqrt(WS+ 20) + WS.48 + sqrt(OBPM + 20) + sqrt(DBPM + 20) + BPM + log(VORP+ 20) + sqrt(FG+ 20) + FG.+ sqrt(FGA) + sqrt(X3PA+ 20) + sqrt(X2P+ 20) + 
            sqrt(X2PA+ 20) + log(X2P.+ 20) + log(eFG.+ 20) + log(FT+ 20) + log(FTA +6) + FT. + log(ORB+ 20) + sqrt(DRB+ 20) + sqrt(TRB+ 20) + log(AST+ 20) + sqrt(STL+ 20) + log(BLK+ 20) + sqrt(TOV+ 20) + PF + sqrt(PTS+ 20)
          , data=final_data); summary(lm2); performance::check_model(lm2)
# See "NBASalary Transformations" R file on Github for predictors transformation visual diagnostics 

# Drop percentages and FTr due to redundancy and impossible ranges
lm2a <- update(lm2, ~. - log(AST.+1) -STL. - sqrt(BLK.) - log(TOV.+ 20) - FT. - log(X2P.+ 20) - log(ORB.+1) -log(TRB.+1) - DRB. -log(FTr+4)); summary(lm2a); performance::check_model(lm2a)
lm3a <- update(lm2a, ~. - log(eFG. + 20) - log(OWS + 20) - sqrt(FG + 20) - sqrt(X2P + 20) - sqrt(X2PA + 20) - log(FT + 20) - 
                sqrt(TRB + 20) - sqrt(X3PA + 20) - sqrt(FGA)); summary(lm3a); performance::check_model(lm3a)
anova(lm2a,lm2)
anova(lm3a,lm2)
lm22 <- step(lm3a, k = log(n))
summary(lm22); performance::check_model(lm22)
plot((final_data$season17_18)^0.25, resid(lm22), data=final_data)

# Drop FTr due to impossible range
lm2b <- update(lm2, ~. -log(FTr+4));summary(lm2b);performance::check_model(lm2b)
lm3b <- update(lm2b, ~. - log(eFG. + 20) - log(OWS + 20) - sqrt(FG + 20) - sqrt(X2P + 20) - sqrt(X2PA + 20) - log(FT + 20) - 
                 sqrt(TRB + 20) - sqrt(X3PA + 20) - sqrt(FGA)); summary(lm3b); performance::check_model(lm3b)
anova(lm2b,lm2)
anova(lm3b,lm2)
lm22b <- step(lm3b, k = log(n))
summary(lm22b); performance::check_model(lm22b)
plot((final_data$season17_18)^0.25, resid(lm22b), data=final_data)

# Is there a relationship between the response and at least one predictor in our regression model?
null <- lm((season17_18)^0.25~1, data=final_data)
anova(null,lm2)
anova(null,lm2a)
anova(null,lm2b)
# Since the p-value is less than 0.05, we reject the null and conclude there is at least one predictor that is useful at predicting salary

# Address multicollinearity through correlation matrix
corr_df <- round(cor(final_data[,1:46]),2)

# Scan the upper right triangle of the correlation matrix. Print indeces that have correlation values greater than .9
corr_vals <- c()
for(i in 1:ncol(corr_df)){
  for(j in i:ncol(corr_df)){
    if(corr_df[i,j] > .9 & corr_df[i,j] < 1){
      corr_vals <- append(corr_vals, c(i,j))
    }
  }
}; length(corr_vals);corr_vals

# Compare variables with high collinearity with the response variable###
# Create a matrix to see the index pairs with high collinearity
colnames(corr_df)
corr_pairs <- matrix(corr_vals, ncol = 2, byrow = TRUE); corr_pairs

# Create a linear model with first set of rejects (rho>0.9) with high collinearity & lower correlation with the response variable
lm3 <- update(lm2, ~. - log(eFG. + 20) - log(TRB. + 1) - log(OWS + 20) - sqrt(FG + 20) - sqrt(X2P + 20) - sqrt(X2PA + 20) - log(FT + 20) - 
                sqrt(TRB + 20) - sqrt(X3PA + 20) - sqrt(FGA)); summary(lm3); performance::check_model(lm3)
# Compare model to full model
anova(lm3,lm2)
# High p-value indicates we can drop these predictors/rejects

# Scan the upper right triangle of the correlation matrix. Print indeces that have correlation values greater than .83 and less than .9
corr_vals2 <- c()
for(i in 1:ncol(corr_df)){
  for(j in i:ncol(corr_df)){
    if(corr_df[i,j] > .83 & corr_df[i,j] <= .9){
      corr_vals2 <- append(corr_vals2, c(i,j))
    }
  }
}; length(corr_vals2); corr_vals2

corr_pairs2 <- matrix(corr_vals2, ncol = 2, byrow = TRUE)
corr_pairs2

# Create a linear model with our second set of rejects (rho>0.83) with high collinearity & lower correlation with the response variable
lm4 <- update(lm3, ~. - G - MP - WS.48 - log(ORB. + 1) - sqrt(TRB + 20) - log(ORB + 20) - log(VORP + 20) - log(FTA + 6) - 
                sqrt(OBPM + 20) - log(FT + 20) - sqrt(TOV + 20) - log(X2P. + 20) - FG. - sqrt(X2P + 20) - sqrt(X2PA + 20) -
                log(AST + 20)); summary(lm4); performance::check_model(lm4)
# Compare model to full model
anova(lm4,lm2)
# Low p-value indicates we cannot drop these predictors/rejects since at least one is useful at predicting salary


# ########### Next we will make a new correlation matrix that only includes the variables in our most recent model
# # lm4
# 
# corr_df2 <- data.frame(corr_df)[,c(1,2,4,6,7,8,9,11,13,14,15,16,17,19,20,23,24,37,39,42,43,45,46)]
# view(corr_df2)
# 
# 
# corr_vals3 <- c()
# for(i in 1:ncol(corr_df2)){
#   for(j in i:ncol(corr_df2)){
#     if(abs(corr_df2[i,j]) >= .7 & abs(corr_df2[i,j]) < 1){
#       corr_vals3 <- append(corr_vals3, c(i,j))
#     }
#   }
# }; length(corr_vals3); corr_vals3
# 
# corr_pairs3 <- matrix(corr_vals3, ncol = 2, byrow = TRUE)
# corr_pairs3
# 
# lm5 <- update(lm4, ~ . - PER - TS. - sqrt(DRB+20) - sqrt(WS + 20)); summary(lm5)
# 
# n = nrow(final_data)
# lm6 <- step(lm5, k = log(n))
# summary(lm6)
# 
# ### Looks good!!! will now do stepwise selection, we will remove variables one-by-one based off of high p-values
# 
# lm7 <- update(lm5,  ~ . - FT.); summary(lm7)
# lm8 <- update(lm7,  ~ . - sqrt(DBPM + 20)); summary(lm8)
# lm9 <- update(lm8,  ~ . - STL.); summary(lm9)
# lm10 <- update(lm9,  ~ . - sqrt(X3PAr)); summary(lm10)
# lm11 <- update(lm10,  ~ . - log(FTr + 1)); summary(lm11)
# lm12 <- update(lm11,  ~ . - USG.); summary(lm12)
# lm13 <- update(lm12,  ~ . - log(BLK + 20)); summary(lm13)
# lm14 <- update(lm13,  ~ . - sqrt(STL +20)); summary(lm14)
# lm15 <- update(lm14,  ~ . - log(AST. + 1)); summary(lm15)
# lm16 <- update(lm15,  ~ . - BPM); summary(lm16)
# lm17 <- update(lm16,  ~ . - log(TOV.+20)); summary(lm17)
# lm18 <- update(lm17,  ~ . - sqrt(BLK.)); summary(lm18)
# 
# summary(lm6);summary(lm18)
# performance::check_model(lm6)
# performance::check_model(lm18)
n <- nrow(final_data)
# Perform backwards stepwise selection on lm3
lm19 <- step(lm3, k = log(n))
summary(lm19); performance::check_model(lm19)
plot((final_data$season17_18)^0.25, resid(lm19), data=final_data)

# Perform backwards elimination on lm19
lm19a <- update(lm19,  ~ . - log(FTr+4)); summary(lm19a);performance::check_model(lm19a)

# Observed versus fitted values diagnostic plot of DWS transformation
par(mfrow=c(1,2))
plot(predict(lm19), sqrt(final_data$DWS+20), data=final_data)
lines(lowess(predict(lm19),sqrt(final_data$DWS+20)), col='red')
plot(predict(lm1), final_data$DWS, data=final_data)
lines(lowess(predict(lm1),final_data$DWS), col='red')

# lm20 <- step(lm4, k = log(n))
# summary(lm20); performance::check_model(lm20)
# anova(lm20,lm2)
# plot((final_data$season17_18)^0.25, resid(lm20), data=final_data)

# Perform backwards stepwise selection on lm2 then backwards elimination
lm21 <- step(lm2, k = log(n));summary(lm21); performance::check_model(lm21) # multicollinearity issues
lm21a <- update(lm21,~.-FG.);summary(lm21a); performance::check_model(lm21a)
lm21b <- update(lm21a,~.-log(eFG.+20));summary(lm21b); performance::check_model(lm21b)
plot((final_data$season17_18)^0.25, resid(lm21b), data=final_data)

# Compare all models against full model
anova(lm3,lm2) # keep lm3
anova(lm4,lm2) # use lm2
anova(lm19,lm2) # keep lm19 ----- high p-value
anova(lm19a,lm2) # keep lm19a
anova(lm21,lm2) # keep lm21 ----- high p-value
anova(lm21a,lm2)# keep lm21a
anova(lm21b,lm2) # keep lm21b


# Assign models to a name
s2 <- summary(lm2)$adj.r.squared
s3 <- summary(lm3)$adj.r.squared
s4 <- summary(lm4)$adj.r.squared
s19 <- summary(lm19)$adj.r.squared
s19a <- summary(lm19a)$adj.r.squared
s21 <- summary(lm21)$adj.r.squared
s21a <- summary(lm21a)$adj.r.squared
s21b <- summary(lm21b)$adj.r.squared

s2;s3;s4;s19;s19a;s21;s21a;s21b # s19 and s21 but s21 has high collinearity

# Using lm19 to predict Curry's and Lillard's 2017-18 salary
pred_curry0 <- predict(lm19,newdata = data.frame(Age=28,G=79.000000,MP=2638.0000, FTr=0.25100000,DWS=3.90000000,FTA=362.000000), interval="prediction")
pred_curry0^4
#fit     lwr      upr
#1 23,517,044 6,187,195 63,883,223
#Actual salary is 34,682,550
pred_dame0 <- predict(lm19,newdata = data.frame(Age=26,G=75.000000,MP=2694.00000, FTr=0.3660000,DWS=1.50000000,FTA=545.000000), interval="prediction")
pred_dame0^4
#fit     lwr      upr
#1 16,455,663 3,704,547 48,636,950
#Actual salary is 26,153,057

# Using lm21c to predict Curry's and Lillard's 2017-18 salary
pred_curry <- predict(lm21c,newdata = data.frame(Age=34,G=64,DWS=11.1,FGA=1224), interval="prediction")
pred_curry^4
#fit      lwr       upr
# 107156311 38834036 240604218
#Steph Curry is getting paid 45,780,000 which is within the prediction interval 38,834,036 and 240,604,218
pred_dame <- predict(lm21c,newdata = data.frame(Age=31,G=29,DWS=1.7,FGA=552), interval="prediction")
pred_dame^4
#fit     lwr      upr
# 15523020 3358957 46837196
#Damian Lillard is getting paid 39,344,900 which is within the prediction interval is 3,358,957 and 46,837,196

# Scatterplot matrix of lm19
pairs((season17_18)^0.25 ~ Age + G + MP + log(FTr + 4) + sqrt(DWS +20) + log(FTA + 6), data=final_data)
round(cor(final_data[,c(1,2,3,5,9,19,36)]),4)
# high collinearity with MP and G

# Scatterplot matrix of lm21
pairs((season17_18)^0.25 ~ Age + G + sqrt(X3PAr) + sqrt(DWS +20) + FG. + sqrt(FGA) + log(eFG. + 20), data = final_data)
round(cor(final_data[,c(1,2,3,8,19,28,27,34)]),4)
# high collinearity with eFG. and FG.

# Scatterplot matrix of lm21c
pairs((season17_18^0.25)~Age+G+sqrt(DWS+20)+sqrt(FGA), data=final_data)
round(cor(final_data[,c(1,2,3,19,27)]),4)
# no high collinearity but lower adjusted r squared than full model

# Scatterplot matrix of lm22
pairs((season17_18)^0.25 ~ Age + G + sqrt(DRB +20) + sqrt(PTS + 20) , data=final_data)
round(cor(final_data[,c(1,2,3,39,46)]),4)
# no high collinearity but lower adjusted r squared than full model

# Scatterplot matrix of lm22b
pairs((season17_18)^0.25 ~ Age + G + sqrt(DWS + 20) + sqrt(PTS + 20) , data=final_data)
round(cor(final_data[,c(1,2,3,19,46)]),4)
# no high collinearity but lower adjusted r squared than full model


# Cross validation
set.seed(111)
n <- nrow(final_data);n
z <- floor(0.7*n)
train <- sample(1:n, z)
test_data <- final_data[-train,]

# Train models lm21c (lm_train), full model (lm_train1), lm19 (lm_train2), lm21 (lm_train3), lm22b (lm_train4)
lm_train <- lm((season17_18)^0.25~Age+G+sqrt(DWS+20)+sqrt(FGA), data=final_data, subset=train)
lm_train1 <- lm((season17_18)^0.25~Age+G+GS+MP+PER+TS.+sqrt(X3PAr)+log(FTr+4)+log(ORB.+1)+DRB.+log(TRB.+1)+log(AST.+1)+STL.+sqrt(BLK.)
                + log(TOV.+ 20) + USG. + log(OWS+ 20) + sqrt(DWS+ 20) + sqrt(WS+ 20) + WS.48 + sqrt(OBPM + 20) + sqrt(DBPM + 20) + BPM + log(VORP+ 20) + sqrt(FG+ 20) + FG.+ sqrt(FGA) + sqrt(X3PA+ 20) + sqrt(X2P+ 20) + 
                  sqrt(X2PA+ 20) + log(X2P.+ 20) + log(eFG.+ 20) + log(FT+ 20) + log(FTA +6) + FT. + log(ORB+ 20) + sqrt(DRB+ 20) + sqrt(TRB+ 20) + log(AST+ 20) + sqrt(STL+ 20) + log(BLK+ 20) + sqrt(TOV+ 20) + PF + sqrt(PTS+ 20)
                , data=final_data, subset=train)
lm_train2 <- lm((season17_18)^0.25 ~ Age + G + MP + log(FTr + 4) + sqrt(DWS +20) + log(FTA + 6), data=final_data, subset=train)
lm_train3 <- lm((season17_18)^0.25 ~ Age + G + sqrt(X3PAr) + sqrt(DWS +20) + FG. + sqrt(FGA) + log(eFG. + 20), data = final_data, subset=train)
lm_train4 <- lm((season17_18)^0.25 ~ Age + G + sqrt(DWS + 20) + sqrt(PTS + 20) , data=final_data, subset=train)

summary(lm_train)
summary(lm_train1)
summary(lm_train2)
summary(lm_train3)
summary(lm_train4)

predictions <- lm_train %>% predict(test_data)
data.frame(R2 = R2(predictions, test_data$season17_18^0.25),
           RMSE = RMSE(predictions, test_data$season17_18^0.25),
           MAE = MAE(predictions, test_data$season17_18^0.25))

predictions1 <- lm_train1 %>% predict(test_data)
data.frame(R2 = R2(predictions1, test_data$season17_18^0.25),
           RMSE = RMSE(predictions1, test_data$season17_18^0.25),
           MAE = MAE(predictions1, test_data$season17_18^0.25))

predictions2 <- lm_train2 %>% predict(test_data)
data.frame(R2 = R2(predictions2, test_data$season17_18^0.25),
           RMSE = RMSE(predictions2, test_data$season17_18^0.25),
           MAE = MAE(predictions2, test_data$season17_18^0.25))

predictions3 <- lm_train3 %>% predict(test_data)
data.frame(R2 = R2(predictions3, test_data$season17_18^0.25),
           RMSE = RMSE(predictions3, test_data$season17_18^0.25),
           MAE = MAE(predictions3, test_data$season17_18^0.25))
predictions4 <- lm_train4 %>% predict(test_data)
data.frame(R2 = R2(predictions4, test_data$season17_18^0.25),
           RMSE = RMSE(predictions4, test_data$season17_18^0.25),
           MAE = MAE(predictions4, test_data$season17_18^0.25))

# Same RMSE for all models except lm21 (lm_train3) and lm22b are one less


# Manually calculate RMSE
sqrt((sum((test_data$season17_18 - predictions)^2))/(n-z))
sqrt((sum((test_data$season17_18 - predictions1)^2))/(n-z))
sqrt((sum((test_data$season17_18 - predictions2)^2))/(n-z))
sqrt((sum((test_data$season17_18 - predictions3)^2))/(n-z))
sqrt((sum((test_data$season17_18 - predictions4)^2))/(n-z))
# Same RMSE for all models except lm21 (lm_train3) and lm22b are one less

# Run predictions to ensure different predictions from different models
pred_dame1 <- predict(lm21b,newdata = data.frame(Age=26,G=75.000000,X3PAr = 0.3880000,DWS = 1.50000000,FG. = 0.4440000 , FGA=1488.000000), interval="prediction")
pred_dame1^4

pred_dame2 <- predict(lm21c,newdata = data.frame(Age=26,G=75.000000,X3PAr = 0.3880000,DWS = 1.50000000, FGA=1488.000000), interval="prediction")
pred_dame2^4

pred_dame3 <- predict(lm21d,newdata = data.frame(Age=26,G=75.000000, FGA=1488.000000), interval="prediction")
pred_dame3^4
# Damian Lillard's 2017-18 salary is 26,153,057
















