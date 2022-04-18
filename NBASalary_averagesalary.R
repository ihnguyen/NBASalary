################################################ DATA CLEANING ##################################################################
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
  summarise(season17_18 = mean(season17_18), Age = mean(Age),
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
################################################ DATA EXPLORATION ##################################################################
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
##################################### OVERALL F-TEST AND MODELING ##################################################################
# Fit the regression, summarize and check assumptions
lm1 <- lm(season17_18 ~ ., data = final_data); summary(lm1);performance::check_model(lm1)
# Fit regression with transformed predictors and response (Full model)
lm2 <- lm((season17_18)^0.33~Age+G+GS+MP+PER+TS.+sqrt(X3PAr)+log(FTr+4)+log(ORB.+1)+DRB.+log(TRB.+1)+log(AST.+1)+STL.+sqrt(BLK.)
          + log(TOV.+ 20) + USG. + log(OWS+ 20) + sqrt(DWS+ 20) + sqrt(WS+ 20) + WS.48 + sqrt(OBPM + 20) + sqrt(DBPM + 20) + BPM + log(VORP+ 20) + sqrt(FG+ 20) + FG.+ sqrt(FGA) + sqrt(X3PA+ 20) + sqrt(X2P+ 20) + 
            sqrt(X2PA+ 20) + log(X2P.+ 20) + log(eFG.+ 20) + log(FT+ 20) + log(FTA +6) + FT. + log(ORB+ 20) + sqrt(DRB+ 20) + sqrt(TRB+ 20) + log(AST+ 20) + sqrt(STL+ 20) + log(BLK+ 20) + sqrt(TOV+ 20) + PF + sqrt(PTS+ 20)
          , data=final_data); summary(lm2); performance::check_model(lm2)
# Check if response needs to be transformed using Box-Cox transformation
ptf<- powerTransform(season17_18~.,data = final_data)
summary(ptf)
par(mfrow=c(2,3));qqPlot(final_data$season17_18);qqPlot((final_data$season17_18)^0.33);hist(final_data$season17_18);hist((final_data$season17_18)^0.33);plot(final_data$season17_18,resid(lm2),data=final_data);plot((final_data$season17_18)^0.33,resid(lm2),data=final_data)
# Is there a relationship between the response and at least one predictor in our regression model?
null <- lm((season17_18)^0.33~1, data=final_data)
anova(null,lm2)
############################### BACKWARDS ELIMINATION (VARIABLE SELECTION) ############################################################
# Drop rejects (rho>0.9) with high collinearity & lower correlation with the response variable
lm3 <- update(lm2, ~. - log(eFG. + 20) - log(TRB. + 1) - log(OWS + 20) - sqrt(FG + 20) - sqrt(X2P + 20) - sqrt(X2PA + 20) - log(FT + 20) - 
                sqrt(TRB + 20) - sqrt(X3PA + 20) - sqrt(FGA)); summary(lm3); performance::check_model(lm3)
# Drop percentages and FTr due to redundancy and impossible ranges
lm2a <- update(lm2, ~. - log(AST.+1) -STL. - sqrt(BLK.) - log(TOV.+ 20) - FT. - log(X2P.+ 20) - log(ORB.+1) -log(TRB.+1) - DRB. -log(FTr+4)); summary(lm2a); performance::check_model(lm2a)
# Remove predictors with high correlation values
lm3a <- update(lm2a, ~. - log(eFG. + 20) - log(OWS + 20) - sqrt(FG + 20) - sqrt(X2P + 20) - sqrt(X2PA + 20) - log(FT + 20) - 
                sqrt(TRB + 20) - sqrt(X3PA + 20) - sqrt(FGA)); summary(lm3a); performance::check_model(lm3a)
# Drop FTr due to impossible range
lm2b <- update(lm2, ~. -log(FTr+4));summary(lm2b);performance::check_model(lm2b)
# Remove predictors with high correlation values
lm3b <- update(lm2b, ~. - log(eFG. + 20) - log(OWS + 20) - sqrt(FG + 20) - sqrt(X2P + 20) - sqrt(X2PA + 20) - log(FT + 20) - 
                 sqrt(TRB + 20) - sqrt(X3PA + 20) - sqrt(FGA)); summary(lm3b); performance::check_model(lm3b)
################################ BACKWARDS STEPWISE SELECTION & ELIMINATION (VARIABLE SELECTION) ######################################
n = nrow(final_data)
lm19 <- step(lm3, k = log(n));summary(lm19); performance::check_model(lm19)
  lm19a <- update(lm19,  ~ . - sqrt(X3PAr)); summary(lm19a);performance::check_model(lm19a) # insignificant predictors
  lm19b <- update(lm19a,  ~ . - PER); summary(lm19b);performance::check_model(lm19b) # collinearity issue
  lm19c <- update(lm19b,  ~ . - log(VORP + 20)); summary(lm19c);performance::check_model(lm19c)
  lm19d <- update(lm19c,  ~ . - BPM); summary(lm19d);performance::check_model(lm19d)
lm20 <- step(lm4, k = log(n));summary(lm20); performance::check_model(lm20)
lm21 <- step(lm2, k = log(n));summary(lm21); performance::check_model(lm21) # multicollinearity issues
  lm21a <- update(lm21,~.-GS);summary(lm21a); performance::check_model(lm21a) # multicollinearity issues
  lm21b <- update(lm21a,~.-log(OWS+20));summary(lm21b); performance::check_model(lm21b) # multicollinearity issues
  lm21c <- update(lm21b,~.-sqrt(X2P+20));summary(lm21c); performance::check_model(lm21c) # multicollinearity issues
lm22 <- step(lm2a, k = log(n));summary(lm22); performance::check_model(lm22)
  lm22a <- step(lm3a, k = log(n));summary(lm22a); performance::check_model(lm22a)
  lm22b <- step(lm3b, k = log(n));summary(lm22b); performance::check_model(lm22b)
  lm22c <- step(lm2b, k = log(n));summary(lm22c); performance::check_model(lm22c)
  lm22d <- update(lm22c,~. -GS); summary(lm22d);performance::check_model(lm22d)
  lm22e <- update(lm22d,~. -sqrt(X2P+20)); summary(lm22e);performance::check_model(lm22e) #### best model??????????????????
  lm22f <- update(lm22e,~. -sqrt(X3PAr)); summary(lm22f);performance::check_model(lm22f)
# View scatterplot matrices
pairs((season17_18)^0.33 ~ Age + G + sqrt(X3PAr) + log(OWS + 20) + sqrt(DWS + 20) + sqrt(FGA), data = final_data)
round(cor(final_data[,c(1,2,3,8,18,19,27)]),4)
pairs((season17_18)^0.33 ~ Age + G + log(OWS + 20) + sqrt(DWS + 20) + sqrt(FGA), data = final_data)
round(cor(final_data[,c(1,2,3,18,19,27)]),4)
################################################ MODEL SELECTION ##################################################################
# Compare all models against full model
anova(lm2a,lm2) # keep lm2a
anova(lm2b,lm2) # keep lm2b
anova(lm3,lm2) # keep lm3
anova(lm3a,lm2) # keep lm3a
anova(lm3b,lm2) # keep lm3b
anova(lm19,lm2) # keep lm19 ----- high p-value
anova(lm19a,lm2) # use lm2
anova(lm19b,lm2) # use lm2
anova(lm19c,lm2) # use lm2
anova(lm20,lm2) # use lm2
anova(lm21,lm2) # keep lm21 ----- high p-value
anova(lm21a,lm2)# keep lm21a
anova(lm21b,lm2) # keep lm21b
anova(lm21c,lm2) # use lm2
anova(lm21d,lm2) # use lm2
anova(lm22,lm2) # keep lm22
anova(lm22a,lm2) # keep lm22a
anova(lm22b,lm2) # keep lm22b
anova(lm22e,lm2) # keep lm22e
anova(lm22f,lm2) # undecided
# Assign models to a name
s2 <- summary(lm2)
s3 <- summary(lm3)
s19 <- summary(lm19)
s20 <- summary(lm20)
s21 <- summary(lm21)
s21a <- summary(lm21a)
s21b <- summary(lm21b)
s21c <- summary(lm21c)
s21d <- summary(lm21d)
s22 <- summary(lm22)
s22a <- summary(lm22a)
s22b <- summary(lm22b)
s22e <- summary(lm22e)
s22f <- summary(lm22f)
# Use assigned models to pull adjusted R-squared values
s2$adj.r.squared
s3$adj.r.squared
s19$adj.r.squared # better than full model
s20$adj.r.squared
s21$adj.r.squared # better than full model
s21a$adj.r.squared
s21b$adj.r.squared
s21c$adj.r.squared
s21d$adj.r.squared
s22$adj.r.squared # better than full model
s22a$adj.r.squared # better than full model
s22b$adj.r.squared # better than full model
s22e$adj.r.squared # better than full model
s22f$adj.r.squared
################################################ CROSS VALIDATION ##################################################################
set.seed(111)
n <- nrow(final_data);n
z <- floor(0.7*n)
train <- sample(1:n, z)
test_data <- final_data[-train,]
# Train models full model (lm_train1) and lm22e
lm_train1 <- lm((season17_18)^0.33~Age+G+GS+MP+PER+TS.+sqrt(X3PAr)+log(FTr+4)+log(ORB.+1)+DRB.+log(TRB.+1)+log(AST.+1)+STL.+sqrt(BLK.)
                + log(TOV.+ 20) + USG. + log(OWS+ 20) + sqrt(DWS+ 20) + sqrt(WS+ 20) + WS.48 + sqrt(OBPM + 20) + sqrt(DBPM + 20) + BPM + log(VORP+ 20) + sqrt(FG+ 20) + FG.+ sqrt(FGA) + sqrt(X3PA+ 20) + sqrt(X2P+ 20) + 
                  sqrt(X2PA+ 20) + log(X2P.+ 20) + log(eFG.+ 20) + log(FT+ 20) + log(FTA +6) + FT. + log(ORB+ 20) + sqrt(DRB+ 20) + sqrt(TRB+ 20) + log(AST+ 20) + sqrt(STL+ 20) + log(BLK+ 20) + sqrt(TOV+ 20) + PF + sqrt(PTS+ 20)
                , data=final_data, subset=train);summary(lm_train1)
lm_train2 <- lm((season17_18)^0.33 ~ Age + G + sqrt(X3PAr) + log(OWS + 20) + sqrt(DWS + 20) + sqrt(FGA), data=final_data, subset=train);summary(lm_train2)
# Calculate R2, RMSE, and MAE
predictions1 <- lm_train1 %>% predict(test_data)
data.frame(R2 = R2(predictions1, test_data$season17_18),
           RMSE = RMSE(predictions1, test_data$season17_18),
           MAE = MAE(predictions1, test_data$season17_18))
predictions2 <- lm_train2 %>% predict(test_data)
data.frame(R2 = R2(predictions2, test_data$season17_18),
           RMSE = RMSE(predictions2, test_data$season17_18),
           MAE = MAE(predictions2, test_data$season17_18))
##################################### VERIFY AND CALCULATE PREDICTION INTERVAL ##################################################################
pred_dame <- predict(lm22e,newdata=data.frame(Age=26, G=75.000000,X3PAr=0.3880000,OWS=8.80000000,DWS=1.50000000,FGA=1488.000000),interval="prediction")
pred_dame^3
# RESULTS:
#       fit     lwr      upr
# 16,020,342 5,028,574 36,879,213
# Actual Salary = 26,153,057.0