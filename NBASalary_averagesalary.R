################################################ DATA CLEANING AND TRANSFORMATION ####################################################
# Load libraries
library(tidyverse);library(car);library(emmeans);library(MASS);library(reshape);library(reshape2);library(faraway);library(caret);library(scales);library(gtsummary);library(knitr);library(kableExtra);library(gt)
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
range(final_data$FTr)
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
round(cor(final_data[,c(1:46)]),4)
##################################### OVERALL F-TEST AND MODELING ##################################################################
# Fit the regression, summarize and check assumptions
lm1 <- lm(season17_18 ~ ., data = final_data); summary(lm1);performance::check_model(lm1)
# Check if response needs to be transformed using Box-Cox transformation
ptf<- powerTransform(season17_18~.,data = final_data)
summary(ptf)
# Fit regression with transformed predictors and response (Full model)
lm2 <- lm((season17_18)^0.33~Age+G+GS+MP+PER+TS.+sqrt(X3PAr)+log(FTr+4)+log(ORB.+1)+DRB.+log(TRB.+1)+log(AST.+1)+STL.+sqrt(BLK.)
          + log(TOV.+ 20) + log(USG.) + log(OWS+ 20) + sqrt(DWS+ 20) + sqrt(WS+ 20) + WS.48 + sqrt(OBPM + 20) + sqrt(DBPM + 20) + BPM + log(VORP+ 20) + sqrt(FG+ 20) + FG.+ sqrt(FGA) + sqrt(X3PA+ 20) + sqrt(X2P+ 20) + 
            sqrt(X2PA+ 20) + log(X2P.+ 20) + log(eFG.+ 20) + log(FT+ 20) + log(FTA +6) + FT. + log(ORB+ 20) + sqrt(DRB+ 20) + sqrt(TRB+ 20) + log(AST+ 20) + sqrt(STL+ 20) + log(BLK+ 20) + sqrt(TOV+ 20) + PF + sqrt(PTS+ 20)
          , data=final_data); summary(lm2); performance::check_model(lm2)
# Visual Diagnostics of untransformed and transformed response
par(mfrow=c(1,2));qqPlot(final_data$season17_18, xlab="Salary",ylab="Residuals");qqPlot((final_data$season17_18)^0.33, xlab="Salary^0.33",ylab="Residuals")
par(mfrow=c(1,2));hist(final_data$season17_18, main="", xlab="Salary");hist((final_data$season17_18)^0.33,main="", xlab="Salary^0.33")
par(mfrow=c(1,2));plot(final_data$season17_18,resid(lm2),data=final_data, xlab="Salary",ylab="Residuals");plot((final_data$season17_18)^0.33,resid(lm2),data=final_data, xlab="Salary^0.33",ylab="Residuals")
# Is there a relationship between the response and at least one predictor in our regression model?
null <- lm((season17_18)^0.33~1, data=final_data)
anova(null,lm2)
##################################### TRANSFORMATIONS of PREDICTORS ##################################################################
## Nhi's Plots for transformation##
# No transformation needed
par(mfrow=c(2,2));hist(final_data$Age, main="Age");qqPlot(final_data$Age);plot(final_data$Age,resid(lm1),data=final_data);plot(sqrt(final_data$Age),resid(lm1),data=final_data)
# No transformation needed
par(mfrow=c(2,2));hist(final_data$G, main="Games");qqPlot(final_data$G);plot(final_data$G,resid(lm1),data=final_data)
# No transformation needed
par(mfrow=c(2,2));hist(final_data$GS, main="Games Started");qqPlot(final_data$GS);plot((final_data$GS),resid(lm1),data=final_data);plot(log(final_data$GS),resid(lm1),data=final_data)
# No transformation needed
par(mfrow=c(2,2));hist(final_data$MP, main="Minutes Played");qqPlot(final_data$MP);plot(final_data$MP,resid(lm1),data=final_data)
# No transformation needed
par(mfrow=c(2,2));hist(final_data$PER, main="Player Efficiency Rating");qqPlot(final_data$PER);plot(final_data$PER,resid(lm1),data=final_data)
# No transformation needed
par(mfrow=c(2,2));hist(final_data$TS., main="True Shooting Percentage");qqPlot(final_data$TS.);plot(final_data$TS.,resid(lm1),data=final_data)
# sqrt transformation
par(mfrow=c(2,2));hist(final_data$X3PAr, main="3-Point Attempt Rate");qqPlot(final_data$X3PAr);plot((final_data$X3PAr),resid(lm1),data=final_data);plot(sqrt(final_data$X3PAr),resid(lm1),data=final_data)
# log transformation
par(mfrow=c(2,2));hist(final_data$FTr, main="Free Throw Rate");qqPlot(final_data$FTr);plot((final_data$FTr),resid(lm1),data=final_data);plot(log(final_data$FTr),resid(lm1),data=final_data)
# log transformation
par(mfrow=c(2,2));hist(final_data$ORB., main="Offensive Rebound Percentage");qqPlot(final_data$ORB.);plot((final_data$ORB.),resid(lm1),data=final_data);plot(log(final_data$ORB.),resid(lm1),data=final_data)
# No transformation needed
par(mfrow=c(2,2));hist(final_data$DRB., main="Defensive Rebound Percentage");qqPlot(final_data$DRB.);plot(final_data$DRB.,resid(lm1),data=final_data)
# log transformation
par(mfrow=c(2,2));hist(final_data$TRB., main="Total Rebound Percentage");qqPlot(final_data$TRB.);plot((final_data$TRB.),resid(lm1),data=final_data);plot(log(final_data$TRB.),resid(lm1),data=final_data)
# log transformation
par(mfrow=c(2,2));hist(final_data$AST., main="Assist Percentage");qqPlot(final_data$AST.);plot((final_data$AST.),resid(lm1),data=final_data);plot(log(final_data$AST.),resid(lm1),data=final_data)
# No transformation needed
par(mfrow=c(2,2));hist(final_data$STL., main="Steal Percentage");qqPlot(final_data$STL.);plot(final_data$STL.,resid(lm1),data=final_data)
# sqrt transformation
par(mfrow=c(2,2));hist(final_data$BLK., main="Block Percentage");qqPlot(final_data$BLK.);plot((final_data$BLK.),resid(lm1),data=final_data);plot(log(final_data$BLK.),resid(lm1),data=final_data)
## Thi's Plots for transformation ##
# right skewed: log transformation
ggplot(final_data, aes(TOV.)) + geom_histogram()
par(mfrow=c(2,3));hist(final_data$TOV.);qqPlot(final_data$TOV);plot(final_data$TOV,resid(lm1),data=final_data)
qqPlot(log(final_data$TOV. + 1))
plot(log(final_data$TOV.),resid(lm1),data=final_data)
# right skewed: log transformation
ggplot(final_data, aes(USG.)) + geom_histogram()
par(mfrow=c(2,3));hist(final_data$USG., main="");qqPlot(final_data$USG.);plot(final_data$USG.,resid(lm1),data=final_data)
qqPlot(log(final_data$USG. + 1))
# right skewed: log transformation
ggplot(final_data, aes(OWS)) + geom_histogram()
par(mfrow=c(2,3));hist(final_data$OWS, main="");qqPlot(final_data$OWS);plot(final_data$OWS,resid(lm1),data=final_data) 
qqPlot(sqrt(final_data$OWS)); qqPlot(log(final_data$OWS + 1))
plot(log(final_data$OWS),resid(lm1),data=final_data)
# right skewed: sqrt transformation
ggplot(final_data, aes(DWS)) + geom_histogram()
par(mfrow=c(2,3));hist(final_data$DWS, main="");qqPlot(final_data$DWS);plot(final_data$DWS,resid(lm1),data=final_data)
plot(sqrt(final_data$DWS),resid(lm1),data=final_data)
# right skewed: sqrt transformation
par(mfrow=c(2,3));hist(final_data$WS, main="");qqPlot(final_data$WS);plot(final_data$WS,resid(lm1),data=final_data)
plot(sqrt(final_data$WS),resid(lm1),data=final_data)
# normal : No Transformation
par(mfrow=c(2,3));hist(final_data$WS.48, main="Win shares per 48 min");qqPlot(final_data$WS.48);plot(final_data$WS.48,resid(lm1),data=final_data)
plot(log(final_data$WS.48),resid(lm1),data=final_data)
plot(sqrt(final_data$WS.48),resid(lm1),data=final_data)
# sqrt transformation
ggplot(final_data, aes(OBPM)) + geom_histogram()
par(mfrow=c(2,3));hist(final_data$OBPM, main="OBPM");qqPlot(final_data$OBPM);plot(final_data$OBPM,resid(lm1),data=final_data)
plot(sqrt(final_data$OBPM),resid(lm1),data=final_data)
# sqrt transformation
ggplot(final_data, aes(DBPM)) + geom_histogram()
par(mfrow=c(2,3));hist(final_data$DBPM, main="DBPM");qqPlot(final_data$DBPM);plot(final_data$DBPM,resid(lm1),data=final_data)
plot(sqrt(final_data$DBPM),resid(lm1),data=final_data)
# normal : NTN : outliers
ggplot(final_data, aes(BPM)) + geom_histogram()
par(mfrow=c(2,2));hist(final_data$BPM, main="BPM");qqPlot(final_data$BPM);plot(final_data$BPM,resid(lm1),data=final_data)
# right skewed: log transformation
ggplot(final_data, aes(VORP)) + geom_histogram()
par(mfrow=c(2,3));hist(final_data$VORP, main="VORP");qqPlot(final_data$VORP);plot(final_data$VORP,resid(lm1),data=final_data)
plot(log(final_data$VORP),resid(lm1),data=final_data)
# right skewed: sqrt transformation
ggplot(final_data, aes(FG)) + geom_histogram()
par(mfrow=c(2,3));hist(final_data$FG, main="FG");qqPlot(final_data$FG);plot(final_data$FG,resid(lm1),data=final_data)
plot(sqrt(final_data$FG),resid(lm1),data=final_data)
# right skewed: sqrt transformation
par(mfrow=c(2,3));hist(final_data$FGA, main="FGA");qqPlot(final_data$FGA);plot(final_data$FGA,resid(lm1),data=final_data)
plot(sqrt(final_data$FGA),resid(lm1),data=final_data)
# normal : NTN
par(mfrow=c(2,3));hist(final_data$FG., main="FG.");qqPlot(final_data$FG.);plot(final_data$FG.,resid(lm1),data=final_data)
plot(log(final_data$FG.),resid(lm1),data=final_data)
plot(sqrt(final_data$FG.),resid(lm1),data=final_data)
# right skewed: sqrt transformation
par(mfrow=c(2,3));qqPlot(sqrt(final_data$X3PA)); qqPlot(log(final_data$X3PA));
plot(sqrt(final_data$X3PA),resid(lm1),data=final_data)
# right skewed: sqrt transformation
ggplot(final_data, aes(X2P)) + geom_histogram()
par(mfrow=c(2,3));hist(final_data$X2P, main="X2P");qqPlot(final_data$X2P);plot(final_data$X2P,resid(lm1),data=final_data)
plot(log(final_data$X2P),resid(lm1),data=final_data)
plot(sqrt(final_data$X2P),resid(lm1),data=final_data)
## Angel's Plots for transformation##
# sqrt transformation
par(mfrow=c(2,2)); hist(sqrt(final_data$X2PA)); qqPlot(sqrt(final_data$X2PA)); plot(final_data$X2PA, lm1$residuals)
# log transformation
ggplot(final_data, aes(log(X2P.))) + geom_histogram()
qqPlot(final_data$X2P.); plot(final_data$X2P., lm1$residuals)
qqPlot((log(final_data$X2P.))); plot(log(final_data$X2P.), lm1$residuals)
# log transformation
ggplot(final_data, aes(eFG.)) + geom_histogram()
par(mfrow=c(1,2));qqPlot(final_data$eFG.);qqPlot(log(final_data$eFG.))
par(mfrow=c(1,2));plot(final_data$eFG., lm1$residuals);plot(log(final_data$eFG.), lm1$residuals)
# Right Skew: log transformation
ggplot(final_data, aes(FT)) + geom_histogram()
par(mfrow=c(1,2));qqPlot(log(final_data$FT+20));qqPlot(final_data$FT)
par(mfrow=c(1,2));plot(final_data$FT, lm1$residuals); plot(log(final_data$FT), lm1$residuals)
# Right Skew: log transformation
ggplot(final_data, aes(FTA)) + geom_histogram()
par(mfrow=c(1,2));qqPlot(log(final_data$FTA+6)); qqPlot(final_data$FTA);
par(mfrow=c(1,2));plot(final_data$FTA, lm1$residuals);plot(log(final_data$FTA), lm1$residuals)
# Left Skew: No transformation
ggplot(final_data, aes(FT.)) + geom_histogram()
# No transformation
par(mfrow=c(1,3));qqPlot(sqrt(final_data$FT.)); qqPlot(log(final_data$FT. + 20)); qqPlot(final_data$FT.)
par(mfrow=c(1,2));plot(final_data$FT., lm1$residuals); plot(log(final_data$FT.), lm1$residuals)
# Right Skew: log transformation
ggplot(final_data, aes(ORB)) + geom_histogram()
par(mfrow=c(2,2));qqPlot(sqrt(final_data$ORB)); qqPlot(log(final_data$ORB + 20)); plot(final_data$ORB, lm1$residuals);  plot(log(final_data$ORB), lm1$residuals)
# Right Skew: sqrt transformation
ggplot(final_data, aes(DRB)) + geom_histogram()
par(mfrow=c(2,2));  qqPlot(sqrt(final_data$DRB)); qqPlot(log(final_data$DRB + 20)); plot(final_data$DRB, lm1$residuals); plot(sqrt(final_data$DRB), lm1$residuals)
# Right Skew: sqrt transformation
ggplot(final_data, aes(TRB)) + geom_histogram()
par(mfrow=c(2,2)); qqPlot(sqrt(final_data$TRB)); qqPlot(log(final_data$TRB + 20)); plot(final_data$TRB, lm1$residuals);  plot(sqrt(final_data$TRB), lm1$residuals)
# Right Skew: log transformation
ggplot(final_data, aes(AST)) + geom_histogram()
par(mfrow=c(2,2)); qqPlot(sqrt(final_data$AST)); qqPlot(log(final_data$AST + 20)); plot(final_data$AST, lm1$residuals); plot(log(final_data$AST), lm1$residuals)
# Right Skew: sqrt transformation
ggplot(final_data, aes(STL)) + geom_histogram()
par(mfrow=c(2,3)); qqPlot(final_data$STL); qqPlot(sqrt(final_data$STL)); qqPlot(log(final_data$STL + 20)); plot(final_data$STL, lm1$residuals);  plot(sqrt(final_data$STL), lm1$residuals)
# Right Skew: log transformation
ggplot(final_data, aes(BLK)) + geom_histogram()
par(mfrow=c(2,3)); qqPlot(final_data$BLK); qqPlot(sqrt(final_data$BLK)); qqPlot(log(final_data$BLK + 20)); plot(final_data$BLK, lm1$residuals);  plot(log(final_data$BLK), lm1$residuals)
# Right Skew: sqrt transformation
ggplot(final_data, aes(TOV)) + geom_histogram()
par(mfrow=c(2,3)); qqPlot(final_data$TOV); qqPlot(sqrt(final_data$TOV)); qqPlot(log(final_data$TOV + 20));plot(final_data$TOV, lm1$residuals); plot(log(final_data$TOV), lm1$residuals); plot(sqrt(final_data$TOV), lm1$residuals)
# Bimodal: No transformation
ggplot(final_data, aes(PF)) + geom_histogram()
par(mfrow=c(2,2)); qqPlot(final_data$PF); qqPlot(sqrt(final_data$PF)); qqPlot(log(final_data$PF + 1)); plot(final_data$PF, lm1$residuals)
# Right Skew: sqrt transformation
ggplot(final_data, aes(PTS)) + geom_histogram()
par(mfrow=c(2,3)); qqPlot(final_data$PTS); qqPlot(sqrt(final_data$PTS)); qqPlot(log(final_data$PTS + 20)); plot(final_data$PTS, lm1$residuals); plot(log(final_data$PTS), lm1$residuals); plot(sqrt(final_data$PTS), lm1$residuals)
############################### BACKWARDS ELIMINATION & STEPWISE SELECTION (VARIABLE SELECTION) ############################################################
n = nrow(final_data)
# Drop rejects (rho>0.9) with high collinearity & lower correlation with the response variable
lm3 <- update(lm2, ~. - log(eFG. + 20) - log(TRB. + 1) - log(OWS + 20) - sqrt(FG + 20) - sqrt(X2P + 20) - sqrt(X2PA + 20) - log(FT + 20) - 
                sqrt(TRB + 20) - sqrt(X3PA + 20) - sqrt(FGA)); summary(lm3); performance::check_model(lm3)
# Drop rejects (rho>0.83) with high collinearity & lower correlation with the response variable
lm4 <- update(lm3,~. -log(ORB+20)-log(FT+20)-sqrt(X2PA+20)-sqrt(TOV+20)-sqrt(X2P+20)-log(X2P.+20));summary(lm4); performance::check_model(lm4);anova(lm4,lm2)
# Run AIC model after dropping rejects with rho>0.83 and perform backwards elimination
lm4a <- step(lm4);summary(lm4a); performance::check_model(lm4a);anova(lm4a,lm2)
  lm4b <- update(lm4a,~.-PF);summary(lm4b); performance::check_model(lm4b);anova(lm4b,lm2)
  lm4c <- update(lm4b,~.-sqrt(BLK.));summary(lm4c); performance::check_model(lm4c);anova(lm4c,lm2) #################### good model
  lm4d <- update(lm4c,~.-GS);summary(lm4d); performance::check_model(lm4d);anova(lm4d,lm2)
  lm4e <- update(lm4d,~.-sqrt(DBPM + 20));summary(lm4e); performance::check_model(lm4e);anova(lm4e,lm2)
  lm4f <- update(lm4e,~.-DRB.);summary(lm4f); performance::check_model(lm4f);anova(lm4f,lm2)
  lm4g <- update(lm4f,~.-BPM);summary(lm4g); performance::check_model(lm4g);anova(lm4g,lm2)
  lm4h <- update(lm4g,~.-PER);summary(lm4h); performance::check_model(lm4h);anova(lm4h,lm2)
# Run BIC model after dropping rejects with rho>0.83
lm4i <- step(lm4, k=log(n)); summary(lm4i); performance::check_model(lm4i)
# Run BIC model without dropping rejects with high collinearity & lower correlation with response variable and perform backwards elimination
lm21 <- step(lm2, k = log(n));summary(lm21); performance::check_model(lm21)
  lm21a <- update(lm21,~.-GS);summary(lm21a); performance::check_model(lm21a)
  lm21b <- update(lm21a,~.-log(OWS+20));summary(lm21b); performance::check_model(lm21b)
  lm21c <- update(lm21b,~.-sqrt(X2P+20));summary(lm21c); performance::check_model(lm21c)
################################################ MODEL SELECTION ##################################################################
# Compare all models against full model
anova(lm3,lm2) # keep lm3
anova(lm4,lm2) # keep lm4
anova(lm4a,lm2) # keep lm4a
anova(lm4b,lm2) # keep lm4b
anova(lm4c,lm2) # keep lm4c
anova(lm4d,lm2) # keep lm4d
anova(lm4e,lm2) # keep lm4e
anova(lm4f,lm2) # keep lm4f
anova(lm4g,lm2) # keep lm4g
anova(lm4h,lm2) # keep lm4h
anova(lm4i, lm2) # keep lm4i
anova(lm21,lm2) # keep lm21
anova(lm21a,lm2)# keep lm21a
anova(lm21b,lm2) # keep lm21b
anova(lm21c,lm2) # use lm2
# Assign models to a name
s2 <- summary(lm2)$adj.r.squared
s3 <- summary(lm3)$adj.r.squared
s4 <- summary(lm4)$adj.r.squared
s4a <- summary(lm4a)$adj.r.squared
s4b <- summary(lm4b)$adj.r.squared
s4c <- summary(lm4c)$adj.r.squared
s4d <- summary(lm4d)$adj.r.squared
s4e <- summary(lm4e)$adj.r.squared
s4f <- summary(lm4f)$adj.r.squared
s4g <- summary(lm4g)$adj.r.squared
s4h <- summary(lm4h)$adj.r.squared
s4i <- summary(lm4i)$adj.r.squared
s21 <- summary(lm21)$adj.r.squared
s21a <- summary(lm21a)$adj.r.squared
s21b <- summary(lm21b)$adj.r.squared
s21c <- summary(lm21c)$adj.r.squared
# View adjusted R squared
s2;s4;s4a;s4b;s4c;s4d;s4e;s4f;s4g;s4h;s4i
s2;s3;s21;s21a;s21b;s21c
# Run BIC/AIC model selection to determine model with lower AIC or BIC
BIC(lm2,lm3,lm21,lm21a,lm21b,lm21c)
AIC(lm2,lm4,lm4a,lm4b,lm4c,lm4d,lm4e,lm4f,lm4g,lm4h)
BIC(lm2,lm4i)
# Create correlation and scatter plot matrices of best model (lower BIC than full model, no multicollinearity, significant predictors, and met all assumptions reasonably)
round(cor(final_data[,c(1,2,3,19,46)]),4)
pairs((season17_18)^0.33 ~ Age + G + sqrt(DWS+20) + sqrt(PTS+20), data = final_data)
################################################ CROSS VALIDATION ##################################################################
set.seed(111)
n <- nrow(final_data);n
z <- floor(0.7*n)
train <- sample(1:n, z)
test_data <- final_data[-train,]
# Train models full model lm2 (lm_train1) and lm4i (lm_train2)
lm_train1 <- lm(season17_18^0.33 ~ Age + G + GS + MP + PER + TS. + 
                  sqrt(X3PAr) + log(FTr + 4) + log(ORB. + 1) + DRB. + log(TRB. + 1) + log(AST. + 1) + STL. + sqrt(BLK.) + log(TOV. + 20) + 
                  log(USG.) + log(OWS + 20) + sqrt(DWS + 20) + sqrt(WS + 20) + WS.48 + sqrt(OBPM + 20) + sqrt(DBPM + 20) + BPM + log(VORP + 20) + 
                  sqrt(FG + 20) + FG. + sqrt(FGA) + sqrt(X3PA + 20) + sqrt(X2P + 20) + sqrt(X2PA + 20) + log(X2P. + 20) + log(eFG. + 20) + 
                  log(FT + 20) + log(FTA + 6) + FT. + log(ORB + 20) + sqrt(DRB + 20) + sqrt(TRB + 20) + log(AST + 20) + sqrt(STL + 20) +
                  log(BLK + 20) + sqrt(TOV + 20) + PF + sqrt(PTS + 20), data = final_data, subset=train); summary(lm_train1)
lm_train2 <- lm((season17_18)^0.33 ~ Age + G + sqrt(DWS+20) + sqrt(PTS+20), data = final_data, subset=train);summary(lm_train2)
# Calculate R2, RMSE, and MAE
predictions1 <- lm_train1 %>% predict(test_data)
aa <- data.frame(R2 = R2(predictions1, test_data$season17_18^0.33),
           RMSE = RMSE(predictions1, test_data$season17_18^0.33),
           MAE = MAE(predictions1, test_data$season17_18^0.33)); aa
predictions2 <- lm_train2 %>% predict(test_data)
ab <-data.frame(R2 = R2(predictions2, test_data$season17_18^0.33),
           RMSE = RMSE(predictions2, test_data$season17_18^0.33),
           MAE = MAE(predictions2, test_data$season17_18^0.33)); ab
##################################### VERIFY AND CALCULATE PREDICTION INTERVAL ##################################################################
# Test predictability on 2017-18 salary
pred_dame1 <- predict(lm4i,newdata = data.frame(Age=26,G=75.000000,DWS=1.50000000,PTS=2024.000000), interval="prediction")
pred_dame1^3
# fit      lwr      upr
# 14,481,628 4,331,569 34,164,723
# Actual 2017-18 Salary = 26,153,057.0

# Test predictability on 2020-21 salary
# https://www.basketball-reference.com/leagues/NBA_2021_advanced.html
pred_dame2 <- predict(lm4i,newdata = data.frame(Age=30,G=67,DWS=0.8,PTS=1928), interval="prediction")
pred_dame2^3
# fit        lwr       upr
# 13,935,753 4,048,273 33,359,456
# Actual 2021-22 Salary = 31,626,953
# Note: 2020-21 salary was calculated rather than 2021-22 salary due to Lillard's injury which doesn't give an accurate estimated salary
############################################################## FIGURES  ###########################################################################
# Observe data with statistically significant predictors (lm4i) using Box Plot for paper and presentation
dd <- final_data %>% 
  dplyr::rename("Games" = G) %>% 
  dplyr::rename("Defensive Win Shares" = DWS) %>% 
  dplyr::rename("Points" = PTS) %>%
  dplyr::mutate(Salary = season17_18)
d <- melt(dd, id="season17_18")
dt1 <- dd[,c(1,2,3)]; e1 <- melt(dt1,id="season17_18")
dt2 <- dd[,c(1,19)]; e2 <- melt(dt2,id="season17_18")
dt3 <- dd[,c(1,46)]; e3 <- melt(dt3,id="season17_18")
dt4 <- dd[,c(1,47)]; e4 <- melt(dt4,id="season17_18")
ggplot(e1,aes(x=variable,y=value)) + geom_boxplot(fill='#FFF2CC', color="black") + xlab("") + ylab("")+ theme(text = element_text(size = 12))# + theme(axis.text.x = element_text(size=8, angle=45))
ggplot(e2,aes(x=variable,y=value)) + geom_boxplot(fill='#FFF2CC', color="black") + xlab("") + ylab("")+ theme(text = element_text(size = 12))
ggplot(e3,aes(x=variable,y=value)) + geom_boxplot(fill='#FFF2CC', color="black") + xlab("") + ylab("")+ theme(text = element_text(size = 12))
ggplot(e4,aes(x=variable,y=value)) + geom_boxplot(fill='#B6D7A8', color="black") + scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) + xlab("") + ylab("")+ theme(text = element_text(size = 12))
# Boxplots without color for research paper
ggplot(e1,aes(x=variable,y=value)) + geom_boxplot() + xlab("") + ylab("")+ theme(text = element_text(size = 12))# + theme(axis.text.x = element_text(size=8, angle=45))
ggplot(e2,aes(x=variable,y=value)) + geom_boxplot() + xlab("") + ylab("")+ theme(text = element_text(size = 12))
ggplot(e3,aes(x=variable,y=value)) + geom_boxplot() + xlab("") + ylab("")+ theme(text = element_text(size = 12))
ggplot(e4,aes(x=variable,y=value)) + geom_boxplot() + scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) + xlab("") + ylab("")+ theme(text = element_text(size = 12))
# Scatterplot matrix untransformed for presentation purposes
pairs(season17_18~Age+G+DWS+PTS,data=final_data)
############################################################## TABLES ###########################################################################
# Cross-Validation Tables for paper and presentation
aa %>% kbl() %>% kable_classic(full_width=F,html_font="Arial")
ab %>% kbl() %>% kable_classic(full_width=F,html_font="Arial")
# Summary Table with gt package for paper and presentation
t1 <- tbl_regression(lm4i);t1
# Summary Table with knitr::kableExtra for paper purposes
knit <- coef(summary(lm4i))
knit %>% 
  kbl() %>% 
  kable_classic(full_width=F,html_font="Arial")
# Descriptive Statistics with gt package (Optional use due to similarity to box plots)
list <- final_data[1:12] %>% 
  tbl_summary(
    statistic = ~"{mean}; {median} ({min},{max})"); list
list1 <- final_data[13:24] %>% 
  tbl_summary(
    statistic = ~"{mean}; {median} ({min},{max})"); list1
list2 <- final_data[25:36] %>% 
  tbl_summary(
    statistic = ~"{mean}; {median} ({min},{max})"); list2
list3 <- final_data[37:46] %>% 
  tbl_summary(
    statistic = ~"{mean}; {median} ({min},{max})"); list3
