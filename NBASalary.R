library(tidyverse)
library(car)
library(emmeans)
library(MASS)
library(reshape)
library(reshape2)

# Read csv files
salary_data <- read.csv("NBA_season1718_salary.csv")
stats_data <- read.csv("Seasons_Stats.csv")

dim(salary_data);dim(stats_data)

# Full Join all data sets
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
# of the 610 observations in the st_fdata, 486 were distinct observations
# omitted height & weight since it shortened final data set from 70% to 60%


# Omit Player column
fdata <- data0 %>% 
  dplyr::select(-Player) %>% 
  na.omit(); dim(fdata)
403/486

# IF PREDICTOR HAS A ZERO VALUE, WE CAN'T DO LOG TRANSFORMATION
final_data <- data.frame(fdata)

# Observe data using Scatter Plot
d <- melt(final_data, id="season17_18") #"Age" ,"G","GS" ,"MP" ,"PER" ,"TS." ,"X3PAr" ,"FTr" ,"ORB." ,"DRB." ,"TRB.","AST.",
#"STL." ,"BLK." ,"TOV."))  ,USG. ,OWS ,DWS ,WS,WS.48 ,OBPM ,DBPM ,BPM ,VORP ,FG ,FGA ,FG.,X3P ,X3PA ,X2P ,X2PA ,X2P. ,eFG. ,FT ,FTA,FT. ,ORB ,DRB ,TRB ,AST ,STL ,BLK ,TOV ,PF,PTS")
  
ggplot(d,aes(x=variable,y=value,color=variable)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(size=10, angle=45))

dt <- final_data[,c(1,2,3,5,6,10)]
e <- melt(dt,id="season17_18")

ggplot(e,aes(x=variable,y=value,color=variable)) +
  geom_boxplot()


# Fit the regression
lm1 <- lm(season17_18 ~ ., data = final_data)
summary(lm1)


# Model with transformed predictors
lm2 <- lm(season17_18~Age+G+GS+MP+PER+TS.+sqrt(X3PAr)+log(FTr+1)+log(ORB.+1)+DRB.+log(TRB.+1)+log(AST.+1)+STL.+sqrt(BLK.)
            
          + log(TOV.+ 20) + USG. + log(OWS+ 20) + sqrt(DWS+ 20) + sqrt(WS+ 20) + WS.48 + sqrt(OBPM + 20) + sqrt(DBPM + 20) + BPM + log(VORP+ 20) + sqrt(FG+ 20) + FG.+ sqrt(FGA) + sqrt(X3PA+ 20) + sqrt(X2P+ 20) + 
            sqrt(X2PA+ 20) + log(X2P.+ 20) + log(eFG.+ 20) + log(FT+ 20) + log(FTA +6) + FT. + log(ORB+ 20) + sqrt(DRB+ 20) + sqrt(TRB+ 20) + log(AST+ 20) + sqrt(STL+ 20) + log(BLK+ 20) + sqrt(TOV+ 20) + PF + sqrt(PTS+ 20)
          
          , data=final_data)
summary(lm2)

# Linear model with our first set of rejects (based on high colinearity & lower correlation with the response variable)
lm3 <- update(lm2, ~. - log(eFG. + 20) - log(TRB. + 1) - log(OWS + 20) - sqrt(FG + 20) - sqrt(X2P + 20) - sqrt(X2PA + 20) - log(FT + 20) - 
                sqrt(TRB + 20) - sqrt(X3PA + 20) - sqrt(FGA))
summary(lm3)

############ Next time do lm before lm4 that removes the negative correlations b/w (-1,-.9] ###############

#Linear model with our second set of rejects (based on high colinearity & lower correlation with the response variable)
lm4 <- update(lm3, ~. - G - MP - WS.48 - log(ORB. + 1) - sqrt(TRB + 20) - log(ORB + 20) - log(VORP + 20) - log(FTA + 6) - 
                sqrt(OBPM + 20) - log(FT + 20) - sqrt(TOV + 20) - log(X2P. + 20) - FG. - sqrt(X2P + 20) - sqrt(X2PA + 20) -
                log(AST + 20))
summary(lm4)

# Check the assumptions
performance::check_model(lm1)
performance::check_model(lm2)

# Model Comparisons
# Is there a relationship between the response variable and at least one of the predictors?
null_model <- lm(season17_18~1,data=final_data)
anova(null_model,lm2)

# Is there one a specific subset of predictors that can be dropped out?
#reduced_model <- lm(season17_18~ , data=final_data)
#anova(reduced_model,lm2)

# Identify predictors that need transformation
# sqrt transformation? doesn't look much different
par(mfrow=c(2,2));hist(final_data$Age, main="Age");qqPlot(final_data$Age);plot(final_data$Age,resid(lm1),data=final_data);plot(sqrt(final_data$Age),resid(lm1),data=final_data)

# unsure which tx to do for Games
par(mfrow=c(2,2));hist(final_data$G, main="Games");qqPlot(final_data$G);plot(final_data$G,resid(lm1),data=final_data)

par(mfrow=c(2,2));hist(final_data$GS, main="Games Started");qqPlot(final_data$GS);plot((final_data$GS),resid(lm1),data=final_data);plot(log(final_data$GS),resid(lm1),data=final_data)

par(mfrow=c(2,2));hist(final_data$MP, main="Minutes Played");qqPlot(final_data$MP);plot(final_data$MP,resid(lm1),data=final_data)

par(mfrow=c(2,2));hist(final_data$PER, main="Player Efficiency Rating");qqPlot(final_data$PER);plot(final_data$PER,resid(lm1),data=final_data)

par(mfrow=c(2,2));hist(final_data$TS., main="True Shooting Percentage");qqPlot(final_data$TS.);plot(final_data$TS.,resid(lm1),data=final_data)

# sqrt transformation
par(mfrow=c(2,2));hist(final_data$X3PAr, main="3-Point Attempt Rate");qqPlot(final_data$X3PAr);plot((final_data$X3PAr),resid(lm1),data=final_data);plot(sqrt(final_data$X3PAr),resid(lm1),data=final_data)

# log transformation
par(mfrow=c(2,2));hist(final_data$FTr, main="Free Throw Rate");qqPlot(final_data$FTr);plot((final_data$FTr),resid(lm1),data=final_data);plot(log(final_data$FTr),resid(lm1),data=final_data)

# log transformation
par(mfrow=c(2,2));hist(final_data$ORB., main="Offensive Rebound Percentage");qqPlot(final_data$ORB.);plot((final_data$ORB.),resid(lm1),data=final_data);plot(log(final_data$ORB.),resid(lm1),data=final_data)

par(mfrow=c(2,2));hist(final_data$DRB., main="Defensive Rebound Percentage");qqPlot(final_data$DRB.);plot(final_data$DRB.,resid(lm1),data=final_data)

# log transformation
par(mfrow=c(2,2));hist(final_data$TRB., main="Total Rebound Percentage");qqPlot(final_data$TRB.);plot((final_data$TRB.),resid(lm1),data=final_data);plot(log(final_data$TRB.),resid(lm1),data=final_data)

# log transformation
par(mfrow=c(2,2));hist(final_data$AST., main="Assist Percentage");qqPlot(final_data$AST.);plot((final_data$AST.),resid(lm1),data=final_data);plot(log(final_data$AST.),resid(lm1),data=final_data)

par(mfrow=c(2,2));hist(final_data$STL., main="Steal Percentage");qqPlot(final_data$STL.);plot(final_data$STL.,resid(lm1),data=final_data)

# log transformation; either sqrt and log work here
par(mfrow=c(2,2));hist(final_data$BLK., main="Block Percentage");qqPlot(final_data$BLK.);plot((final_data$BLK.),resid(lm1),data=final_data);plot(log(final_data$BLK.),resid(lm1),data=final_data)



## Angel's Plots for transformation###################################

#Right Skew
par(mfrow=c(2,2)); ggplot(final_data, aes(X2PA)) + geom_histogram()
#log() made it left skewed.. sqrt() makes data most normal
par(mfrow=c(2,2)); hist(sqrt(final_data$X2PA)); qqPlot(sqrt(final_data$X2PA)); plot(final_data$X2PA, lm1$residuals)

#Normal-Looking
ggplot(final_data, aes(sqrt(X2P.))) + geom_histogram()
qqPlot((sqrt(final_data$X2P.))); plot(final_data$X2P., lm1$residuals)

#Normal-Looking
ggplot(final_data, aes(eFG.)) + geom_histogram()
qqPlot(final_data$eFG.); plot(final_data$eFG., lm1$residuals)

#Right Skew
ggplot(final_data, aes(FT)) + geom_histogram()
par(mfrow=c(2,2));qqPlot(sqrt(final_data$FT)); qqPlot(final_data$FT + 1); plot(final_data$FT, lm1$residuals); plot(log(final_data$FT), lm1$residuals)

#Right Skew
ggplot(final_data, aes(FTA)) + geom_histogram()
# I think log() is better here?
qqPlot(log(final_data$FTA)); qqPlot(sqrt(final_data$FTA)); plot(final_data$FTA, lm1$residuals);plot(log(final_data$FTA), lm1$residuals)

#Left Skew
ggplot(final_data, aes(FT.)) + geom_histogram()
#Log and sqrt don't seem to change it much. After residuals, lets keep original
qqPlot(sqrt(final_data$FT.)); qqPlot(log(final_data$FT. + 20)); plot(final_data$FT., lm1$residuals); plot(log(final_data$FT.), lm1$residuals)


#Right Skew
ggplot(final_data, aes(ORB)) + geom_histogram()
#Both help, but not sure which is better. Let's use the log transform
par(mfrow=c(2,2));qqPlot(sqrt(final_data$ORB)); qqPlot(log(final_data$ORB + 1)); plot(final_data$ORB, lm1$residuals);  plot(log(final_data$ORB), lm1$residuals)

#Right Skew
ggplot(final_data, aes(DRB)) + geom_histogram()
#sqrt really helped here! Lets use sqrt
par(mfrow=c(2,2));  qqPlot(sqrt(final_data$DRB)); qqPlot(log(final_data$DRB + 1)); plot(final_data$DRB, lm1$residuals); plot(sqrt(final_data$DRB), lm1$residuals)

#Right Skew
ggplot(final_data, aes(TRB)) + geom_histogram()
#sqrt normalizes here use sqrt
par(mfrow=c(2,2)); qqPlot(sqrt(final_data$TRB)); qqPlot(log(final_data$TRB + 1)); plot(final_data$TRB, lm1$residuals);  plot(sqrt(final_data$TRB), lm1$residuals)

#Right Skew
ggplot(final_data, aes(AST)) + geom_histogram()
#sqrt seems to help more use log
par(mfrow=c(2,2)); qqPlot(sqrt(final_data$AST)); qqPlot(log(final_data$AST + 1)); plot(final_data$AST, lm1$residuals); plot(log(final_data$AST), lm1$residuals)


#Right Skew
ggplot(final_data, aes(STL)) + geom_histogram()
#sqrt normalizes; use sqrt
par(mfrow=c(2,3)); qqPlot(final_data$STL); qqPlot(sqrt(final_data$STL)); qqPlot(log(final_data$STL + 1)); plot(final_data$STL, lm1$residuals);  plot(sqrt(final_data$STL), lm1$residuals)


#Right Skew
ggplot(final_data, aes(BLK)) + geom_histogram()
#I think log is better here use log
par(mfrow=c(2,3)); qqPlot(final_data$BLK); qqPlot(sqrt(final_data$BLK)); qqPlot(log(final_data$BLK + 1)); plot(final_data$BLK, lm1$residuals);  plot(log(final_data$BLK), lm1$residuals)

#Right Skew
ggplot(final_data, aes(TOV)) + geom_histogram()
#sqrt worked use sqrt
par(mfrow=c(2,3)); qqPlot(final_data$TOV); qqPlot(sqrt(final_data$TOV)); qqPlot(log(final_data$TOV + 1));plot(final_data$TOV, lm1$residuals); plot(log(final_data$TOV), lm1$residuals); plot(sqrt(final_data$TOV), lm1$residuals)

#Sort of Bimodal
ggplot(final_data, aes(PF)) + geom_histogram()
#Probably best the way it is with no transformation
par(mfrow=c(2,2)); qqPlot(final_data$PF); qqPlot(sqrt(final_data$PF)); qqPlot(log(final_data$PF + 1)); plot(final_data$PF, lm1$residuals)

#Right Skew
ggplot(final_data, aes(PTS)) + geom_histogram()
#sqrt; use sqrt
par(mfrow=c(2,3)); qqPlot(final_data$PTS); qqPlot(sqrt(final_data$PTS)); qqPlot(log(final_data$PTS + 1)); plot(final_data$PTS, lm1$residuals); plot(log(final_data$PTS), lm1$residuals); plot(sqrt(final_data$PTS), lm1$residuals)

# Correlation matrix as a data frame
corr_df <- round(cor(final_data[,1:46]),2) #Not including response variable, Salary, which is 50th column

#find indeces with correlation greater than .9 and less than 1
ind <- which(corr_df >0.9 & corr_df <1)
ind

corr_df[ind,]

# Scan the upper right triangle of the correlation matrix. Print indeces that have correlation values greater than .9
corr_vals <- c()
for(i in 1:ncol(corr_df)){
  for(j in i:ncol(corr_df)){
    if(corr_df[i,j] > .9 & corr_df[i,j] < 1){
      corr_vals <- append(corr_vals, c(i,j))
    }
  }
}
length(corr_vals)
corr_vals

#Compare variables with high colinearity with the response variable###
#Make a matrix to see the index pais with high colinearity
colnames(corr_df)
corr_pairs <- matrix(corr_vals, ncol = 2, byrow = TRUE)


# Scan the upper right triangle of the correlation matrix. Print indeces that have correlation values greater than .83 and less than .9
corr_vals2 <- c()
for(i in 1:ncol(corr_df)){
  for(j in i:ncol(corr_df)){
    if(corr_df[i,j] > .83 & corr_df[i,j] <= .9){
      corr_vals2 <- append(corr_vals2, c(i,j))
    }
  }
}
length(corr_vals2)
corr_vals2

corr_pairs2 <- matrix(corr_vals2, ncol = 2, byrow = TRUE)
corr_pairs2




###Pairs that we commented out 
# corr_pairs <- matrix(corr_vals, ncol = 2, byrow = TRUE)
# corr_pairs

#pairs(season17_18 ~ TS. + eFG. + DRB + TRB + OWS + WS + FG + FGA + X2P + X2PA ,data = final_data)




#Thi's transformation:

#RIGHT SKEWED: 
ggplot(final_data, aes(TOV.)) + geom_histogram()
par(mfrow=c(2,3));hist(final_data$TOV., main="Games");qqPlot(final_data$TOV);plot(final_data$TOV,resid(lm1),data=final_data)
qqPlot(log(final_data$TOV. + 1))

#Use log
plot(log(final_data$TOV.),resid(lm1),data=final_data)

#LIGHT RIGHT SKEWED : No transfromation needed
ggplot(final_data, aes(USG.)) + geom_histogram()

par(mfrow=c(2,3));hist(final_data$USG., main="Games");qqPlot(final_data$USG.);plot(final_data$USG.,resid(lm1),data=final_data)
qqPlot(log(final_data$USG. + 1))

#right skewed: Use log
ggplot(final_data, aes(OWS)) + geom_histogram()
##Normalized:

par(mfrow=c(2,3));hist(final_data$OWS, main="Games");qqPlot(final_data$OWS);plot(final_data$OWS,resid(lm1),data=final_data) 
qqPlot(sqrt(final_data$OWS)); qqPlot(log(final_data$OWS + 1))
plot(log(final_data$OWS),resid(lm1),data=final_data)

#right skewed : sqrt
ggplot(final_data, aes(DWS)) + geom_histogram()
##Normalized:

par(mfrow=c(2,3));hist(final_data$DWS, main="Games");qqPlot(final_data$DWS);plot(final_data$DWS,resid(lm1),data=final_data)

plot(sqrt(final_data$DWS),resid(lm1),data=final_data)

#right skewed : use sqrt



par(mfrow=c(2,3));hist(final_data$WS, main="Games");qqPlot(final_data$WS);plot(final_data$WS,resid(lm1),data=final_data)

plot(sqrt(final_data$WS),resid(lm1),data=final_data)

#normal : no

par(mfrow=c(2,3));hist(final_data$WS.48, main="Win shares per 48 min");qqPlot(final_data$WS.48);plot(final_data$WS.48,resid(lm1),data=final_data)

plot(log(final_data$WS.48),resid(lm1),data=final_data)
plot(sqrt(final_data$WS.48),resid(lm1),data=final_data)


#normal : use sqrt
ggplot(final_data, aes(OBPM)) + geom_histogram()
par(mfrow=c(2,3));hist(final_data$OBPM, main="OBPM");qqPlot(final_data$OBPM);plot(final_data$OBPM,resid(lm1),data=final_data)


plot(sqrt(final_data$OBPM),resid(lm1),data=final_data)
#normal : use sqrt
ggplot(final_data, aes(DBPM)) + geom_histogram()
par(mfrow=c(2,3));hist(final_data$DMPM, main="DBPM");qqPlot(final_data$DBPM);plot(final_data$DBPM,resid(lm1),data=final_data)



plot(sqrt(final_data$DBPM),resid(lm1),data=final_data)


#normal : NTN : outliers
ggplot(final_data, aes(BPM)) + geom_histogram()
par(mfrow=c(2,2));hist(final_data$BPM, main="BPM");qqPlot(final_data$BPM);plot(final_data$BPM,resid(lm1),data=final_data)



#right skewed : use log
ggplot(final_data, aes(VORP)) + geom_histogram()
##normalized:

par(mfrow=c(2,3));hist(final_data$VORP, main="VORP");qqPlot(final_data$VORP);plot(final_data$VORP,resid(lm1),data=final_data)
plot(log(final_data$VORP),resid(lm1),data=final_data)


# chucky right skewed?  USE sqrt
ggplot(final_data, aes(FG)) + geom_histogram()
##Normalized:

par(mfrow=c(2,3));hist(final_data$FG, main="FG");qqPlot(final_data$FG);plot(final_data$FG,resid(lm1),data=final_data)


plot(sqrt(final_data$FG),resid(lm1),data=final_data)

# right skewed but weird right : USE sqrt


par(mfrow=c(2,3));hist(final_data$FGA, main="FG");qqPlot(final_data$FGA);plot(final_data$FGA,resid(lm1),data=final_data)


plot(sqrt(final_data$FGA),resid(lm1),data=final_data)


#normal : NTN

par(mfrow=c(2,3));hist(final_data$FG., main="FG.");qqPlot(final_data$FG.);plot(final_data$FG.,resid(lm1),data=final_data)


plot(log(final_data$FG.),resid(lm1),data=final_data)
plot(sqrt(final_data$FG.),resid(lm1),data=final_data)


# right skewed


##normalized : USE sqrt
par(mfrow=c(2,3));qqPlot(sqrt(final_data$X3PA)); qqPlot(log(final_data$X3PA));

plot(sqrt(final_data$X3PA),resid(lm1),data=final_data)



#right skewed USE qrt
ggplot(final_data, aes(X2P)) + geom_histogram()

par(mfrow=c(2,3));hist(final_data$X2P, main="X2P");qqPlot(final_data$X2P);plot(final_data$X2P,resid(lm1),data=final_data)

plot(log(final_data$X2P),resid(lm1),data=final_data)
plot(sqrt(final_data$X2P),resid(lm1),data=final_data)


# #Taking sums of annual stats to calculate career stats 
# career_st <- stats_data %>% 
#   group_by(Player) %>% 
#   summarise(CareerFT = sum(FT), CareerAST = sum(AST), CareerRB = sum(TRB), CareerBLK = sum(BLK), 
#             CareerSTL = sum(STL), )
# #Making a dataframe with only numerical variables
# data_num <- subset(data, select = c(Age ,G,
#                                     GS ,MP ,PER ,TS. ,X3PAr ,FTr ,ORB. ,DRB. ,TRB.
#                                     ,AST. ,STL. ,BLK. ,TOV. ,USG. ,OWS ,DWS ,WS
#                                     ,WS.48 ,OBPM ,DBPM ,BPM ,VORP ,FG ,FGA ,FG.
#                                     ,X3P ,X3PA ,X2P ,X2PA ,X2P. ,eFG. ,FT ,FTA
#                                     ,FT. ,ORB ,DRB ,TRB ,AST ,STL ,BLK ,TOV ,PF
#                                     ,PTS, Salary)) %>%
#   na.omit()
