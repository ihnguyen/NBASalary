library(tidyverse)
library(car)
library(emmeans)

# Read csv files
salary_data <- read.csv("NBA_season1718_salary.csv")
stats_data <- read.csv("Seasons_Stats.csv")

# Full Join all data sets
data0 <- sal_fdata %>% 
  full_join(.,st_fdata, by=unique("Player")) %>% 
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
  select(-Player) %>% 
  na.omit(); dim(fdata)
403/486

# IF PREDICTOR HAS A ZERO VALUE, WE CAN'T DO LOG TRANSFORMATION
final_data <- data.frame(fdata)

# Fit the regression
lm1 <- lm(season17_18 ~ ., data = final_data)
summary(lm1)


# Model with transformed predictors
lm2 <- lm(season17_18~Age+G+GS+MP+PER+TS.+sqrt(X3PAr)+log(FTr+1)+log(ORB.+1)+DRB.+log(TRB.+1)+log(AST.+1)+STL.+sqrt(BLK.)
            
          
          
          , data=final_data)
summary(lm2)

# Check the assumptions
performance::check_model(lm1)

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



# Correlation matrix as a data frame
corr_df <- round(cor(final_data[,1:47]),2) #Not including response variable, Salary, which is 50th column

# Scan the upper right triangle of the correlation matrix. Print indeces that have correlation values greater than .88
corr_vals <- c()
for(i in 1:ncol(corr_df)){
  for(j in i:ncol(corr_df)){
    if(corr_df[i,j] > .88 & corr_df[i,j] < 1){
      corr_vals <- append(corr_vals, c(i,j))
    }
  }
}
corr_vals



# #Taking sums of annual stats to calculate career stats 
# career_st <- stats_data %>% 
#   group_by(Player) %>% 
#   summarise(CareerFT = sum(FT), CareerAST = sum(AST), CareerRB = sum(TRB), CareerBLK = sum(BLK), 
#             CareerSTL = sum(STL), )
# #Making a dataframe with only numerical variables
# data_num <- subset(data, select = c(Height ,Weight ,born ,YrsExperience ,Age ,G,
#                                     GS ,MP ,PER ,TS. ,X3PAr ,FTr ,ORB. ,DRB. ,TRB.
#                                     ,AST. ,STL. ,BLK. ,TOV. ,USG. ,OWS ,DWS ,WS
#                                     ,WS.48 ,OBPM ,DBPM ,BPM ,VORP ,FG ,FGA ,FG.
#                                     ,X3P ,X3PA ,X2P ,X2PA ,X2P. ,eFG. ,FT ,FTA
#                                     ,FT. ,ORB ,DRB ,TRB ,AST ,STL ,BLK ,TOV ,PF
#                                     ,PTS, Salary)) %>%
#   na.omit()
