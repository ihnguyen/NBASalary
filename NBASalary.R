library(tidyverse)
library(car)
library(emmeans)

# Read csv files
salary_data <- read.csv("NBA_season1718_salary.csv")
player_data <- read.csv("player_data.csv")
stats_data <- read.csv("Seasons_Stats.csv")

# Omit X column from all data sets
sal_data <- salary_data %>% 
  select(-c(X))
st_data <- stats_data %>% 
  select(-c(X)) %>% 
  subset(Year == 2017)
# Calculate Years of Experience from Player data set
play_data <- player_data %>% 
  mutate(YrsExperience = (year_end - year_start))

# Store data sets as data frames and omit irrelevant columns
st_fdata <- data.frame(st_data) %>% 
  select(-c(blanl,blank2)); dim(st_fdata)
sal_fdata <- data.frame(sal_data); dim(sal_fdata)
play_fdata <- data.frame(play_data) %>% 
  select(-c(position,year_start,year_end,height,weight)) %>% 
  rename(Player = name); dim(play_fdata)

# Full Join all data sets
data0 <- play_fdata %>% 
  full_join(.,sal_fdata, by="Player") %>% 
  full_join(.,st_fdata, by=unique("Player")) %>% 
  group_by(Player) %>% 
  summarise(season17_18 = sum(season17_18), YrsExperience = sum(YrsExperience), Age = mean(Age),
            G = mean(G),GS = mean(GS),MP = mean(MP),PER = mean(PER),TS. = mean(TS.),
            X3PAr =mean(X3PAr),FTr = mean(FTr),ORB. = mean(ORB.),DRB. = mean(DRB.),
            TRB. = mean(TRB.),AST. = mean(AST.),STL. = mean(STL.),BLK. = mean(BLK.),
            TOV.  = mean(TOV.),USG. = mean(USG.) ,OWS = mean(OWS) ,DWS = mean(DWS) ,WS = mean(WS),
            WS.48 = mean(WS.48) ,OBPM = mean(OBPM) ,DBPM = mean(DBPM) ,BPM = mean(BPM) ,VORP = mean(VORP),
            FG = mean(FG) ,FGA = mean(FGA) ,FG. = mean(FG.),X3P = mean(X3P) ,X3PA = mean(X3PA) ,
            X2P = mean(X2P) ,X2PA = mean(X2PA) ,X2P. = mean(X2P.) ,eFG. = mean(eFG.),FT = mean(FT),
            FTA = mean(FTA),FT. = mean(FT.),ORB = mean(ORB),DRB = mean(DRB),TRB = mean(TRB),
            AST = mean(AST),STL = mean(STL),BLK = mean(BLK),TOV = mean(TOV),PF = mean(PF),PTS = mean(PTS)) %>% 
  na.omit(season17_18); dim(data0)
# of the 595 observations in the st_fdata, 486 were distinct observations
# omitted height & weight since it shortened final data set from 70% to 60%
403/486

# Omit Player column
final_data <- data0 %>% 
  select(-Player)

# Fit the regression
lm1 <- lm(season17_18 ~ ., data = final_data)

# Check the assumptions
performance::check_model(lm1)

# Identify predictors that need transformation
# log transformation
par(mfrow=c(2,2))
hist(log(final_data$YrsExperience),main="Years of Experience")
qqPlot(final_data$YrsExperience,main="Years of Experience")
plot(final_data$YrsExperience,resid(lm1),data=final_data)
plot(log(final_data$YrsExperience),resid(lm1),data=final_data)

# sqrt transformation? doesn't look much different
par(mfrow=c(2,2))
hist(final_data$Age, main="Age")
qqPlot(final_data$Age)
plot(final_data$Age,resid(lm1),data=final_data)
plot(sqrt(final_data$Age),resid(lm1),data=final_data)

par(mfrow=c(2,2))
hist(final_data$G, main="Games")
qqPlot(final_data$G)
plot(final_data$G,resid(lm1),data=final_data)
# unsure which tx to do for Games

# log transformation
par(mfrow=c(2,2))
hist(final_data$GS, main="Games Started")
qqPlot(final_data$GS)
plot((final_data$GS),resid(lm1),data=final_data)
plot(log(final_data$GS),resid(lm1),data=final_data)
# possibly better?

par(mfrow=c(2,2))
hist(final_data$MP, main="Minutes Played")
qqPlot(final_data$MP)
plot(final_data$MP,resid(lm1),data=final_data)

par(mfrow=c(2,2))
hist(final_data$PER, main="Player Efficiency Rating")
qqPlot(final_data$PER)
plot(final_data$PER,resid(lm1),data=final_data)

par(mfrow=c(2,2))
hist(final_data$TS., main="True Shooting Percentage")
qqPlot(final_data$TS.)
plot(final_data$TS.,resid(lm1),data=final_data)

# sqrt transformation
par(mfrow=c(2,2))
hist(final_data$X3PAr, main="3-Point Attempt Rate")
qqPlot(final_data$X3PAr)
plot((final_data$X3PAr),resid(lm1),data=final_data)
plot(sqrt(final_data$X3PAr),resid(lm1),data=final_data)

# log transformation
par(mfrow=c(2,2))
hist(final_data$FTr, main="Free Throw Rate")
qqPlot(final_data$FTr)
plot((final_data$FTr),resid(lm1),data=final_data)
plot(log(final_data$FTr),resid(lm1),data=final_data)

# log transformation
par(mfrow=c(2,2))
hist(final_data$ORB., main="Offensive Rebound Percentage")
qqPlot(final_data$ORB.)
plot((final_data$ORB.),resid(lm1),data=final_data)
plot(log(final_data$ORB.),resid(lm1),data=final_data)

par(mfrow=c(2,2))
hist(final_data$DRB., main="Defensive Rebound Percentage")
qqPlot(final_data$DRB.)
plot(final_data$DRB.,resid(lm1),data=final_data)

# log transformation
par(mfrow=c(2,2))
hist(final_data$TRB., main="Total Rebound Percentage")
qqPlot(final_data$TRB.)
plot((final_data$TRB.),resid(lm1),data=final_data)
plot(log(final_data$TRB.),resid(lm1),data=final_data)

# log transformation
par(mfrow=c(2,2))
hist(final_data$AST., main="Assist Percentage")
qqPlot(final_data$AST.)
plot((final_data$AST.),resid(lm1),data=final_data)
plot(log(final_data$AST.),resid(lm1),data=final_data)

par(mfrow=c(2,2))
hist(final_data$STL., main="Steal Percentage")
qqPlot(final_data$STL.)
plot(final_data$STL.,resid(lm1),data=final_data)

# log transformation
par(mfrow=c(2,2))
hist(final_data$BLK., main="Block Percentage")
qqPlot(final_data$BLK.)
plot((final_data$BLK.),resid(lm1),data=final_data)
plot(log(final_data$BLK.),resid(lm1),data=final_data)
# either sqrt and log work here



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
