library(tidyverse);library(car);library(emmeans);library(MASS);library(reshape);library(reshape2);library(faraway)

# Read csv files
salary_data <- read.csv("NBA_season1718_salary.csv"); stats_data <- read.csv("Seasons_Stats.csv")
dim(salary_data);dim(stats_data)

# Full Join both data sets
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

# Observe data using Box Plot
d <- melt(final_data, id="season17_18")
ggplot(d,aes(x=variable,y=value,color=variable)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(size=10, angle=45))
dt <- final_data[,c(1,2,3,5,6,10)]
e <- melt(dt,id="season17_18")
ggplot(e,aes(x=variable,y=value,color=variable)) +
  geom_boxplot()


# Fit the regression, summarize and check assumptions
lm1 <- lm(season17_18 ~ ., data = final_data); summary(lm1);performance::check_model(lm1)

# Model with transformed predictors
lm2 <- lm((season17_18)^0.25~Age+G+GS+MP+PER+TS.+sqrt(X3PAr)+log(FTr+1)+log(ORB.+1)+DRB.+log(TRB.+1)+log(AST.+1)+STL.+sqrt(BLK.)
          + log(TOV.+ 20) + USG. + log(OWS+ 20) + sqrt(DWS+ 20) + sqrt(WS+ 20) + WS.48 + sqrt(OBPM + 20) + sqrt(DBPM + 20) + BPM + log(VORP+ 20) + sqrt(FG+ 20) + FG.+ sqrt(FGA) + sqrt(X3PA+ 20) + sqrt(X2P+ 20) + 
            sqrt(X2PA+ 20) + log(X2P.+ 20) + log(eFG.+ 20) + log(FT+ 20) + log(FTA +6) + FT. + log(ORB+ 20) + sqrt(DRB+ 20) + sqrt(TRB+ 20) + log(AST+ 20) + sqrt(STL+ 20) + log(BLK+ 20) + sqrt(TOV+ 20) + PF + sqrt(PTS+ 20)
          , data=final_data); summary(lm2); performance::check_model(lm2)
# Check if response needs to be transformed; Box-Cox transformation
par(mfrow=c(2,3));qqPlot(final_data$season17_18);qqPlot((final_data$season17_18)^0.25);hist(final_data$season17_18);hist((final_data$season17_18)^0.25);plot(final_data$season17_18,resid(lm2),data=final_data);plot((final_data$season17_18)^0.25,resid(lm2),data=final_data)
ptf<- powerTransform(season17_18~.,data = final_data)
summary(ptf)



# Correlation matrix as a data frame
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

#Compare variables with high colinearity with the response variable###
#Make a matrix to see the index pairs with high colinearity
colnames(corr_df)
corr_pairs <- matrix(corr_vals, ncol = 2, byrow = TRUE); corr_pairs


# Linear model with our first set of rejects (based on high colinearity & lower correlation with the response variable)
lm3 <- update(lm2, ~. - log(eFG. + 20) - log(TRB. + 1) - log(OWS + 20) - sqrt(FG + 20) - sqrt(X2P + 20) - sqrt(X2PA + 20) - log(FT + 20) - 
                sqrt(TRB + 20) - sqrt(X3PA + 20) - sqrt(FGA)); summary(lm3); performance::check_model(lm3)

############ Next time do lm before lm4 that removes the negative correlations b/w (-1,-.9] ###############

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

#Linear model with our second set of rejects (based on high colinearity & lower correlation with the response variable)
lm4 <- update(lm3, ~. - G - MP - WS.48 - log(ORB. + 1) - sqrt(TRB + 20) - log(ORB + 20) - log(VORP + 20) - log(FTA + 6) - 
                sqrt(OBPM + 20) - log(FT + 20) - sqrt(TOV + 20) - log(X2P. + 20) - FG. - sqrt(X2P + 20) - sqrt(X2PA + 20) -
                log(AST + 20)); summary(lm4); performance::check_model(lm4)

plot((final_data$season17_18)^0.25, resid(lm4), data=final_data)
round(vif(lm4),2)

########### Next we will make a new correlation matrix that only includes the variables in our most recent model
# lm4

corr_df2 <- data.frame(corr_df)[,c(1,2,4,6,7,8,9,11,13,14,15,16,17,19,20,23,24,37,39,42,43,45,46)]
view(corr_df2)


corr_vals3 <- c()
for(i in 1:ncol(corr_df2)){
  for(j in i:ncol(corr_df2)){
    if(abs(corr_df2[i,j]) >= .7 & abs(corr_df2[i,j]) < 1){
      corr_vals3 <- append(corr_vals3, c(i,j))
    }
  }
}; length(corr_vals3); corr_vals3

corr_pairs3 <- matrix(corr_vals3, ncol = 2, byrow = TRUE)
corr_pairs3

lm5 <- update(lm4, season17_18 ~ . - PER - TS. - sqrt(DRB+20) - sqrt(WS + 20)); summary(lm5)

n = nrow(final_data)
lm6 <- step(lm5, k = log(n))
summary(lm6)

performance::check_model(lm6)
### Looks good!!! will now do stepwise selection, we will remove variables one-by-one based off of high p-values

lm7 <- update(lm5, season17_18 ~ . - FT.); summary(lm7)
lm8 <- update(lm7, season17_18 ~ . - sqrt(DBPM + 20)); summary(lm8)
lm9 <- update(lm8, season17_18 ~ . - STL.); summary(lm9)
lm10 <- update(lm9, season17_18 ~ . - log(FTr + 1)); summary(lm10)
lm11 <- update(lm10, season17_18 ~ . - log(BLK + 20)); summary(lm11)
lm12 <- update(lm11, season17_18 ~ . - sqrt(BLK.)); summary(lm12)
lm13 <- update(lm12, season17_18 ~ . - sqrt(X3PAr)); summary(lm13)
lm14 <- update(lm13, season17_18 ~ . - DRB.); summary(lm14)
lm15 <- update(lm14, season17_18 ~ . - sqrt(STL + 20)); summary(lm15)
lm16 <- update(lm7, season17_18 ~ . - sqrt(DBPM + 20)); summary(lm8)

#


