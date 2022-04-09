library(tidyverse)
library(car)

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



## Angel's Plots for transformation##

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
