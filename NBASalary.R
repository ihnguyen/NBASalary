library(tidyverse)
library(car)
library(emmeans)

# Read csv files
salary_data <- read.csv("NBA_season1718_salary.csv")
player_data <- read.csv("player_data.csv")
stats_data <- read.csv("Seasons_Stats.csv")
player_data2 <- read.csv("Player.csv") # has height and weight variables we want

# Histograms
par(mfrow=c(2,3))
hist(stats_data$PTS,main="Points")
hist(player_data$weight, main="Weight")
hist(salary_data$season17_18, main="Salary")
hist(stats_data$Age, main="Age")
hist(stats_data$G, main="Games")
hist(stats_data$DBPM, main="Defensive Blocks +/-")

# Count observations in each data set
salary_count <- salary_data %>%
  distinct(Player); nrow(salary_count)
stats_count <- stats_data %>%
  distinct(Player); nrow(stats_count)
player_count <- player_data %>%
  distinct(name); nrow(player_count)
player_count2 <- player_data2 %>%
  distinct(Player); nrow(player_count2)

# Dimensions of each data set
dim(salary_data)
dim(player_data)
dim(stats_data)
dim(player_data2)

# Omit X column from all datasets
sal_data <- salary_data %>% 
  select(-c(X))
st_data <- stats_data %>% 
  select(-c(X)) %>% 
  subset(Year == 2017)
play_data2 <-player_data2 %>% 
  select(-c(X))

# Calculate Years of Experience from Player data set
play_data <- player_data %>% 
  mutate(YrsExperience = (year_end - year_start))

#Taking sums of annual stats to calculate career stats 
career_st <- stats_data %>% 
  group_by(Player) %>% 
  summarise(CareerFT = sum(FT), CareerAST = sum(AST), CareerRB = sum(TRB), CareerBLK = sum(BLK), 
            CareerSTL = sum(STL), )

# Store data sets as data frames
st_fdata <- data.frame(st_data) %>% 
  select(-c(blanl,blank2))
sal_fdata <- data.frame(sal_data)
play_fdata <- data.frame(play_data) %>% 
  select(-c(position,year_start,year_end,height,weight)) %>% 
  rename(Player = name)
play_fdata2 <- data.frame(play_data2) %>% 
  select(-c(birth_city,birth_state)) %>% 
  rename(college = collage)

# Full Join all datasets
data0 <- play_fdata %>% 
#  full_join(play_fdata2, by=c("Player","college")) %>% 
  full_join(.,sal_fdata, by="Player") %>% 
  full_join(.,st_fdata, by=unique("Player")) %>% 
  group_by(Player) %>% 
  summarise(season17_18 = sum(season17_18), Pos, YrsExperience = sum(YrsExperience), Age = mean(Age),
            G = mean(G),GS = mean(GS),MP = mean(MP),PER = mean(PER),TS. = mean(TS.),
            X3PAr =mean(X3PAr),FTr = mean(FTr),ORB. = mean(ORB.),DRB. = mean(DRB.),
            TRB. = mean(TRB.),AST. = mean(AST.),STL. = mean(STL.),BLK. = mean(BLK.),
            TOV.  = mean(TOV.),USG. = mean(USG.) ,OWS = mean(OWS) ,DWS = mean(DWS) ,WS = mean(WS),
            WS.48 = mean(WS.48) ,OBPM = mean(OBPM) ,DBPM = mean(DBPM) ,BPM = mean(BPM) ,VORP = mean(VORP),
            FG = mean(FG) ,FGA = mean(FGA) ,FG. = mean(FG.),X3P = mean(X3P) ,X3PA = mean(X3PA) ,
            X2P = mean(X2P) ,X2PA = mean(X2PA) ,X2P. = mean(X2P.) ,eFG. = mean(eFG.),FT = mean(FT),
            FTA = mean(FTA),FT. = mean(FT.),ORB = mean(ORB),DRB = mean(DRB),TRB = mean(TRB),
            AST = mean(AST),STL = mean(STL),BLK = mean(BLK),TOV = mean(TOV),PF = mean(PF),PTS = mean(PTS)) %>% 
  na.omit(season17_18)

final_data <- distinct(data0) # omitted height & weight since it shortened final data set from 70% to 60%
dim(final_data)
405/535


# play_fdata <- play_fdata %>%
#   rename(Player = name)
# 
# # Inner Joined Statistics and Salary data sets
# data0 <- stringdist_inner_join(st_fdata,sal_fdata, by=c("Player","Tm"))
# data0 <- data0 %>%
#   select(-c(Player.y)) %>%
#   rename(Player = Player.x)
# 
# # Inner Joined new data set with Player data set
# data1 <- stringdist_inner_join(data0,play_fdata, by="Player")
# data1 <- data1 %>%
#   select(-c(Player.y,Tm.y,position)) %>%
#   rename(Player = Player.x) %>%
#   rename(Tm = Tm.x) %>%
#   rename(Salary = season17_18)
# 
# # Inner Joined new data set with Player data set 2
# data <- stringdist_inner_join(data1, play_fdata2,by="Player")
# data <- data %>%
#   select(-c(Player.y, height.y, weight.y)) %>%
#   rename(Player = Player.x) %>%
#   rename(Height = height.x) %>%
#   rename(Weight = weight.x)
#   
# data <- data %>% 
#   select(-c(collage, blanl, blank2))

# #Making a dataframe with only numerical variables
# data_num <- subset(data, select = c(Height ,Weight ,born ,YrsExperience ,Age ,G,
#                                     GS ,MP ,PER ,TS. ,X3PAr ,FTr ,ORB. ,DRB. ,TRB.
#                                     ,AST. ,STL. ,BLK. ,TOV. ,USG. ,OWS ,DWS ,WS
#                                     ,WS.48 ,OBPM ,DBPM ,BPM ,VORP ,FG ,FGA ,FG.
#                                     ,X3P ,X3PA ,X2P ,X2PA ,X2P. ,eFG. ,FT ,FTA
#                                     ,FT. ,ORB ,DRB ,TRB ,AST ,STL ,BLK ,TOV ,PF
#                                     ,PTS, Salary)) %>%
#   na.omit()

#Correlation matrix as a dataframe
corr_df <- round(cor(data_num[,1:50]),2) #Not including response variable, Salary, which is 50th column

#Scan the upper right triangle of the correlation matrix. Print indeces that have correlation values greater than .88
corr_vals <- c()
for(i in 1:ncol(corr_df)){
  for(j in i:ncol(corr_df)){
    if(corr_df[i,j] > .88 & corr_df[i,j] < 1){
      corr_vals <- append(corr_vals, c(i,j))
    }
  }
}
corr_vals


#Linear model with all numberical variables
Salarylm <- lm(Salary ~ ., data = data_num)



# Subset data set to include only data from 2017-2018 Data
#dt <- subset(data, Year == 2017) %>%
#  distinct(Player,birth_date, .keep_all = TRUE)
#head(dt)

# 535 salaries available to 462 salaries in new data set
#nrow(dt)
#462/535

# par(mfrow=c(2,3))
# hist(dt$PTS, main="Points")
# hist(dt$weight, main="Weight")
# hist(dt$Salary, main="Salary")
# hist(dt$Age, main="Age")
# hist(dt$G, main="Games")
# hist(dt$DBPM, main="Defensive Blocks +/-")
# 
# par(mfrow=c(2,3))
# qqPlot(dt$PTS, main="Points" )
# qqPlot(dt$weight, main="Weight")
# qqPlot(dt$Salary, main="Salary")
# qqPlot(dt$Age, main="Age")
# qqPlot(dt$G, main="Games")
# qqPlot(dt$DBPM, main="Defensive Blocks +/-")
# 
# # Data Analysis
# 
# ## Create Full Model
# full_model <- lm(Salary~YrsExperience+Age+G+TS.+DBPM+FG.+eFG.+AST+PF+PTS+weight, data=dt) 
# summary(full_model)
# 
# ## Create Reduced Model
# ### Omit Variables that are not statistically significant and linearly dependent
# reduced_model <- lm(Salary~Age+G+DBPM+PTS+weight, data=dt)
# summary(reduced_model)
# 
# ## Visualize Relationships
# pairs(Salary~Age+G+DBPM+PTS+weight, data=dt) 
# 
# ## Compare Full and Reduced Models with Partial F-test
# anova(reduced_model,full_model)