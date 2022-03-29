library(tidyverse)
library(fuzzyjoin)
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
  distinct(Player)
nrow(salary_count)

stats_count <- stats_data %>%
  distinct(Player)
nrow(stats_count)

player_count <- player_data %>%
  distinct(name)
nrow(player_count)

player_count2 <- player_data2 %>%
  distinct(Player)
nrow(player_count2)

# Dimensions of each data set
dim(salary_data)
dim(player_data)
dim(stats_data)
dim(player_data2)

# Identify variable names
names(salary_data)
names(player_data)
names(stats_data)
names(player_data2)

# Omit N/A's
sal_data <- salary_data %>% 
  select(-c(X)) %>%
  na.omit()

# Omit N/A's and Calculate Years of Experience from Player data set
play_data <- player_data %>% 
  mutate(YrsExperience = (year_end - year_start)) %>% 
  na.omit()

# Omit N/A's and Only include statistics data from 2017
st_data <- stats_data %>% 
  select(-c(X)) %>% 
  subset(Year == 2017)

play_data2 <-player_data2 %>% 
  select(-c(X)) %>% 
  na.omit()
######################## PROBLEM: multiple observations for players in different teams

# Store data sets as data frames
st_fdata <- data.frame(st_data)
sal_fdata <- data.frame(sal_data)
play_fdata <- data.frame(play_data)
play_fdata2 <- data.frame(play_data2)

play_fdata <- play_fdata %>%
  rename(Player = name)

# Inner Joined Statistics and Salary data sets
data0 <- stringdist_inner_join(st_fdata,sal_fdata, by="Player")
data0 <- data0 %>%
  select(-c(Player.y)) %>%
  rename(Player = Player.x)

# Inner Joined new data set with Player data set
data1 <- stringdist_inner_join(play_fdata,data0, by="Player")
data1 <- data1 %>%
  select(-c(Player.y,Tm.y)) %>%
  rename(Player = Player.x) %>%
  rename(Tm = Tm.x) %>%
  rename(Salary = season17_18)

# Inner Joined new data set with Player data set 2
data <- stringdist_inner_join(play_fdata2,data1, by="Player")
data <- data %>%
  select(-c(Player.y, height.y, weight.y)) %>%
  rename(Player = Player.x) %>%
  rename(Height = height.x) %>%
  rename(Weight = weight.x)
  
data <- data %>% 
  select(-c(collage, blanl, blank2))

#Making a dataframe with only numerical variables
data_num <- subset(data, select = c(Height ,Weight ,born ,YrsExperience ,Age ,G, GS ,MP ,PER ,TS. ,X3PAr ,FTr ,ORB. ,DRB. ,TRB. ,AST. ,STL. ,BLK. ,TOV. ,USG. ,OWS ,DWS ,WS ,WS.48 ,OBPM ,DBPM ,BPM ,VORP ,FG ,FGA ,FG. ,X3P ,X3PA ,X2P ,X2PA ,X2P. ,eFG. ,FT ,FTA ,FT. ,ORB ,DRB ,TRB ,AST ,STL ,BLK ,TOV ,PF ,PTS, Salary))

#Correlation matrix as a dataframe
corr_df <- data.frame(round(cor(data_num[,-51]),2)) #Not including response variable, Salary, which is 50th column


#Linear model with all numberical variables
Salarylm <- lm(Salary ~ ., data = data_num)



# Subset data set to include only data from 2017-2018 Data
#dt <- subset(data, Year == 2017) %>%
#  distinct(Player,birth_date, .keep_all = TRUE)
#head(dt)

# 535 salaries available to 462 salaries in new data set
#nrow(dt)
#462/535

par(mfrow=c(2,3))
hist(dt$PTS, main="Points")
hist(dt$weight, main="Weight")
hist(dt$Salary, main="Salary")
hist(dt$Age, main="Age")
hist(dt$G, main="Games")
hist(dt$DBPM, main="Defensive Blocks +/-")

par(mfrow=c(2,3))
qqPlot(dt$PTS, main="Points" )
qqPlot(dt$weight, main="Weight")
qqPlot(dt$Salary, main="Salary")
qqPlot(dt$Age, main="Age")
qqPlot(dt$G, main="Games")
qqPlot(dt$DBPM, main="Defensive Blocks +/-")

# Data Analysis

## Create Full Model
full_model <- lm(Salary~YrsExperience+Age+G+TS.+DBPM+FG.+eFG.+AST+PF+PTS+weight, data=dt) 
summary(full_model)

## Create Reduced Model
### Omit Variables that are not statistically significant and linearly dependent
reduced_model <- lm(Salary~Age+G+DBPM+PTS+weight, data=dt)
summary(reduced_model)

## Visualize Relationships
pairs(Salary~Age+G+DBPM+PTS+weight, data=dt) 

## Compare Full and Reduced Models with Partial F-test
anova(reduced_model,full_model)