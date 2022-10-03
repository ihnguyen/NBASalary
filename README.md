# [Predicting NBA Salary based on 2017-2018 Salary Data Using Multiple Linear Regression](https://docs.google.com/document/d/1Rwb_BOdaVGwd19w2bWoTJaOnrQaeUJaYNPnnIg2ihjk/edit)
### By Thuy-Nhi Nguyen, Miguel (Angel) Moreno, Thi Tran

**Introduction**

  The National Basketball Association (NBA) is a professional basketball league in North America. It is the world's finest men's basketball league who are also the highest-paid athletes in the world. One of the most well-known names in the NBA are Kobe Bryant, LeBron James, Bill Russell, Michael Jordan, and Stephen Curry. The phenomenal success of the NBA players has brought them not just fame but also wealth. According to Salary Cap Overview1, the highest earning NBA player in 2021 was Stephen Curry with an earning of 46M; meanwhile the lowest earning of the same season was only $92,000. The disparity in player salaries prompted us to wonder what factors influence NBA salary. 

  The goal of this study is to establish the importance of  player's performance statistics that have the most impact on a player's annual earnings as an NBA player.  A multiple linear regression model along with data transformations were used to explain the NBA salary prediction model : (Estimated Salary)0.33 = -776+2.43(Age) -0.621(Games) +178(Defensive Win Shares+20)0.5+3.95(Points+20)0.5.  With a few exceptions, we have come to determine that a player's age, number of games one has played, defensive win share and points accumulated are important factors in determining a player's salary.

**Data Description**

  While easily accessible from Kaggle2, this dataset was sewn together through full-joins of multiple datasets that are each a product of scraping from Basketball-reference.com. After data wrangling, cleaning, and analysis of outliers, the final dataset used for this study contains 403 samples and 46 variables in total. The response variable is 2017-18 salary in USD while the predictors contain statistics that describe player attributes and player success on the court (points, games, rebounds, etc.). As this study is a multiple linear regression model, all of our variables are quantitative. 

  Typically in sports, the majority of athletes are competitive, but only a select few truly stand out from the crowd. This dataset tells a similar story after taking initial observations. Many variables within the data follow a right-skewed distribution as displayed by the model’s most significant predictors in Figure 1. With the exception of games, each variable shows 75% of players within the lower third of values while the upper quartile of players have accrued statistics within the upper two-thirds of values. Unsurprisingly, the distribution of pay seems to reflect accordingly.

![Pred_2](https://user-images.githubusercontent.com/73903035/193509976-04312981-fbad-4672-9570-1dab0d770792.png)
![Pred_1](https://user-images.githubusercontent.com/73903035/193510000-89c6edba-d5e4-4031-8c67-bf1e3cfe0d93.png)
![pred_3](https://user-images.githubusercontent.com/73903035/193510008-71b9bc60-2fac-473d-88a4-11c4b422329b.png)
![Resp_1](https://user-images.githubusercontent.com/73903035/193510017-8ce1f04a-5a62-4f07-b94a-32005c14505f.png)
Figure 1: Box plots of the distributions of response variable (left) in millions and four significant predictors (middle and right)

  Performing Box-Cox transformation resulted in transforming the response variable to the .33 power while visual inference and trial and error led to predictor transformations that include square root and log among others. As for the final model, the only predictor transformation used was square root as seen in Figure 2. Once transformed, the predictors were not exactly linear with respect to the response variable, but they showed a somewhat positive correlation. Age and games were the exceptions as it appeared that age was positive up to a peak, where the correlation then became negative. Additionally, game was negatively correlated with the response variable.

![trxformed_scatterplot_matrix](https://user-images.githubusercontent.com/73903035/193510070-fa24aa49-bffc-4c95-8ccb-9b1f93048f12.png)
Figure 2: Scatterplot matrix of transformed response variable and four significant predictors

**Methods**

An overall F-test was conducted on the full model against the null model to determine if there was a relationship between the response and at least one predictor. To address multicollinearity issue amongst predictors in the full model and produce a better model, a correlation matrix was used to calculate highly correlated predictors (⍴≥0.83,⍴≤-0.83). Multiple models were developed through a variety of variable selection methods such as backwards elimination and backwards stepwise selection. Each model was evaluated by their overall F-test and partial F-test to determine if any or a subset of predictors were useful at predicting salary. The model with a lower Bayesian Information Criterion (BIC) value than the full model was determined the better model.
Visual diagnostics were used to check multicollinearity and assumptions regarding normality, linearity, and constant variance. To further assess the model, the final model was evaluated on how well it performed at making future salary predictions. Thus the final data set was split into 70% training set and 30% test set for cross-validation purposes. The mean squared difference between the observed and predicted values were calculated for the full and final model to quantify prediction error.

**Results**

After transforming salary, defensive win shares, and points, the final fitted regression model with associated p-values less than the significance level ⍺=0.05 indicated that age, games, defensive win shares, and points were statistically significant at predicting salary in the presence of each other according to table 1. The equation for the final model is as follows:

(Estimated Salary)0.33 = -776 +2.43(Age) -0.621(Games) +178(Defensive Win Shares+20)0.5 +3.95(Points+20)0.5

![Summary_table(cropped)](https://user-images.githubusercontent.com/73903035/193510172-405d6bb2-3eff-47d0-912a-e0e31c17ee5e.png)
Table 1: Regression summary table of final model’s statistically significant predictors
The overall F-test with a F-statistic 14.5 greater than 1 and an associated p-value 2.2x10-16 less than the significance level ⍺=0.05 indicated that at least one predictor was significant at predicting salary. The correlation matrix determined that 16 variables were highly correlated; therefore, were removed from the model to solve the multicollinearity problem. Subsequently, many insignificant variables were removed using backwards stepwise selection. When comparing the final and full model, a p-value 0.0741 greater than the significance level ⍺=0.05 resulted in the acceptance of the final model over the full model. Additionally, the BIC value 4160 of the final model being lower than the BIC value 4340 of the full model provided evidence that the final model was the better model.
The visual diagnostics in Figure 3 indicated that the residuals versus fitted values and standardized residuals versus fitted values plots reasonably met the constant variance and linearity assumptions. The low variance inflation factor (VIF) values in the collinearity plot confirmed multicollinearity was no longer an issue. To assess the normality assumption, the quantile-quantile (QQ) plot showed that most data points follow the diagonal line thus in agreement that the data followed a normal distribution. 

![model_check](https://user-images.githubusercontent.com/73903035/193510196-d5d1f6cd-e833-4fbe-a2ff-189ad9159518.png)
Figure 3: Plots of the fitted values versus residuals (top left), fitted values versus standardized residuals (top right), predictors versus VIF (bottom left), and QQ plot (bottom right)

![cv_table1(cropped)](https://user-images.githubusercontent.com/73903035/193510318-7d41ead0-0073-470c-b065-66465a4fa843.png)
![cv_table2(cropped)](https://user-images.githubusercontent.com/73903035/193510326-558c01d4-0660-4edd-985a-e673559b1d97.png)
Table 2: Cross-validation results of full model (left) and final model (right)

  The R2 values for full model and final model were 0.505 and 0.638, respectively. In practice, any full model should have a larger R2  value compared to a reduced model. This is one of the problems that occurred during this study while finding a better fit model. This could account for the multicollinearity of numerous predictors that were excluded from the model. However, based on the final R2 value, this indicated that 63.8% of the final model prediction was dependent on the predictors Age, G, DWS, and Points. The full model RMSE of 47.0 and the final model RMSE of 38.2 was illustrated in table 2. Since the final model had a lower RMSE compared to the full model, the final model was proven to be the better model. This also defined that the chosen predictors performed 19% less in root mean squared error compared to the full model.
  Evidently, it appeared that compensation in the NBA was a reward for previous performance as well as for projected or expected future performance in the league. It is possible, however, that previous performance and predicted performance were not reliable indications of appropriate remuneration. For instance, Stephen Curry was observed as one of the extreme outliers in this study who gets paid over 46M in 20213. Nevertheless, the final model predicted his salary based on Stephen’s statistics to be $14M with confidence interval between $4M ~ 33M.  Meanwhile, Damian Lillard’s salary was 30M in 2021. Statistically, the final model was able to confirm his salary which gave an interval between $4M ~ 32M in 2021.  

**Conclusion**

  According to the literature and several NBA pay databases, it was concluded that a player's age, the number of games, defensive win shares and points per game would be the most relevant factors in determining a player's salary. The discrepancy in salary estimation could be from non-scoring distributions such as player’s popularity, ticket price, merchandise goods, social media rating and more. As a result, possible collinearity problems might have developed when comparing various statistics that may possibly be related to each other in this study, but only main performance statistics were included in the model. In order to improve model performance, future research should take non-performance aspects into consideration as well as performance variables. In conclusion, there are a variety of elements that might influence the wage gap  between NBA players, with individual performance statistics playing a critical role in projecting performance models.

**Sources**

1: https://cbabreakdown.com/salary-cap-overview
2:https://www.kaggle.com/code/koki25ando/nba-salary-prediction-using-multiple-regression/data
3: https://www.basketball-reference.com/leagues/NBA_2021_advanced.html
Code Appendix
https://github.com/ihnguyen/NBASalary/blob/main/NBASalary_averagesalary.R

Additional Sources:
NBA Variable Description:
[NBA Variables .pdf](https://github.com/ihnguyen/NBASalary/files/8448178/NBA.Variables.pdf)

Link to find recent NBA player's statistics:
https://www.nba.com/stats/players/traditional/?sort=PLAYER_NAME&dir=1&Season=2021-22&SeasonType=Regular%20Season&PerMode=Totals
