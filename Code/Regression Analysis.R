#Group by month and school name, select only desired columns
df2 <- df1 %>%
  group_by(schname, month) %>%
  mutate(std_index = mean(std_index)) %>%
  select(-'keyword', -'schid', -'keynum') %>% 
  distinct(.keep_all = TRUE) %>% 
  select(schname, month, std_index, SC_avail, high_earn, quarter, 
         month_only)

#Regression 1: Interaction terms, no controls
reg1 <- df2 %>% 
  feols(std_index ~ SC_avail + high_earn + I(SC_avail*high_earn))

#Regression 2:  Interaction terms, control for quarter

reg2 <- df2 %>% 
  feols(std_index ~ SC_avail + high_earn + I(SC_avail*high_earn)
        + quarter)

#Regression 3:  Interaction terms, control for month
reg3 <- df2 %>% 
  feols(std_index ~ SC_avail + high_earn + I(SC_avail*high_earn)
        + month_only)

#View regressions
etable(reg1, reg2, reg3)

#Wald test for control coefficients

wald(reg2, c('quarter'))
wald(reg3, c('month'))

#In reg2, the p-value indicates that we cannot reject the null that the
# coefficient of quarter is 0, meaning that the model is not significantly
# better when we control for quarter.

#In reg3, the  p-value of the wald test indicates that this model predicts
# significantly better with month included as a control, at a 1% confidence
# level.


#Plot density of standard index scores

#This density plot proves standardization of scores - 0 is average score
#Box plots show little change in average score when filtered to only
# high-earnings or advent of scorecard, or effect on each other

#Plot density of standardized scores to ensure normality
density <- density(df2$std_index)

plot(density)

sd(df2$std_index)

#Plot scores vs month

df2 %>% ggplot(aes(high_earn*SC_avail, std_index)) + 
  geom_boxplot(aes(group=high_earn*SC_avail))

#Plot scores vs month and scores vs quarter

df2 %>% ggplot(aes(month_only, std_index)) + geom_point()

df2 %>% ggplot(aes(quarter, std_index)) + geom_point()

#Create box plots by month and quarter

df2 %>% ggplot(aes(month_only, std_index)) + 
  geom_boxplot(aes(group = month_only))

df2 %>% ggplot(aes(month_only, std_index)) + 
  geom_boxplot(aes(group = quarter))
