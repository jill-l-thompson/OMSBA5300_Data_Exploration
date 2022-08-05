#Did the release of the Scorecard in Sept 2015 shift student interest
#  to high-earnings colleges?

#Need a binary variable - high-earning - yes or no?
#Income number is above median or below for all? Above is high?

#Panel data:  monthly-comparison (categorical data?)

#Y = student interest in high-earning colleges
#X = release of scorecard (Y/N)

#Y = interest in college
#X = release of scorecard or college
#Y changes between colleges and between time (panel data)
#Between variation is variation between individual colleges
#Within variation is variation between time periods

#Y = student interest in low-earning colleges
#X = release of score card (Y/N)

#control for month of year
#panel data is student interest at each month of year


initial_plot <- df1 %>% 
  ggplot(aes())


#Create column for high-earnings (1 = high, 0 = low), low is <= $25K
#Student interest = std_index
#Release of score card, high-earnings, interaction between the two?

#Group by month and school name, select only desired columns
df2 <- df1 %>%
  group_by(schname, month) %>%
  mutate(std_index = mean(std_index)) %>%
  select(-'keyword', -'schid', -'keynum') %>% 
  distinct(.keep_all = TRUE) %>% 
  select(schname, month, std_index, SC_avail, high_earn, quarter, month_only)


#index = B0 + B1()

reg1 <- df2 %>% 
  feols(std_index ~ SC_avail + high_earn + I(SC_avail*high_earn))

reg2 <- df2 %>% 
  feols(std_index ~ SC_avail + high_earn + I(SC_avail*high_earn)
        + quarter)

wald(reg2, c('quarter'))
#very small p-value means that the model predicts significantly better
# with included as control

etable(reg1, reg2)

#Plot density of standard index scores

#This density plot proves standardization of scores - 0 is average score
#Box plots show little change in average score when filtered to only
# high-earnings or advent of scorecard, or effect on each other

density <- density(df2$std_index)

plot(density)

sd(df2$std_index)

#Plot scores vs month

df2 %>% ggplot(aes(high_earn*SC_avail, std_index)) + 
  geom_boxplot(aes(group=high_earn*SC_avail))

df2 %>% ggplot(aes(month_only, std_index)) + geom_point()

df2 %>% ggplot(aes(month_only, std_index)) + 
  geom_boxplot(aes(group = month_only))
