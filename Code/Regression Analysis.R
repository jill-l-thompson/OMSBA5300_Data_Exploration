#Group by month and school name, select only desired columns
df2 <- df1 %>%
  group_by(schname, month) %>%
  mutate(std_index = mean(std_index)) %>%
  select(-'keyword', -'schid', -'keynum') %>% 
  distinct(.keep_all = TRUE) %>% 
  select(schname, month, std_index, SC_avail, high_earn, monthorweek)

#Regression 1: Interaction terms, no controls
reg1 <- df2 %>% 
  feols(std_index ~ SC_avail + high_earn + I(SC_avail*high_earn))

etable(reg1)

interaction.plot(x.factor = df2$SC_avail, trace.factor = df2$high_earn,
                 response = df2$std_index, xlab = 'Scorecard Available', ylab = 'Google score',
                 trace.label = 'High Earning')

#Regression 2:  Interaction terms, control for month
reg2 <- df2 %>% 
  feols(std_index ~ SC_avail + high_earn + I(SC_avail*high_earn)
        + month)

etable(reg2)

wald(reg2, c('month'))

df2 %>% ggplot(aes(month, std_index)) + geom_point()
