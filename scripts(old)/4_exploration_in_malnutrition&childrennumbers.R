bdhs_clean$wasting = ifelse(bdhs_clean$WHZ_clean < -2, 1, 0)
bdhs_clean$stunting = ifelse(bdhs_clean$HAZ_clean < -2, 1, 0)
bdhs_clean$stunting = as.factor(bdhs_clean$stunting)
bdhs_clean$wasting = as.factor(bdhs_clean$wasting)

plot(bdhs_clean$stunting ~ bdhs_clean$wasting)
table(bdhs_clean$stunting)
table(bdhs_clean$stunting, bdhs_clean$wasting)
chisq.test(bdhs_clean$stunting, bdhs_clean$wasting) # stunting and wasting are likely to happen together

bdhs_clean$family_type = ifelse(bdhs_clean$household_members < 5, 1, 2) # can save the data for multiple parameters for the final assignment
fit.1 = glm(bdhs_clean$stunting ~ bdhs_clean$household_members, family = "binomial")
summary(fit.1) # not that related to household members

## bdhs_clean$children_numbers <- bdhs$V208 #births last five years
tab = table(bdhs_clean$stunting, bdhs_clean$children_numbers) # just figured out that total children numbers is related to stunting status
hist(bdhs$V201)
plot(bdhs_clean$stunting ~ bdhs_clean$children_numbers)

#=================Finding relations between children numbers (under 5) and stunting and wasting
# stunting and children numbers - related
# wasting and childeren numbers - not related

tab = table(bdhs_clean$wasting, bdhs_clean$children_numbers)  # wasting
hist(bdhs$V201)
plot(bdhs_clean$wasting ~ bdhs_clean$children_numbers)


#===========drawing the plots==============
tab_df <- as.data.frame(as.table(tab))
colnames(tab_df) <- c("stunting","children_numbers","count")

head(tab_df)

library(ggplot2)

ggplot(tab_df, aes(x = factor(children_numbers), 
                   y = count, 
                   fill = factor(stunting))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Number of Children",
       y = "Proportion",
       fill = "Stunting",
       title = "Proportion of Stunting by Number of Children") +
  theme_minimal()

chisq.test(bdhs_clean$stunting, bdhs_clean$children_numbers)
chisq.test(bdhs_clean$wasting, bdhs_clean$children_numbers)


