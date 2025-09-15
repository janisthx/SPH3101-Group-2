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

bdhs_clean$children_numbers <- bdhs$V201 # - bdhs$V204 - bdhs$V205 # total numbers of children at home
table(bdhs_clean$stunting, bdhs_clean$children_numbers) # just figured out that total children numbers is related to stunting status
hist(bdhs$V201)
plot(bdhs_clean$stunting ~ bdhs_clean$children_numbers)

table(bdhs_clean$wasting, bdhs_clean$children_numbers) 
hist(bdhs$V201)
plot(bdhs_clean$wasting ~ bdhs_clean$children_numbers)


bdhs_clean$wealth <- bdhs$V190A # wealth status
table(bdhs_clean$wealth, bdhs_clean$children_numbers) # poorer families tend to have more children
