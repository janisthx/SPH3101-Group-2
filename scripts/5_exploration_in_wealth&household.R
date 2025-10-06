## bdhs_clean$wealth <- bdhs$V190A # wealth status(moved to 1_clean)
table(bdhs_clean$wealth, bdhs_clean$children_numbers) # poorer families tend to have more children

table(bdhs_clean$wealth, bdhs_clean$household_members)

# wealth vs children_numbers (related)
chisq.test(table(bdhs_clean$wealth, bdhs_clean$children_numbers))
correlation_1 <- cor(bdhs_clean$wealth, bdhs_clean$children_numbers, method = "pearson")
cat("Pearson Correlation Coefficient:", correlation_1, "\n")

# wealth vs household_members (related)
chisq.test(table(bdhs_clean$wealth, bdhs_clean$household_members))
correlation_2 <- cor(bdhs_clean$wealth, bdhs_clean$household_members, method = "pearson")
cat("Pearson Correlation Coefficient:", correlation_2, "\n")

# regress malnutrition on wealth and children number (linear/on z-scores)
model_lm1 <- lm(WHZ_clean ~ children_numbers + wealth, data = bdhs_clean)
summary(model_lm1)

model_lm2 <- lm(WHZ_clean ~ household_members + wealth, data = bdhs_clean)
summary(model_lm2)

# regress malnutrition on wealth and children number (binomial/on malnutrition outcomes)
model_logit1 <- glm(wasting ~ children_numbers + wealth,
                   data = bdhs_clean,
                   family = binomial(link = "logit"))
summary(model_logit1)

model_logit2 <- glm(wasting ~ household_members + wealth,
                    data = bdhs_clean,
                    family = binomial(link = "logit"))
summary(model_logit2)
