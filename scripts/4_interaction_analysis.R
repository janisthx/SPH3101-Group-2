# 4. Urban-Rural Interaction Analysis

cat('===== Urban-Rural Interaction Analysis =====')

# Take stunting as example:

final_main <- glm(stunted ~ wealth_c + children_c + average_parent_edu_c,
                     data = bdhs_final, family = binomial)
# Trying to add residence into consideration:
cat('Trying to add residence into consideration:')
tryout <- glm(stunted ~ wealth_c + children_c + average_parent_edu_c + residence,
                     data = bdhs_final, family = binomial)
summary(tryout)

# Not significant at all

# Testing Interaction of residence on three different variables
cat('Testing Interaction of residence on three different variables:')
# 1) wealth
test_m1 <- glm(stunted ~ wealth_c * residence + children_c + average_parent_edu_c,
             data = bdhs_final, family = binomial)
lr1 <- anova(final_main, test_m1, test = "Chisq")
# 2) children
test_m2 <- glm(stunted ~ wealth_c + children_c * residence + average_parent_edu_c,
             data = bdhs_final, family = binomial)
lr2 <- anova(final_main, test_m2, test = "Chisq")
# 3) education
test_m3 <- glm(stunted ~ wealth_c + children_c + average_parent_edu_c * residence,
             data = bdhs_final, family = binomial)
lr3 <- anova(final_main, test_m3, test = "Chisq")

cat('1. Wealth x Residence:')
print(lr1)

cat('2. ChildrenN x Residence:')
print(lr2)

cat('3. Edu x Residence:')
print(lr3)


# Draw a AIC Line Chart to show its insignificance
interaction_aic <- data.frame(
  Model = c("Final Model", "+ Residence", 
            "Wealth x Residence", "CN x Residence", "Edu x Residence"),
  AIC = c(AIC(final_main), AIC(tryout), 
          AIC(test_m1), AIC(test_m2), AIC(test_m3)),
  order = 1:5
)

ggplot(interaction_aic, aes(x = order, y = AIC)) +
  geom_line(size = 1.2, color = "steelblue") +
  geom_point(size = 3, color = "steelblue") +
  scale_x_continuous(breaks = 1:5, 
                     labels = c("Final", "+Residence", 
                                "W&Res", "CN&Res", "Edu&Res")) +
  labs(title = "AIC Comparison: Residence Interaction Effects on Stunting",
       x = "Model", y = "AIC Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Try the Linear Model (Just a tryout, can mention in the presentation if there's time)
cat('Try the Linear Model (Z-Scores on the variables):')
haz_final <- lm(haz ~ wealth_c + children_c + average_parent_edu_c, 
                         data = bdhs_final)
summary(haz_final)
waz_final <- lm(waz ~ wealth_c + children_c + average_parent_edu_c, 
                data = bdhs_final)
summary(waz_final)

# Linear models have small R2, not a good option.
