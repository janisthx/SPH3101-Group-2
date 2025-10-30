model_withwealth <- glm(stunting ~ children_numbers + average_parent_eduyears + residence + wealth,
                 data = bdhs_clean,
                 family = binomial(link = "logit"))
summary(model_withwealth)

# with wealth is not a good option since all things are related to wealth, and wealth is not easy to define

model_stu <- glm(stunting ~ children_numbers + average_parent_eduyears + residence,
                     data = bdhs_clean,
                     family = binomial(link = "logit"))
summary(model_stu)

model_was_withedu <- glm(wasting ~ children_numbers + average_parent_eduyears + residence,
                    data = bdhs_clean,
                    family = binomial(link = "logit"))
summary(model_was)

# urban rural residence area is no longer useful if we use other variables