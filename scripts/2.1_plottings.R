# 2.1. Plotting for the exploratory analysis

# Create output directory if it doesn't exist
if (!dir.exists("plots")) dir.create("plots")
if (!dir.exists("plots/exploratory_analysis")) dir.create("plots/exploratory_analysis", recursive = TRUE)

# PART 1: BASIC DESCRIPTIVE STATISTICS
# Seeing the correlation of different malnutrition outcomes
# 1. Stunted vs Wasted
eda_data1 <- bdhs_final[!is.na(bdhs_final$stunted) & !is.na(bdhs_final$wasted), ]
chi1 <- chisq.test(eda_data1$stunted, eda_data1$wasted)
p1 <- ggplot(eda_data1, aes(factor(stunted), fill = factor(wasted))) +
  geom_bar(position = "fill") +
  geom_text(stat = "count", aes(label = after_stat(count)), 
            position = position_fill(vjust = 0.5), color = "white", size = 4) +
  scale_fill_manual(
    name = "Wasted",
    breaks = c("0","1"),
    labels = c("0" = "No", "1" = "Yes"),
    values = c("0" = "chartreuse4", "1" = "brown")
  ) +
  scale_x_discrete(labels = c("0" = "No Stunting", "1" = "Stunting")) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = paste0("Stunted vs Wasted (χ² = ", round(chi1$statistic, 2), 
                      ", p = ", round(chi1$p.value, 4), ")"),
       x = "Stunted", y = "Percent")
ggsave("plots/exploratory_analysis/01_stunted_vs_wasted.png", p1, width = 8, height = 6, dpi = 300)

# 2. Stunted vs Underweight
eda_data2 <- bdhs_final[!is.na(bdhs_final$stunted) & !is.na(bdhs_final$underweight), ]
chi2 <- chisq.test(eda_data2$stunted, eda_data2$underweight)
p2 <- ggplot(eda_data2, aes(factor(stunted), fill = factor(underweight))) +
  geom_bar(position = "fill") +
  geom_text(stat = "count", aes(label = after_stat(count)), 
            position = position_fill(vjust = 0.5), color = "white", size = 4) +
  scale_fill_manual(
    name = "Underweight",
    breaks = c("0","1"),
    labels = c("0" = "No", "1" = "Yes"),
    values = c("0" = "chartreuse4", "1" = "brown")
  ) +
  scale_x_discrete(labels = c("0" = "No Stunting", "1" = "Stunting")) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = paste0("Stunted vs Underweight (χ² = ", round(chi2$statistic, 2), 
                      ", p < 2.2e-16)"), # Will display 0 if set round(chi1$p.value, 4), so we edit it manually
       x = "Stunted", y = "Percent")
ggsave("plots/exploratory_analysis/02_stunted_vs_underweight.png", p2, width = 8, height = 6, dpi = 300)

# 3. Wasted vs Underweight
eda_data3 <- bdhs_final[!is.na(bdhs_final$wasted) & !is.na(bdhs_final$underweight), ]
chi3 <- chisq.test(eda_data3$wasted, eda_data3$underweight)
p3 <- ggplot(eda_data3, aes(factor(wasted), fill = factor(underweight))) +
  geom_bar(position = "fill") +
  geom_text(stat = "count", aes(label = after_stat(count)), 
            position = position_fill(vjust = 0.5), color = "white", size = 4) +
  scale_fill_manual(
    name = "Underweight",
    breaks = c("0","1"),
    labels = c("0" = "No", "1" = "Yes"),
    values = c("0" = "chartreuse4", "1" = "brown")
  ) +
  scale_x_discrete(labels = c("0" = "No Wasting", "1" = "Wasting")) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = paste0("Wasted vs Underweight (χ² = ", round(chi3$statistic, 2), 
                      ", p = ", round(chi3$p.value, 4), ")"),
       x = "Wasted", y = "Percent")
ggsave("plots/exploratory_analysis/03_wasted_vs_underweight.png", p3, width = 8, height = 6, dpi = 300)

# PART 2: OBJECTIVE 1 - WEALTH AND MALNUTRITION
# Visualizing Chi-square tests for wealth vs. malnutritions
# Stunting
p4 <- ggplot(bdhs_final, aes(wealth_quintile, fill = factor(stunted))) +
  geom_bar(position = "fill") +
  scale_fill_manual(
    name = "",
    breaks = c("0","1"),
    labels = c("0" = "No Stunting", "1" = "Stunting"),
    values = c("0" = "chartreuse4", "1" = "brown")
  ) +
  scale_x_discrete(limits = c("Poorest","Poorer","Middle","Richer","Richest")) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = paste0("Stunting by Wealth Quintile (χ² = ", round(chi_stunting_wealth$statistic, 2), 
                      ", p < 0.001)"),
       x = "Wealth Quintile", y = "Percent within quintile")
ggsave("plots/exploratory_analysis/04_stunting_by_wealth.png", p4, width = 8, height = 6, dpi = 300)

# Wasting
p5 <- ggplot(bdhs_final, aes(wealth_quintile, fill = factor(wasted))) +
  geom_bar(position = "fill") +
  scale_fill_manual(
    name = "",
    breaks = c("0","1"),
    labels = c("0" = "No Wasting", "1" = "Wasting"),
    values = c("0" = "chartreuse4", "1" = "brown")
  ) +
  scale_x_discrete(limits = c("Poorest","Poorer","Middle","Richer","Richest")) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = paste0("Wasting by Wealth Quintile (χ² = ", round(chi_wasting_wealth$statistic, 2), 
                      ", p = ", round(chi_wasting_wealth$p.value, 4), ")"),
       x = "Wealth Quintile", y = "Percent within quintile")
ggsave("plots/exploratory_analysis/05_wasting_by_wealth.png", p5, width = 8, height = 6, dpi = 300)

# Underweight
p6 <- ggplot(bdhs_final, aes(wealth_quintile, fill = factor(underweight))) +
  geom_bar(position = "fill") +
  scale_fill_manual(
    name = "",
    breaks = c("0","1"),
    labels = c("0" = "No Underweight", "1" = "Underweight"),
    values = c("0" = "chartreuse4", "1" = "brown")
  ) +
  scale_x_discrete(limits = c("Poorest","Poorer","Middle","Richer","Richest")) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = paste0("Underweight by Wealth Quintile (χ² = ", round(chi_uw_wealth$statistic, 2), 
                      ", p < 0.001)"),
       x = "Wealth Quintile", y = "Percent within quintile")
ggsave("plots/exploratory_analysis/06_underweight_by_wealth.png", p6, width = 8, height = 6, dpi = 300)

# PART 3: OBJECTIVE 2 - HOUSEHOLD CONDITIONS AND MALNUTRITION
# Objective 2A: Head Age & Sex & Family Type with malnutrition outcomes
# Stunting vs. normal - Head Age
df_stunting_head_age <- data.frame(
  group = c(rep("Stunting", length(stunted_head_age)),
            rep("No Malnutrition", length(normal_head_age))),
  head_age = c(stunted_head_age, normal_head_age)
)

p7 <- ggplot(df_stunting_head_age, aes(group, head_age, fill = group)) +
  geom_violin(trim = TRUE, width = 0.9, alpha = 0.35, color = NA) +
  geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", size = 2, shape = 21, fill = "white") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.06) +
  scale_fill_manual(values = c("No Malnutrition" = "chartreuse4", "Stunting" = "brown")) +
  labs(title = paste0("Head Age: Stunting vs Normal\n(t = ", round(t_stunting_age$statistic, 2), 
                      ", p = ", round(t_stunting_age$p.value, 4), ")"),
       x = NULL, y = "Head Age (years)", fill = NULL) +
  theme(legend.position = "none")
ggsave("plots/exploratory_analysis/07_head_age_stunting.png", p7, width = 8, height = 6, dpi = 300)

# Wasting vs. normal - Head Age
df_wasting_head_age <- data.frame(
  group = c(rep("Wasting", length(wasted_head_age)),
            rep("No Malnutrition", length(normal_head_age))),
  head_age = c(wasted_head_age, normal_head_age)
)

p8 <- ggplot(df_wasting_head_age, aes(group, head_age, fill = group)) +
  geom_violin(trim = TRUE, width = 0.9, alpha = 0.35, color = NA) +
  geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", size = 2, shape = 21, fill = "white") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.06) +
  scale_fill_manual(values = c("No Malnutrition" = "chartreuse4", "Wasting" = "brown")) +
  labs(title = paste0("Head Age: Wasting vs Normal\n(t = ", round(t_wasting_age$statistic, 2), 
                      ", p = ", round(t_wasting_age$p.value, 4), ")"),
       x = NULL, y = "Head Age (years)", fill = NULL) +
  theme(legend.position = "none")
ggsave("plots/exploratory_analysis/08_head_age_wasting.png", p8, width = 8, height = 6, dpi = 300)

# underweight vs. normal - Head Age
df_uw_head_age <- data.frame(
  group = c(rep("Underweight", length(uw_head_age)),
            rep("No Malnutrition", length(normal_head_age))),
  head_age = c(uw_head_age, normal_head_age)
)

p9 <- ggplot(df_uw_head_age, aes(group, head_age, fill = group)) +
  geom_violin(trim = TRUE, width = 0.9, alpha = 0.35, color = NA) +
  geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", size = 2, shape = 21, fill = "white") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.06) +
  scale_fill_manual(values = c("No Malnutrition" = "chartreuse4", "Underweight" = "brown")) +
  labs(title = paste0("Head Age: Underweight vs Normal\n(t = ", round(t_uw_age$statistic, 2), 
                      ", p = ", round(t_uw_age$p.value, 4), ")"),
       x = NULL, y = "Head Age (years)", fill = NULL) +
  theme(legend.position = "none")
ggsave("plots/exploratory_analysis/09_head_age_underweight.png", p9, width = 8, height = 6, dpi = 300)

# Malnutrition vs. normal - Head Sex
data_mal_sex <- bdhs_final[!is.na(bdhs_final$any_malnutrition), ]
chi_mal_sex <- chisq.test(table(data_mal_sex$any_malnutrition, data_mal_sex$head_sex))
p10 <- ggplot(data_mal_sex, aes(head_sex, fill = factor(any_malnutrition))) +
  geom_bar(position = "fill") +
  scale_fill_manual(
    name = "",
    breaks = c("0","1"),
    labels = c("0" = "No Malnutrition", "1" = "Malnutrition"),
    values = c("0" = "chartreuse4", "1" = "brown")
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = paste0("Malnutrition by Head Sex (χ² = ", round(chi_mal_sex$statistic, 2), 
                      ", p = ", round(chi_mal_sex$p.value, 4), ")"),
       x = "Head Sex", y = "Percent")
ggsave("plots/exploratory_analysis/10_malnutrition_by_head_sex.png", p10, width = 8, height = 6, dpi = 300)

# Malnutrition vs. normal - Family Type
data_mal_rel <- bdhs_final[!is.na(bdhs_final$relationship), ]
chi_mal_rel <- chisq.test(table(data_mal_rel$any_malnutrition, data_mal_rel$relationship))
p11 <- ggplot(data_mal_rel, aes(relationship, fill = factor(any_malnutrition))) +
  geom_bar(position = "fill") +
  scale_x_discrete(labels = c(
    "Traditional"     = "Traditional",
    "Non_traditional" = "Non-Traditional")) +
  scale_fill_manual(
    name = "",
    breaks = c("0","1"),
    labels = c("0" = "No Malnutrition", "1" = "Malnutrition"),
    values = c("0" = "chartreuse4", "1" = "brown")
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = paste0("Malnutrition by Family Type (χ² = ", round(chi_mal_sex$statistic, 2), 
                      ", p = ", round(chi_mal_sex$p.value, 4), ")"),
       x = "Family Type", y = "Percent")
ggsave("plots/exploratory_analysis/11_malnutrition_by_family_type.png", p11, width = 8, height = 6, dpi = 300)

# Objective 2B: Household size with malnutrition outcomes
# Stunting
df_stunting_hh <- data.frame(
  group = c(rep("Stunting", length(stunted_hh_size)),
            rep("No Malnutrition", length(normal_hh_size))),
  hh_size = c(stunted_hh_size, normal_hh_size)
)

p12 <- ggplot(df_stunting_hh, aes(group, hh_size, fill = group)) +
  geom_violin(trim = TRUE, width = 0.9, alpha = 0.35, color = NA) +
  geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", size = 2, shape = 21, fill = "white") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.06) +
  scale_fill_manual(values = c("No Malnutrition" = "chartreuse4", "Stunting" = "brown")) +
  labs(title = paste0("Household Size: Stunting vs Normal\n(t = ", round(t_stunting_hh$statistic, 2), 
                      ", p = ", round(t_stunting_hh$p.value, 4), ")"),
       x = NULL, y = "Household Members", fill = NULL) +
  theme(legend.position = "none")
ggsave("plots/exploratory_analysis/12_household_size_stunting.png", p12, width = 8, height = 6, dpi = 300)

# Wasting
df_wasting_hh <- data.frame(
  group = c(rep("Wasting", length(wasted_hh_size)),
            rep("No Malnutrition", length(normal_hh_size))),
  hh_size = c(wasted_hh_size, normal_hh_size)
)

p13 <- ggplot(df_wasting_hh, aes(group, hh_size, fill = group)) +
  geom_violin(trim = TRUE, width = 0.9, alpha = 0.35, color = NA) +
  geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", size = 2, shape = 21, fill = "white") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.06) +
  scale_fill_manual(values = c("No Malnutrition" = "chartreuse4", "Wasting" = "brown")) +
  labs(title = paste0("Household Size: Wasting vs Normal\n(t = ", round(t_wasting_hh$statistic, 2), 
                      ", p = ", round(t_wasting_hh$p.value, 4), ")"),
       x = NULL, y = "Household Members", fill = NULL) +
  theme(legend.position = "none")
ggsave("plots/exploratory_analysis/13_household_size_wasting.png", p13, width = 8, height = 6, dpi = 300)

# Underweight
df_uw_hh <- data.frame(
  group = c(rep("Underweight", length(uw_hh_size)),
            rep("No Malnutrition", length(normal_hh_size))),
  hh_size = c(uw_hh_size, normal_hh_size)
)

p14 <- ggplot(df_uw_hh, aes(group, hh_size, fill = group)) +
  geom_violin(trim = TRUE, width = 0.9, alpha = 0.35, color = NA) +
  geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", size = 2, shape = 21, fill = "white") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.06) +
  scale_fill_manual(values = c("No Malnutrition" = "chartreuse4", "Underweight" = "brown")) +
  labs(title = paste0("Household Size: Underweight vs Normal\n(t = ", round(t_uw_hh$statistic, 2), 
                      ", p = ", round(t_uw_hh$p.value, 4), ")"),
       x = NULL, y = "Household Members", fill = NULL) +
  theme(legend.position = "none")
ggsave("plots/exploratory_analysis/14_household_size_underweight.png", p14, width = 8, height = 6, dpi = 300)

# Objective 2C: Children Number with malnutrition outcomes
# Stunting
df_stunting_child <- data.frame(
  group = c(rep("Stunting", length(stunted_children)),
            rep("No Malnutrition", length(normal_children))),
  children = c(stunted_children, normal_children)
)

p15 <- ggplot(df_stunting_child, aes(group, children, fill = group)) +
  geom_violin(trim = TRUE, width = 0.9, alpha = 0.35, color = NA) +
  geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", size = 2, shape = 21, fill = "white") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.06) +
  scale_fill_manual(values = c("No Malnutrition" = "chartreuse4", "Stunting" = "brown")) +
  labs(title = paste0("Total Children Born: Stunting vs Normal\n(t = ", round(t_stunting_child$statistic, 2), 
                      ", p < 0.001)"),
       x = NULL, y = "Total Children Born", fill = NULL) +
  theme(legend.position = "none")
ggsave("plots/exploratory_analysis/15_children_number_stunting.png", p15, width = 8, height = 6, dpi = 300)

# Wasting
df_wasting_child <- data.frame(
  group = c(rep("Wasting", length(wasted_children)),
            rep("No Malnutrition", length(normal_children))),
  children = c(wasted_children, normal_children)
)

p16 <- ggplot(df_wasting_child, aes(group, children, fill = group)) +
  geom_violin(trim = TRUE, width = 0.9, alpha = 0.35, color = NA) +
  geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", size = 2, shape = 21, fill = "white") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.06) +
  scale_fill_manual(values = c("No Malnutrition" = "chartreuse4", "Wasting" = "brown")) +
  labs(title = paste0("Total Children Born: Wasting vs Normal\n(t = ", round(t_wasting_child$statistic, 2), 
                      ", p = ", round(t_wasting_child$p.value, 4), ")"),
       x = NULL, y = "Total Children Born", fill = NULL) +
  theme(legend.position = "none")
ggsave("plots/exploratory_analysis/16_children_number_wasting.png", p16, width = 8, height = 6, dpi = 300)

# underweight
df_uw_child <- data.frame(
  group = c(rep("Underweight", length(uw_children)),
            rep("No Malnutrition", length(normal_children))),
  children = c(uw_children, normal_children)
)

p17 <- ggplot(df_uw_child, aes(group, children, fill = group)) +
  geom_violin(trim = TRUE, width = 0.9, alpha = 0.35, color = NA) +
  geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", size = 2, shape = 21, fill = "white") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.06) +
  scale_fill_manual(values = c("No Malnutrition" = "chartreuse4", "Underweight" = "brown")) +
  labs(title = paste0("Total Children Born: Underweight vs Normal\n(t = ", round(t_uw_child$statistic, 2), 
                      ", p < 0.001)"),
       x = NULL, y = "Total Children Born", fill = NULL) +
  theme(legend.position = "none")
ggsave("plots/exploratory_analysis/17_children_number_underweight.png", p17, width = 8, height = 6, dpi = 300)

# Part 4: Objective 3: Wealth & Household correlation
# WEalth & Household Numbers
wealth_hh_table <- table(bdhs_final$wealth_quintile, bdhs_final$household_size_cat)
wealth_hh_test <- chisq.test(wealth_hh_table)
p18 <- ggplot(bdhs_final, aes(wealth_quintile, fill = household_size_cat)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set2", name = "Household Size") +
  scale_x_discrete(limits = c("Poorest","Poorer","Middle","Richer","Richest")) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = paste0("Household Size by Wealth Quintile (χ² = ", 
                      round(wealth_hh_test$statistic, 2), ", p < 0.001)"),
       x = "Wealth Quintile", y = "Percent within quintile")
ggsave("plots/exploratory_analysis/18_wealth_household_size.png", p18, width = 8, height = 6, dpi = 300)

# Wealth & Children Numbers
wealth_child_table <- table(bdhs_final$wealth_quintile, bdhs_final$children_cat)
wealth_child_test <- chisq.test(wealth_child_table)
p19 <- ggplot(bdhs_final, aes(wealth_quintile, fill = children_cat)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set2", name = "Children Category") +
  scale_x_discrete(limits = c("Poorest","Poorer","Middle","Richer","Richest")) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = paste0("Children Number by Wealth Quintile (χ² = ", 
                      round(wealth_child_test$statistic, 2), ", p < 0.001)"),
       x = "Wealth Quintile", y = "Percent within quintile")
ggsave("plots/exploratory_analysis/19_wealth_children_number.png", p19, width = 8, height = 6, dpi = 300)

# Part 5: Objective 4: Parent Education and malnutrition
mal_avg_edu <- bdhs_final$average_parent_edu[bdhs_final$any_malnutrition == 1]
normal_avg_edu <- bdhs_final$average_parent_edu[bdhs_final$any_malnutrition == 0]

edu_comparison <- data.frame(
  education = c(mal_avg_edu, normal_avg_edu),
  group = c(rep("Malnutrition", length(mal_avg_edu)), 
            rep("No Malnutrition", length(normal_avg_edu)))
)
p20 <- ggplot(edu_comparison, aes(x = group, y = education, fill = group)) +
  geom_violin(trim = TRUE, width = 0.9, alpha = 0.35, color = NA) +
  geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", size = 2, shape = 21, fill = "white") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.06) +
  scale_fill_manual(values = c("No Malnutrition" = "chartreuse4", "Malnutrition" = "brown")) +
  labs(title = paste0("Average Parent Education: Malnutrition vs Normal\n(t = ", 
                      round(t_test_avg$statistic, 2), ", p < 0.001)"),
       x = NULL, y = "Years of Education", fill = NULL) +
  theme(legend.position = "none")
ggsave("plots/exploratory_analysis/20_parent_education_malnutrition.png", p20, width = 8, height = 6, dpi = 300)

# Part 6: Urban Rural Distribution of Malnutrition 
malnutrition_residence <- table(bdhs_final$any_malnutrition, bdhs_final$residence)
malnutrition_res_test <- chisq.test(malnutrition_residence)
p21 <- ggplot(bdhs_final, aes(residence, fill = factor(any_malnutrition))) +
  geom_bar(position = "fill") +
  scale_fill_manual(
    name = "",
    breaks = c("0","1"),
    labels = c("0" = "No Malnutrition", "1" = "Any Malnutrition"),
    values = c("0" = "chartreuse4", "1" = "brown")
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = paste0("Any Malnutrition by Residence (χ² = ", round(malnutrition_res_test$statistic, 2), 
                      ", p = ", round(malnutrition_res_test$p.value, 4), ")"),
       x = "Residence", y = "Percent within residence")
ggsave("plots/exploratory_analysis/21_malnutrition_by_residence.png", p21, width = 8, height = 6, dpi = 300)