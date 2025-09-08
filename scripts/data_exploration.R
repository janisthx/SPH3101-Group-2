data <- read.csv('data/bdhs.csv')
# 绘制体重/年龄Z-值的直方图
data$HW71[data$HW71 %in% c(9996, 9997, 9998)] <- NA
hist(data$HW71, main = "Distribution of Weight-for-Age Z-score", xlab = "HW71 Z-score", col = "skyblue", border = "black")

# 计算均值和标准差
mean(data$HW71, na.rm = TRUE)
sd(data$HW71, na.rm = TRUE)

# 查看财富指数（V190）的分布
table(data$V190)

# 创建一个二元变量，表示儿童是否营养不良（HW71 < -2）
data$underweight <- ifelse(data$HW71 < -200, 1, 0)

# 计算每个财富等级下的营养不良发生率
wealth_underweight_rate <- aggregate(underweight ~ V190, data = data, FUN = mean, na.rm = TRUE)
print(wealth_underweight_rate)

# 确保 V190 是因子类型
data$V190 <- factor(data$V190)

# 创建一个列联表
contingency_table <- table(data$V190, data$underweight)
print(contingency_table)

# 进行卡方检验
chisq.test(contingency_table)

# 确保 V102 是因子类型，并为每个类别命名以便于理解
data$V102 <- factor(data$V102, levels = c(1, 2), labels = c("Urban", "Rural"))

# 使用 t 检验比较城市和农村儿童的平均 HW71
t.test(HW71 ~ V102, data = data)



# 确保母亲教育水平 (V106) 是因子类型
data$V106 <- factor(data$V106, levels = c(0, 1, 2, 3), labels = c("No education", "Primary", "Secondary", "Higher"))

# 运行 ANOVA 来比较不同母亲教育水平组的 HW71 均值
anova_mother_edu <- aov(HW71 ~ V106, data = data)

# 查看 ANOVA 结果
summary(anova_mother_edu)
