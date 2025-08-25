bdhs = read.csv('data/bdhs.csv')

#interested in:
#V102 type of place of residence (rural/urban)
#V150 relationship to household head
#V151 sex of household head
#V152 age of household head

bdhs$V102 <- factor(bdhs$V102, levels = c(1,2), labels = c("Urban", "Rural"))
summary(bdhs$V102)

bdhs$V150 <- factor(bdhs$V150, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,98), labels = c("Head","Wife","Daughter","Daughter-in-law","Granddaughter", "Mother","Mother-in-law","Sister","Co-spouse","Other relative", "Adopted/foster child","Not related","Don't know"))
summary(bdhs$V150)

bdhs$V151 <- factor(bdhs$V151, levels = c(1,2), labels=c("Male", "Female"))
summary(bdhs$V151)

summary(bdhs$V152)
