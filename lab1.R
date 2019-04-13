library(readxl)
data <- read_excel("C:\\Users\\jlanj\\Desktop\\HU\\510\\Lab1\\WeeklyLab1Data.xlsx")


# Check if Rating is normally distributed
den <- density(data$Rating)
plot(den)

##Shapiro test to check if data is normal
shapiro.test(data$Rating)

##Agostino test to check negative skewness
library(moments)
agostino.test(data$Rating)

# kurtosis
anscombe.test(data$Rating)

#Remove kurtosis
data$Rating2 <- log(data$Rating)
plot(density(data$Rating2))

agostino.test(data$Rating2)
anscombe.test(data$Rating2)

##Factor all data
data$Ad <- factor(data$Ad)
data$Day <- factor(data$Day)
data$Audience <- factor(data$Audience)

bartlett.test(data$Rating2,data$Ad)

##Fit the data to Anova, check residuals
ano <- aov(Rating2~Day+Day/Audience+Ad, data = data)
ano


qqnorm(block$residuals)


##Check the impact of the ads, get Mean and SD
TukeyHSD(ano, "Ad")

tapply(data$Rating2, data$Ad, mean)
tapply(data$ Rating2, data$Ad, sd)
