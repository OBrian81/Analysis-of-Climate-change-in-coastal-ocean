#Load the datasets
benguela <- read.csv("https://raw.githubusercontent.com/tgouhier/biostats/main/assn3_benguela.csv")
california <- read.csv("https://raw.githubusercontent.com/tgouhier/biostats/main/assn3_california.csv")
canary  <-  read.csv("https://raw.githubusercontent.com/tgouhier/biostats/main/assn3_canary.csv")
humboldt <- read.csv("https://raw.githubusercontent.com/tgouhier/biostats/main/assn3_humboldt.csv")


#Add categorical variable, Period 

california$period <- ifelse(california$year <= 2024, "before", "after")
benguela$period <- ifelse(benguela$year <= 2024, "before", "after")
canary$period <- ifelse(canary$year <= 2024, "before", "after")
humboldt$period <- ifelse(humboldt$year <= 2024, "before", "after")


#create a new data frame containing the following three variables (columns):
#year, period, and the multimodel mean

california.mean <- data.frame(year=california$year, period=california$period, upwelling=rowMeans(california[, 1:22]))
canary.mean <- data.frame(year=canary$year, period=canary$period, upwelling=rowMeans(canary[, 1:22]))
humboldt.mean <- data.frame(year=humboldt$year, period=humboldt$period, upwelling=rowMeans(humboldt[, 1:22]))
benguela.mean <- data.frame(year=benguela$year, period=benguela$period, upwelling=rowMeans(benguela[, 1:22]))




# Check the distribution of normality and conduct the appropriate test

# California
shapiro.test(california.mean$upwelling[california.mean$period == "before"])
shapiro.test(california.mean$upwelling[california.mean$period == "after"])

# Canary
shapiro.test(canary.mean$upwelling[canary.mean$period == "before"])
shapiro.test(canary.mean$upwelling[canary.mean$period == "after"])

# Humboldt
shapiro.test(humboldt.mean$upwelling[humboldt.mean$period == "before"])
shapiro.test(humboldt.mean$upwelling[humboldt.mean$period == "after"])

# Benguela
shapiro.test(benguela.mean$upwelling[benguela.mean$period == "before"])
shapiro.test(benguela.mean$upwelling[benguela.mean$period == "after"])

#For Canary and Humboldt EBCS, data is not normally distributed hence we will use non-parametric test e.g Wilcoxon rank-sum test 
#For California and Benguela EBCS, data is normally distributed hence we will use parametric test e.g t-test 




#4. , conduct the appropriate test to compare the multi- model mean across periods for each EBCS.

#Null hypothesis (H0): The mean upwelling in the 'before' period is equal to the mean upwelling in the 'after' period.

# For California and Benguela (parametric t-test)
t.test(california.mean$upwelling[california.mean$period == "before"],
       california.mean$upwelling[california.mean$period == "after"])

t.test(benguela.mean$upwelling[benguela.mean$period == "before"],
       benguela.mean$upwelling[benguela.mean$period == "after"])

# For Canary and Humboldt (non-parametric Wilcoxon rank-sum test)
wilcox.test(canary.mean$upwelling[canary.mean$period == "before"],
            canary.mean$upwelling[canary.mean$period == "after"])

wilcox.test(humboldt.mean$upwelling[humboldt.mean$period == "before"],
            humboldt.mean$upwelling[humboldt.mean$period == "after"])

#5.Find the mean and standard error of the upwelling at each site and for the before and after periods

mean.upwelling.california <- tapply(california.mean$upwelling, california.mean$period, mean)
sderr.upwelling.california <- tapply(california.mean$upwelling, california.mean$period, function(x) sd(x) / sqrt(length(x)))

mean.upwelling.canary <- tapply(canary.mean$upwelling, canary.mean$period, mean)
sderr.upwelling.canary <- tapply(canary.mean$upwelling, canary.mean$period, function(x) sd(x) / sqrt(length(x)))

mean.upwelling.humboldt <- tapply(humboldt.mean$upwelling, humboldt.mean$period, mean)
sderr.upwelling.humboldt <- tapply(humboldt.mean$upwelling, humboldt.mean$period, function(x) sd(x) / sqrt(length(x)))

mean.upwelling.benguela <- tapply(benguela.mean$upwelling, benguela.mean$period, mean)
sderr.upwelling.benguela <- tapply(benguela.mean$upwelling, benguela.mean$period, function(x) sd(x) / sqrt(length(x)))




# 6. Combine the means and standard errors into one dataframe using the following code:


upwelling.mean <- data.frame(region=c("Canary", "California", "Benguela", "Humboldt"),
                             after=c(mean.upwelling.canary[2],
                                     mean.upwelling.california[2],
                                     mean.upwelling.benguela[2],
                                     mean.upwelling.humboldt[2]),
                             before=c(mean.upwelling.canary[1],
                                      mean.upwelling.california[1],
                                      mean.upwelling.benguela[1],
                                      mean.upwelling.humboldt[1]))

bp <- barplot(height = c(upwelling.mean$before, upwelling.mean$after),
              beside = TRUE,
              ylim = c(0, max(c(upwelling.mean$before, upwelling.mean$after)) + max(c(sderr.upwelling$before, sderr.upwelling$after))),
              main = "Multimodel Mean Upwelling",
              xlab = "EBCS",
              ylab = "Upwelling (m^2/s)",
              col = c("blue", "red"),
              names.arg = rep(upwelling.mean$region, 2))

# Add error bars
arrows(bp, upwelling.mean[, 2] - sderr.upwelling[, 1],
       bp, upwelling.mean[, 2] + sderr.upwelling[, 1],
       length = 0.05, angle = 90, code = 3)

arrows(bp, upwelling.mean[, 3] + sderr.upwelling[, 2],
       bp, upwelling.mean[, 3] - sderr.upwelling[, 2],
       length = 0.05, angle = 90, code = 3)

arrows(bp, upwelling.mean[, 2] - upwelling.sderr[, 2], bp, upwelling.mean[, 2] + upwelling.sderr[, 2], length = 0.05, angle = 90, code = 3)
arrows(bp, upwelling.mean[, 3] + upwelling.sderr[, 3], bp, upwelling.mean[, 3] - upwelling.sderr[, 3], length = 0.05, angle = 90, code = 3)



#7. 

upwelling.mean

#For California, the mean upwelling before and after 2024 is 0.34157 and 0.35151, respectively, indicating a slight increase in upwelling after 2024. This trend is supported by the results of the t-test, which shows a statistically significant difference between the two periods (p-value = 4.72e-05).
#For Canary, the mean upwelling before and after 2024 is 0.01572 and 0.01722, respectively, indicating a slight increase in upwelling after 2024. However, the Wilcoxon rank-sum test shows a statistically significant difference between the two periods (p-value < 2.2e-16), suggesting that the increase in upwelling after 2024 may not be due to chance.
#For Benguela, the mean upwelling before and after 2024 is 0.82060 and 0.91594, respectively, indicating a significant increase in upwelling after 2024. This trend is supported by the results of the t-test, which shows a statistically significant difference between the two periods (p-value < 2.2e-16).
#For Humboldt, the mean upwelling before and after 2024 is 0.01572 and 0.01722, respectively, indicating a slight increase in upwelling after 2024. However, the Wilcoxon rank-sum test shows a statistically significant difference between the two periods (p-value < 2.2e-16), suggesting that the increase in upwelling after 2024 may not be due to chance.


#8.

# F-test for California
f.test.california <- var.test(california.mean$upwelling[california.mean$period == "before"],
                              california.mean$upwelling[california.mean$period == "after"])
f.test.california

# F-test for Benguela
f.test.benguela <- var.test(benguela.mean$upwelling[benguela.mean$period == "before"],
                            benguela.mean$upwelling[benguela.mean$period == "after"])
f.test.benguela


# Levene's test for Canary
levene.test.canary <- leveneTest(upwelling ~ period, data = canary.mean)
levene.test.canary

# Levene's test for Humboldt
levene.test.humboldt <- leveneTest(upwelling ~ period, data = humboldt.mean)
levene.test.humboldt


#To test for the difference in variance in annual upwelling before and after a certain period, two potential tests that could be used are the Levene's test and the F-test.
#Levene's Test: This test is a non-parametric test that does not assume normality of the data. It is used to determine if there is a significant difference in variance between two or more groups. The null hypothesis is that the variances are equal, and the alternative hypothesis is that at least one of the variances is different.

#Assumptions of Levene's Test:
#The data should be independent and identically distributed.
#The groups should have equal sample sizes.
#The test is sensitive to departures from normality, but it is robust to moderate violations.
#F-test: This test is a parametric test that assumes normality of the data. It is used to compare the variances of two groups. The null hypothesis is that the variances are equal, and the alternative hypothesis is that the variances are different.

#Assumptions of F-test:
#The data should be independent and identically distributed.
#The data should be normally distributed.
#The variances should be equal in the two groups being compared.

#Both tests can be conducted using the leveneTest() and var.test() functions in R, respectively. The p-value from each test can be compared to a significance level (e.g., 0.05) to determine if there is a significant difference in variance between the two periods. If the p-value is less than the significance level, the null hypothesis is rejected, and it is concluded that there is a significant difference in variance.




# 9.

leveneTest(california.mean$upwelling ~ california.mean$period)
leveneTest(canary.mean$upwelling ~ canary.mean$period)
leveneTest(humboldt.mean$upwelling ~ humboldt.mean$period)
leveneTest(benguela.mean$upwelling ~ benguela.mean$period)


#From the output, we can see that the p-values for California, Canary, and Humboldt EBCS are all less than 0.05, indicating that we can reject the null hypothesis and conclude that the variances of the upwelling in the before and after periods are not equal.
#For the Benguela EBCS, the p-value is 0.468, which is greater than 0.05. Therefore, we cannot reject the null hypothesis and conclude that the variances of the upwelling in the before and after periods are equal.
#Therefore, we can conclude that the multimodel variance differs between periods for California, Canary, and Humboldt EBCS, but not for Benguela EBCS