# looking at a data set of a collection of cars, a magazine is interested in 
# exploring the relationship between a set of variables and miles per gallon 
# (MPG) (outcome). In particular, explore the following questions:
# 1. Is an automatic or manual transmission better for MPG
# 2. Quantify the MPG difference between automatic and manual transmissions

library(ggplot2)
library(dplyr)

# the "am" variable in the mtcars dataset is of binary type, indicating manual
# tranmission with "1" and automatic transmission with "0"


g <- ggplot(data = mtcars, aes(y = mpg, x = factor(am), fill = factor(cyl)))
g <- g + geom_boxplot()
g <- g + xlab("transmission") + ylab("miles/gallon")
g


# fit a linear model with transmission as the only variable
fit <- lm(mpg ~ am, data = mtcars)
coef <- summary(fit)$coefficients
coef

# As the slope of the linear model indicates, there is a coef[2, 1] increase in
# miles/gallon going from automatic transmission to manual.

# confidence interval
confInterval <- confint(fit)
confInterval

# The confidence interval indicates that with a 95% confidence, there is an
# increase from confInterval[2, 1] to confInterval[2, 2] miles/gallon going from
# automatic transmission to manual.



# hypothesis testing


# including more variables
# weight

g <- ggplot(data = mtcars, aes(y = mpg, x = wt, color = factor(am)))
g <- g + geom_point()
g

# this plot indicates that weight influences the mpg outcome directly, so this
# is a variable worth having into account when analyzing whether automatic or
# manual transmission is better for mpg
# manual cars weight less than automatic cars

fit2 <- lm(mpg ~ am + wt, data = mtcars)
coef2 <- summary(fit2)$coefficients
coef2






















