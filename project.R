# looking at a data set of a collection of cars, a magazine is interested in 
# exploring the relationship between a set of variables and miles per gallon 
# (MPG) (outcome). In particular, explore the following questions:
# 1. Is an automatic or manual transmission better for MPG
# 2. Quantify the MPG difference between automatic and manual transmissions

library(ggplot2)
require(cowplot)
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
# this model assumes one line through the data.

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

ggplot(data = mtcars, aes(y = mpg, x = wt, color = factor(am))) + 
    geom_point() + xlab("weight (1000 lbs)") + ylab("miles/gallon")

# this plot indicates that weight influences the mpg outcome directly, so this
# is a variable worth having into account when analyzing whether automatic or
# manual transmission is better for mpg
# manual cars weight less than automatic cars

# linear fit for manual cars
y1 <- mtcars$mpg[mtcars$am == 1]
x1 <- mtcars$wt[mtcars$am == 1]
data1 <- data.frame(x1, y1)
fit1 <- lm(y1 ~ mtcars$am[mtcars$am == 1] + x1)
coef1 <- summary(fit1)$coefficients
coef1

# linear fit for automatic cars
y0 <- mtcars$mpg[mtcars$am == 0]
x0 <- mtcars$wt[mtcars$am == 0]
data0 <- data.frame(x0, y0)
fit0 <- lm(y0 ~ mtcars$am[mtcars$am == 0] + x0)
coef0 <- summary(fit0)$coefficients
coef0

fit2 <- lm(mpg ~ am + wt, data = mtcars)
coef2 <- summary(fit2)$coefficients
coef2

# adding a second variable to the model, the weight of the cars, namely, wt
# one finds that the coefficient in front of the binary variable, am, is the 
# change in the intercept between automatic and manual transmission. This model 
# generates two parallel lines with different interceps and the same slope 
# (coef2[3, 1]), which indicates the change in mpg per unit increase (1000 lbs) 
# in the car weight.

# quantify the uncertainty(?)

# residual plots
# plot residuals of the two fits that correspond to the values of the binary
# variable (am), with their respective residuals

e1 <- resid(fit1)
yhat1 <- predict(fit1)
max(abs(e1 - (y1 - yhat1)))

e0 <- resid(fit0)
yhat0 <- predict(fit0)
max(abs(e0 - (y0 - yhat0)))

#x <- mtcars$wt
#y <-mtcars$mpg
n1 <- length(y1)
n0 <- length(y0)

#plot(x1, y1)
#abline(a = coef1[1, 1], b = coef1[2, 1], lwd = 2)
#for (i in 1:n)
#    lines(c(x1[i], x1[i]), c(y1[i], yhat1[i]), col = "red", lwd = 2)

plot_manual <- ggplot(data1, aes(x = x1, y = y1)) + geom_point() + 
    geom_abline(intercept = coef1[1, 1], slope = coef1[2, 1]) + 
    stat_smooth(method = "lm", col = "blue") + 
    xlab("weight (1000 lbs)") + ylab("miles/gallon") + 
    ggtitle("manual")

plot_auto <- ggplot(data0, aes(x = x0, y = y0)) + geom_point() + 
    geom_abline(intercept = coef0[1, 1], slope = coef0[2, 1]) + 
    stat_smooth(method = "lm", col = "blue") + 
    xlab("weight (1000 lbs)") + ylab("") + 
    ggtitle("automatic")

plot_grid(plot_manual, plot_auto)


# executive summary



















