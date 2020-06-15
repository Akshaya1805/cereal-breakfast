slr <- read.csv("E:/data science/R/datas/8Regression/slr.csv")
str(slr)
mod = lm(Sales ~ Advt, data = slr)
summary(mod)

pred = predict(mod)
pred

slr$pred = pred
View(slr)

res = residuals(mod)
res
slr$res = res

cor(slr$Advt, slr$Sales)
plot(slr$Advt, slr$Sales, col = "blue", pch =16, cex = 1.5)

str(mtcars)
mod2 = lm(mpg ~ disp + hp + drat + wt, data = mtcars)
summary(mod2)

par(mfrow = c(2,2))
plot(mtcars$wt, mtcars$mpg)
plot(mtcars$drat, mtcars$mpg)
plot(mtcars$hp, mtcars$mpg)
plot(mtcars$disp, mtcars$mpg)

ss = mtcars[, c(1,3,4,5)]
ssm = as.matrix(ss)
library(psych)
pairs.panels(ss)
