data("mtcars")


str(mtcars)
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$vs <- as.factor(mtcars$vs)
mtcars$am <- as.factor(mtcars$am)
mtcars$gear <- as.factor(mtcars$gear)
mtcars$carb <- as.factor(mtcars$carb)


hist(mtcars$mpg,breaks = 15, col='aquamarine3')
plot(mtcars$am,mtcars$mpg,col=c('lightsteelblue2','pink3'))

fit <- lm(mpg~am,data = mtcars)
summary(fit)
plot(as.numeric(mtcars$am)-1,mtcars$mpg,xlab='Transmission',ylab='mpg',
     col=rgb(0,.6,.6,.6), bg=rgb(0,.6,.6,.3),cex=1.8, pch=21)
abline(fit)

fitall <- lm(mpg~.,data = mtcars)
summary(fitall)


par(mfrow=c(2,2))
plot(fitall)

plot(mtcars$am,resid(fit))
abline(h=0)
