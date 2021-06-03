
lambda <- 0.2
n <- 40
B <- 10000

samples <- matrix(rexp(B*n, lambda),B, n)
hist(samples)
sampledAvg <- apply(samples, 1, mean)
hist(sampledAvg)

avg <- mean(sampledAvg)
# actual mean = 1/lambda = 5
sigma <- var(sampledAvg)
# actual sd = (1/lambda)/sqrt(n)


hist(sampledAvg,breaks = 100, freq = FALSE)
x <- seq(from=1,to=9,length.out = 100)
y <- dnorm(x, mean = 1/lambda, sd = (1/lambda)/sqrt(n))
lines(x,y,type='l',lty=5,lwd=2,col='red')