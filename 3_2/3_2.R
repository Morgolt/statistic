# Lab 3_2

# выборки -----------------------------------------------------------------
a = 4
sigma = 2
N = 100
x.norm <- sort(rnorm(N, a, sigma))
# разбиение и интервалы -----------------------------------------------------------------
r = 10
x.breaks <- seq(min(x.norm), max(x.norm), length.out = r+1)
x.norm.hist <- hist(x.norm, breaks=x.breaks, plot = FALSE)
# относительные частоты -----------------------------------------------------------------
x.norm.relfreq <- x.norm.hist$сounts / N
x.breaks[1] <- (-Inf)
x.breaks[r+1] <- (Inf)
# теоретические вероятности -----------------------------------------------------------------
x.norm.p.theor <- pnorm(x.breaks, mean=a, sd = sigma)
x.norm.p.theor <- (x.norm.p.theor[2:(r+1)]-x.norm.p.theor[1:r])
# значение статистики chisq
alpha = 0.05
x.schisq <- qchisq(alpha, df=r-1)
# гипотеза согласия простая H_0 -----------------------------------------------------------------
x.test1 <- chisq.test(x.norm.hist$counts, p=x.norm.p.theor)
if (x.test1$p.value > alpha) {
  print('H0 is accepted.')
} else {
  print('H0 is declined.')
}
# оценки a и sigma -----------------------------------------------------------------
x.a <- sum(x.norm.hist$counts * x.norm.hist$mids)/N
x.sigma <- sqrt(sum(x.norm.hist$counts * (x.norm.hist$mids - a)^2)/N)
# новые вероятности относительно оценок -----------------------------------------------------------------
x.p.calculated <- pnorm(x.breaks, mean=x.a, sd = x.sigma)
x.p.calculated <- (x.p.calculated[2:(r+1)]-x.p.calculated[1:r])
x.schisq_calculated <- qchisq(1-alpha, r-3)
# гипотеза согласия сложная -----------------------------------------------------------------
x.test2 <- chisq.test(x.norm.hist$counts, p=x.p.calculated)
# 20 выборок с тестом -----------------------------------------------------------------
n = 30
df = 10
y.means <- vector('numeric')
for (i in 1:20) {
  temp <- rt(n, df)
  y.means <- c(y.means, mean(temp))
}
rm(temp)
r1 = 5
y.breaks <- seq(min(y.means), max(y.means), length.out = r1+1)
y.means.hist <- hist(y.means, breaks=y.breaks, plot = FALSE)
y.breaks[1] <- -Inf
y.breaks[r1+1] <- Inf
y.means.p.theor <- pnorm(y.breaks, mean=a, sd = sigma/sqrt(n))
y.means.p.theor <- (y.means.p.theor[2:(r1+1)]-y.means.p.theor[1:r1])
y.test <- chisq.test(y.means.hist$counts, p=y.means.p.theor)
if (y.test$p.value > alpha) {
  print('H0 is accepted.')
} else {
  print('H0 is declined.')
}
# критерий Колмогорова ----------------------------------------------------
x.test3 = ks.test(x.norm, "pnorm", a, sigma)
if (x.test3$p.value > alpha) {
  print('H0 is accepted.')
} else {
  print('H0 is declined.')
}
y.test2 = ks.test(y.means, "pnorm", a, sigma/sqrt(n))
if (y.test2$p.value > alpha) {
  print('H0 is accepted.')
} else {
  print('H0 is declined.')
}


