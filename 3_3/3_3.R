
# Лабораторная 3_3 --------------------------------------------------------
## utility functions ####
# check_H - check if statistic is in critical interval. ####
# if type = 1 - one-sided interval
# if type = 2 - two-sided interval
check_H <- function(statistic, interval, type) {
  b <- switch(type,
              cbind(interval[1] < statistic & interval[2] > statistic),
              cbind((interval[1] < statistic & interval[2] > statistic) | (interval[3] < statistic & interval[4] > statistic)))
  return(ifelse(b, "H0 is declined", "H0 is accepted"))
}
# выборки -----------------------------------------------------------------
x.n = 50
y.m = 30
x.a = -3
x.sigma = 2
x.norm <- rnorm(x.n, x.a, x.sigma)
y.a = 2
y.sigma = 3
y.norm <- rnorm(y.m, y.a, y.sigma)

# проверка гипотезы H_0: x.a = y.a, зная дисперсии ------------------------
alpha = 0.05
ztest.stat = (mean(x.norm) - mean(y.norm)) / sqrt(x.sigma^2/x.n + y.sigma^2/y.m)
ztest.cinterval = c(-Inf, qnorm(alpha/2, 0, 1))
print(check_H(ztest.stat, ztest.cinterval, 1))

# проверка гипотезы H_0: x.sigma^2 = y.sigma^2 ------------------------------------------
ftest.stat = var(x.norm)/var(y.norm)
ftest.cinterval = c(-Inf, qf(alpha/2, x.n-1, y.m-1), qf(1-alpha/2, x.n-1, y.m-1), Inf)
print(check_H(ftest.stat, ftest.cinterval, 2))
print(var.test(x.norm, y.norm, alternative="two.sided", conf.level = 0.95))

# новые выборки с одинаковыми дисперсиями ---------------
x2.n = 50
y2.m = 30
x2.a = 4
y2.a = 3
x2.sigma = 2
y2.sigma = 2
x2.norm = rnorm(x2.n, x2.a, x2.sigma)
y2.norm = rnorm(y2.m, y2.a, y2.sigma)

# H_0: x2.a = y2.a, x2.sigma = y2.sigma -------------
ttest.s = sqrt((var(x2.norm)*(x2.n-1) + var(y2.norm)*(y2.m-1))/((x2.n-1) + (y2.m-1)))
ttest.stat = (mean(x2.norm) - mean(y2.norm))/(ttest.s * sqrt(1/x2.n + 1/y2.m))

ttest.cinterval = c(qt(1-alpha/2, x2.n + y2.m - 2), Inf)
print(check_H(abs(ttest.stat), ttest.cinterval, 1))
print(t.test(x2.norm, y2.norm, var.equal = TRUE, alternative = "two.sided"))

# Критерий Вилкоксона H_0: x.a = y.a H_1: x.a != y.a ------------
x3.n = 9
y3.m = 7
x3.a = 2
y3.a = 2.1
x3.sigma = y3.sigma = 1.5
x3.norm = rnorm(x3.n, x3.a, x3.sigma)
y3.norm = rnorm(y3.m, y3.a, y3.sigma)
print(wilcox.test(x3.norm, y3.norm, alternative = "two.sided"))      


