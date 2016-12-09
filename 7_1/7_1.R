
# Лабораторная 7_1 --------------------------------------------------------


# выборки -----------------------------------------------------------------

n = 30
eps.sigma = 1
eps.a = 0
eps = rnorm(n, eps.a, eps.sigma)
alpha = 0.2
beta = 0.3
x.a = 2
x.sigma = sqrt(6)
x = sort(rnorm(n, x.a, x.sigma))
sigma.i = c(rep(1, 5), rep(3,5), rep(5,5), rep(8,5), rep(12,5), rep(15,5))
y = alpha + beta*x + sigma.i*eps

# гипотеза о гомоскедастичности ---------------------
# разбиение выборки
h.m = 8
y.first = y[1:((n-h.m)/2)]
y.second = y[(n - (n-h.m)/2 + 1):n]
x.first = x[1:((n-h.m)/2)]
x.second = x[(n - (n-h.m)/2 + 1):n]
# парные регрессии
h.first.reg = lm(y.first ~ x.first)
h.second.reg = lm(y.second ~ x.second)
h.first.a = summary(h.first.reg)$coefficients[1]
h.first.b = summary(h.first.reg)$coefficients[2]
h.second.a = summary(h.second.reg)$coefficients[1]
h.second.b = summary(h.second.reg)$coefficients[2]
h.first.y.i = h.first.a + h.first.b*x.first
h.second.y.i = h.second.a + h.second.b * x.second
# остаточные суммы квадратов
h.first.s2 = sum((h.first.y.i - y.first)^2)
h.second.s2 = sum((h.second.y.i - y.second)^2)
# F-статистика
h.F = h.second.s2/h.first.s2
h.F.quant = qf(0.05, (n-h.m)/2 - 2, (n-h.m)/2 - 2)
if(h.F > h.F.quant) {
  print("Гипотеза гомоскедатичности отклоняется.")
} else {
  print("Гипотеза гомоскедатичности принимается.")
}

# регрессия по всем наблюдениям -------------------------------------------

test.fit = lm(y~x)
test.a = summary(test.fit)$coefficients[1]
test.b = summary(test.fit)$coefficients[2]
