
# Лабораторная №6_1 -------------------------------------------------------


# выборка -----------------------------------------------------------------
eps.n = 50
eps.sigma = 2
eps.a = 0
eps = rnorm(eps.n, eps.a, eps.sigma)
alpha = 0.3
beta = 0.2
x.a = 0
x.sigma = sqrt(6)
x = rnorm(eps.n, x.a, x.sigma)
# наблюдения ----------
y = alpha + beta*x + eps
# график зависимости x от y----------
plot(x, y)
title(main="Диаграмма начальных данных")
# модель линейной регрессии ----------
test.fit = lm(y ~ x)
print(summary(test.fit))
test.a = summary(test.fit)$coefficients[1]
test.a.se = summary(test.fit)$coefficients[1,2]
test.b = summary(test.fit)$coefficients[2]
test.b.se = summary(test.fit)$coefficients[2,2]
test.cor = cor(x, y)
test.y.i = test.a + x*test.b
test.e.i = y - test.y.i
test.A = (1/eps.n)*sum(abs(y - test.y.i)/abs(y))
# графики остатков в зависимости от x и от n ----------
plot(x, test.e.i, ylab = "Остаток")
title(main = "График остатков по отношению к x")
plot(c(1:eps.n), test.e.i, ylab = "Остаток", xlab = "n")
title(main = "График остатков по отношению к n")
# прямая линейной регрессии ----------
plot(x,y)
abline(a = test.a, b = test.b)
title(main = "Прямая линейной регрессии")
# t-статистика для коэффициента корреляции ----------
test.t.cor = sqrt(eps.n - 2) * test.cor / sqrt(1 - test.cor^2)
# доверительные интервалы для a и b ----------
test.alpha = 0.05
test.a.interval = c(test.a - qt(1 - test.alpha, eps.n - 2 - 1)*test.a.se, test.a + qt(1 - test.alpha, eps.n - 2 - 1)*test.a.se)
test.b.interval = c(test.b - qt(1 - test.alpha, eps.n - 2 - 1)*test.b.se, test.b + qt(1 - test.alpha, eps.n - 2 - 1)*test.b.se)
# точечный прогноз ----------
test.xp = 2*mean(x)
test.yp = test.a + test.b * test.xp
test.my = (sum((y - test.y.i)^2)/eps.n - 2 - 1) * sqrt(1 + 1/eps.n + ((test.xp - mean(x))^2)/((eps.n - 1)*var(x)))
test.yp.interval = c(test.yp - test.my*qt(1-alpha/2, eps.n - 2 - 1), test.yp + test.my*qt(1-alpha/2, eps.n - 2 - 1))
