# Лабораторная №8_1 -------------------------------------------------------


# выборки -----------------------------------------------------------------
n = 30
eps.sigma = 2
eps.a = 0
eps = rnorm(n, eps.a, eps.sigma)
alpha = 0.2
beta1 = 0.3
beta2 = 0.4
x1.a = 2
x1.sigma = sqrt(6)
x1 = sort(rnorm(n, x1.a, x1.sigma))
x2.a = 1
x2.sigma = sqrt(8)
x2 = sort(rnorm(n, x2.a, x2.sigma))
y = alpha + beta1*x1 + beta2*x2 + eps

# парные линейные регрессии -----------------------------------------------
# первая: y = a + b*x1 --------------------
fit1 = lm(y~x1)
# коэффициенты уравнения регрессии
fit1.a = summary(fit1)$coefficients[1]
fit1.b = summary(fit1)$coefficients[2]
# прямая линейной регрессии
plot(x1,y)
abline(a = fit1.a, b = fit1.b)
title(main = "Прямая линейной регрессии 1")
# коэффициент корреляции
fit1.cor = cor(x1, y)
# графики остатков
fit1.y.i = fit1.a + x1*fit1.b
fit1.e.i = y - fit1.y.i
plot(x1, fit1.e.i, ylab = "Остаток")
title(main = "График остатков по 
      отношению к фактору 1")
plot(c(1:n), fit1.e.i, ylab = "Остаток", xlab = "n")
title(main = "График остатков по отношению
      к номеру наблюдения 1")
# вторая: y = a + b*x2 --------------
fit2 = lm(y~x2)
# коэффициенты уравнения регрессии
fit2.a = summary(fit2)$coefficients[1]
fit2.b = summary(fit2)$coefficients[2]
# прямая линейной регрессии
plot(x2,y)
abline(a = fit2.a, b = fit2.b)
title(main = "Прямая линейной регрессии 2")
# коэффициент корреляции
fit2.cor = cor(x2, y)
# графики остатков
fit2.y.i = fit2.a + x2*fit2.b
fit2.e.i = y - fit2.y.i
plot(x2, fit2.e.i, ylab = "Остаток")
title(main = "График остатков по 
      отношению к фактору 2")
plot(c(1:n), fit2.e.i, ylab = "Остаток", xlab = "n")
title(main = "График остатков по отношению
      к номеру наблюдения 2")

# проверка на мультиколлинеарность -------------
mtcol.X = matrix(c(rep(1,n), x1, x2),nrow = n, ncol = 3)
mtcol.XT = t(mtcol.X)
mtcol.XTX = mtcol.XT %*% mtcol.X
mtcol.detXTX = det(mtcol.XTX)

# множественная регрессия ----------
mfit.coef = solve(mtcol.XTX) %*% mtcol.XT %*% y
mfit.y.i = mfit.coef[1] + mfit.coef[2]*x1 + mfit.coef[3]*x2
# остатки
mfit.e.i = y - mfit.y.i
plot(c(1:n), mfit.e.i, ylab = "Остаток", xlab = "n")
title(main = "График остатков по отношению
      к номеру наблюдения")
# средняя ошибка аппроксимации и коэффициент детерминации
k = 3
alpha = 0.05
mfit.A = sum(abs((y - mfit.y.i)/y))/n
mfit.S2 = sum((y - mfit.y.i)^2)/(n-k-1)
mfit.R2 = 1 - sum(mfit.e.i^2)/((n-1)*var(y))
# статистика Фишера
mfit.F = (mfit.R2 / (k - 1))/((1 - mfit.R2)/(n-k))
mfit.F.quant = qf(alpha, k - 1, n - k)
# частные коэффициенты корреляции
ryx1x2 = (cor(y, x1) - cor(y, x2)*cor(x1, x2))/sqrt((1 - cor(y, x2)^2)*(1 - cor(x1, x2)^2))
ryx2x1 = (cor(y, x2) - cor(y, x1)*cor(x2, x1))/sqrt((1 - cor(y, x1)^2)*(1 - cor(x2, x1)^2))
# стандартные ошибки коэффициентов
mfit.ma = sqrt(mfit.S2) * sqrt(solve(mtcol.XTX)[1, 1])
mfit.mb1 = sqrt(mfit.S2) * sqrt(solve(mtcol.XTX)[2, 2])
mfit.mb2 = sqrt(mfit.S2) * sqrt(solve(mtcol.XTX)[3, 3])
mfit.tstat = qt(alpha, n-k-1)
# точечный прогноз
single.x1 = mean(x1)*3
single.x2 = mean(x2)*3
single.y = mfit.coef[1] + mfit.coef[2]*single.x1 + mfit.coef[3]*single.x2
single.my = sqrt(mfit.S2)*sqrt(1 + 1/n + ((((single.x1 - mean(x1))^2)/(var(x1))) + ((single.x2 - mean(x2))^2)/(var(x2)))/(n-1))
single.interval = c(single.y - single.my*qt(1-alpha/2, n - k - 1), single.y + single.my*qt(1-alpha/2, n - k - 1))
# встроенная функция
mfit.reg = lm(y~x1+x2)
print(summary(mfit.reg))
