# Тест 1 --------------
library(lmtest)
library(tseries)
# данные ------------------------------------------------------------------
y = c(120, 107, 102, 94, 96, 105, 99, 115, 77, 100, 116, 122, 87, 
      109, 95, 92, 91, 102, 120, 94, 98, 101, 97, 101, 97, 98, 112, 115, 111, 97)
x1 = c(128, 99, 92, 80, 97, 113, 86, 84, 114, 113, 91, 80, 88, 82, 91, 83, 
       84, 113, 84, 98, 114, 111, 108, 85, 91, 91, 80, 102, 113, 91)
x2 = c(79, 49, 82, 63, 99, 64, 73, 86, 60, 56, 118, 104, 50, 111, 78, 62, 75, 83, 116,
       103, 54, 90, 76, 72, 75, 80, 88, 85, 107, 110)
# тест y~x1---------------
# график зависимости x от y
plot(x1, y)
title(main="Диаграмма начальных данных")
# линейная регрессия
fit = lm(y~x1)
print(summary(fit))
fit.a = summary(fit)$coefficients[1]
fit.b = summary(fit)$coefficients[2]
fit.R2 = summary(fit)$r.squared
fit.rxy = cor(y, x1)
print('Коэффициент корреляции:')
print(fit.rxy)
# диаграмма остатков
plot(x1, fit$residuals, ylab = "Остаток")
title(main = "График остатков по отношению к x")
# прямая линейной регрессии
plot(x1, y)
abline(a = fit.a, b = fit.b)
title(main = "Прямая линейной регрессии")
# F-статистика
fit.F = summary(fit)$fstatistic[1]
# гипотеза гомоскедастичности 
fit.gmsk = gqtest(fit)
print(fit.gmsk)
# проверка нормальности остатков
fit.hb = jarque.bera.test(summary(fit)$residuals)
print(fit.hb)
# доверительный интервал для коэффициентов
print(confint(fit))
# прогноз
fit.new = 1.7 * mean(x1)
print("Новое значение для предсказания:")
print(fit.new)
# результат предсказания
print(predict(fit, data.frame(x1 = fit.new), se.fit = TRUE, interval="predict"))
# тест y~x2 -------------------
# график зависимости x от y
plot(x2, y)
title(main="Диаграмма начальных данных")
# линейная регрессия
fit1 = lm(y~x2)
print(summary(fit1))
fit1.a = summary(fit1)$coefficients[1]
fit1.b = summary(fit1)$coefficients[2]
fit1.R2 = summary(fit1)$r.squared
fit1.rxy = cor(y, x2)
print('Коэффициент корреляции:')
print(fit1.rxy)
# диаграмма остатков
plot(x2, fit1$residuals, ylab = "Остаток")
title(main = "График остатков по отношению к x")
# прямая линейной регрессии
plot(x2, y)
abline(a = fit1.a, b = fit1.b)
title(main = "Прямая линейной регрессии")
# F-статистика
fit1.F = summary(fit1)$fstatistic[1]
# гипотеза гомоскедастичности 
fit1.gmsk = gqtest(fit1)
print(fit1.gmsk)
# проверка нормальности остатков
fit1.hb = jarque.bera.test(summary(fit1)$residuals)
print(fit1.hb)
# доверительный интервал для коэффициентов
print(confint(fit1))
# прогноз
fit1.new = 1.7 * mean(x2)
print("Новое значение для предсказания:")
print(fit1.new)
# результат предсказания
print(predict(fit1, data.frame(x2 = fit1.new), se.fit = TRUE, interval="predict"))
