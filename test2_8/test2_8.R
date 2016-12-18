
# Тест 2.8 ----------------------------------------------------------------
library(lmtest)
library(tseries)

# данные ----------
y = c(271.7, 282.1, 269.9, 413.5, 436.5, 314.9, 561.9, 571.8, 427.8, 319.3, 401.4, 228.0, 345.6, 641.2, 527.4, 
      323.5, 425.8, 343.0, 457.4, 421.8, 246.1, 505.3, 379.3, 265.1, 623.6, 457.5, 271.1, 367.0, 551.8, 573.0)
x1 = c(5, 8, 7, 10, 12, 7, 11, 16, 13, 5, 10, 5, 9, 19, 20, 
       10, 12, 7, 14, 14, 2, 16, 9, 4, 18, 18, 4, 8, 11, 13)
x2 = c(4, 6, 2, 5, 8, 4, 7, 9, 4, 2, 6, 5, 4, 8, 6,
       3, 5, 3, 4, 5, 4, 7, 5, 3, 6, 5, 3, 6, 6, 6)

# парные линейные регрессии ---------
# y ~ x1 ---------
# график зависимости x от y
# линейная регрессия
fit = lm(y~x1)
print(summary(fit))
# диаграмма остатков
plot(x1, fit$residuals, ylab = "Остаток")
title(main = "График остатков по отношению к x")
plot(c(1:length(y)), fit$residuals, ylab = "Остаток")
title(main = "График остатков по отношению к n")
# прямая линейной регрессии
plot(x1, y)
lines(x1, predict(fit))
title(main = "Прямая линейной регрессии")
# проверка нормальности остатков
fit.hb = jarque.bera.test(summary(fit)$residuals)
print(fit.hb)
# прогноз
fit.new = 3.5 * mean(x1)
print("Новое значение для предсказания:")
print(fit.new)
# результат предсказания
print(predict(fit, data.frame(x1 = fit.new), se.fit = TRUE, interval="predict"))
# y ~ x2 ------------
# линейная регрессия
fit1 = lm(y~x2)
print(summary(fit1))
# диаграмма остатков
plot(x2, fit1$residuals, ylab = "Остаток")
title(main = "График остатков по отношению к x")
plot(c(1:length(y)), fit1$residuals, ylab = "Остаток")
title(main = "График остатков по отношению к n")
# прямая линейной регрессии
plot(x2, y)
lines(x2, predict(fit1))
title(main = "Прямая линейной регрессии")
# проверка нормальности остатков
fit1.hb = jarque.bera.test(summary(fit1)$residuals)
print(fit1.hb)
# прогноз
fit1.new = 3.5 * mean(x2)
print("Новое значение для предсказания:")
print(fit1.new)
# результат предсказания
print(predict(fit1, data.frame(x2 = fit1.new), se.fit = TRUE, interval="predict"))

# множественная регрессия -----------
mfit = lm(y ~ x1 + x2)
print(summary(mfit))
# диаграммы остатков
plot(x1+x2, mfit$residuals, ylab = "Остаток")
title(main = "График остатков по отношению к x")
plot(c(1:length(y)), mfit$residuals, ylab = "Остаток")
title(main = "График остатков по отношению к n")
# критерий Жарка-Бера
mfit.hb = jarque.bera.test(summary(mfit)$residuals)
print(mfit.hb)
# прогноз
# прогноз
mfit.new = c(3.5 * mean(x1), 3.5 * mean(x2))
print("Новое значение для предсказания:")
print(mfit.new)
# результат предсказания
print(predict(mfit, data.frame(x1 = mfit.new[1], x2 = mfit.new[2]), se.fit = TRUE, interval="predict"))
