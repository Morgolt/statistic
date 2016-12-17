
# Лабораторная №10_1 ------------------------------------------------------

# выборки --------------
n = 30
eps.sigma = 2
eps.a = 0
eps = rnorm(n, eps.a, eps.sigma)
a = 1
b1 = 1.5
b2 = 6
b3 = 8
t = c(1:30)
f1 = a + b1*t
f2 = b2*sin(2*pi*t/5) + b3*cos(2*pi*t/5)
y = a + b1*t + b2*sin(2*pi*t/5) + b3*cos(2*pi*t/5) + eps
# период функции f2 ----------------
f2.T = 5
# линейная регрессия ----------------
tr = lm(y~t)
# разности ----------------
y.diff = y - predict(tr)
plot(y.diff ~ t, main = "График разностей", ylab = "Разности")
# оценка периодической составляющей ---------------------------------------
m = length(t)/f2.T
t.m = t[1:f2.T]
j = c(0:(m-1))
y.S2 = c(t.m)
for (i in t.m) {
  y.S2[i] = sum(y[t.m[i] + j*f2.T] - y.diff[t.m[i] + j*f2.T])/m
}
y.S2 = rep(y.S2, m)
# график тренда и f1 ------------------------------------------------------
plot(predict(tr)~t, main = "Сравнение оценки тренда и f1", ylab = "y")
points(f1~t, col='red')
# график периодической составляющей и f2 --------------------
plot(y.S2 ~ t, main = "Сравнение оценки сезонной
     составляющей и f2", ylab = "y")
points(y.S2~t, col="red")
# график общей функции и её оценки ------------------
haty = predict(tr) + y.S2
plot(y ~ t, main="Основная функция и её приближение", ylim = c(0, 100))
points(haty ~ t, col="red")
# остатки -----------------------------------------------------------------
e.i = y - haty
plot(e.i~t, main = "Диаграмма остатков")
# критерии случайности ----------------------------------------------------
# критерий серий, основанный на медиане выборки ----------------
vary = sort(y)
vary.med = (vary[n/2] + vary[n/2 + 1])/2
vary.d = (vary - vary.med)/abs((vary - vary.med))
vary.d.v = length(rle(vary.d)$values)
vary.d.tau = max(rle(vary.d)$lengths)
if (vary.d.v > floor(0.5 * (n + 1 - 1.96*sqrt(n-1))) && 
    vary.d.tau < floor(3.3*log(n+1))) {
  print("Гипотеза о случайности принимается.")
} else {
  print("Гипотеза о случайности отвергается.")
}
# критерий восходящих/нисходящих серий --------------
vary.d2 = y[2:30] - y[1:29]
vary.d2 = vary.d2 / abs(vary.d2)
vary.d2.v = length(rle(vary.d2)$values)
vary.d2.tau = max(rle(vary.d2)$lengths)
if (vary.d2.v > floor((2*n - 1)/3 - 1.96 * sqrt((16*n - 29)/90)) && 
    vary.d2.tau < 6) {
  print("Гипотеза о случайности принимается.")
} else {
  print("Гипотеза о случайности отвергается.")
}
# критерий Дарбина-Уотсона ----------------------------------------------
dw.d = sum((e.i[2:n] - e.i[1:29])^2)/sum(e.i^2)
dw.dl = 1.28
dw.du = 1.57
if (dw.d < dw.dl) {
  print("Гипотеза о независимости отклоняется.")
} else {
  if (dw.d > dw.du) {
    print("Гипотеза о независимости принимается.")
  } else {
    print("Невозможно принять решение на основании критерия.")
  }
}
# прогноз ----------
t.fut = c(31, 34, 37, 41)
tr.fut = predict(tr, data.frame(t = t.fut))
S2.fut = y.S2[t.fut - 30]
y.fut = tr.fut + S2.fut
plot(y~t, main = 'Прогноз', ylim = c(0, 90), xlim = c(0, 45))
points(haty~t, col='red')
points(y.fut~t.fut, col="green")
legend("topleft", c("Наблюдения", "Оценка", "Прогноз"), col = c('black', 'red', 'green'), bty = "n", pch = 1, cex=1, y.intersp = 0.2, x.intersp = 0.5)
