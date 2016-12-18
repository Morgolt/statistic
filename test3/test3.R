# Тест 3
library(tseries)
# данные ---------
y = c(32.72993, 48.14029, 56.33927, 41.87616, 39.37516, 52.02464, 57.6976, 53.15508, 43.60135, 56.67816, 65.29583, 
      52.99953, 53.93471, 61.31481, 71.92019, 63.99426, 55.44915, 70.76152, 76.99954, 64.44463, 62.88851, 78.35295, 
      85.5967, 76.06627)
y.ts = ts(y, start = c(1998, 1), deltat = 0.25, names = c('Продажа'))
t = c(1:length(y))

# тест
# критерии случайности ---------
rand.median.criter <- function(ts) {
  vary = sort(ts)
  n = length(vary)
  vary.med = (vary[n/2] + vary[n/2 + 1])/2
  print("Медиана:")
  print(vary.med)
  vary.d = (vary - vary.med)/abs((vary - vary.med))
  vary.d.v = length(rle(vary.d)$values)
  print('Число серий:')
  print(vary.d.v)
  vary.d.tau = max(rle(vary.d)$lengths)
  print('Протяженность макс. серии:')
  print(vary.d.tau)
  if (vary.d.v > floor(0.5 * (n + 1 - 1.96*sqrt(n-1))) && 
      vary.d.tau < floor(3.3*log(n+1))) {
    print("Гипотеза о случайности принимается.")
  } else {
    print("Гипотеза о случайности отвергается.")
  }
}
rand.updown.criteria <- function(ts) {
  vary = sort(ts)
  n = length(vary)
  tau = 5
  if (n > 26) {
    tau = 6
  }
  if (n > 153) {
    tau = 7
  }
  vary.d2 = y[2:n] - y[1:n-1]
  vary.d2 = vary.d2 / abs(vary.d2)
  vary.d2.v = length(rle(vary.d2)$values)
  print('Число серий:')
  print(vary.d2.v)
  vary.d2.tau = max(rle(vary.d2)$lengths)
  print('Протяженность макс. серии:')
  print(vary.d2.tau)
  if (vary.d2.v > floor((2*n - 1)/3 - 1.96 * sqrt((16*n - 29)/90)) && 
      vary.d2.tau < tau) {
    print("Гипотеза о случайности принимается.")
  } else {
    print("Гипотеза о случайности отвергается.")
  }
}
rand.median.criter(y.ts)
rand.updown.criteria(y.ts)
# тренд ---------
t = time(y.ts)
y.tr = lm(y.ts ~ t)
# график тренда ----------
plot(y.ts, main = 'Временной ряд', ylab = 'Продажи')
abline(a = y.tr$coefficients[1], b = y.tr$coefficients[2])
# сезонная составляющая ---------
y.S = decompose(y.ts)$seasonal
# остатки --------------
haty = ts(predict(y.tr) + y.S, start = c(1998, 1), deltat = 0.25, names = c('Продажа'))
y.e = y - haty
rand.median.criter(y.e)
rand.updown.criteria(y.e)
# график эмпирической зависимости --------
ts.plot(haty, y.ts, col = c('red', 'black'), main = 'Эмп. зависимость', ylab = 'Продажа')
abline(a = y.tr$coefficients[1], b = y.tr$coefficients[2], col = 'blue')
# критерий Дарбина-Уотсона -----------
darb.wats.criteria <- function(e) {
  n = length(e)
  e.d = sum(diff(e)^2)/sum(e^2)
  if (e.d > 2) {
    e.d = 4 - e.d
  }
  return(e.d)
}
y.e.d = darb.wats.criteria(y.e)
y.e.hb = jarque.bera.test(y.e)
print(y.e.hb)
# прогноз ---------
y.2004.tr = predict(y.tr, newdata = data.frame(t = tail(time(y.ts), 4) + 1))
haty.2004 = y.2004.tr + y.S[1:length(y.2004.tr)]
