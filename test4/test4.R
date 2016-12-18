# Тест 4

# данные -----------
y = c(30.45965, 42.90348, 54.09639, 41.55471, 36.34753, 51.0597, 54.09462, 42.92873,
      39.83253, 50.46995, 59.97469, 44.38435, 39.05963, 54.27356, 63.48974, 51.3831, 
      52.61811, 70.77393, 84.49228, 73.65176, 72.61951, 90.46464, 105.944, 93.71207)
y.ts = ts(y, start = c(1998, 1), deltat = 0.25)
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
# сезонная составляющая ---------
y.S = decompose(y.ts)$seasonal
# тренд ----------
darb.wats = function(e) return(sum(diff(e)^2)/sum(e^2))
y.uv = seq(from = 2000, to = 2003.5, by = 0.25)
findt0.step <- function(ct) {
  ct.ind = match(ct, time(y.ts))
  t2 = c(rep(0, ct.ind - 1), time(y.ts)[ct.ind:length(y.ts)] - ct)
  fit = lm(y.ts ~ time(y.ts) + t2)
  emp = ts(predict(fit) + y.S, start = c(1998, 1), deltat = 0.25)
  e.i = y.ts - emp
  return(abs(darb.wats(e.i) - 2))
}
findt0 <- function(uv) {
  rv = uv[1]
  min.val = findt0.step(rv)
  for (ct in uv[2:length(uv)]) {
    cur.val = findt0.step(ct)
    if (cur.val < min.val) {
      rv = ct
      min.val = cur.val
    }
  }
  return(rv)
}
y.t0 = findt0(y.uv)
y.t0.ind = match(y.t0, time(y.ts))
t2 = c(rep(0, y.t0.ind - 1), time(y.ts)[y.t0.ind:length(y.ts)] - y.t0)
t = time(y.ts)
y.tr = lm(y.ts ~ t + t2)
# график тренда-------------
y.tr.i = y.tr$coefficients[1] + y.tr$coefficients[2] * t + y.tr$coefficients[3] * t2
plot.ts(y.ts, main = 'Временной ряд', ylab = 'Продажи')
lines(y.tr.i, col = 'red')
# остатки --------------
haty = ts(predict(y.tr) + y.S, start = c(1998, 1), deltat = 0.25, names = c('Продажа'))
y.e = y - haty
rand.median.criter(y.e)
rand.updown.criteria(y.e)
# график эмпирической зависимости --------
ts.plot(haty, y.ts, col = c('red', 'black'), main = 'Эмп. зависимость', ylab = 'Продажа')
lines(y.tr.i)
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
y.2004.tr = predict(y.tr, newdata = data.frame(t = tail(t, 4) + 1, t2 = tail(t2, 4) + 1))
haty.2004 = y.2004.tr + y.S[1:length(y.2004.tr)]
