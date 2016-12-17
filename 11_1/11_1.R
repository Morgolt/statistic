# Лабораторная №11_1 ------------------------------------------------------

# выборки -----------------------------------------------------------------
n = 40
eps.sigma = 2
eps.a = 0
eps = rnorm(n, eps.a, eps.sigma)
a1 = 1
a2 = 2
a3 = -1
b1.1 = 1.5
b2.1 = 2
b1.2 = 3
b2.2 = 0.75
b1.3 = -2.1
b2.3 = -1
t = c(1:n)
y1 = a1 + b1.1*t + b2.1*cos(2*pi*t/5) + eps
y2 = a2 + b1.2*t + b2.2*cos(2*pi*t/4) + eps
y3 = a3 + b1.3*t + b2.3*cos(2*pi*t/3) + eps
y3[21] = -500 + eps[21]
y3[30] = 500 + eps[30]
# сглаживание методом скользящей средней  ---------------------------------
simple_mean_relaxation = function(tseries, g) {
  p = floor(g / 2)
  t = c((p+1):(length(tseries) - p))
  haty = c(tseries)
  
  if ((g %% 2) == 1) {
    for (i in t) {
      haty[i] = sum(tseries[(i-p):(i+p)])/g
    } 
  } else {
    p = g / 2
    for (i in t) {
      haty[i] = sum(c(0.5*tseries[i-p], tseries[(i-p+1):(i+p-1)], 0.5*tseries[i+p]))/g
    }
  }
  return(haty)
}
# y1 --------------
y1.g3 = simple_mean_relaxation(y1, 3)
y1.g4 = simple_mean_relaxation(y1, 4)
y1.g5 = simple_mean_relaxation(y1, 5)
ts.plot(ts(y1), ts(y1.g3), ts(y1.g4), ts(y1.g5), 
        main = expression("Ряд" ~ y[1] ~ "и его сглаживание."), 
        col = c('black', 'red', 'green', 'blue'))
legend("topleft", c("Исходные зн.", "g = 3", "g = 4", "g = 5"), 
       col = c('black', 'red', 'green', 'blue'), 
       bty = "n", cex=1, lty = c(1,1,1,1), 
       y.intersp = 0.4, x.intersp = 0.5)
# y2 -----------
y2.g3 = simple_mean_relaxation(y2, 3)
y2.g4 = simple_mean_relaxation(y2, 4)
y2.g5 = simple_mean_relaxation(y2, 5)
ts.plot(ts(y2), ts(y2.g3), ts(y2.g4), ts(y2.g5), 
        main = expression("Ряд" ~ y[2] ~ "и его сглаживание."), 
        col = c('black', 'red', 'green', 'blue'))
legend("topleft", c("Исходные зн.", "g = 3", "g = 4", "g = 5"), 
       col = c('black', 'red', 'green', 'blue'), 
       bty = "n", cex=1, lty = c(1,1,1,1), 
       y.intersp = 0.4, x.intersp = 0.5)
# y3 ----------
y3.g3 = simple_mean_relaxation(y3, 3)
y3.g4 = simple_mean_relaxation(y3, 4)
y3.g5 = simple_mean_relaxation(y3, 5)
ts.plot(ts(y3), ts(y3.g3), ts(y3.g4), ts(y3.g5), 
        main = expression("Ряд" ~ y[3] ~ "и его сглаживание."), 
        col = c('black', 'red', 'green', 'blue'))
legend("topleft", c("Исходные зн.", "g = 3", "g = 4", "g = 5"), 
       col = c('black', 'red', 'green', 'blue'), 
       bty = "n", cex=1, lty = c(1,1,1,1), 
       y.intersp = 0.4, x.intersp = 0.5)
# остатки -----------------------------------------------------------------
# y1 -------
y1.e.3 = y1 - y1.g3
y1.e.4 = y1 - y1.g4
y1.e.5 = y1 - y1.g5
plot(y1.e.3 ~ t, main = expression("Диаграмма остатков" ~ y[1] ~ "для различного значения параметра g"), col = "red")
points(y1.e.4 ~ t, col = "green")
points(y1.e.5 ~ t, col = "blue")
legend("topleft", c("g = 3", "g = 4", "g = 5"), col = c('red', 'green', 'blue'), 
       lty = c(1,1,1), bty = "n", pch = 1, cex=1, 
       y.intersp = 0.5, x.intersp = 0.5)
# y2 -------
y2.e.3 = y2 - y2.g3
y2.e.4 = y2 - y2.g4
y2.e.5 = y2 - y2.g5
plot(y2.e.3 ~ t, main = expression("Диаграмма остатков" ~ y[2] ~ " для различного значения параметра g"), col = "red")
points(y2.e.4 ~ t, col = "green")
points(y2.e.5 ~ t, col = "blue")
legend("topleft", c("g = 3", "g = 4", "g = 5"), col = c('red', 'green', 'blue'), 
       lty = c(1,1,1), bty = "n", pch = 1, cex=1, 
       y.intersp = 0.5, x.intersp = 0.5)
# y3 -------
y3.e.3 = y3 - y3.g3
y3.e.4 = y3 - y3.g4
y3.e.5 = y3 - y3.g5
plot(y3.e.3 ~ t, main = expression("Диаграмма остатков" ~ y[3] ~ " для различного значения параметра g"), col = "red")
points(y3.e.4 ~ t, col = "green")
points(y3.e.5 ~ t, col = "blue")
legend("topleft", c("g = 3", "g = 4", "g = 5"), col = c('red', 'green', 'blue'), 
       lty = c(1,1,1), bty = "n", pch = 1, cex=1, 
       y.intersp = 0.5, x.intersp = 0.5)
