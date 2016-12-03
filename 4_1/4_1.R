
# Лабораторная 4_1 --------------------------------------------------------

# выборки -----------------------------------------------------------------
data.x = c(10.1, 7.3, 5.6, 6.2, 8.4, 8.1, 8.0, 7.6, 5.3, 7.2)
data.y = c(11.7, 12.2, 11.8, 7.8, 8.9, 9.9, 12.4, 11.0, 10.3, 13.8, 10.5, 9.8, 9.1)
data.z = c(10.2, 12.0, 8.8, 8.7, 10.5, 11.0, 9.1)
data.stack = stack(list("1" = data.x, "2" = data.y, "3" = data.z))
data.m.i = tapply(data.stack$values, data.stack$ind, mean)
data.n.i = tapply(data.stack$values, data.stack$ind, length)
data.v.i = tapply(data.stack$values, data.stack$ind, var)
data.n = sum(data.n.i)
data.k = 3
alpha = 0.05

# Однофакторный дисперсионный анализ  -------------------------------------
# H_0: x.a = y.a = z.a
#test.n = length(data.x) + length(data.y) + length(data.z)
#test.M = sum(sum(data.x, data.y, data.z))/test.n
test.Q1 = sum(data.n.i * (data.m.i - mean(data.stack$values))^2)
test.Q2 = sum((data.n.i - 1) * data.v.i)
test.stat = ((test.Q1/(data.k - 1))/(test.Q2/(data.n - data.k)))
test.quant = qf(alpha, data.k-1, data.n - data.k)
if (test.stat < test.quant) {
  print('H_0 accepted.')
} else {
  print('H_0 declined.')
}
# ANOVA средствами R
oneway.test(data.stack$values ~ data.stack$ind, var.equal = TRUE)

# линейные контрасты ------------------------------------------------------
lincontrast <- function(c, alpha=0.05) {
  #
  lk = sum(c * data.m.i)
  s2 = (test.Q2/(data.n - data.k))*sum((c^2)/data.n.i)
  tmp = sqrt((data.k - 1)*qf(1-alpha, data.k - 1, data.n - data.k))
  cinterval = c(lk - sqrt(s2) * tmp,  lk + sqrt(s2) * tmp)
  print("Interval: ")
  print(cinterval)
  if (cinterval[1] < 0 && cinterval[2] > 0) {
    print('H_0 accepted.')
  } else {
    print('H_0 declined.')
  }
}
# H_0: m_1 = m_2
test2.c = c(1, -1, 0)
lincontrast(test2.c)
# H_0: m_1 = m_3
test3.c = c(1,0,-1)
lincontrast(test3.c)
# H_0: m_2 = m_3
test4.c = c(0, 1, -1)
lincontrast(test4.c)
# H_0: 0.5*(m_1 + m_3) = m_2
test5.c = c(0.5, -1, 0.5)
lincontrast(test5.c)

# пример из интернета №1 --------------------------------------------------
# Студентов 1-го курса опрашивали с целью выявления занятий, 
# которым они посвящают свое свободное время. Проверьте, 
# различаются ли распределение вербальных и невербальных предпочтений студентов.
data2.x = c(12, 18, 23, 10, 15)
data2.y = c(17, 19, 25, 7, 17)
data2.stack = stack(list("П1" = data2.x, "П2" = data2.y))
data2.m.i = tapply(data2.stack$values, data2.stack$ind, mean)
data2.n.i = tapply(data2.stack$values, data2.stack$ind, length)
data2.v.i = tapply(data2.stack$values, data2.stack$ind, var)
data2.n = sum(data2.n.i)
data2.k = 2
data2.m = mean(data2.stack$values)                    
data2.Q1 = sum(data2.n.i * (data2.m.i - mean(data2.stack$values))^2)
data2.Q2 = sum((data2.n.i - 1) * data2.v.i)
data2.stat = ((data2.Q1/(data2.k - 1))/(data2.Q2/(data2.n - data2.k)))
data2.quant = qf(alpha, data2.k-1, data2.n - data2.k)
if (data2.stat < data2.quant) {
  print('H_0 accepted.')
} else {
  print('H_0 declined.')
}
oneway.test(data2.stack$values ~ data2.stack$ind, var.equal = TRUE)

# пример из интернета №2 --------------------------------------------------
# Произведено 13 испытаний, из них – 4 на первом уровне фактора, 4 – на 
# втором, 3 – на третьем и 2 на четвертом. Методом дисперсионного анализа
# при уровне значимости 0,05 проверить нулевую гипотезу о равенстве 
# групповых средних. Предполагается, что выборки извлечены из нормальных
# совокупностей с одинаковыми дисперсиями.
data3.x = c(1.38, 1.38, 1.42, 1.42)
data3.y = c(1.41, 1.42, 1.44, 1.45)
data3.z = c(1.32, 1.33, 1.34)
data3.w = c(1.31, 1.33)
data3.stack = stack(list("П1" = data3.x, "П2" = data3.y, "П3" = data3.z, "П4" = data3.w))
data3.m.i = tapply(data3.stack$values, data3.stack$ind, mean)
data3.n.i = tapply(data3.stack$values, data3.stack$ind, length)
data3.v.i = tapply(data3.stack$values, data3.stack$ind, var)
data3.n = sum(data3.n.i)
data3.k = 4
data3.m = mean(data3.stack$values)                    
data3.Q1 = sum(data3.n.i * (data3.m.i - mean(data3.stack$values))^2)
data3.Q2 = sum((data3.n.i - 1) * data3.v.i)
data3.stat = ((data3.Q1/(data3.k - 1))/(data3.Q2/(data3.n - data3.k)))
data3.quant = qf(alpha, data3.k-1, data3.n - data3.k)
if (data3.stat < data3.quant) {
  print('H_0 accepted.')
} else {
  print('H_0 declined.')
}
oneway.test(data3.stack$values ~ data3.stack$ind, var.equal = TRUE)

# попарные линейные конрасты:
lincontrast3 <- function(c, alpha=0.05) {
  #
  lk = sum(c * data3.m.i)
  s2 = (data3.Q2/(data3.n - data3.k))*sum((c^2)/data3.n.i)
  tmp = sqrt((data3.k - 1)*qf(1-alpha, data3.k - 1, data3.n - data3.k))
  cinterval = c(lk - sqrt(s2) * tmp,  lk + sqrt(s2) * tmp)
  print("Interval: ")
  print(cinterval)
  if (cinterval[1] < 0 && cinterval[2] > 0) {
    print('H_0 accepted.')
  } else {
    print('H_0 declined.')
  }
}
# m_1 = m_2
c = c(1, -1, 0, 0)
lincontrast3(c)
# m_1 = m_3
c = c(1, 0, -1, 0)
lincontrast3(c)
# m_1 = m_4
c = c(1, 0, 0, -1)
lincontrast3(c)
# m_2 = m_3
c = c(0, 1, -1, 0)
lincontrast3(c)
# m_2 = m_4
c = c(0, -1, 0, 1)
lincontrast3(c)
# m_3 = m_4
c = c(0, 0, 1, -1)
lincontrast3(c)