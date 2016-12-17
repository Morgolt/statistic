
# Лабораторная №12_1 ------------------------------------------------------
# данные ------------------------------------------------------------------
y1 = c(106000, 107500, 75500, 88000, 83000, 52980)
x1 = c(843, 907, 824, 672, 698, 723)
y2 = c(146500, 128500, 179000, 219770, 139000, 131000, 197500, 116000)
x2 = c(1317, 1080, 1688, 1738, 1040, 1404, 1558, 976)
y3 = c(166500, 275000, 164500, 333000, 330000, 302120, 229000, 201800)
x3 = c(1318, 1507, 1292, 2350, 1732, 2010, 1749, 1762)
X = c(x1, x2, x3)
Y = c(y1, y2, y3)
X.means = c(mean(x1), mean(x2), mean(x3))
Y.means = c(mean(y1), mean(y2), mean(y3))
# регрессии --------------------------------
fc = lm(y1~x1)
sc = lm(y2~x2)
tc = lm(y3~x3)
# E, T --------------------
Eyx = sum(var(y1, x1)*(length(y1)-1),var(x2, y2)*(length(y2)-1),var(x3, y3)*(length(y3)-1))
Eyy = sum(var(y1)*(length(y1)-1), var(y2)*(length(y2)-1), var(y3)*(length(y3)-1))
Exx = sum(var(x1)*(length(x1)-1), var(x2)*(length(x2)-1), var(x3)*(length(x3)-1))
Tyx = sum(sum((y1 - mean(Y))*(x1 - mean(X))), sum((y2 - mean(Y))*(x2 - mean(X))), sum((y3 - mean(Y))*(x3 - mean(X))))
Tyy = sum(sum((y1 - mean(Y))^2), sum((y2 - mean(Y))^2), sum((y3 - mean(Y))^2))
Txx = sum(sum((x1 - mean(X))^2), sum((x2 - mean(X))^2), sum((x3 - mean(X))^2))
# оценки ---------
hatb = Eyx/Exx
Y.m.j = c((mean(y1) - hatb*(mean(x1)-mean(X))), (mean(y2) - hatb*(mean(x2)-mean(X))), (mean(y3) - hatb*(mean(x3)-mean(X))))
e.ij = c((y1 - Y.m.j[1]) - hatb*(x1 - mean(X)), (y2 - Y.m.j[2]) - hatb*(x2 - mean(X)), (y3 - Y.m.j[3]) - hatb*(x3 - mean(X)))
S1.s = Eyy - hatb*Eyx
# Гипотеза о равенстве средних ---------
hatmu = mean(Y)
hatb.g = Tyx/Txx
e.ij.s = c(y1 - mean(Y) - hatb.g*(x1 - mean(X)), y2 - mean(Y) - hatb.g*(x2 - mean(X)), y3 - mean(Y) - hatb.g*(x3 - mean(X)))
S0.s = Tyy - hatb.g * Tyx
S.c.s = S0.s - S1.s
F = (S.c.s/2)/(S1.s / (length(Y) - 4))
F.quant = qf(0.95, 2, length(Y) - 4)
hatsigma2 = S1.s/(length(Y) - 4)
# линейные контрасты ------------------------------------------------------
Lk1.c = c(1, 0, 0)
Lk2.c = c(0, 1, 0)
Lk3.c = c(0, 0, 1)
Lk1 = sum(Lk1.c * (Y.m.j - hatb.g*(X.means - mean(X))))
Lk2 = sum(Lk2.c * (Y.m.j - hatb.g*(X.means - mean(X))))
Lk3 = sum(Lk3.c * (Y.m.j - hatb.g*(X.means - mean(X))))
Lk1.S2 = hatsigma2 * sum(Lk1.c^2/c(length(y1), length(y2), length(y3))) + ((sum(Lk1.c*(X.means - mean(X))))^2)/Exx
Lk2.S2 = hatsigma2 * sum(Lk2.c^2/c(length(y1), length(y2), length(y3))) + ((sum(Lk2.c*(X.means - mean(X))))^2)/Exx
Lk3.S2 = hatsigma2 * sum(Lk3.c^2/c(length(y1), length(y2), length(y3))) + ((sum(Lk3.c*(X.means - mean(X))))^2)/Exx
# интервалы ------
Lk1.int = c(Lk1 - sqrt(Lk1.S2*2*qf(1 - 0.05, 2, length(Y) - 3)), Lk1 + sqrt(Lk1.S2*2*qf(1 - 0.05, 2, length(Y) - 3)))
Lk2.int = c(Lk2 - sqrt(Lk2.S2*2*qf(1 - 0.05, 2, length(Y) - 3)), Lk2 + sqrt(Lk1.S2*2*qf(1 - 0.05, 2, length(Y) - 3)))
Lk3.int = c(Lk3 - sqrt(Lk3.S2*2*qf(1 - 0.05, 2, length(Y) - 3)), Lk3 + sqrt(Lk1.S2*2*qf(1 - 0.05, 2, length(Y) - 3)))
