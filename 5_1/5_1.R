
# Лабораторная № 5_1 ------------------------------------------------------


# выборки ------------------------------------------------------------------
data.x.a = -1
data.x.sigma = 2
data.y.a = 3
data.y.sigma = 1.5
data.n = 200
data.x = rnorm(data.n, data.x.a, data.x.sigma)
data.y = rnorm(data.n, data.y.a, data.y.sigma)
data.x.r = 3
data.y.s = 5

# таблица сопряженности --------------------------------
data.factorx = factor(cut(data.x, breaks=data.x.r))
data.factory = factor(cut(data.y, breaks=data.y.s))
data.contingencytable = table(data.factorx, data.factory)
data.n.i = rowSums(data.contingencytable)
data.m.i = colSums(data.contingencytable)
data.n = sum(data.contingencytable)
# гипотеза H_0: p_ij = P(A cross B) ---------------------
alpha = 0.05
data.mini = matrix(,nrow=data.x.r, ncol = data.y.s)
for (i in c(1:data.x.r)) { 
    data.mini[i,] = data.m.i * data.n.i[i] / data.n
}
test.X2 = sum(((data.contingencytable - data.mini)^2)/data.mini)
test.Y2 = 2 * sum(data.contingencytable * log(data.contingencytable/data.mini))
test.quant = qchisq(alpha, (data.x.r-1)*(data.y.s - 1))
if (test.X2 < test.quant) {
  print("H_0 принимается. X2 < chi^2.")
}
if (test.Y2 < test.quant) {
  print("H_0 принимается. Y2 < chi^2.")
}

# пример данных из R - погибшие на Титанике -------------------------------

tdata = ftable(Titanic, row.vars = c("Class"), col.vars = "Survived")
tdata.n.i = rowSums(tdata)
tdata.m.i = colSums(tdata)
tdata.n = sum(tdata)

# гипотеза H_0: выживаемость на Титанике не зависит от класса -------------
alpha = 0.05
tdata.mini = matrix(,nrow=length(tdata[, 1]), ncol = length(tdata[1, ]))
for (i in c(1:length(tdata[,1]))) { 
  tdata.mini[i,] = tdata.m.i * tdata.n.i[i] / tdata.n
}
titest.X2 = sum(((tdata - tdata.mini)^2)/tdata.mini)
titest.Y2 = 2 * sum(tdata * log(tdata/tdata.mini))
titest.quant = qchisq(alpha, (length(tdata[,1])-1)*(length(tdata[1,]) - 1))
if (titest.X2 < titest.quant) {
  print("H_0 принимается. X2 < chi^2.")
}
if (titest.Y2 < titest.quant) {
  print("H_0 принимается. Y2 < chi^2.")
}

