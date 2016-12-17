
# Лабораторная 14_1 -------------------------------------------------------

# данные ---------
inds = matrix(c(80, 82, 84, 88, 87, 87, 90, 95, 91, 91, 96,
                76, 75, 78, 81, 81, 81, 85, 90, 87, 91, 94,
                67.7, 70.6, 74.8, 74.8, 76.1, 76.0, 78.2, 82.0, 84.4, 88.6, 93.1), ncol = 3, byrow = FALSE)
rownames(inds) = c(1900, 1901, 1902, 1903, 1904, 1905, 1906, 1907, 1908, 1909, 1910)
colnames(inds) = c('Индекс ФРБ', 'Индекс Хансена', 'Индекс Берджеса')
inds = as.table(inds)

# метод главных компонент -------------------------------------------------
# S
S = cov(inds)
S.tr = sum(diag(S))
S.eigs = eigen(S)
S.fc = S.eigs$vectors[, 1]
S.fc.imp = S.eigs$values[1]/S.tr
# R
R = cor(inds)
R.tr = sum(diag(R))
R.eigs = eigen(R)
R.fc = R.eigs$vectors[, 1]
R.fc.imp = R.eigs$values[1]/R.tr

# метод главных компонент для quakes dataset ------------------------------
data = quakes
# S
data.S = cov(data)
data.S.tr = sum(diag(data.S))
data.S.eigs = eigen(data.S)
data.S.fc = data.S.eigs$vectors[, 1]
data.S.fc.imp = data.S.eigs$values[1]/data.S.tr
# R
data.R = cor(data)
data.R.tr = sum(diag(data.R))
data.R.eigs = eigen(data.R)
data.R.fc = data.R.eigs$vectors[, 1]
data.R.fc.imp = data.R.eigs$values[1]/data.R.tr
