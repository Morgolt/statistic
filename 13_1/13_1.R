
# Лабораторная 13_1 -------------------------------------------------------

# данные ---------------
tgoods = matrix(c(72, 66.5, 54, 67, 44, 41, 34.5, 34.5, 24, 
                 50, 48, 57, 60, 57, 52, 50, 46, 54,
                 8, 15, 14, 15, 14, 18, 4, 8.5, 3,
                 0.5, 1, 1, 0.9, 0.3, 1.9, 0.5, 1, 1.2), ncol = 4, byrow = FALSE)
colnames(tgoods) = c('x1', 'x2', 'x3', 'x4')
rownames(tgoods) = c('Рис', 'Чай', 'Сахар', 'Мука', 'Кофе', 'Картофель', 'Масло', 'Сыр', 'Говядина')
tgoods = as.table(tgoods)

mgoods = matrix(c(57, 100, 100, 96.5, 79, 78, 48, 155, 84, 105,
                  57, 54, 32, 65, 51, 53, 40, 44, 64, 35,
                  12.5, 17, 16.5, 20.5, 18, 18, 21, 20.5, 13, 17,
                  0.9, 0.5, 0.7, 0.9, 0.9, 1.2, 1.6, 1.4, 0.8, 1.8), ncol = 4, byrow = FALSE)
colnames(mgoods) = c('x1', 'x2', 'x3', 'x4')
rownames(mgoods) = c('Бензин', 'Свинец', 'Чугун', 'Медь', 'Цинк', 'Олово', 'Каучук', 'Ртуть', 'Медный лист', 'Железо')
mgoods = as.table(mgoods)
n1 = length(tgoods[, 1])
n2 = length(mgoods[, 1])
# линейный дискриминантный анализ -----------------------------------------
tgoods.X = colMeans(tgoods)
mgoods.X = colMeans(mgoods)
tgoods.Xw = tgoods - matrix(c(rep(tgoods.X, n1)), ncol = length(tgoods[1, ]), byrow = TRUE)
mgoods.Xw = mgoods - matrix(c(rep(tgoods.X, n2)), ncol = length(mgoods[1, ]), byrow = TRUE)
S = (1/(n1 + n2 - 2))*(t(tgoods.Xw) %*% tgoods.Xw + t(mgoods.Xw) %*% mgoods.Xw)
c = -61.25086
beta = c * solve(S) %*% (tgoods.X - mgoods.X)
Yc = 0.5 * t(beta) %*% (tgoods.X + mgoods.X)
# классификация -----------------------------------------------------------
cl1 = rowSums(sweep(tgoods, MARGIN = 2, beta, '*'))
cl2 = rowSums(sweep(mgoods, MARGIN = 2, beta, '*'))

