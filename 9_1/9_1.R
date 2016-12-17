
# Лабораторная №9_1 -------------------------------------------------------

# выборки -----------------------------------------------------------------
n = 100
eps.sigma = 2
eps.a = 0
eps = rnorm(n, eps.a, eps.sigma)
a = 1
b1 = 2
b2 = 5
b3 = 0.1
b4 = 0.01
x = runif(n, 0, 2)
y = a*(x^b1)*exp(b2*x + b3*x^2 + b4*x^3)*exp(eps)
y.ln = log(y)
# график начальных данных
default_par = par()
par(mfrow = c(1, 2))
plot(y~x, main = "График начальных данных")
plot(y.ln~x, main = "ln(y)")
# модель линейной регрессии -----------------------------------------------

# матричным способом
mtcol.X = matrix(c(rep(1,n), log(x), x, x^2, x^3),nrow = n, ncol = 5)
mtcol.XT = t(mtcol.X)
mtcol.XTX = mtcol.XT %*% mtcol.X
mtcol.detXTX = det(mtcol.XTX)

mfit.coef = solve(mtcol.XTX) %*% mtcol.XT %*% log(y)
mfit = lm(y.ln ~ I(log(x)) + x + I(x^2) + I(x^3))

# график регрессии ----------
par(default_par)
plot(y.ln~x, ylab = "ln(y)", main = "Регрессионные значения")
points(predict(mfit)~x, col = "red", type='p')

# остатки
mfit.e.i = y.ln - predict(mfit)
plot(mfit.e.i~x, ylab="Остаток", main = "Диаграмма остатков в зависимости
     от значения фактора")
