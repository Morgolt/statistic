## utility functions ####
# check_H - check if statistic is in critical interval. ####
# if type = 1 - one-sided interval
# if type = 2 - two-sided interval
check_H <- function(statistic, interval, type) {
  b <- switch(type,
         cbind(interval[1] < statistic & interval[2] > statistic),
         cbind((interval[1] < statistic & interval[2] > statistic) | (interval[3] < statistic & interval[4] > statistic)))
  return(ifelse(b, "H0 is declined", "H0 is accepted"))
}

## initial parameters ####
a_0 = 5
sigma_0 = 2
alpha = 0.05
beta = 0.1
a_1 = 4.2
sigma_1 = 1.5

## Task 1 ####
## N for Neyman-Pirson criteria
# quntile z_alpha
z_alpha = qnorm(beta, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
z_beta = qnorm(1 - alpha, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
n = trunc(((sigma_0^2)*(z_alpha - z_beta)^2)/((a_0 - a_1)^2))

## samples size of N from normal distribution
sample1 = rnorm(n, mean = a_0, sd = sigma_0)
sample2 = rnorm(n, mean = a_1, sd = sigma_0)
means <- c(mean(sample1), mean(sample2))
print('Task1. Means:')
print(means)

## H0: a = a_0; alternative: H1: a = a_1
# condition for alpha(phi) <= alpha
c1 = a_0 + (sigma_0/sqrt(n))*qnorm(alpha)
# condition for beta(phi) < beta
c2 = a_1 + (sigma_0/sqrt(n))*qnorm(beta)
# critical area S for H0 (S = (-inf; c1)):
S1 <- c(-Inf, c1)
print('Task1. S')
print(S1)
# if mean in S, we decline H0
print(check_H(means, S1, 1))

## Task 2 ####
# samples size of m from normal distribution
m = 100
samples1 = matrix(c(rnorm(m, mean = a_0, sd = sigma_0), rnorm(m, mean = a_0, sd = sigma_1)),
  nrow = 2,
  ncol = m,
  byrow = TRUE)
#rm(sample1, sample2)
sample_disp = rowSums( (samples1 - rowMeans(samples1))^2)
print('Task2. Disp:')
print(sample_disp)
## Neyman-Pirson criteria
## H0: sigma = sigma_0; alternative: H1: sigma = sigma_1
cq0 = qchisq(alpha, m)
print('Task2. c1:')
print(cq0)
cq1 = qchisq(1 - alpha, m)
# critical area for H0: S = (0; sigma_0^2*c0)
S2 <- c(0, sigma_0^2*cq0)
print('Task2. Interval')
print(S2)
print(check_H(sample_disp, S2, 1))

## Task 3 and 4 ####
# two samples from Bernulli distribution
p_0 = 0.3
p_1 = 0.5
n = 500
samples2 <- matrix(c(rbinom(n, 1, p_0), rbinom(n, 1, p_1)),
                  nrow = 2,
                  ncol = n,
                  byrow = TRUE)
m = rowSums(samples2)
## Neyman-Pirson criteria
# H0: p = p_0, alternative: H1: p = p1
c1 = n*p_0 + sqrt(n*p_0*(1-p_0))*qnorm(alpha)
# critical area for H0: S = (c2; Inf)
c0 = n*p_0 + sqrt(n*p_0*(1-p_0))*qnorm(alpha)
c2 = n*p_0 + sqrt(n*p_0*(1-p_0))*qnorm(1 - alpha)
print('Task3-4. c1:')
print(c2)
S3 <- c(c2, Inf)
print('Task3-4. Interval')
print(S3)
print(check_H(m, S3, 1))


# Task 5 ------------------------------------------------------------------
# H0: a = a_0, H1: a != a_0
c0 = a_0 + (sigma_0/sqrt(n))*qnorm(alpha/2)
c1 = a_0 + (sigma_0/sqrt(n))*qnorm(1 - alpha/2)
S4 <- c(-Inf, c0, c1, Inf)
print('Task5. Interval')
print(S4)
print(check_H(means, S4, 2))


# Task 6 ------------------------------------------------------------------
#H0: a = a_0, H1: a != a_0, sigma unknown
t <- c(sqrt(n)*(mean(sample1) - a_0)/sqrt(var(sample1)), sqrt(n)*(mean(sample2) - a_0)/sqrt(var(sample2)))
print('Task6. T-stat:')
print(t)
S5 <- c(-Inf, qt(alpha/2, n-1), qt(1-alpha/2, n-1), Inf)
print('Task6. Interval')
print(S5)
print(check_H(t, S5, 2))


# Task7 -------------------------------------------------------------------
# H0: p = p_0, alternative: H1: p != p1
# H0: p = p_0, alternative: H1: p = p1
c0 = n*p_0 + sqrt(n*p_0*(1-p_0))*qnorm(alpha/2)
c1 = n*p_0 + sqrt(n*p_0*(1-p_0))*qnorm(1 - alpha/2)
S6 <- c(-Inf, c0, c1, Inf)
print('Task7. Interval')
print(S6)
print(check_H(m, S6, 2))
