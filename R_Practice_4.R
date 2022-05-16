
#---------------------------Part1)-------------------------------
# a)
#   -Solve with formula:
#     -PMF
n = 5; p = 0.4; time = c(0:n)
choose(n, time)*(p^time) * (1-p)^(n - time)

#     -CDF
cumsum(choose(n, time)*(p^time) * (1-p)^(n - time))

#   -Solve with R function:
#     -PMF
height_p = dbinom(0:n, n, p)
height_p

#     -CDF
height_c = pbinom(0:n, n, p)
height_c

#   -Plot:
plot(0:n, height_p, main = "PMF",
     xlab = "Number of perfect scores",
     ylab = "Probability",
     pch = 16,
     type = "h"
)
abline(0, 0)
points(0:n, height_p, pch = 19)

plot(stepfun(0:n, c(0, height_c)), main = "CDF",
     xlab = "Maximum number of perfect scores",
     ylab = "Probability",
     verticals = F,
     pch = 16
)
abline(0, 0)

# b)
#   -Solve with formula:
time = 2
choose(n, time)*(p^time) * (1-p)^(n - time)

#   -Solve with R function:
dbinom(2, n, p)

# c)
#   -Solve with formula:
time = c(2:n)
sum(choose(n, time)*(p^time) * (1-p)^(n - time))

#   -Solve with R function:
pbinom(1, n, p, lower.tail = F)

# d)
barplot(table(rbinom(1000, 5, 0.4)),
        main = "1000 Student simulation distribution",
        xlab = "Scores",
        ylab = "Number of students")
#----------------------------------------------------------------


#----------------------------Part2)------------------------------
# a)
#   -Solve with formula:
p = 0.6; r = 3; x = c(0:10)
pmf = choose(r+x-1,r-1)*(p^r)*(1-p)^x
pmf
cdf = cumsum(pmf)
cdf
#   -Solve with R function:
pmf = dnbinom(x, r, p)
pmf
cdf = pnbinom(x, r, p)
cdf

#   -plot
plot(x, pmf, main = "PMF",
     xlab = "Failure times before success",
     ylab = "PMF",
     pch = 16,
     type = "h"
)
points(x, pmf, pch = 19)
abline(0, 0)

plot(stepfun(x, c(0, cdf)), main = "CDF",
     xlab = "Maximum failure times before success",
     ylab = "CDF",
     verticals = F,
     pch = 16,
)
abline(0, 0)

# b)
#   -Solve with formula:
x = 4
choose(r+x-1,r-1)*(p^r)*(1-p)^x

#   -Solve with R function
dnbinom(x, r, p)

# c)
#   -Solve with formula:
x = c(0:4)
sum(choose(r+x-1,r-1)*(p^r)*(1-p)^x)

#   -Solve with R function
pnbinom(4, r, p)

# d)
barplot(table(rnbinom(1000, 10, 0.6)),
        main = "1000 Student simulation distribution",
        xlab = "Failure times before success",
        ylab = "Number of students")
#----------------------------------------------------------------


#---------------------------Part3)-------------------------------
# a)
K = 20; M = 60; N = 40; x = c(0:K)

#   -Solve with formula:
pmf = choose(M, x)*choose(N, K-x)/choose(M+N, K)
pmf
cdf = cumsum(pmf)
cdf

#   -Solve with R function:
pmf = dhyper(x, M, N, K)
pmf
cdf = phyper(x, M, N, K)
cdf

#   -Plot:
plot(x, pmf, main = "PMF",
     xlab = "Number of multiple choice questions",
     ylab = "PMF",
     pch = 16,
     type = "h"
)
abline(0, 0)
points(x, pmf, pch = 19)

plot(stepfun(x, c(0, cdf)), main = "CDF",
     xlab = "Maximum number of multiple choice questions",
     ylab = "CDF",
     verticals = F,
     pch = 16,
)
abline(0, 0)
# b)
x = 10
#   -Solve with formula:
choose(M, x)*choose(N, K-x)/choose(M+N, K)

#   -Solve with R function:
dhyper(x, M, N, K)

# c)
#   -Solve with formula:
x = c(10:K)
sum(choose(M, x)*choose(N, K-x)/choose(M+N, K))

#   -Solve with R function:
phyper(9, M, N, K, lower.tail = F)

# d)
barplot(table(rhyper(1000, M, N, K)),
        main = "1000 Student simulation distribution",
        xlab = "Number of multiple choice questions",
        ylab = "Number of students")
#----------------------------------------------------------------


#---------------------------Part4)-------------------------------
# a)
# Since mean is 10 given by the question, λ = 10
lambda = 10; x = 8

#   -Solve with formula:
(exp(-lambda))*(lambda^x)/factorial(x)

#   -Solve with R function:
dpois(x, lambda)

# b)
x = c(0:8)

#   -Solve with formula:
sum((exp(-lambda))*(lambda^x)/factorial(x))

#   -Solve with R function:
ppois(8,lambda)

# c)
#   -Solve with formula:
x = c(6:12)
sum((exp(-lambda))*(lambda^x)/factorial(x)) 

#   -Solve with R function:
ppois(12, lambda) - ppois(5, lambda)

# d)
#   -Solve with formula:
x = c(0:20)
pmf = (exp(-lambda))*(lambda^x)/factorial(x)
pmf

#   -Solve with R function:
pmf = dpois(x, lambda)
pmf

#   -Plot:
plot(x, pmf, main = "PMF",
     xlab = "Number of questions per day",
     ylab = "PMF",
     pch = 16,
     type = "h",
)
abline(0, 0)
points(x, pmf, pch = 19)

# e)
experiment = rpois(50, lambda = 10)
barplot(table(experiment),
        main = "50 days simulation",
        xlab = "Number of questions per day",
        ylab = "Number of days")

boxplot(experiment)

# Inference:
# From the bar plot, the center of the distribution can is around 10. It's
# slightly right-skewed. The sample has a very high variance due to the size 
# of the sample. For higher sample size, 500 for example, the shape will be a
# much smoother curve. 
# From the boxplot, there are several outliers above and below. Theoretically, 
# there should be higher possibility to get some outliers above than below the
# box since we cannot go below 0, which means the each individual has a lower 
# bound with no upper bound. That also explains the skewness. 
#----------------------------------------------------------------


#---------------------------Part5)-------------------------------
# a)
sigma = 10; mu =100
x <- seq(mu - 3*sigma, mu + 3*sigma)
pdf <- dnorm(x, mu, sigma)
plot(x, pdf, type = "l", col = "blue", xlim = c(70, 130), 
     xlab = "Money spent")

# b)
#   -Solve with R function:
pnorm(120, mu, sigma, lower.tail = F)

# c)
#   -Solve with R function:
pnorm(90, mu, sigma) - pnorm(80, mu, sigma)

# d)
# within one sigma:
pnorm(mu+sigma, mu, sigma) - pnorm(mu-sigma, mu, sigma) 

# within two sigma:
pnorm(mu+2*sigma, mu, sigma) - pnorm(mu-2*sigma, mu, sigma) 

# within three sigma:
pnorm(mu+3*sigma, mu, sigma) - pnorm(mu-3*sigma, mu, sigma) 

# e)
qnorm(0.9, mu, sigma)
qnorm(0.1, mu, sigma)
# within 87.18448 and 112.8155, 80% will fall into this area. 

# f)
qnorm(0.98, mu, sigma)
# the minimum amount is 120.5375 to get the T-shirt.

# g)
hist(rnorm(10000, mu, sigma),
     main = "10,000 visitors distribution simulation",
     xlab = "money spent",
     ylab = "number of people")
#----------------------------------------------------------------

library(sampling)
cardata <- read.csv('http://people.bu.edu/kalathur/datasets/cardata.csv')
s <- srswor(15, nrow(cardata))
sample1 = cardata[(1:nrow(cardata))[s!=0], ]
sample1
table(sample1$Make)

pik <- inclusionprobabilities(cardata$mpg, 15)
s <- UPsystematic(pik)
sample2 = cardata[s != 0, ]
sample2
table(sample2$Make)


st.1 <-  strata(cardata, 
                stratanames = c('cyl'), 
                size = rep(15/length(table(cardata$cyl)), 
                           length(table(cardata$cyl))), 
                method = 'srswor', 
                description = T)
sample3 <- getdata(cardata, st.1)
sample3
table(sample3$Make)

mean(sample1$mpg)
mean(sample2$mpg)
mean(sample3$mpg)



for (s in ) {
        mean(s$mpg)
}





library(stringr)



s <- 's2789$'
str_sub(s, 
        seq(1, str_length(s), 3), 
        seq(3, str_length(s), 3))


s <- 'cefjlpqrz156'
str_sub(s, 
        seq(1, str_length(s), 3), 
        seq(3, str_length(s), 3))


data("words")
sum(str_starts(words, 'a'))



str_length(words[str_starts(words, 'a')]) == max(str_length(words[str_starts(words, 'a')]))

words[str_starts(words, 'a')][str_length(words[str_starts(words, 'a')]) == max(str_length(words[str_starts(words, 'a')]))]

