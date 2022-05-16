library(sampling)

boston <- read.csv(
  "https://people.bu.edu/kalathur/datasets/bostonCityEarnings.csv",
  colClasses = c("character", "character", "character", "integer", "character"))
head(boston)
attach(boston)

# Part1)
# a)
set.seed(9150)
max_1 = 400000
min_1 = 40000
step_1 = 20000
h <- hist(Earnings,
     xlim = c(min_1, max_1),
     breaks = (max_1 - min_1)/step_1,
     xaxt = "n",
     xlab = "Earning",
     main = "Employee earning distribution"
     )
axis(1, seq(min_1, max_1, step_1), labels = seq(min_1, max_1, step_1))
mean(Earnings)
sd(Earnings)

# b)
xbar = 9150
sample.size = 10
for (i in 1:5000) {
  xbar[i] = mean(sample(Earnings, sample.size))
}
mean(xbar)
sd(xbar)
hist(xbar,
     main = "10 Sample test",
     xlab = "Earning",
     xlim = c(50000,200000))

# c)
xbar = 9150
sample.size = 40
for (i in 1:5000) {
  xbar[i] = mean(sample(Earnings, sample.size))
}
mean(xbar)
sd(xbar)
hist(xbar,
     main = "40 Sample test",
     xlab = "Earning",
     xlim = c(50000,200000))

# Part2)
# a)
random_data <- rnbinom(5000, 3, 0.5)
barplot(table(random_data),
        main = "Negative binomial distribution",
        xlab = paste("Mean: ", 
                     round(mean(random_data), 2), 
                     " SD: ", 
                     round(sd(random_data), 2)))

# b)
par(mfrow = c(2, 2))
result_mean = c()
result_sd = c()
for (sample.size in c(10, 20, 30, 40)) {
  xbar = 9156
  for (i in 1:1000) {
    xbar[i] = mean(sample(random_data, sample.size, replace = F))
  }
  hist(xbar, 
       main = paste("Sample size: ", 
                    sample.size),
       xlab = paste("Mean: ", 
                    round(mean(xbar), 2), 
                    " SD: ", 
                    round(sd(xbar), 2)))
}

# Part3)
top_department_list <- as.data.frame(sort(table(boston$Department), 
                                     decreasing = T)[1:5])
top_department <- boston[boston$Department %in% top_department_list$Var1,]
# a)
sample.size = 50
set.seed(9150)
sample_1 <- top_department[srswor(sample.size, nrow(top_department)) != 0,]
head(sample_1)
freq <-table(sample_1$Department)
freq
prop.table(freq)*100

# b)
sys_smpl = function(N,n){
  k = ceiling(N/n)
  r = sample(k, 1)
  return: seq(r, k, n)
}
sample_2 <- top_department[sys_smpl(nrow(top_department), sample.size),]
head(sample_2)
freq <-table(sample_2$Department)
freq
prop.table(freq)*100

# c)
prob <- inclusionprobabilities(top_department$Earnings, sample.size)
sum(prob)
s <- UPsystematic(prob)
sample_3 <- top_department[s == 1,]
head(sample_3)
freq <-table(sample_3$Department)
freq
prop.table(freq)*100

# d)
ordered_top_list <-top_department[order(top_department$Department),]
f <- table(ordered_top_list$Department)
f
st_sizes <- sample.size * f/sum(f)
st_sizes
sample_4 <- strata(ordered_top_list, 
       stratanames = c('Department'), 
       size = st_sizes, 
       method = 'srswor',
       description = T)
head(sample_4)
nrow(sample_4)
freq <-table(sample_4$Department)
freq
prop.table(freq)*100

# e)
# Sample 1-4 mean values
mean(sample_1$Earnings)
mean(sample_2$Earnings)
mean(sample_3$Earnings)
mean(top_department[sample_4$ID_unit,]$Earnings)

# total subset 
mean(top_department$Earnings)







