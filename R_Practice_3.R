library(ggplot2)

# Part 1
df <- read.csv("http://people.bu.edu/kalathur/datasets/myPrimes.csv") 

# (1)
last_digit <- df$LastDigit
barplot(table(last_digit), main='Frequencies of Last digit')

# (2)
first_digit <- df$FirstDigit
barplot(table(first_digit), main='Frequencies of First digit')

# Part 2
us_quarters <- read.csv("http://people.bu.edu/kalathur/datasets/us_quarters.csv", header = T)

# (a)
max_den <- us_quarters[us_quarters$DenverMint == max(us_quarters$DenverMint), "State"]
max_phi <- us_quarters[us_quarters$PhillyMint == max(us_quarters$PhillyMint), "State"]
min_den <- us_quarters[us_quarters$DenverMint == min(us_quarters$DenverMint), "State"]
min_phi <- us_quarters[us_quarters$PhillyMint == min(us_quarters$PhillyMint), "State"]

# (b)
total_dollars <- (sum(us_quarters$DenverMint) + sum(us_quarters$PhillyMint))*0.25

# (c)
options(scipen = 3)
plot_us_quarters <-rbind(us_quarters$DenverMint, us_quarters$PhillyMint)
colnames(plot_us_quarters) <- us_quarters$State
rownames(plot_us_quarters) <- c("Denver Mint", "Philly Mint")
barplot(plot_us_quarters, 
        main = "US Quarters Barplot",
        col = c("blue", "grey"),
        legend.text = T,
        las = 2,
        ylim = c(0, 1000000),
        beside = T)

# (d)
m <- lm(us_quarters$PhillyMint ~ us_quarters$DenverMint )
summary(m)
r <- cor(us_quarters$DenverMint, us_quarters$PhillyMint)
plot(us_quarters$DenverMint, us_quarters$PhillyMint,
     main = "Denver vs. Pilly",
     xlab = "Denver Mint",
     ylab = "Philly Mint",
     xlim = c(150000, 1000000),
     ylim = c(150000, 1000000),
     abline(m)
)

# (e)
par(mfrow = c(1, 2))
boxplot(us_quarters$DenverMint, col = "blue", xlab = "Denver Mint")
boxplot(us_quarters$PhillyMint, col = "blue", xlab = "Philly Mint")

denver_five <- fivenum(us_quarters$DenverMint)
denver_lower_outlier <- us_quarters$State[us_quarters$DenverMint 
                                          < denver_five[2] 
                                          - 1.5*(denver_five[4] 
                                                 - denver_five[2])]
denver_higher_outlier <- us_quarters$State[us_quarters$DenverMint 
                                           > denver_five[4] 
                                           + 1.5*(denver_five[4] 
                                                  - denver_five[2])]

philly_five <- fivenum(us_quarters$PhillyMint)
philly_lower_outlier <- us_quarters$State[us_quarters$PhillyMint
                                          < philly_five[2] 
                                          - 1.5*(philly_five[4] 
                                                 - philly_five[2])]
philly_higher_outlier <- us_quarters$State[us_quarters$PhillyMint
                                           > philly_five[4] 
                                           + 1.5*(philly_five[4] 
                                                  - philly_five[2])]

par(mfrow = c(1, 1))

# Part 3
stocks <- read.csv("http://people.bu.edu/kalathur/datasets/faang.csv")

# (a)
pairs(stocks[2:6])

# (b)
cor(stocks[2:6])

# Part 4
scores <- read.csv("http://people.bu.edu/kalathur/datasets/scores.csv")

# (a)
results <- hist(scores$Score, 
                breaks = seq(35, 85, 5), 
                col = "blue", 
                ylim = c(0,30))

# The first line of past is for right-aligning each line.
# cat function is to apply new line sign \n.
# Multiply a SPACE by [2- (the number of digits of counts)]
cat(paste(strrep(" " ,(2 - (nchar(as.character(results$counts))))),
      results$counts, 
      " students in range (", 
      results$breaks[-length(results$breaks)],
      ", ",
      results$breaks[-1],
      "]",
      sep = "",
      collapse = "\n"))

# (b)
results <- hist(scores$Score, 
                breaks = seq(30, 90, 20), 
                col = "blue", 
                ylim = c(0, 80),
                main = "Score",
                xlab = "Score")

letter_grade <- c("C", "B", "A")
cat(paste(results$counts, 
          " students in ",
          letter_grade,
          " range (", 
          results$breaks[-length(results$breaks)],
          ", ",
          results$breaks[-1],
          "]",
          sep = "",
          collapse = "\n"))



barplot(table(c(3, 7, 2, 5, 8, 6, 1, 9, 5, 8, 5, 6, 8, 4, 1)))
summary(c(34, 52, 64, 16, 44, 54, 40, 48, 40, 68))

hist(c(71, 42, 18, 73, 16, 78, 38, 62, 46, 85, 95, 91, 61, 58, 69)
  
  
)


pnorm(74.8, sd =0.7376, mean = 72.9) - pnorm(71, sd = 0.7376, mean = 72.9)
1- pnorm(208.4, sd = 11.9, mean = 188)
qnorm(208.4,  mean = 188, sd = 11.9, lower.tail = T)

a <- c(2,2,2,2,2,2,2,3,3,3,4,5,5,6,7)
boxplot(a)
summary(a)

t.test()


weighta<- c (76, 81, 80, 65,60, 73, 95, 90, 82, 58, 77, 71)
meta <- c (83, 77, 85, 70, 62, 70, 92, 95, 86, 61, 70, 80)
m <-lm(meta ~ weighta)
x<-data.frame(weighta, meta)
m <-lm(x$meta ~x$weighta  )
plot(x$weighta,x$meta,
     abline(m))
predict.lm(x, 40.5)
summary(m)
setwd("C:\RCode")

library("Metrics") 
rmse(m, predict.lm(m, x$weighta))




