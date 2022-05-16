library(prob)
options(digits=4)
sample_space <- rolldie(3, 6, T)

# Part2)
# (a)
sum <- rowSums(sample_space[c('X1','X2','X3')])
less_than_10 <- sample_space[(sum < 10),]
sum <- rowSums(less_than_10[c('X1','X2','X3')])
between_6_and_10 <- less_than_10[(sum > 6),]
between_6_and_10
sum(between_6_and_10$probs)

# (b)
three_identical_bool <- (sample_space['X1']==sample_space['X2']) &
                        (sample_space['X2']==sample_space['X3'])
three_identical <- sample_space[three_identical_bool,]
three_identical
sum(three_identical$probs)

# (c)
# ONLY two rolls are identical which means can't be three identical.
two_identical_bool <- ((sample_space['X1']==sample_space['X2'])  |
                       (sample_space['X2']==sample_space['X3'])  |
                       (sample_space['X1']==sample_space['X3'])) &
                      !((sample_space['X1']==sample_space['X2']) &
                       (sample_space['X2']==sample_space['X3']))
two_identical <- sample_space[two_identical_bool,]
two_identical
sum(two_identical$probs)

# (d)
no_identical_bool <- (!three_identical_bool) &
                     (!two_identical_bool)
no_identical <- sample_space[no_identical_bool,]
no_identical
sum(no_identical$probs)

# (e)
sum <- rowSums(two_identical[c('X1','X2','X3')])
two_identical_greater_than_9 <- two_identical[(sum > 9),]
two_identical_greater_than_9
sum(two_identical_greater_than_9$probs)


#------------------------------------------
# Part3)
# Version 1
sum_of_first_N_odd_squares <- function(n) {
  sum <- 0
  for (i in 1:n) {
    odd_num <- 2*i - 1
    sum = sum + odd_num^2
  }
  return(sum)
}

# Test version 1
sum_of_first_N_odd_squares(2)
sum_of_first_N_odd_squares(5)
sum_of_first_N_odd_squares(10)

# Version 2
sum_of_first_N_odd_squares_V2 <- function(n) {
  if (n <= 1) {
    return(n)
  }
  return(((2*n - 1)^2) + sum_of_first_N_odd_squares_V2(n - 1))
}

# Test version 2
sum_of_first_N_odd_squares_V2(2)
sum_of_first_N_odd_squares_V2(5)
sum_of_first_N_odd_squares_V2(10)


#------------------------------------------
# Part4)
# Read csv from the website
dow <- read.csv('http://people.bu.edu/kalathur/datasets/DJI_2020.csv', 
                stringsAsFactors = FALSE)
head(dow)

# (a)
sm <- summary(dow$Close)
num_sm <- c()
for (i in 1:6) {
  num_sm[i] <- as.numeric(sub('.*:', '', sm[i]))
}
num_sm <- num_sm[-4]
variation <- c()
for (i in 1:4) {
  variation[i] <- num_sm[i+1] - num_sm[i]
}
header <- c('First','Second','Third','Fourth')

# Use cat() to enable displaying \n as new line

cat(paste(header[1:4], variation[1:4], 
          sep = " Quartile variation is ", 
          collapse = '\n'))

# (b)
# Due to the use of sprintf, output has the quotation marks that are not contained 
# in the given sample output. Subtract quotation marks using gsub:
cat(sprintf("\nThe minimum Dow value of %.f is at row %.f on %s. ",
                      num_sm[1], 
                      which(dow$Close == num_sm[1]), 
                      dow$Date[which(dow$Close == num_sm[1])]))

# (c)
max_price_after_min <- as.numeric(max(dow$Close[which(dow$Close == num_sm[1]):length(dow$Close)]))
cat(sprintf("\nI would sell on %s when Dow is at %.f for a gain of %1.2f%%. ",
                      dow$Date[which(dow$Close == max_price_after_min)], 
                      max_price_after_min,
                      100*(max_price_after_min - num_sm[1])/num_sm[1]))

# (d)
diff <- diff(dow$Close)
diff <- c(0, diff)
dow$DIFFS <- diff
head(dow)

# (e)
sprintf("%.f days Dow closed higher than previous day.", sum(dow$DIFFS > 0))
sprintf("%.f days Dow closed lower than previous day.", sum(dow$DIFFS < 0))

# (f)
gain_higher_than_1000 <- dow[dow$DIFFS >= 1000,]

library(fitur)
sum(dbinom(9, 9:15, 1/4))


dbinom(9, 9, 1/4)
(1/4)**9


min(c(0:50)[qbinom(0.5, 0:50, 1/4) == 9])

rbinom(100, )


min(c(0:50)[qbinom(0.5, 0:50, 1/4) == 9])

set.seed(731)
max_chance = 80
records = c()
for (i in 1: 100){
  records = c(records, 
              min(c(0:max_chance)[cumsum(rbinom(max_chance, 1, 1/4)) == 9]))
}
print(table(records))
print(table(records)[table(records) == max(table(records))])
aplot(table(records))

























      
