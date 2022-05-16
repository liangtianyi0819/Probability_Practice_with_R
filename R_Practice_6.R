library(stringr)
library(tidyverse)
library(plotly)
file <- "http://people.bu.edu/kalathur/datasets/lincoln.txt"
words <- scan(file, what=character())

#Part1)
# a)
head(words[str_detect(words, '[:punct:]')])

# b)
new_words <- tolower(str_replace_all(words, '[:punct:]', ''))
head(new_words)

# c)
sort(table(new_words), decreasing = T)[1:5]

# d)
length_table <- table(str_length(new_words))
barplot(length_table, 
        main = 'Frequency Distribution of Word Lengths', 
        xlab = 'Word Length',
        ylab = 'Frequency')

# e)
new_words[str_length(new_words) == max(str_length(new_words))]

# f)
start_with_p <- new_words[str_sub(new_words, 1, 1) == 'p']

# g)
end_with_r <- new_words[str_sub(new_words, -1, -1) == 'r']

# h)
intersect(start_with_p, end_with_r)

stopfile <- "http://people.bu.edu/kalathur/datasets/stopwords.txt"
stopwords <- scan(stopfile, what=character())

new_words <- new_words[!(new_words %in% stopwords)]

# Repeat
# c)
sort(table(new_words), decreasing = T)[1:5]

# d)
length_table <- table(str_length(new_words))
barplot(length_table, 
        main = 'Frequency Distribution of Word Lengths', 
        xlab = 'Word Length', 
        ylab = 'Frequency')

#Part2)
# a)
df <- read.csv('usa_daily_avg_temps.csv')
usaDailyTemps <- as_tibble(df)

# b)
max_temp_by_year <- usaDailyTemps %>% group_by(year) %>% summarise(max = max(avgtemp))
max_temp_by_year
plot(max_temp_by_year, type="l")

# c)
max_temp_by_state <- usaDailyTemps %>% group_by(state) %>% summarise(max = max(avgtemp))
max_temp_by_state
plot_ly(as.data.frame(max_temp_by_state), x = ~state, y = ~max, type = 'bar') %>%
  layout(yaxis = list(range = c(70, 110)))

# d)
bostonDailyTemps <- filter(usaDailyTemps, city == 'Boston')
bostonDailyTemps

# e)
bostonDailyTemps %>% 
  group_by(month) %>% 
  summarise(average_by_month = mean(avgtemp))
plot(bostonDailyTemps %>% 
       group_by(month) %>% 
       summarise(average_by_month = mean(avgtemp)),
     type = 'l', ylab = 'Average Temperatures Monthly',
     main = 'Boston Monthly Average Temperatures')

