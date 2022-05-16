# Q1
# (a)
score <- c(58,46,50,90,42,52,62,44,96,92,54,82)
# (b)
n <- length(score)
# (c)  
first_and_second <- score[c(1,2)]
# (d)
first_and_last <- score[c(1, n)]
# (e)
middle_two <- score[c(n/2, n/2+1)]

# Q2
# (a)
median_score <- median(score)
# (b)
below_median <- score <= median_score
# (c)
above_median <- score > median_score
# (d)
count_below_median = sum(below_median)
# (e)
count_above_median = sum(above_median)

# Q3
# (a)
score_below_median <- score[below_median]
# (b)
score_above_median <- score[above_median]

# Q4
# (a)
odd_index_values <- score[seq(1, n, 2)]
# (b)
even_index_value <- score[seq(2, n, 2)]

# Q5
# (a)
format_scores_version1 <- paste(LETTERS, score, sep="=")
# (b)
format_scores_version2 <- paste(LETTERS[n:1], score, sep="=")

# Q6
# (a)
scores_matrix <- matrix(score,2, byrow = TRUE)
# (b)
first_and_last_version1 = scores_matrix[, c(1, ncol(scores_matrix))]

# Q7
# (a)
named_matrix <- scores_matrix
rownames(named_matrix) <- rownames(named_matrix, do.NULL = FALSE, prefix = "Quiz_")
colnames(named_matrix) <- colnames(named_matrix, do.NULL = FALSE, prefix = "Student_")
# (b)
first_and_last_version2 = named_matrix[, c(1, ncol(named_matrix))]
print(first_and_last_version2)





