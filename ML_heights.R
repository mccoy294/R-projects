library(caret)
library(dslabs)


data(heights)

heights

y <- heights$sex
x <- heights$height

set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights[-test_index, ]
test_set <- heights[test_index, ]

y_hat <- sample(c("Male","Female"), length(test_index), replace = TRUE) %>%
  factor(level = levels(test_set$sex))

mean(y_hat == test_set$sex)


