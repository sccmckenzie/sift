set.seed(1)
df <- data.frame(a = runif(100, 0, 100),
                 b = sample(LETTERS[1:3], size = 100, prob = c(0.49, 0.49, 0.02), replace = TRUE))
df[sample(1:100, size = 1), "a"] <- NA_real_

result <- sift(df, a, 4, b == "C")
result2 <- sift(dplyr::group_by(df, b), a, 0.1, b == "C")
