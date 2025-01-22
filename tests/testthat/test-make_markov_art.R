test_that("make_markov_art works correctly as compared to hardcoded solution", {
  # Test 1: Matrix dimensions
  result <- round(make_markov_art(3, 1.5, 42))
  expected <- matrix(c(2, 1, 1, 7, 2, 0, 1, 5, 2, 4, 4, 6),
                              nrow = 4, ncol = 3, byrow = FALSE)

  expect_equal(nrow(result), 4) # n_rows should match size
  expect_equal(ncol(result), 3) # n_cols should match size

  # Test 2: Values
  expect_equal(result, expected)
})
