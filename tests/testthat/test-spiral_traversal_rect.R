test_that("spiral_traversal_rect works correctly as compared to hardcoded solution", {
  # Test 1: Output structure
  result <- spiral_traversal_rect(3, 1.5)
  expected_sp_three <- list(
    order = matrix(
      c(2, 3, 3, 2, 1, 1, 1, 2, 3, 4, 4, 4,
        2, 2, 1, 1, 1, 2, 3, 3, 3, 3, 2, 1),
      nrow = 12, ncol = 2, byrow = FALSE,
      dimnames = list(NULL,c("row", "col"))),
    n_rows = 4,
    n_cols = 3
  )
  expect_type(result, "list") # Should return a list
  expect_equal(length(result), 3) # Should have 3 elements

  # Test 2: Matrix dimensions
  expect_equal(result$n_rows, 4) # n_rows should match size
  expect_equal(result$n_cols, 3) # n_cols should match size

  # Test 3: Spiral order
  expect_equal(result$order, expected_sp_three$order)
})
