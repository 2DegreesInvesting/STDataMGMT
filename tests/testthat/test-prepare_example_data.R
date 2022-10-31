test_that("data is filtered corrected", {
  example_test_res <- prepare_example_data(tibble::tibble(
    some_col = 1:10,
    some_val = "A"
  ))
  expect_true(max(example_test_res$some_col) < 5)
})
