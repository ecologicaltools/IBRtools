test_that("assess_ibr_std", {
  expected <- ibr_std(enzact, enzact_coef)
  expect_type(expected, "list")
})
