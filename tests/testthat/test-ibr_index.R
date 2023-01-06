test_that("assess_ibr_index", {
  expected <- ibr_index(enzact, enzact_coef)
  expect_type(expected, "list")
  expect_silent(enzact)
})
