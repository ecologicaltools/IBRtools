test_that("assess_ibr_index", {
  outstd <- ibr_std(enzact)
  expected <- ibr_index(outstd)
  expect_type(expected, "list")
  expect_silent(enzact)
})
