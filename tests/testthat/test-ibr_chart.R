test_that("assess_ibr_chart", {
  std <- ibr_std(enzact)
  expected <- ibr_chart(std)
  expect_type(expected, "list")
})
