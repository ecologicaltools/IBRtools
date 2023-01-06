test_that("assess_ibrv2_index", {
  expected <- ibrv2_index(enzact2)
  expect_type(expected, "list")
})
