test_that("assess_ibrv2_bdi", {
  expected <- ibrv2_bdi(enzact2)
  expect_type(expected, "list")
})
