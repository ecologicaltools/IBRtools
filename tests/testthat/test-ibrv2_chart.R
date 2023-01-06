test_that("assess_ibrv2_chart", {
  bdi <- ibrv2_bdi(enzact2)
  expected <- ibrv2_chart(bdi)
  expect_type(expected, "list")
})
