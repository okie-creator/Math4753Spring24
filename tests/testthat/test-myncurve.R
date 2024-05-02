test_that("sigma works", {
  v <- myncurve(5, 3, 2)
  expect_equal(v$mu, 5)
  expect_equal(v$sigma, 3)
  expect_equal(v$area, 0.1587)
})
