test_that("eldd", {
  expect_snapshot(head(eldd))
  expect_snapshot(tail(eldd))
  expect_snapshot(summary(eldd))
})
