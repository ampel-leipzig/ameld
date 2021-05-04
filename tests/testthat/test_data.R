test_that("eldd", {
  expect_snapshot(head(eldd))
  expect_snapshot(tail(eldd))
  expect_snapshot(summary(eldd))
})

test_that("eldr", {
  expect_snapshot(head(eldr))
  expect_snapshot(tail(eldr))
  expect_true(all(eldr$Code %in% names(eldd)))
  expect_true(all(names(eldd)[grep("_.$", names(eldd))] %in% eldr$Code))
})
