test_that("moe_sum works with no zero estimates", {
  # This was the #637 regression: max(numeric(0)) returned -Inf
  moes <- c(100, 200, 150)
  ests <- c(500, 300, 400)

  result <- moe_sum(moes, ests)

  expect_true(is.finite(result))
  expect_equal(result, sqrt(100^2 + 200^2 + 150^2))
})

test_that("moe_sum handles zero estimates correctly", {
  # When there are zero estimates, only the largest MOE among zeros
  # should be kept (per Census guidance), combined with non-zero MOEs
  moes <- c(100, 200, 150, 50)
  ests <- c(500, 0, 0, 400)

  result <- moe_sum(moes, ests)

  # Zero estimates have MOEs 200, 150; max is 200

  # Non-zero estimates have MOEs 100, 50
  # Result: sqrt(200^2 + 100^2 + 50^2)
  expected <- sqrt(200^2 + 100^2 + 50^2)
  expect_equal(result, expected)
})

test_that("moe_sum warns when estimate is not provided", {
  moes <- c(100, 200, 150)

  expect_warning(
    result <- moe_sum(moes),
    "You have not specified the estimates"
  )
  expect_equal(result, sqrt(100^2 + 200^2 + 150^2))
})

test_that("moe_sum handles all-zero estimates", {
  moes <- c(100, 200, 150)
  ests <- c(0, 0, 0)

  result <- moe_sum(moes, ests)

  # All zeros: keep only the largest MOE
  expect_equal(result, 200)
})

test_that("moe_sum handles single element", {
  expect_equal(moe_sum(c(100), c(500)), 100)
  expect_equal(moe_sum(c(100), c(0)), 100)
})

test_that("moe_sum handles na.rm", {
  moes <- c(100, NA, 150)
  ests <- c(500, 300, 400)

  expect_true(is.na(moe_sum(moes, ests, na.rm = FALSE)))
  expect_true(is.finite(moe_sum(moes, ests, na.rm = TRUE)))
})

test_that("moe_sum handles missing MOEs for zero estimates", {
  moes <- c(100, NA, NA)
  ests <- c(500, 0, 0)

  expect_true(is.na(moe_sum(moes, ests, na.rm = FALSE)))
  expect_warning(
    result <- moe_sum(moes, ests, na.rm = TRUE),
    NA
  )
  expect_equal(result, 100)
})

test_that("moe_sum handles missing estimates", {
  moes <- c(100, 200, 150)
  ests <- c(500, NA, 400)

  expect_equal(moe_sum(moes, ests), sqrt(100^2 + 200^2 + 150^2))
})

test_that("moe_ratio returns correct values", {
  result <- moe_ratio(num = 100, denom = 500, moe_num = 20, moe_denom = 40)
  r2 <- (100 / 500)^2
  expected <- sqrt(20^2 + r2 * 40^2) / 500
  expect_equal(result, expected)
})

test_that("moe_product returns correct values", {
  result <- moe_product(est1 = 100, est2 = 50, moe1 = 20, moe2 = 10)
  expected <- sqrt((100^2 * 10^2) + (50^2 * 20^2))
  expect_equal(result, expected)
})

test_that("moe_prop returns correct values", {
  result <- moe_prop(num = 100, denom = 500, moe_num = 20, moe_denom = 40)
  expect_true(is.finite(result))
  expect_true(result > 0)
})

test_that("moe_prop errors on mismatched lengths", {
  expect_error(
    moe_prop(num = c(1, 2), denom = c(1), moe_num = c(1, 2), moe_denom = c(1)),
    "same length"
  )
})

test_that("moe functions are vectorized where expected", {
  # moe_ratio, moe_product, moe_prop should handle vectors
  nums <- c(100, 200)
  denoms <- c(500, 1000)
  moe_nums <- c(20, 30)
  moe_denoms <- c(40, 50)

  expect_length(moe_ratio(nums, denoms, moe_nums, moe_denoms), 2)
  expect_length(moe_product(nums, denoms, moe_nums, moe_denoms), 2)
  expect_length(moe_prop(nums, denoms, moe_nums, moe_denoms), 2)
})
