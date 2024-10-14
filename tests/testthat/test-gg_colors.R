test_that("output", {
  expect_error(gg_colors(), class = "simpleError")
  expect_true(is.character(gg_colors(8)))
  expect_false(is.factor(gg_colors(8)))
})

res <- try(lapply(1:100, \(x) gg_colors(x)))
matches <- try({
  base::Map(res, seq_along(res), f = \(res, ind) length(res) == ind) |>
    unlist()
})

test_that("expected length", {
  expect_true(all(matches))
})
