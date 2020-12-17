test_that("dynDT", {
  local_edition(3)
  expect_snapshot(dynDT(x = mtcars), cran = TRUE)
})
