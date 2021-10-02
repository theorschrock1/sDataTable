test_that("dynDT", {
  local_edition(3)
  expect_snapshot(class(dynDT(x = mtcars)), cran = TRUE)
})
