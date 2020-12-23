test_that("sDataTable_main", {
  local_edition(3)
  library(ggplot2)
  di = DT(diamonds)
  di[, date := Sys.Date()]
  di[, `Today's Date` := Sys.Date()]
  self <- dynDT(di)
  expect_snapshot(class(self$new_variable(name = "tmp", J = "`Today's Date`+1",
    I = NULL, by = NULL, include_LOD = NULL, exclude_LOD = NULL, from_source = F,
    replace_old = T)), cran = TRUE)
})
