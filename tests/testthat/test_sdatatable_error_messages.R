test_that("sdatatable_error_messages", {
  local_edition(3)
  library(ggplot2)
  di = DT(diamonds)
  self <- dynDT(di)
  di[, date := Sys.Date()]
  di[, `Today's Date` := Sys.Date()]
  self <- dynDT(di)
  expect_snapshot(self$new_variable(name = "price", J = "SUM(price)", I = NULL, by = NULL,
    include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T), cran = TRUE)
  expect_snapshot(class(self$new_variable(name = "price", J = "SUM(price)", I = NULL, by = NULL,
    include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T)), cran = TRUE)
  expect_snapshot(self$new_variable(name = "price", J = "sum(price", I = NULL, by = NULL,
    include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T), cran = TRUE)
  expect_snapshot(class(self$new_variable(name = "price", J = "sum(price", I = NULL, by = NULL,
    include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T)), cran = TRUE)
  expect_snapshot(self$new_variable(name = "price", J = "sum('price)", I = NULL, by = NULL,
    include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T), cran = TRUE)
  expect_snapshot(class(self$new_variable(name = "price", J = "sum('price)", I = NULL, by = NULL,
    include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T)), cran = TRUE)
  expect_snapshot(self$new_variable(name = NULL, J = "sum(price)", I = NULL, by = NULL,
    include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T), cran = TRUE)
  expect_snapshot(class(self$new_variable(name = NULL, J = "sum(price)", I = NULL, by = NULL,
    include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T)), cran = TRUE)
  expect_snapshot(self$new_variable(name = "sd", J = NULL, I = NULL, by = NULL, include_LOD = NULL,
    exclude_LOD = NULL, from_source = F, replace_old = T), cran = TRUE)
  expect_snapshot(class(self$new_variable(name = "sd", J = NULL, I = NULL, by = NULL, include_LOD = NULL,
    exclude_LOD = NULL, from_source = F, replace_old = T)), cran = TRUE)
  expect_snapshot(self$new_variable(name = "sds", J = "sum(sd)", I = NULL, by = NULL, include_LOD = NULL,
    exclude_LOD = NULL, from_source = F, replace_old = T), cran = TRUE)
  expect_snapshot(self$new_variable(name = "Today's Date", J = "SUM(price)", I = NULL, by = NULL,
    include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = F), cran = TRUE)
  expect_snapshot(class(self$new_variable(name = "Today's Date", J = "SUM(price)", I = NULL, by = NULL,
    include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = F)), cran = TRUE)
  di[, `Yesterday's Date` := Sys.Date() - 1]
  self <- dynDT(di)
  expect_snapshot(self$new_variable(name = "tmp", J = "`Today's Date`+`Today's Date`", I = NULL,
    by = NULL, include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T), cran = TRUE)
  expect_snapshot(class(self$new_variable(name = "tmp", J = "`Today's Date`+`Today's Date`", I = NULL,
    by = NULL, include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T)), cran = TRUE)
  expect_snapshot(self$new_variable(name = "tmp", J = "carat+cut", I = NULL, by = NULL,
    include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T), cran = TRUE)
  expect_snapshot(class(self$new_variable(name = "tmp", J = "carat+cut", I = NULL, by = NULL,
    include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T)), cran = TRUE)
  expect_snapshot(self$new_variable(name = "tmp", J = "cut+cut", I = NULL, by = NULL, include_LOD = NULL,
    exclude_LOD = NULL, from_source = F, replace_old = T), cran = TRUE)
  expect_snapshot(class(self$new_variable(name = "tmp", J = "cut+cut", I = NULL, by = NULL,
    include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T)), cran = TRUE)
  expect_snapshot(class(self$new_variable(name = "tmp", J = "cut+cut", I = NULL, by = NULL,
    include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T)), cran = TRUE)
  expect_snapshot(class(self$new_variable(name = "duration", J = "`Today's Date`-`Yesterday's Date`",
    I = NULL, by = NULL, include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T)),
  cran = TRUE)
  expect_snapshot(self$new_variable(name = "tmp", J = "duration+carat", I = NULL, by = NULL,
    include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T), cran = TRUE)
  expect_snapshot(class(self$new_variable(name = "tmp", J = "duration+carat", I = NULL, by = NULL,
    include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T)), cran = TRUE)
  expect_snapshot(self$new_variable(name = "tmp", J = "{fd=cut+cut}", I = NULL, by = NULL,
    include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T), cran = TRUE)
  expect_snapshot(class(self$new_variable(name = "tmp", J = "{fd=cut+cut}", I = NULL, by = NULL,
    include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T)), cran = TRUE)
  expect_snapshot(self$new_variable(name = "tmp", J = "{fd=cut+cut\nfd+gg}", I = NULL, by = NULL,
    include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T), cran = TRUE)
  expect_snapshot(class(self$new_variable(name = "tmp", J = "{fd=cut+cut\nfd+gg}", I = NULL, by = NULL,
    include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T)), cran = TRUE)
  expect_snapshot(self$new_variable(name = "tmp", J = "{caret2=carat+table\nc\n}", I = NULL, by = NULL,
    include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T), cran = TRUE)
  expect_snapshot(class(self$new_variable(name = "tmp", J = "{caret2=carat+table\nc\n}", I = NULL,
    by = NULL, include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T)), cran = TRUE)
  expect_snapshot(self$new_variable(name = "tmp", J = "SUM()", I = NULL, by = NULL, include_LOD = NULL,
    exclude_LOD = NULL, from_source = F, replace_old = T), cran = TRUE)
  expect_snapshot(class(self$new_variable(name = "tmp", J = "SUM()", I = NULL, by = NULL,
    include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T)), cran = TRUE)
  expect_snapshot(self$new_variable(name = "tmp", J = "c", I = NULL, by = NULL, include_LOD = NULL,
    exclude_LOD = NULL, from_source = F, replace_old = T), cran = TRUE)
})
