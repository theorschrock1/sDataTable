library(dymDataTable)
init <- {
    dt = DT(diamonds)
    self = newDT(dt)
}
self[, .(MELT(d2 = .(MIN(x), MEDIAN(y), MEAN(y), MAX(y)))), by = .()]
self[, .(MELT(d2 = .(MIN(x), MEDIAN(y), MEAN(y), MAX(y)))), by = .(`d2 names`)]
self[, .(MELT(d2 = .(MIN(x), MEDIAN(y), MEAN(y), MAX(y)))), by = .(cut)]
self[, .(MELT(`summary(x)` = .(MIN(x), MEDIAN(x), MEAN(x), MAX(x))), 
    MELT(`summary(x)` = .(MIN(x), MEDIAN(x), MEAN(x), MAX(x)))), 
    by = .(cut, `summary(x) names`)]
self[, .(MELT(`summary(x)` = .(MIN(x), MEDIAN(x), MEAN(x), MAX(x))), 
    MELT(`summary(y)` = .(MIN(y), MEDIAN(y), MEAN(y), MAX(y)))), 
    by = .(cut, `summary(x) names`, `summary(y) names`)]
self[, .(MELT(new = .(MIN(x), MEDIAN(x), MEAN(x)))), by = `new names` ~ 
    cut]
self[, .(MELT(new = .(MIN(x), MEDIAN(x), MEAN(x)))), by = cut ~ 
    `new names`]
self[, .(MELT(`summary(x)` = .(MIN(x), MEDIAN(x), MEAN(x), MAX(x))), 
    MELT(`summary(y)` = .(MIN(x), MEDIAN(y), MEAN(y)))), by = .(cut, 
    `summary(x) names`)]
self[, .(MELT(new = .(MIN(x), MEDIAN(x), MEAN(x), `SUM(x,y,z)` = SUM(STACK(d1 = .(x, 
    y, z)))))), by = .(cut, `new names`)]
self[, .(MELT(`summary(x)` = .(MIN(x), MEDIAN(x), MEAN(x))), 
    MELT(`summary(y)` = .(MIN(y), MEDIAN(y), MEAN(y)))), by = .(color, 
    `summary(x) names`), calculate_totals = T]
