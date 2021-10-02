library(dymDataTable)
init <- {
    dt = DT(diamonds)
    self = newDT(dt)
}
self[, .(d2 = SUM(STACK(d2 = .(x, y, z)))), by = .(`d2 names`)]
self[, .(SUM(STACK(d2 = .(x, y, z)))), by = .(`d2 names`)]
self[, .(SUM(STACK(d2 = .(x, y, z)))), by = .()]
self[, .(SUM(STACK(variable = .(x, y, z)))), by = .(cut)]
self[, .(SUM(STACK(d2 = .(x, y, z)))), by = .(cut, `d2 names`)]
self[, .(SUM(STACK(d2 = .(x, y, z))), SUM(carat)), by = .(cut, 
    `d2 names`)]
self[, .(SUM(STACK(d2 = .(x, y, z))), SUM(carat)), by = .(cut)]
self[, .(SUM(STACK(d2 = .(x, y, z))), SUM(carat)), by = ]
self[, .D[, .(SUM(STACK(d2 = .(x, y, z)))), by = .(`d2 names`)]]
self[, .D[, .(d2 = SUM(STACK(d2 = .(x, y, z)))), by = .(`d2 names`)], 
    by = .(cut, `d2 names`)]
self[, .(d2 = .D[, .(SUM(STACK(d2 = .(x, y, z)))), by = .(`d2 names`)]), 
    by = .(cut, `d2 names`)]
self[, .(SUM(STACK(d2 = .(x, y, z))), SUM(STACK(d2 = .(x, y, 
    z)))), by = .(cut, `d2 names`)]
self[, .(SUM(STACK(mm = .(x, y, z))), SUM(STACK(nums = .(price, 
    carat, table)))), by = .(`mm names`, `nums names`)]
self[, .SD[, SUM(STACK(d2 = .(x, y, z))), by = .(`d2 names`)], 
    by = .(cut)]
self[, .SD[, .(SUM(STACK(d2 = .(x, y, z)))), by = .(`d2 names`)], 
    by = .(cut)]
self[, max(J(.SD[, .(SUM(STACK(d2 = .(x, y, z)))), by = .(`d2 names`)])), 
    by = .(cut)]
