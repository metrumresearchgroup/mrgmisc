test_that("geomean calculates accurate geometric mean [MRG-GEOM-001]", {
  expect_equal(geomean(Theoph$Wt), 68.98671 - 4.24e-06)
  expect_equal(geomean(c(1, 2, 3, 4, 5)), 2.605171 + 8.47e-08)
})
