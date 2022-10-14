test_that("pretty_dose converts numeric dose column to string [MRG-PDOS-001]", {
  dose_fix <- pretty_dose(Theoph, .dosecol = "Dose", .units = "mg")
  expect_equal(as.character(dose_fix$Dose[1]), "4.02 mg")
  expect_equal(as.character(dose_fix$Dose[13]), "4.4 mg")
})

test_that("pretty_dose creates correct factor levels [MRG-PDOS-002]", {
  dose_fix <- pretty_dose(Theoph, .dosecol = "Dose", .units = "mg")
  expect_equal(levels(dose_fix$Dose),
               c("3.1 mg", "4 mg", "4.02 mg", "4.4 mg", "4.53 mg", "4.92 mg", 
                 "4.95 mg", "5.3 mg", "5.5 mg", "5.86 mg"))
})
