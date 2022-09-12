
# Break argument ----------------------------------------------------------

test_that("works with breaks out of order [MRG-MISC-0171]", {
  x <- Theoph$conc
  res <- set_bins_df(.df = Theoph, .x= "conc", breaks = stats::quantile(x, na.rm = T, probs= c(1, 0, 0.5)))
  res2 <- set_bins_df(.df = Theoph, .x= "conc", breaks = stats::quantile(x, na.rm = T, probs= c(0, 0.5, 1)))
  expect_equal(
    res,
    res2
  )
})

# if one break provided, assumes breaks <- c(-Inf, breaks, Inf)
test_that("works with 1 break provided [MRG-MISC-0171]", {
  x <- Theoph$conc
  xbreak <- stats::quantile(x, na.rm = T, probs= c(0.2))
  xbin <- length(xbreak) + 1
  res <- set_bins_df(.df = Theoph, .x= "conc", breaks = xbreak)
  expect_equal(
    length(xbreak),
    1 
  )
  expect_equal(
    sort(unique(res$conc_bins)),
    c(0,1) 
  )
  expect_equal(
    as.character(unique(levels(res$conc_bins_label))[xbin-1]),
    paste("[", paste("-Inf", {xbreak[1]}, sep  = "_"), ")", sep = "")
  )
  expect_equal(
    as.character(unique(levels(res$conc_bins_label))[xbin]),
    paste("[", paste({xbreak[1]}, "Inf", sep  = "_"), ")", sep = "")
  )
})



# Upper and lower bounds argument -----------------------------------------

## Lower bound ------------------------------------------------------------
# lower_bound default -Inf
test_that("lower_bound test: -Inf [MRG-MISC-0172]", {
  x <- Theoph$conc
  res <- set_bins_df(.df = Theoph, .x= "conc", breaks = stats::quantile(x, na.rm = T, probs= c(0.4, 0.5, 1)), lower_bound = -Inf)
  expect_true(all(!is.na(res)))
  expect_equal(
    sort(unique(res$conc_bins)),
    c(0,1,2)
  )
})

# if 1st break > lower_bound and lower_bound is not NA, then lower_bound overrides 1st break
test_that("lower_bound test: discrete min number, break[1] > lower_bound [MRG-MISC-0172]", {
  x <- Theoph$conc
  xbreak <- stats::quantile(x, na.rm = T, probs= c(min(x) + 0.4, 0.5, 1))
  res <- set_bins_df(.df = Theoph, .x= "conc", breaks = xbreak, lower_bound = min(x))
  expect_gt(min(xbreak),
            min(x))
  expect_true(all(!is.na(res)))
  expect_equal(
    sort(unique(res$conc_bins)),
    c(0,1,2)
  )
})

# if lower_bound > actual min, then all obs less than lower_bound put into NA bin
test_that("lower_bound test: discrete number > actual min [MRG-MISC-0172]", {
  x <- Theoph$conc
  res <- set_bins_df(.df = Theoph, .x= "conc", breaks = stats::quantile(x, na.rm = T, probs= c(min(x) + 0.1, 0.5, 1)), lower_bound = min(x)+ 0.1)
  expect_false(all(!is.na(res)))
  expect_equal(
    sort(unique(res$conc_bins)),
    c(0,1,2)
  )
})

 
## Upper bound ------------------------------------------------------------
#upper_bound default: Inf
test_that("upper_bound test: -Inf [MRG-MISC-0172]", {
  x <- Theoph$conc
  res <- set_bins_df(.df = Theoph, .x= "conc",, upper_bound = Inf)
  expect_true(all(!is.na(res)))
  expect_equal(
    sort(unique(res$conc_bins)),
    c(1, 2, 3, 4)
  )
})

# if last break < upper_bound and upper_bound is not NA, then upper_bound overrides last break
test_that("upper_bound test: discrete max number, break[n] > upper_bound [MRG-MISC-0172]", {
  x <- Theoph$conc
  xbreak <- stats::quantile(x, na.rm = T, probs= c(0.4, 0.5, 0.8))
  res <- set_bins_df(.df = Theoph, .x= "conc", breaks = xbreak, upper_bound = max(x)) 
  expect_lt(max(xbreak),
            max(x))
  expect_true(all(!is.na(res)))
  expect_equal(
    sort(unique(res$conc_bins)),
    c(0, 1, 2, 3)
  )
})

# if upper_bound < actual max, then all obs greater than upper_bound put into NA bin
test_that("upper_bound test: discrete number < actual max [MRG-MISC-0172]", {
  x <- Theoph$conc
  xbreak <- stats::quantile(x, na.rm = T, probs= c(0.4, 0.5, 0.8))
  res <- set_bins_df(.df = Theoph, .x= "conc", breaks = xbreak, upper_bound = max(x)-0.2) 
  expect_lt(max(xbreak),
            max(x))
  expect_false(all(!is.na(res)))
  expect_equal(
    sort(unique(res$conc_bins)),
    c(0, 1, 2, 3)
  )
})

# Inclusive argument ------------------------------------------------------
#include max value of largest user defined bin even though lower bins are non-inclusive
test_that("inclusive test: inclusive = TRUE and upper bound = Inf, n bins = n breaks -1 [MRG-MISC-0173]", {
  x <- Theoph$conc
  xbreak <- stats::quantile(x, na.rm = T, probs= c(0, 0.5, 1))
  xupper = Inf
  res <- set_bins_df(.df = Theoph, .x= "conc", breaks = xbreak, upper_bound = xupper, inclusive = TRUE)
  expect_equal(
    length(xbreak)-1,
    length(unique(res$conc_bins))
  )
})

test_that("inclusive test: inclusive = FALSE and upper bound = Inf, n bins = n breaks [MRG-MISC-0173]", {
  x <- Theoph$conc
  xbreak <- stats::quantile(x, na.rm = T, probs= c(0, 0.5, 1))
  xupper = Inf
  res <- set_bins_df(.df = Theoph, .x= "conc", breaks = xbreak, upper_bound = xupper, inclusive = FALSE)
  expect_equal(
    length(xbreak),
    length(unique(res$conc_bins))
  )
})

test_that("inclusive test: inclusive = TRUE and discrete upper bound, n bins = n breaks -1 [MRG-MISC-0173]", {
  x <- Theoph$conc
  xbreak <- stats::quantile(x, na.rm = T, probs= c(0, 0.5, 1))
  xupper = max(x)-2
  res <- set_bins_df(.df = Theoph, .x= "conc", breaks = xbreak, upper_bound = xupper, inclusive = TRUE)
  expect_equal(
    length(xbreak)-1,
    length(unique(res$conc_bins))
  )
})

test_that("inclusive test: inclusive = FALSE and discrete upper bound, n bins = n breaks -1 [MRG-MISC-0173]", {
  x <- Theoph$conc
  xbreak <- stats::quantile(x, na.rm = T, probs= c(0, 0.5, 1))
  xupper = max(x)-2
  res <- set_bins_df(.df = Theoph, .x= "conc", breaks = xbreak, upper_bound = xupper, inclusive = FALSE)
  expect_equal(
    length(xbreak)-1,
    length(unique(res$conc_bins))
  )
})

# Between argument --------------------------------------------------------

# if val between c(min_between, max_between), then bin 1. Less than min_between, then bin 0. Greater than max_between, then bin 2
  test_that("between test: discrete number with between argument [MRG-MISC-0174]", {
    x <- Theoph$conc
    min_between <- min(x) + 1
    max_between <- max(x) - 5
    res <- set_bins_df(.df = Theoph, .x= "conc",  between = c(min_between, max_between)) 
    res2 <- ifelse(dplyr::between(x, min_between, max_between), 1, 
                   ifelse(x > max_between, 2, 0))
    expect_equal(
      res,
      res2
    )
    expect_true(all(!is.na(res)))
  })

test_that("between test: discrete number with between argument all > max val [MRG-MISC-0174]", {
  x <- Theoph$conc
  min_between <- max(x) + 1
  max_between <- max(x) + 5
  res <- set_bins_df(.df = Theoph, .x= "conc",  between = c(min_between, max_between)) 
  res2 <- ifelse(dplyr::between(x, min_between, max_between), 1, 
                 ifelse(x > max_between, 2, 0))
  expect_equal(
    res,
    res2
  )
  expect_true(all(!is.na(res)))
})

test_that("between test: discrete number with between argument all < min val [MRG-MISC-0174]", {
  x <- Theoph$conc
  min_between <- min(x) + 1
  max_between <- min(x) - 5
  res <- set_bins_df(.df = Theoph, .x= "conc",  between = c(min_between, max_between)) 
  res2 <- ifelse(dplyr::between(x, min_between, max_between), 1, 
                 ifelse(x > max_between, 2, 0))
  expect_equal(
    res,
    res2
  )
  expect_true(all(!is.na(res)))
})

# error message if between length(between) != 2
  test_that("between test: error length(between) !=2 [MRG-MISC-0174]", {
    expect_error(set_bins_df(.df = Theoph, .x= "conc",  between = c(1)),
                 "can only have 2 breaks to use the between functionality") 
  })

# Quiet argument ----------------------------------------------------------

test_that("quiet test: quiet= false print message [MRG-MISC-0175]", {
  x <- Theoph$conc
  xbreak = stats::quantile(x, na.rm = T, probs= c(0, 0.5, 1))
  res <- set_bins_df(.df = Theoph, .x= "conc", breaks= xbreak, quiet = FALSE)
  xbin <- length(xbreak) + 1
  expect_message(any(grepl(
    glue::glue("there were {xbin} bins calculated, with the following range for each bin:
               BIN: 0 range: -Inf - {xbreak[1]}
               BIN: 1 range: 0 - {xbreak[2]}
               BIN: 2 range: {xbreak[2]} - {xbreak[3]}
               BIN: 3 range: {xbreak[3]} - Inf"),
    set_bins_df(.df = Theoph, .x= "conc", breaks = xbreak, quiet = FALSE),
    fixed =TRUE
  )))
})

  test_that("quiet test: quiet= between argument and false print message[MRG-MISC-0175]", {
    x <- Theoph$conc
    xbreak = stats::quantile(x, na.rm = T, probs= c(0, 0.5, 1))
    res <- set_bins_df(.df = Theoph, .x= "conc", breaks= xbreak, quiet = FALSE, between = c(5, 8))
    xbin <- as.numeric(paste0(length(xbreak) + 1))
    expect_message(any(grepl(
      glue::glue("there were {xbin} bins calculated, with the following range for each bin:
               BIN: 0 range: All values less than {xbreak[1]}
               BIN: 1 range: All values between {xbreak[1]} and {xbreak[3]}
               BIN: 2 range: All values greater than {xbreak[3]}"),
      set_bins_df(.df = Theoph, .x= "conc", breaks = xbreak, quiet = FALSE),
      fixed =TRUE
    )))
  })


# Name and label arguments ------------------------------------------------

  test_that("name and label test: default [MRG-MISC-0176]", {
    x <- Theoph$conc
    .x= "conc"
    res <- set_bins_df(.df = Theoph, .x= "conc", breaks = stats::quantile(x, na.rm = T, probs= c(0, 0.5, 1)))
    expect_equal(
      names(res)[ncol(res)-1],
      paste0(.x, "_bins")
    )
    expect_equal(
      names(res)[ncol(res)],
      paste0(names(res)[ncol(res)-1], "_label")
    )
    expect_equal(
      sort(unique(res$conc_bins)),
      c(1,2)
    )
  })
  
  test_that("name test: specified name [MRG-MISC-0176]", {
    x <- Theoph$conc
    .name = "conc_category"
    res <- set_bins_df(.df = Theoph, .x= "conc", .name = "conc_category", breaks = stats::quantile(x, na.rm = T, probs= c(0, 0.5, 1)))
    expect_equal(
      names(res)[ncol(res)-1],
      .name
    )
    expect_equal(
      names(res)[ncol(res)],
      paste0(.name, "_label")
    )
  })
  
  test_that("name test: specified name and label [MRG-MISC-0176]", {
    x <- Theoph$conc
    .name = "conc_category"
    .label = "conc_ranges"
    res <- set_bins_df(.df = Theoph, .x= "conc", .name = "conc_category", .label = "conc_ranges", breaks = stats::quantile(x, na.rm = T, probs= c(0, 0.5, 1)))
    expect_equal(
      names(res)[ncol(res)-1],
      .name
    )
    expect_equal(
      names(res)[ncol(res)],
      paste0(.label)
    )
  })
  
  test_that("name test: specified label [MRG-MISC-0176]", {
    x <- Theoph$conc
    .x= "conc"
    .label = "conc_ranges"
    res <- set_bins_df(.df = Theoph, .x= "conc", .label = "conc_ranges", breaks = stats::quantile(x, na.rm = T, probs= c(0, 0.5, 1)))
    expect_equal(
      names(res)[ncol(res)-1],
      paste0(.x, "_bins")
    )
    expect_equal(
      names(res)[ncol(res)],
      paste0(.label)
    )
  })
  
# Label column output -----------------------------------------------------

  
  test_that("label test: default [MRG-MISC-0176]", {
    x <- Theoph$conc
    .x= "conc"
    res <- set_bins_df(.df = Theoph, .x= "conc", breaks = stats::quantile(x, na.rm = T, probs= c(0, 0.5, 1)))
    expect_equal(
      names(res)[ncol(res)],
      paste0(.x, "_bins_label")
    )
  })
  
  test_that("label test: contents of label column with default lower and upper bounds [MRG-MISC-0177]", {
    x <- Theoph$conc
    xbreak = stats::quantile(x, na.rm = T, probs= c(0, 0.5, 1))
    res <- set_bins_df(.df = Theoph, .x= "conc", breaks= xbreak)
    xbin <- length(xbreak) + 1
    expect_equal(
      as.character(unique(levels(res$conc_bins_label))[xbin-3]),
      paste("[", paste("-Inf", {xbreak[1]}, sep  = "_"), ")", sep = "")
    )
    expect_equal(
      as.character(unique(levels(res$conc_bins_label))[xbin-2]),
      paste("[", paste({xbreak[1]}, {xbreak[2]}, sep  = "_"), ")", sep = "")
    )
    expect_equal(
      as.character(unique(levels(res$conc_bins_label))[xbin-1]),
      paste("[", paste({xbreak[2]}, {xbreak[3]}, sep  = "_"), ")", sep = "")
    )
    expect_equal(
      as.character(unique(levels(res$conc_bins_label))[xbin]),
      paste("[", paste({xbreak[3]}, "Inf", sep  = "_"), ")", sep = "")
    )
  })
  
  test_that("label test: contents of label column with specified lower and upper bounds [MRG-MISC-0177]", {
    x <- Theoph$conc
    xbreak <- stats::quantile(x, na.rm = T, probs= c(0.4, 0.5, 0.8))
    lbound = 2
    ubound = 9
    res <- set_bins_df(.df = Theoph, .x= "conc", breaks = xbreak, lower_bound = lbound, upper_bound = ubound)
    xbin <- length(xbreak) + 1
    expect_equal(
      as.character(unique(levels(res$conc_bins_label))[xbin-3]),
      paste("[", paste({lbound}, {xbreak[1]}, sep  = "_"), ")", sep = "")
    )
    expect_equal(
      as.character(unique(levels(res$conc_bins_label))[xbin-2]),
      paste("[", paste({xbreak[1]}, {xbreak[2]}, sep  = "_"), ")", sep = "")
    )
    expect_equal(
      as.character(unique(levels(res$conc_bins_label))[xbin-1]),
      paste("[", paste({xbreak[2]}, {xbreak[3]}, sep  = "_"), ")", sep = "")
    )
    expect_equal(
      as.character(unique(levels(res$conc_bins_label))[xbin]),
      paste("[", paste({xbreak[3]}, {ubound}, sep  = "_"), ")", sep = "")
    )
  })