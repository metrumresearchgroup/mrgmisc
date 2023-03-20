list_of_ids <- sd_oral_richpk %>% 
  capitalize_names() %>%
  dplyr::mutate(plotnum = ids_per_plot(ID)) %>% # default 9 per plot
  split(.$plotnum)

plot_list <- list_of_ids %>%
  lapply(function(df) {
    df %>%
      ggplot2::ggplot(ggplot2::aes(x = TIME, y = CONC, group = ID)) +
      ggplot2::geom_line() + ggplot2::facet_wrap(~ID)
  })

p1 <- print_plots(plot_list)

test_that("print_plots outputs 1 new page per input provided", {
  expect_equal(length(p1), 6)
})
