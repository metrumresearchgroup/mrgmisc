#' get gradients for nonmem run running via PSN
#' @name check_gradients
#' @param runnum Xpose-style run number 
#' @param wd relative directly location where PSN run results are output
#' @param folder_str folder name besides run number
#' @param full_directory can be specified if folder where gradient file stored non-traditional
check_gradients <- function(runnum, wd = NULL, folder_str = '.mod.dir.1', full_directory = NULL) {
  if(is.null(wd)) wd <- getwd()

raw_grd <- read.table(paste0(wd,'/run', runnum, folder_str, '/NM_run1/psn.grd'), skip = 1, header = T)

grd <- melt(raw_grd, id.vars='ITERATION')

grd <- within(grd, iszero <- ifelse(value == 0, 1, 0))
bdry_df <- ddply(grd, .(variable), summarize, boundary = any(iszero))
grd <- merge(grd, bdry_df, all.x = TRUE)
  out <- ggplot(data = grd, aes(x= ITERATION, y = value, group = variable)) 
  if (any(grd$boundary == TRUE)) { 
    out <- out + geom_rect(data = subset(grd, boundary == TRUE),
                           aes(fill = "red"), 
                           xmin = -Inf, 
                           xmax = Inf, 
                           ymin = -Inf,
                           ymax = Inf, 
                           alpha = 0.01)} 
  out <- out +  geom_point() +
    facet_wrap(~variable, scales = "free") +
    scale_color_manual(values = c("black", "red")) + ggtitle(paste0("run", runnum))
  print(out)
}