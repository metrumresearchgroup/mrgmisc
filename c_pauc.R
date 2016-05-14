library(PKPDdatasets)
library(dplyr)

df_rep <- function(df, reps) {
  big_df <- list()
  for(i in 1:reps) {
    big_df[[i]] <- df %>% mutate(REP = i)
  }
  out <- dplyr::bind_rows(big_df)
  return(out)
}
bigdf <- df_rep(sd_oral_richpk, 1000)
meddf <- df_rep(sd_oral_richpk, 100)
smalldf <- df_rep(sd_oral_richpk, 1)
sd_oral_richpk %>% group_by(ID) %>% 
  summarize(pauc_c = pauc(Time, Conc, c(0, max(Time))),
            pauc_r = auc_partial(Time, Conc, c(0, max(Time))))

bigdf %>% group_by(ID, REP) %>% 
  summarize(pauc_c = pauc(Time, Conc, c(0, max(Time))),
            pauc_r = auc_partial(Time, Conc, c(0, max(Time))))


id1 <- sd_oral_richpk %>% filter(ID == 1)
rep(sd_oral_richpk, 2) %>% View
pauc(id1$Time, id1$Conc, range = c(0, 99))
library("microbenchmark")
f_r <- function(df) {
  df %>% group_by(ID, REP) %>% 
    summarize(pauc_r = auc_partial(Time, Conc, c(0, max(Time))))
}

f_c <- function(df) {
  df %>% group_by(ID, REP) %>% 
    summarize(pauc_c = pauc(Time, Conc, c(0, max(Time))))
  
}
f_cpp <- function(df) {
  df %>% group_by(ID, REP) %>% 
    summarize(pauc_c = auc_partial_cpp(Time, Conc, c(0, max(Time))))
  
}
bm <- microbenchmark(f_r(bigdf),
                     f_r(meddf),
                     f_r(smalldf),
                     f_c(bigdf),
                     f_c(meddf),
                     f_c(smalldf),
                     f_cpp(bigdf),
                     f_cpp(meddf),
                     f_cpp(smalldf),
                   times=10L, unit = 's')

write.csv(bigdf, "~/Repos/misc-datasets/bigconc.csv", row.names=F)
