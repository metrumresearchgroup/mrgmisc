\name{AUC_inf}
\alias{AUC_inf}
\title{Calculate AUCt-inf}
\usage{
AUC_inf(time = "TIME", conc = "DV", last_points = c(3, 4, 5),
  AUCinf_only = TRUE)
}
\arguments{
  \item{time}{column name for time}

  \item{conc}{column name for conc}

  \item{last_points}{vector of amount of points in terminal
  phase that will be evaluated for extrapolation}

  \item{AUCinf_only}{default TRUE only return single value
  for AUCinf}
}
\description{
Calculate AUCt-inf
}
\details{
last_points defaults to 3, 4, 5 AUCinf_only for use in
dplyr/plyr or other summarizations that can only handle a
vector length 1 outputs when AUCinf_only is false also
return %extrapolated, num points used for lambda calc,
adjusted Rsquared value
}

