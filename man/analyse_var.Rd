% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analyse_var.R
\name{analyse_var}
\alias{analyse_var}
\title{Analyse variable by AB test group}
\usage{
analyse_var(df, var_chr, group_variable = "ab_test_group",
  quant_focus = "mid", confidence_interval = 0.9)
}
\arguments{
\item{df}{Data frame with both variables of interest.}

\item{var_chr}{(character) Column name of variable you want to be analysed as string.}

\item{group_variable}{Variable to group by.}

\item{quant_focus}{("up", "mid", "down") Controls on which quantiles to focus more.
"up" focuses more on median and upper quantiles, "down" is the opposite and "mid" is balanced.}

\item{confidence_interval}{Probability of confidence interval range.}
}
\value{
list of two tibbles:
\item{quant_data}{Contains quantiles.}
\item{mean_data}{Contains means, standard errors and confidence interval bounds.}
}
\description{
If you have 2 columns, one representing variable of interest, and another group for that observation,
this function analyses distribution of variable in each group in a way that is easy to compare.
It calculates mean and confidence intervals, quantiles and density plots.
}
\examples{
#library(tidyverse) #For tribble and pipe \%>\% notation
library(tibble) #For tribble
library(magrittr) # For pipe \%>\%

df <- tribble(
  ~x, ~group,
  1, 1,
  3.2, 1,
  2.4, 1,
  3.1, 1,
  5, 2,
  6, 2,
  4.7, 2
)

df \%>\%
  analyse_var("x", "group")

}
\author{
Elio Bartoš
}
