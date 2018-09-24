#' Non-parametric test for any statistic
#'
#' Performs a non parametric test for any stat by bootstraping. Combines all values and samples
#' vectors of length a_data, b_data. Then calculates differences in stat to get distribution
#' of differences in stat. Then compares observed difference with that distribution.
#'
#' H0: No difference in stat
#'
#' @param a_data a numeric vector of values from group a
#' @param b_data a numeric vector of values from group b
#' @param n_sim number of times to sample groups and calcutale difference
#' @param plot (logical) plot the distribution of differences in mean if TRUE
#'
#' @return returns a named list containing:
#' \item{diff}{all the mean differences}
#' \item{observed}{observed mean difference}
#' \item{n_sim}{number of simulations ran}
#' \item{p_value}{two sided p-value for the test}
#'
#' @examples
#' x = rnorm(300, mean = 5, sd = 1)
#' y = rnorm(200, mean = 4, sd = 1.5)
#'
#' np_mean_test(x, y)
#'
#' @author Elio Bartos
#'
#' @export
np_stat_test <- function(a_data, b_data, stat_fun = mean, n_sim = 1000, plot = TRUE, ...) {
  # Nonparametric mean test, comparing mean(b_data) - mean(a_data)
  # Negative observed means mean in a group is higher

  observed = stat_fun(b_data, ...) - stat_fun(a_data, ...)

  data = c(a_data, b_data)
  n_a = length(a_data)
  n_b = length(b_data)

  diff = vector('numeric', length = n_sim)
  for(i in 1:n_sim) {
    tmp_a = sample(data, n_a, replace = TRUE)
    tmp_b = sample(data, n_b, replace = TRUE)

    diff[[i]] = stat_fun(tmp_b, ...) - stat_fun(tmp_a, ...)
  }

  p_value = sum(abs(observed) < abs(diff))/n_sim

  # Plot
  if(plot) {
    bounds = quantile(diff, prob = c(0.025, 0.975))

    x_min = min(c(diff, observed))
    x_max = max(c(diff, observed))

    plot(density(diff), xlim = c(x_min, x_max), main = "Diff distribution")
    abline(v = observed, col = "red")
    abline(v = bounds[[1]], col = "blue", lty = 2 )
    abline(v = bounds[[2]], col = "blue", lty = 2)
  }

  # Create Output
  output = list()
  output$diff = diff
  output$observed = observed
  output$n_sim = n_sim
  output$p_value = p_value

  return(output)
}