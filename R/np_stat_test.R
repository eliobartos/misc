#' Non-parametric test for any statistic
#'
#' Performs a non parametric test for any statistics by bootstraping. Combines all values and samples
#' vectors of length a_data, b_data. Then calculates differences in statistics to get distribution
#' of differences in statatistics. Then compares observed difference with that distribution.
#'
#' H0: No difference in statistics
#'
#' @param a_data Numeric vector of values from group a.
#' @param b_data Numeric vector of values from group b.
#' @param stat_fun Function that calculates desired statistic.
#' @param n_sim Number of times to sample groups and calculate difference.
#' @param alternative Setting up alternative hypothesis:
#' "two.sided" meaning stat(A) != stat(B),
#' "less" meaning stat(A) < stat(B),
#' "greater" meaning stat(A) > stat(B)
#' @param alpha statistical significance
#' @param plot (logical) Plot the distribution of differences in mean if TRUE.
#' @param ... Any additional parameters that stat_fun needs.
#'
#' @return returns a named list containing:
#' \item{diff}{all the statistics differences}
#' \item{observed}{observed statistics difference}
#' \item{n_sim}{number of simulations ran}
#' \item{p_value}{two sided p-value for the test}
#'
#' @examples
#' x = rnorm(300, mean = 5, sd = 1)
#' y = rnorm(200, mean = 4, sd = 1.5)
#' y2 = c(y, 110)
#'
#' np_stat_test(x, y, mean)
#' np_stat_test(x, y2, mean)
#' np_stat_test(x, y, mean, trim = 0.1)
#' np_stat_test(x, y, median)
#'
#' @author Elio Bartoš
#'
#' @export
#'
#' @importFrom graphics abline
#' @importFrom stats density
np_stat_test <- function(a_data, b_data, stat_fun = mean, n_sim = 1000, alternative = "two.sided",
                         alpha = 0.05, plot = TRUE, ...) {
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

  diff = c(diff, observed)

  if(alternative == "two.sided") {
    p_value = sum(abs(observed) < abs(diff))/n_sim
    bounds = quantile(diff, prob = c(alpha/2, 1-alpha/2))
  } else if (alternative == "less") {
    p_value = sum(observed < diff)/n_sim
    bounds = quantile(diff, prob = c(1-alpha, 1-alpha))
  } else { # if (alternative == "greater")
    p_value = sum(observed > diff)/n_sim
    bounds = quantile(diff, prob = c(alpha, alpha))
  }

  # Plot
  if(plot) {


    x_min = min(c(diff, observed))
    x_max = max(c(diff, observed))

    plot(density(diff), xlim = c(x_min, x_max), main = "(Left: A higher)   Diff distribution B - A   (Right: B higher)")
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
