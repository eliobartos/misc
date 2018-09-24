#' Non-parametric mean test
#'
#' Performs a non parametric mean test by bootstraping. Combines all values and samples
#' vectors of length a_data, b_data. Then calculates differences in mean to get distribution
#' of differences in mean. Then compares observed difference with that distribution.
#'
#' H0: No difference in mean
#'
#' @param a_data Numeric vector of values from group a.
#' @param b_data Numeric vector of values from group b.
#' @param n_sim Number of times to sample groups and calcutale difference.
#' @param plot (logical) Plot the distribution of differences in mean if TRUE.
#'
#' @return returns a named list containing:
#' \item{diff}{All the mean differences.}
#' \item{observed}{Observed mean difference.}
#' \item{n_sim}{Number of simulations ran.}
#' \item{p_value}{Two sided p-value for the test.}
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
np_mean_test <- function(a_data, b_data, n_sim = 1000, plot = TRUE) {
  # Nonparametric mean test, comparing mean(b_data) - mean(a_data)
  # Negative observed means mean in a group is higher

  observed = mean(b_data) - mean(a_data)

  data = c(a_data, b_data)
  n_a = length(a_data)
  n_b = length(b_data)

  diff = vector('numeric', length = n_sim)
  for(i in 1:n_sim) {
    tmp_a = sample(data, n_a, replace = TRUE)
    tmp_b = sample(data, n_b, replace = TRUE)

    diff[[i]] = mean(tmp_b) - mean(tmp_a)
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
