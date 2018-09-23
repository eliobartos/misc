#' Analyse variable by ab test groups
#'
#' If you have 2 columns, one representing variable of interest, and another group for that observation
#' this function analyses distribution of variable in each group in a way easy to compare
#'
#' @param df data frame with both variables of interest
#' @param var_chr column name of variable you want to be analysed as string
#' @param group_variable variable to group by
#' @param quant_focut ("up", "mid", "down") controls which quantiles to focus more.
#' "up" focuses more on median and upper quantiles, "down" opposite and "mid" is balanced.
#' @param confidence_interval probability of confidence interval range
#'
#' @return list of two tibbles: first containing quantile information, and second containing mean,
#' standard deviation, standard error and lower and upped confidence bounds
#'
#' @examples
#' df <- tribble(
#'   ~x, ~group,
#'   1, 1,
#'   3.2, 1,
#'   2.4, 1,
#'   3.1, 1,
#'   5, 2,
#'   6, 2,
#'   4.7, 2
#' )
#'
#' df %>%
#'   analyse_var("x", "group")
#'
#' @author Elio Bartoš
#'
#' @export
#'
analyse_var = function(df, var_chr, group_variable = "ab_test_group",
                       quant_focus = "mid", confidence_interval = 0.9) {
  # If one row represents one user, this function analyses 1 variable (column)
  var = rlang::sym(var_chr)

  if(quant_focus == "up") {
    quant_data = df %>%
      group_by_(group_variable) %>%
      summarise(q50 = quantile(!!var, 0.5),
                q65 = quantile(!!var, 0.65),
                q75 = quantile(!!var, 0.75),
                q85 = quantile(!!var, 0.85),
                q95 = quantile(!!var, 0.95))

  } else if(quant_focus == "down") {
    quant_data = df %>%
      group_by_(group_variable) %>%
      summarise(q05 = quantile(!!var, 0.05),
                q15 = quantile(!!var, 0.15),
                q25 = quantile(!!var, 0.25),
                q35 = quantile(!!var, 0.35),
                q50 = quantile(!!var, 0.50))

  } else {
    quant_data = df %>%
      group_by_(group_variable) %>%
      summarise(q05 = quantile(!!var, 0.05),
                q25 = quantile(!!var, 0.25),
                q50 = quantile(!!var, 0.50),
                q75 = quantile(!!var, 0.75),
                q95 = quantile(!!var, 0.95))

  }

  alpha = 1 - confidence_interval

  tmp = df %>%
    group_by_(group_variable) %>%
    summarise(mean = mean(!!var),
              sd = sd(!!var),
              se = sd/sqrt(n()),
              lower = mean + qnorm(alpha/2)*se,
              upper = mean + qnorm(1-alpha/2)*se)

  # Converting to factor for graph
  tmp[,1] = as.factor(tmp[,1] %>% unlist)

  # Mean and SE plot
  p = ggplot(tmp, aes_string(x = group_variable, y = "mean", group = group_variable)) +
    geom_point() +
    geom_errorbar(data = tmp, aes_string(ymin = "lower", ymax = "upper", color = group_variable),
                  width=.1) +
    ggtitle(var_chr)

  print(p)

  readline(prompt="Press [enter] for next plot")

  # Density plot
  df = df %>%
    select(one_of(group_variable, var_chr))

  df[,1] = as.factor(df[,1] %>% unlist)

  p = ggplot(df, aes_string(x = var_chr, group = group_variable, fill = group_variable)) +
    geom_density(alpha = 0.5)
  print(p)
  return(list(quant_data, tmp))
}

