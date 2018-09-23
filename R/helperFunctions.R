
## Useful function

#DONE
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  clip <- pipe("pbcopy", "w")
  write.table(x, file = clip, sep="\t" , row.names=row.names , col.names=col.names, ...)
  close(clip)
}

parametrized_query <- function(query, params) {
  for(i in seq_along(params)) {
    pattern = paste0('x', i, 'x')
    query = gsub(pattern, params[[i]], query)
  }
  return(query)
}


split_per_group <- function(data, variable, drop_pct = 0, drop_n = 0,
                            set_top_pct = 0) {
  # splits data per ab_test_group and takes variable (as string)
  # drops or sets values, before of after splitting (just for set_to_max_pct)
  var = rlang::sym(variable)

  if(set_top_pct != 0) {
    # set top x pct of users to max value of rest
    x = data %>% dplyr::select(!!var) %>% dplyr::filter((!!var) > 0) %>% unlist
    names(x) = NULL
    quantile = quantile(x, 1-set_top_pct)

    column_index = grep(variable, colnames(data))
    data[data[,column_index] > quantile ,column_index] = quantile
  }


  out = list()
  unique_groups = sort(unique(data$ab_test_group))
  i = 1

  for(group in unique_groups)
  {
    item <- data %>% filter(ab_test_group == group) %>% dplyr::select(one_of(variable))
    out[[i]] = item[[1]]
    i = i + 1
  }

  # Drop percentage of users
  if(drop_pct != 0) {
    for(index in seq_along(out)) {
      max_index = round( length(out[[index]])*(1-drop_pct) )
      out[[index]] = sort(out[[index]])[1:max_index]
    }
  }

  # Drop top n spenders
  if(drop_pct == 0 & drop_n > 0) {
    for(index in seq_along(out)) {
      out[[index]] = sort(out[[index]])[1:(length(out[[index]]) - drop_n)]
    }
  }

  # Set top
  return(out)
}

np_mean_test <- function(a_data, b_data, n_sim = 100, plot = TRUE) {
  # Nonparametric mean test, comparing mean(b_data) - mean(a_data)
  # Negative observed means mean in a group is higher
  # drop_pct drops top x pct of best payers in both groups

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

    plot(density(diff))
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

analyse_var1 = function(df, var_chr) {
  # If one row represents one user, this function analyses 1 variable (column)
  var = rlang::sym(var_chr)

  quant_data = df %>%
    group_by(ab_test_group) %>%
    summarise(q50 = quantile(!!var, 0.5),
              q65 = quantile(!!var, 0.5),
              q75 = quantile(!!var, 0.75),
              q85 = quantile(!!var, 0.85),
              q95 = quantile(!!var, 0.95))

  tmp = df %>%
    group_by(ab_test_group) %>%
    summarise(mean = mean(!!var),
              sd = sd(!!var),
              se = sd/sqrt(n()),
              lower = mean - 1.645*se,
              upper = mean + 1.645*se)

  p = ggplot(tmp, aes(x = ab_test_group, y = mean, group = ab_test_group)) +
    geom_point() +
    geom_errorbar(data = tmp, aes(ymin = lower, ymax = upper, color = ab_test_group),
                  width=.1) +
    ggtitle(var_chr)

  print(p)
  return(list(quant_data, tmp))
}

# From dataset of x and y values for given x returns y value by step function interpolation
get_value <- function(df, x_value, method = "constant")
{
  colnames(df)  <-  c("x", "y")
  df <- df %>% arrange(x)

  return(approx(df$x, df$y, x_value, method = method)$y)
}


