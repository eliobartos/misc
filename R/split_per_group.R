#' Split one variable into multiple vectors by group variable
#'
#' Selects variable and variable to group by and for each group returns a vector of values belonging to that group.
#' Used to split selected variable by group because some functions for statistical tests as input receive two or more
#' vectors and don't accept formula. In the process function also can drop percentage or some number of observations
#' from each group (after grouping) or sets top x values to max of all other values (used for skewed distributions).
#'
#' @param data Data frame containing variable of interest and grouping variable.
#' @param variable (character) Variable we wish to split into multiple vectors.
#' @param split_variable (character) Variable to split by, grouping variable.
#' @param drop_pct Percentage of users to drop from top of each vector (after grouping).
#' @param drop_n Number of users to rop from top of each vector (after grouping).
#' @param set_top_pct Sets top set_top_pct values to 1-set_top_pct quantile of variable (before grouping). It
#' is used to reduce big postive outliers in skewed distributions.
#'
#' @return Named list of vectors belonging to each group.
#'
#' @examples
#' df <- tribble(
#'   ~x, ~group,
#'    1, 1,
#'  3.2, 1,
#'  2.4, 1,
#'  3.1, 1,
#'    5, 2,
#'    6, 2,
#'  4.7, 2
#' )
#'
#' split_per_group(df, "x", "group")
#' split_per_group(df, "x", "group", drop_n = 1) #Drops max value from each vector
#' split_per_group(df, "x", "group", set_top_pct = 0.15) #Value of 6 is reduced to 5.1
#'
#' @author Elio Bartoš
#'
#' @export
#'
#' @import dplyr
split_per_group <- function(data, variable, split_variable = "ab_test_group", drop_pct = 0, drop_n = 0,
                                 set_top_pct = 0) {
  # splits data per ab_test_group and takes variable (as string)
  # drops or sets values, before of after splitting (just for set_to_max_pct)
  var = rlang::sym(variable)

  if(set_top_pct != 0) {
    # set top x pct of users to max value of rest
    x = data %>%
      dplyr::select(!!var) %>%
      dplyr::filter((!!var) > 0) %>%
      unlist

    names(x) = NULL
    quantile = quantile(x, 1-set_top_pct)

    column_index = grep(variable, colnames(data))
    data[data[,column_index] > quantile ,column_index] = quantile
  }

  out = list()
  unique_groups = data %>%
                    select(one_of(split_variable)) %>%
                    unlist %>%
                    unique %>%
                    sort()
  i = 1

  for(group_label in unique_groups)
  {
    item <- data %>%
      filter(UQ(rlang::sym(split_variable)) == group_label) %>%
      dplyr::select(one_of(variable))

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
  names(out) = unique_groups
  return(out)
}
