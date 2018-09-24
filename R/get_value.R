#' Get a value from function defined by points
#'
#' For a function defined by points and then interpolated as constant or linear function get a value
#' for specific x. Function points should be provided in data frame with 2 columns: x and y values.
#'
#' @param df Data frame with two columns, first column representing x values and second y values.
#' @param x_value Value for which we want to calculate this function.
#' @param method ('constant', 'linear') Should function be step function (constant between points), or interpolated linearly.
#'
#' @return Value at x_value point.
#'
#' @examples
#' df = tribble(
#' ~x, ~y,
#'  1, 1,
#'  5, 5,
#'  10, 10
#' )
#'
#' get_value(df, 2)           # returns 1
#' get_value(df, 2, "linear") # returns 2
#' get_value(df, 7)           # returns 5
#' get_value(df, 7, "linear") # returns 7
#'
#' @author Elio Bartoš
#'
#' @export
get_value <- function(df, x_value, method = "constant")
{
  if(!(method %in% c("constant", "linear"))) {
    print("Parameter method should be 'constant' or 'linear'. Error!")
    return(NULL)
  }

  colnames(df)  <-  c("x", "y")
  df <- df %>% arrange(x)

  return(approx(df$x, df$y, x_value, method = method)$y)
}

