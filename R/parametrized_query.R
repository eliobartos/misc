#' Parametrize a SQL query
#'
#' In a query string put x1x, x2x and so on characters where you want to insert your strings. Provide a list of
#' of strings and those values will be replaced and query will be ready to be sent to database.
#'
#' @param query String representing your query with x1x, x2x, ... on places where you want to put your parameters
#' @param params Vector of strings to be placed in a query on places x1x, x2x,.. respectively.
#'
#' @return A parametrised query ready to be sent to the database.
#'
#' @examples
#' query = "
#' select date, sum(revenue) as revenue
#'   from my_sample_table
#'   where date between 'x1x' and 'x2x'
#'   group by 1
#'   order by 1;
#' "
#' p_query = parametrized_query(query, c('2018-09-01', as.character(Sys.Date())))
#' cat(p_query)
#'
#' @author Elio Bartoš
#'
#' @export
parametrized_query <- function(query, params) {
  for(i in seq_along(params)) {
    pattern = paste0('x', i, 'x')
    query = gsub(pattern, params[[i]], query)
  }
  return(query)
}
