#' Copy data frame to clipboard
#'
#' Copies data frame to clipboard. Usually used when you want to copy data frame to Excel. Just run
#' function with desired df and press Paste or CTRL + V to paste it in the desired place. This function is like
#' pressing CTRL + C on a dataframe. It should work on MAC and Windows.
#'
#' @param x Data frame to be copied.
#' @param row.names (boolean) Should row names be copied? Default: FALSE
#' @param col.names (boolean) should column names be copied? Default: TRUE
#' @param ... parameters forwarded to write.table
#' @return None
#'
#' @examples
#' x = data.frame(var1 = c(0, 1), var2 = c(3, 4))
#' write_excel(x)
#'
#' @author Elio Bartoš
#'
#' @export
#'
#' @importFrom utils write.table
write_excel <- function(x, row.names=FALSE, col.names=TRUE, ...) {

  if(Sys.info()[['sysname']] == "Windows") {
    write.table(x, "clipboard", sep="\t", row.names=row.names, col.names = col.names, ...)
  } else { #For MAC
    clip <- pipe("pbcopy", "w")
    write.table(x, file = clip, sep="\t" , row.names=row.names , col.names=col.names, ...)
    close(clip)
  }

  print("Data copied to clipboard!")
}
