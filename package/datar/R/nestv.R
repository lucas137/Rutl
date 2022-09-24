#______________________________________________________________________________
#' Nested Vector
#'
#' @param    inner  ?
#' @param    outer  ?
#' @param    skip   ?
#' @return   Returns a nested vector.
#'
#' @author   Nathan Lucas
#' @export
#' @examples
#' nestv(4, 3, 1)  #  1 2 3 4   6 7 8 9   11 12 13 14
#' nestv(4, 3, 2)  #  1 2 3 4   7 8 9 10   13 14 15 16
#' nestv(3, 4, 1)  #  1 2 3   5 6 7   9 10 11   13 14 15
#______________________________________________________________________________
nestv <- function(inner, outer, skip = 0) {
  if (inner < 0) { return(NULL) }
  if (outer < 0) { return(NULL) }
  (1:(inner * outer)) + rep(skip * (0:(outer - 1)), each = inner)
}

#______________________________________________________________________________
# Changelog
# 2017-04-28  Created.
# 2017-10-11  Copied from multirobot-results analysis.
# 2017-10-27  Incorporated into 'datar' package.
# 2021-04-18  Formatting.
#______________________________________________________________________________
