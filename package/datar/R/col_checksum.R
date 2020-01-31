#==============================================================================
#' Column Checksum
#'
#' @param    data    a data frame.
#' @param    c1      first column.
#' @param    c2      second column.
#' @param    digits  integer indicating the number of decimal places to round.
#' @return   Returns the cumulative difference between the corresponding
#'   values in columns \code{c1} and \code{c2}.
#'
#' @author   Nathan Lucas
#' @export
#------------------------------------------------------------------------------
col_checksum <- function(data, c1, c2, digits = NA)
{
  if (length(c1) <= 0)
  {
    stop("ERROR: util_checksum_column(): argument 'c1' must have length > 0")
  }
  if (length(c2) <= 0)
  {
    stop("ERROR: util_checksum_column(): argument 'c2' must have length > 0")
  }
  if (length(c1) == 1) { s1 <- data[, c1] } else { s1 <- rowSums(data[, c1]) }
  if (length(c2) == 1) { s2 <- data[, c2] } else { s2 <- rowSums(data[, c2]) }
  if (is.na(digits))
  {
    sum(s1 - s2)
  }
  else
  {
    round(sum(s1 - s2), digits = digits)
  }
}

#------------------------------------------------------------------------------
# Changelog
# 2017-04-28  Created.
# 2017-10-11  Copied from multirobot-results analysis.
# 2017-10-27  Incorporated into 'datar' package.
#==============================================================================
