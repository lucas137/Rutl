#==============================================================================
#' Data Frame Summary
#'
#' Print summary statistics to console.
#'
#' @param    data    a data frame.
#' @param    col     a name of column containing the factor interest.
#' @param    label   a string label for factor.
#' @author   Nathan Lucas
#' @export
#------------------------------------------------------------------------------
summary_df <- function(data, col, label)
{
  if (!is.data.frame(data))
  {
    stop("'data' must be a 'data.frame'")
  }
  x <- levels(data[, col])
  for (i in x)
  {
    cat("\nSummary:", i, label, "\n")
    print(summary(data[data[, col] == i, label]))
  }
}

#------------------------------------------------------------------------------
# Changelog
# 2017-04-28  Created.
# 2017-10-11  Copied from multirobot-results analysis.
# 2017-10-27  Incorporated into 'datar' package.
#==============================================================================
