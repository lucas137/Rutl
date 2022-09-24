#______________________________________________________________________________
#' Check Data Balance
#'
#' Checks whether data \code{data} are balanced by cross-tabulating
#' variables in \code{formula}.  Outputs a message and contingency
#' table if the data are not balanced.
#'
#' @param    formula  a formula object with the cross-classifying variables
#'   (separated by +) on the right hand side. Interactions are not allowed.
#'   On the left hand side, one may optionally give a vector or a matrix of
#'   counts.  See \code{\link[stats]{xtabs}}.
#' @param    data     a matrix or data frame containing the
#'   variables in \code{formula}.  See \code{\link[stats]{xtabs}}.
#' @param    ...      further arguments passed to \code{\link[stats]{xtabs}}.
#'
#' @return   Returns \code{TRUE} if balanced, \code{FALSE} otherwise.
#'
#' @author   Nathan Lucas
#' @export
#' @examples \dontrun{
#' dat <- read_csv(file = "mydata", factors = c(1:5))
#' balanced(~ PID + mode + trial + robot, dat)
#' }
#______________________________________________________________________________
balanced <- function(formula, data) {
  ct  <- stats::xtabs(formula, data)    # contingency table
  bal <- all(ct == 1)                   # ct values should all be '1'
  if (!bal) {
    cat(paste0("WARNING: unbalaned design: ",
               deparse(substitute(data)), "\n",   # data frame variable name
               "contingency table:\n"
    ))
    print(stats::ftable(ct))    # 'ftable' creates a flat contingency table
  }
  bal   # return
}

#______________________________________________________________________________
# Changelog
# 2017-10-26  Created.
# 2021-04-18  Formatting.
#______________________________________________________________________________
