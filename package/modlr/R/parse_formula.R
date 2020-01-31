#==============================================================================

#------------------------------------------------------------------------------
#' Formula Left-Hand Side
#'
#' Extracts the response expression of \code{formula}
#' (everything to the left of the \code{~} operator).
#'
#' @param   x   a formula object of the form \code{y ~ x | g}.
#' @return  Returns a character string containing the response
#'   expression of \code{formula}.
#'
#' @author Nathan Lucas
#' @export
#' @examples
#' formula_lhs(y ~ x | g)                   # "y"
#' formula_lhs(y1 + y2 ~ x1 + x2 | g1/g2)   # "y1 + y2"
#------------------------------------------------------------------------------
formula_lhs <- function(x)
{
  if (!inherits(x, "formula"))
  {
    stop("'x' must contain a formula.")
  }
  lhs_index <- attr(stats::terms(x), "response")
  if (lhs_index > 0)  { deparse(x[[2]]) } else { "" }
}

#------------------------------------------------------------------------------
#' Formula Right-Hand Side
#'
#' Extracts model term expression of \code{formula}
#' (everything to the right of the \code{~} operator).
#'
#' @param   x   a formula object of the form \code{y ~ x | g}.
#' @return  Returns a character string containing the model term
#'   expression of \code{formula}.
#'
#' @author Nathan Lucas
#' @export
#' @examples
#' formula_rhs(y ~ x | g)                   # "x | g"
#' formula_rhs(y1 + y2 ~ x1 + x2 | g1/g2)   # "x1 + x2 | g1/g2"
#------------------------------------------------------------------------------
formula_rhs <- function(x)
{
  if (!inherits(x, "formula"))
  {
    stop("'x' must contain a formula.")
  }
  lhs_index <- attr(stats::terms(x), "response")
  if (lhs_index > 0)  { deparse(x[[3]]) } else { deparse(x[[2]]) }
}

#------------------------------------------------------------------------------
#' Parse Formula Expressions
#'
#' Extracts component expressions of a \code{formula} object.
#' Returns a list containing the response (left-hand side),
#' model (right-hand side), covariates (model without conditioning
#' expressions), and conditioning (grouping) expressions.
#'
#' @param   formula   a \code{formula} object of the form \code{y ~ x | g}.
#' @return  Returns a list of named character strings:
#' \tabular{ll}{
#'   \code{response}  \tab :  left-hand side (lhs) of \code{formula}.
#'     Everything to the left of the \code{~} operator. \cr
#'   \code{model}     \tab :  right-hand side (rhs) of \code{formula}.
#'     Everything to the right of the \code{~} operator. \cr
#'   \code{covariate} \tab :  covariate expression from the rhs.
#'     Everything between the \code{~} and \code{|} operators. \cr
#'   \code{condition} \tab :  conditioning expression from the rhs.
#'     Everything to the right of the \code{|} operator.
#' }
#' @author Nathan Lucas
#' @export
#' @examples
#' fe <- formula_expr(y ~ x | g)
#' fe$response      # "y"
#' fe$model         # "x | g"
#' fe$covariate     # "x"
#' fe$condition     # "g"
#'
#' f <- "y1 + y2 ~ x1 + x2 | g1/g2"
#' fe <- formula_expr(as.formula(f))
#' fe$response      # "y1 + y2"
#' fe$model         # "x1 + x2 | g1/g2"
#' fe$covariate     # "x1 + x2"
#' fe$condition     # "g1/g2"
#------------------------------------------------------------------------------
formula_expr <- function(formula)
{
  if (!(inherits(formula, "formula")))
  {
    stop("'formula' must contain a formula.")
  }
  lhs <- formula_lhs(formula)
  rhs <- formula_rhs(formula)
  cov <- (unlist(strsplit(rhs, "[|]")))[1]
  con <- (unlist(strsplit(rhs, "[|]")))[2]
  list(
    response  = lhs,
    model     = rhs,
    covariate = trimws(cov),
    condition = trimws(con)
  )
}

#------------------------------------------------------------------------------
# Alternative using 'nlme' package:
# parse_formula <- function(formula)
# {
#   if (!requireNamespace("nlme", quietly = TRUE))
#   {
#     stop("Package 'nlme' needed for this function to work.")
#   }
#   f <- stats::as.formula(formula)
#   if (!(inherits(f, "formula")))
#   {
#     stop("'formula' must contain a formula.")
#   }
#   y <- if (attr(stats::terms(f), "response"))
#   {
#     nlme::getResponseFormula(f)
#   }
#   x <- nlme::getCovariateFormula(f)
#   g <- nlme::getGroupsFormula(f)
#   list(response  = all.vars(y),
#        covariate = all.vars(x),
#        group     = all.vars(g))
# }
# @seealso \code{\link{[nlme]getResponseFormula}},
#          \code{\link{[nlme]getCovariateFormula}},
#          \code{\link{[nlme]getGroupsFormula}}

#------------------------------------------------------------------------------
# Changelog
# 2017-10-23  Created.
# 2017-10-27  Added check of formula class inheritance.
# 2017-10-29  Rewritten to remove dependence on 'nlme' package.
#==============================================================================
