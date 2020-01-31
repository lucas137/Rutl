#==============================================================================
#' Stripplot
#'
#' Produces a series of one-dimensional scatterplot.
#'
#' @param  formula a formula object of the form \code{y ~ x}.
#'   \code{y} is a response, \code{x} is a covariate expression.
#' @param  data    a data frame.
#' @param  groups  an optional grouping variable.
#' @param  cex     relative character size of labels.
#' @param  jitter  amount of jittering, or 0 for no jitter.
#' @param  ...     further arguments.
#' @return A \code{\link[lattice]{trellis.object}}.
#'
#' @author Nathan Lucas
#' @export
#' @seealso  \code{\link[lattice]{stripplot}}
#' @examples \dontrun{
#' data_stripplot(errors ~ mode, data = data2)
#' }
#------------------------------------------------------------------------------
data_stripplot <- function(formula, data, groups = NULL,
                           cex = 0.8, jitter = 0.1, ...)
{
  #---------------------------------------------------------
  # Plot parameters

  auto_key <- list(         # legend parameters
      space   = "right"     # key outside plot, to the right
    , points  = FALSE       # do not show data points
    , lines   = TRUE        # show lines between mean values
    , cex     = cex         # relative character size
  )

  #---------------------------------------------------------
  # The lattice function stripplot() produces one-dimenstional scatter plots.
  # 'formula' is of the form y ~ x, where x is on the X-axis.
  # The 'quote' and 'eval' hack is necessary to pass variable "groups".
  #   https://stat.ethz.ch/pipermail/r-help/2011-August/286707.html
  # A legend is drawn according to the parameters given via 'auto.key'.
  # Note:  The legend is only drawn as required identify data groups.
  #---------------------------------------------------------
  ccall <- quote(lattice::stripplot(
      x           = formula        # formula
    , data        = data           # data frame
    , jitter.data = (jitter > 0)   # TRUE to jitter points to avoid overlap
    , factor      = jitter         # amount of jitter
    #, auto.key   = TRUE           # simple legend
    , auto.key    = auto_key       # legend parameters
    , type        = c("p", "a")    # points; average
    , lwd         = 1              # line width
    , grid        = "h"            # horizontal grid lines
    , ...                          # further arguments
  ))
  ccall$groups <- substitute(groups)
  eval(ccall)
}

#------------------------------------------------------------------------------
# Examples:
#   https://www.stat.ubc.ca/~jenny/STAT545A/block07_univariatePlotsLattice.html
#------------------------------------------------------------------------------
# 2017-10-13  Created.
# 2017-10-23  Added variable 'jitter'.
# 2017-10-27  Incorporated into 'plotr' package.
#==============================================================================
