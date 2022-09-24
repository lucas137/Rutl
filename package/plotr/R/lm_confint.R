#______________________________________________________________________________
#' Produces a lattice plot with an array of confidence intervals plots.
#'
#' Plots 95% confidence interval on within-subjects intercept and slope.
#' Orders subjects by increasing intercept to check for relationships
#' between slopes and intercepts.
#'
#' See:  Bates, D. M. (2010). "lme4: Mixed-effects modeling with R." 73.
#'
#' @param  formula a formula object of the form \code{y ~ x | c}.
#'   \code{y} is a response, \code{x} is a covariate, and \code{c}
#'   is a conditioning variable for which to generate plot panels.
#' @param  data    a data frame.
#' @param  cex     relative character size of labels.
#' @param  ...     further arguments.
#'
#' @return A \code{\link[lattice]{trellis.object}}.
#'
#' @author Nathan Lucas
#' @export
#' @seealso  \code{\link[lattice]{dotplot}}
#' @examples \dontrun{
#' plot_lm_confint(errors ~ mode | PID, data = data2, cex = 0.8)
#' }
#
# More tips/ideas:
#   http://www.ashander.info/posts/2015/04/D-RUG-mixed-effects-viz/
#
#______________________________________________________________________________
plot_lm_confint <- function(formula, data, cex = 1.0, ...) {

  # Get response and conditioning (group) variables
  f_vars <- modlr::formula_expr(formula)

  # Fit a list of 'lm' objects with a common model for different
  # subgroups of data; 'lmList' returns an 'lmList4-class' object
  fm_plm <- lme4::lmList(formula, pool = TRUE, data = data)

  # Confirm coefficients are the same, pooled or unpooled variance estimate;
  # 'pool': should the variance estimate pool the residual sum of squares
  #fm_ulm  <- update(fm_plm, pool = FALSE)
  fm_ulm  <- stats::update(fm_plm, pool = FALSE)
  #stopifnot(all.equal(coef(fm_ulm), coef(fm_plm)))
  stopifnot(all.equal(stats::coef(fm_ulm), stats::coef(fm_plm)))

  # 95% confidence intervals, ordered by intercept (coefficient 1),
  # to see how widely they vary for the individuals
  ci <- stats::confint(fm_plm, pooled = TRUE)   # confint.lmList4
  # print(ci)

  #_______________________________________________________
  # Plot parameters

  # Axis parameters
  x_par <- list(
      cex   = cex               # relative char size of label
  )
  y_par <- list(
      label = f_vars$condition  # label text
    , cex   = cex               # relative char size of label
  )
  axis_scales <- list(
    cex   = cex                 # relative character size of scale numbers
  )

  # Plot title parameters
  title <- list(
      label = paste("95% confidence intervals of linear model",
                    "coefficients for", f_vars$response)
    , cex   = cex               # relative char size of title
    , font  = 1                 # font face
  )

  # Parameter settings supplied to trellis.par.set()
  par_settings <- list(
      strip.background = list(
        col = "gray80"            #   color
    )
    , layout.widths = list(
        left.padding    = 1       #   adjust default left margin
      , right.padding   = 0       #   adjust default right margin
    )
    , layout.heights = list(
        top.padding     = 1       #   adjust default top margin
      , bottom.padding  = 1       #   adjust default bottom margin
    )
  )

  #_______________________________________________________
  # Note:  Attempting to set 'scales' parameter results in error:
  #   "formal argument "scales" matched by multiple actual arguments"
  # Work around:  Use 'default.scales' to set axis scale settings

  # plot.lmList4.confint
  graphics::plot(
      ci                                # confidence intervals
    , order           = 1               # ordered by intercept (coefficient 1)
    , xlab            = x_par           # x-axis parameters
    , ylab            = y_par           # y-axis parameters
    # , scales        = axis_scales     # tick marks and labels of axes
    , default.scales  = axis_scales     # tick marks and labels of axes
    , as.table        = TRUE            # draw panels left-right, top-bottom
    , main            = title           # plot title parameters
    , cex             = cex             # relative char size of labels
    , par.strip.text  = list(cex = cex) # strip text character size
    , par.settings    = par_settings    # parameters for trellis.par.set()
    , ...                               # additional arguments passed to
                                        # panel.dotpot, then panel.xyplot
  )
}

# For comparison:  Model coefficients from 'lmList4' object
# cf <- coef(fm_plm)
# Run regression to get confidence intervals for each person
# ci <- sapply(data$PID, function(formula) {
#   confint(lm(f, data = subset(data, PID == formula)))
# })

#______________________________________________________________________________
# 2017-10-21  Created.
# 2017-10-27  Incorporated into 'plotr' package.
# 2017-10-30  Replace 'nlme' package with 'modlr' package
#   for formula variable extraction.
# 2021-04-18  Formatting.
#______________________________________________________________________________
