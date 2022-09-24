#______________________________________________________________________________
#' Produces a lattice plot with an array of scatter plots in panels.
#
#' Each panel includes a reference line obtained by simple linear regression.
#' The aspect ratio of the panels (width-to-height) is adjusted so a typical
#' reference line is ~45 degrees, which makes it easier to detect systematic
#' changes in slopes.
#' The panels are ordered by increasing intercept of simple linear regression,
#' not by PID (which is a random order).  If the slopes and the intercepts are
#' highly correlated we should see a pattern across the panels in the slopes.
#'
#' @param  formula a formula object of the form \code{y ~ x | c}.
#'   \code{x} and \code{y} are x- and y-axis variable names respectively,
#'   and \code{c} is a conditioning variable for which to generate plot panels.
#' @param  data    a data frame.
#' @param  cex     relative character size of labels.
#' @param  jitter  amount of X-axis jittering, or \code{0} for no jitter.
#' @param  ...     further arguments.
#'
#' @return A \code{\link[lattice]{trellis.object}}.
#'
#' @author Nathan Lucas
#' @export
#' @seealso  \code{\link[lattice]{xyplot}}
#' @examples \dontrun{
#' plot_lm_scatter(y ~ x | group, data = dat, cex = 0.8)
#' }
# See:  Bates, D. M. (2010). "lme4: Mixed-effects modeling with R." 58.
#______________________________________________________________________________
plot_lm_scatter <- function(formula, data, cex = 1.0, jitter = 0.5, ...) {

  # x_var <- gsub("\"", "", deparse(substitute(x)), fixed = TRUE)
  # y_var <- gsub("\"", "", deparse(substitute(y)), fixed = TRUE)
  # c_var <- gsub("\"", "", deparse(substitute(c)), fixed = TRUE)

  fe <- modlr::formula_expr(formula)
  y_var <- fe$response
  x_var <- fe$covariate
  c_var <- fe$condition

  #_______________________________________________________
  # Simple linear regression for reference lines

  # Linear model formula without conditioning variable
  lm_formula <- stats::as.formula(paste(y_var, "~", x_var))

  # Run linear regression to get slope and intercept
  # coefficients for value of the conditioning variable
  #   lm_coef[1,]  intercepts
  #   lm_coef[2,]  slopes
  lm_coef <- sapply(data[[c_var]], function(c_value) {
    sub <- paste(c_var, "== c_value")
    stats::coef(stats::lm(lm_formula,
                          data = subset(data, eval(parse(text = sub)))))
  })
  # print(lm_coef)
  # print(lm_coef[1, ])

  #_______________________________________________________

  # Create a new factor ordered by the intercept and
  # use it as the conditioning variable in the plot
  cond <- stats::reorder(data[[c_var]], lm_coef[1, ])[]

  # Define plot formula, including conditioning variable
  formula <- stats::as.formula(paste(y_var, "~", x_var, "|cond"))

  #_______________________________________________________
  # Plot parameters
  #
  # See 'textGrob' and 'gpar' for generic parameter values
  #
  # font : font face
  #     1 = plain
  #     2 = bold
  #     3 = italic
  #     4 = bold italic
  # just : justification of text relative to its (x, y) location
  #_______________________________________________________

  # Axis parameters
  x_par <- list(
      label = x_var           # label text
    , cex   = cex             # relative character size of label
  )
  y_par <- list(
      cex   = cex             # relative character size of label
  )
  axis_scales <- list(
      cex   = cex             # relative character size of scale numbers
  )

  # # Plot title parameters
  # title <- list(
  #     label = paste0("Scatterplots with reference lines\n",
  #                    "fit by simple linear regression")
  #   , cex   = cex             # relative char size of title
  #   , font  = 1               # font face
  # )

  # Subtitle placed at bottom of page
  # https://www.magesblog.com/post/2015-06-16-how-to-place-titles-in-lattice-plots/
  subtitle <- list(
        label = "Reference lines fit by simple linear regression"
      , cex   = (0.9 * cex)       # relative character size
      , font  = 1                 # font face
      # , x     = grid::unit(       # horizontal position
      #       x     = 0.05          #   value
      #     , units = "npc"         #   normalized parent coordinates
      #   )
      # , just  = "left"            # justification relative to location
  )

  # Parameter settings supplied to trellis.par.set()
  par_settings <- list(
      strip.background = list(
        col = "gray80"          #   color
      )
    , layout.widths = list(
        left.padding    = 1       #   adjust default left margin
      , right.padding   = 0       #   adjust default right margin
      )
    , layout.heights = list(
        top.padding     = 0       #   adjust default top margin
      , bottom.padding  = 1       #   adjust default bottom margin
      )
  )

  # Margin parameters 'oma' and 'mar' have little apparent impact, possibly
  # due to panel dimensions being largely dictated by the 'aspect' parameter.

  #_______________________________________________________
  # The lattice function xyplot() produces scatter plots.
  # 'formula' is of the form y ~ x|f, where x is on the X-axis,
  # y is on the Y-axis, and f is a factor that determines panels.
  # 'type' argument values:
  #   "p"       points
  #   "l"       lines
  #   "b","o"   both points and lines
  #   "h"       histogram-like vertical lines
  #   "s","S"   sorts values along one of the axes (depending on "horizontal")
  #   "r"       linear regression line
  #   "a"       average (can be useful for creating interaction plots)
  #   "smooth"  loess fit
  #   "spline"  cubic smoothing split fit
  #_______________________________________________________
  # lattice::trellis.object
  lattice::xyplot(
      x         = formula             # lattice plot formula
    , data      = data                # data frame
    , aspect    = "xy"                # 45 degree banking rule
    , xlab      = x_par               # x-axis parameters
    , ylab      = y_par               # y-axis parameters
    , scales    = axis_scales         # tick marks and labels of axes
    , type      = c("p", "r")         # points; linear regression line
    # , as.table  = TRUE                # panels left to right, top to bottom
    # , main      = title               # plot title
    , sub       = subtitle            # subtitle underneath x-axis
    , col.line  = "darkorange"        # line color
    , lwd       = 1                   # line width
    , grid      = TRUE                # add reference grid
    , jitter.x  = (jitter > 0)        # TRUE to jitter points to avoid overlap
    , factor    = jitter              # amount of jitter
    , par.strip.text = list(cex = cex)  # strip text character size
    , par.settings   = par_settings   # parameters for trellis.par.set()
    , ...                             # further arguments
  )
}

# # Alternative based on lme4 sleepstudy example
# lattice::xyplot(
#     x      = formula
#   , data   = data
#   , type   = c("g","p","r")
#   , index  = function(x,y) { stats::coef(stats::lm(y ~ x))[1] }
#   , xlab   = x_var
#   , ylab   = y_var
#   , aspect = "xy"
# )
# (fm1 <- lme4::lmer(cmdRatio ~ mode + (mode|PID), data))
# (fm2 <- lme4::lmer(cmdRatio ~ mode + (1|PID) + (0+mode|PID), data))

#______________________________________________________________________________
# 2017-10-13  Created.
# 2017-10-23  Added variable 'jitter'.
# 2017-10-27  Incorporated into 'plotr' package.
# 2017-10-29  Changed from 'formula' argument to 'x', 'y', and 'c' arguments
#   to clarify how the variables are used to generate the plot.  Generalized
#   to remove references to "PID", a variable name specific to the data
#   frames used when initially creating the function.
# 2017-10-30  Refined plot parameters.
# 2021-04-18  Formatting.
#______________________________________________________________________________
