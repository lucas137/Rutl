#______________________________________________________________________________
#' Scatterplot Matrix
#'
#' Creates a scatter plot matrix.
#'
#' @param   x           a formula describing the variables to plotted.
#' @param   data        a data frame.
#' @param   cor.method  correlation method.
#' @param   bold.limit  coefficients >= this threshold are show in bold text.
#' @param   pch         plot point symbol (default is a circle).
#' @param   cex.axis    relative character size of axis labels.
#' @param   cex.cor     relative character size of correlation coefficient text.
#' @param   cex.labels  relative character size of ... .
#' @param   cex.main    relative character size of plot title.
#' @param   ...         more parameters passed to ... .
#' @return  Returns results of the correlation tests.
#'
#' @author  Nathan Lucas
#' @export
#______________________________________________________________________________
scattermat <- function(x, data, cor.method = "pearson",
                       bold.limit = 0.7, pch = 1,
                       cex.axis = 1.0, cex.cor = 1.0,
                       cex.labels = 1.0, cex.main = 1.0, ...) {

  # Vectors to be return within data frame
  method_      <- c()
  coefficient_ <- c()
  statistic_   <- c()
  df_          <- c()
  pvalue_      <- c()

  # Perform correlation test and record results
  cor_test <- function(x, y, method, digits) {
    result <- stats::cor.test(x, y, method = method, exact = FALSE)

    # Operator `<<-` assigns value to variable in parent environment
    method_      <<- c(method_, method)
    coefficient_ <<- c(coefficient_, result$estimate)
    statistic_   <<- c(statistic_, result$statistic)

    if (is.null(t_df <- result$parameter)) {
      t_df <- NA
    }
    df_     <<- c(df_, t_df)
    pvalue_ <<- c(pvalue_, result$p.value)

    format(result$estimate, digits = digits, nsmall = digits)  # return
  }

  # Upper panel containing correlation coefficient(s)
  panel.cor <- function(x, y, digits = 2, ...) {
    horizontal <- (graphics::par("usr")[1] + graphics::par("usr")[2]) / 2
    vertical   <- (graphics::par("usr")[3] + graphics::par("usr")[4]) / 2

    # Suppress warnings that standard deviation is zero
    prevWarn <- getOption("warn")
    options(warn = -1); on.exit(options(warn = prevWarn))

    # Pearson correlation
    if (!is.na(match("pearson", cor.method))) {
      r <- cor_test(x, y, "pearson", digits)
      rFont <- 1
      if (r != "NA") {
        if (abs(as.numeric(r)) >= bold.limit) {
          rFont <- 2
        }
      }
    }
    # Spearman correlation
    if (!is.na(match("spearman", cor.method))) {
      s <- cor_test(x, y, "spearman", digits)
      sFont <- 1
      if (s != "NA") {
        if (abs(as.numeric(s)) >= bold.limit) {
          sFont <- 2
        }
      }
    }

    # Print coefficient(s) in panel
    if (exists("r")) {
      coeff <- if (exists("s")) {
        paste0(r, "\n")
      } else {
        paste0(r)
      }
      graphics::text(horizontal, vertical, coeff, cex = cex.cor, font = rFont)
    }
    if (exists("s")) {
      coeff <- if (exists("s")) {
        paste0("\n", s)
      } else {
        paste0(s)
      }
      graphics::text(horizontal, vertical, coeff, cex = cex.cor, font = sFont)
    }
  }

  # Diagonal panel containing density plot
  panel.density <- function(x, ...) {
    usr <- graphics::par("usr"); on.exit(graphics::par(usr))
    d   <- stats::density(x)
    graphics::par("usr" = c(usr[1:2], 0, max(d$y * 1.5)))
    graphics::lines(d)
  }

  panel_smooth <- graphics::panel.smooth

  # Create scatterplot matrix with custom diagonal and upper panels
  graphics::pairs(x, data = data, ...
      , pch         = pch             # plot point symbol
      , diag.panel  = panel.density   # function applied on diagonals
      , lower.panel = panel_smooth    # function used below the diagonals
      , upper.panel = panel.cor       # function used above the diagonals
      , cex.axis    = cex.axis        # size of axis text
      , cex.labels  = cex.labels      # size of text panel text
      , cex.main    = cex.main        # size of main title text
  )

  # Return results of correlation tests
  dat <- data.frame(method_, coefficient_, statistic_, df_, pvalue_)
  names(dat) <- c("method", "coefficient", "statistic", "df", "p.value")
  dat
}

#______________________________________________________________________________
# 2017-04-30  Created.
# 2017-10-27  Incorporated into 'plotr' package.
# 2021-04-18  Formatting.
#______________________________________________________________________________
