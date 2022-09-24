#______________________________________________________________________________
#' Save Trellis Plot to PNG
#'
#' Saves one or more trellis plots as PNG files,
#' and optionally prints the plot(s).
#'
#' @param   x     a \code{trellis} object or list of \code{trellis} objects
#'                (see \code{trellis.object} from the \pkg{lattice} package).
#' @param   file  name of output file without extension, up to 500 characters.
#' @param   prt   \code{TRUE} to also print plot.
#' @param   ...   more parameters passed to \code{png()}.
#' @return  Returns the file name of the output PNG image.
#'
#' @author  Nathan Lucas
#' @export
#' @examples
#' # directory for PNG files
#' dir.create("plot", showWarnings = FALSE, recursive = TRUE)
#'
#' # trellis plots
#' tpl <- lattice::xyplot(sin(1:100) ~ cos(1:100), type = "l")
#' tpp <- lattice::xyplot(sin(1:100) ~ cos(1:100))
#'
#' trellis_png(tpl, file = "plot/sincos", width = 7, height = 7)
#' trellis_png(list(tpl, tpp), file = "plot/sincos", width = 7, height = 7)
#______________________________________________________________________________
trellis_png <- function(x, file, prt = TRUE, ...) {

  if (!requireNamespace("lattice", quietly = TRUE)) {
    stop("Package 'lattice' needed for this function to work.")
  }

  # If "x" is a single "trellis" class object, add its value to the "pl" list;
  # otherwise assume "x" is list of "trellis" objects and assign it to "pl";
  # (see "trellis.object" from the "lattice" package)
  #pl <- if (is(x, "trellis")) { list(x) } else { x }
  pl <- if (inherits(x, "trellis")) {
    list(x)
  } else {
    x
  }

  if (is.null(file) || (file == "")) {
    stop("'file' must contain a file path.")
  }
  if (length(pl) > 1) {
    file <- paste0(file, "%02d.png")
  }
  file <- paste0(file, ".png")

  # Note: `seq_len(length(X))` is equivalent to `1:length(X)`

  # Confirm all elements in "pl" are "trellis" class objects
  lapply(seq_len(length(pl)), function(i) {
    #if (!is(pl[[i]], "trellis"))
    if (!inherits(pl[[i]], "trellis")) {
      stop("'x' must contain 'trellis' objects.")
    }
  })

  # "pl" is a list of "trellis" objects

  if (prt) {
    # Print the plot from each element in "pl"
    utils::capture.output(lapply(seq_len(length(pl)), function(i) { pl[[i]] } ))
  }
  # Copy the plot from each element in "pl" to a PNG file;
  # this will result in 1 file created or overwritten for each element
  grDevices::png(filename = file, type = "cairo", ...)
  utils::capture.output(
    lapply(seq_len(length(pl)), function(i) { pl[[i]] })
  )

  grDevices::dev.off()    # shut down the current device
  file                    # return file name
}

# # Alternative if 'x' is a single 'trellis' object
# if (prt) {
#   utils::capture.output(x)   # print plot
# }
# file <- paste(file, ".png", sep = "")
# grDevices::png(file, units = "in", res = 300, type = "cairo", ...)
# utils::capture.output(x)   # copy plot to PNG file

#______________________________________________________________________________
# Changelog
# 2017-10-23  Created.
# 2017-10-30  Simplified/generalized arguments for grDevices::png().
# 2021-04-18  Formatting.
#______________________________________________________________________________
