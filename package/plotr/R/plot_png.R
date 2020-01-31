#==============================================================================
#' Save Plot to PNG
#'
#' Saves a plot as a PNG file and optionally prints the plot.
#'
#' @param   x     a plot to save.
#' @param   file  name of output file without extension, up to 507 characters.
#' @param   prt   \code{TRUE} to also print plot.
#' @param   ...   more parameters passed to \code{png()}.
#' @return  Returns the file name of the output PNG image.
#'
#' @author  Nathan Lucas
#' @export
#' @examples
#' dir.create("plot", showWarnings = FALSE, recursive = TRUE)
#' plot_png(plot(sin, -pi, pi), file = "plot/sine")
#------------------------------------------------------------------------------
plot_png <- function(x, file, prt = TRUE, ...)
{
  if (is.null(file) || (file == ""))
  {
    stop("'file' must contain a file path.")
  }
  filename <- paste(file, ".png", sep = "")
  if (prt)
  {
    utils::capture.output(x)        # print plot
    grDevices::dev.copy(            # copy to file
        device   = grDevices::png
      , filename = filename
      , type     = "cairo"
      , ...
    )
  }
  else
  {
    grDevices::png(             # graphics device
        filename = filename
      , type     = "cairo"
      , ...
    )
    utils::capture.output(x)    # copy plot to file
  }
  grDevices::dev.off()    # shut down the current device
  filename                # return file name
}

#------------------------------------------------------------------------------
# Changelog
# 2017-10-23  Created.
# 2017-10-30  Simplified/generalized arguments for grDevices::png().
#==============================================================================
