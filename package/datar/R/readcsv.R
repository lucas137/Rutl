#______________________________________________________________________________
#' Data Input from CSV
#'
#' Import data frame from CSV file with header labels.
#'
#' @param    file     name of the file from which the data are to be read
#'   (e.g., \code{"csv/mydata.csv"}).
#' @param    factors  range of columns that are factors
#'   (e.g., \code{2:4}, \code{c(1, 3:4)}, etc.).
#' @param    ...      further arguments passed to \code{utils::read.csv}.
#' @return   A \code{data.frame}.
#'
#' @author   Nathan Lucas
#' @export
#' @examples \dontrun{
#' # Columns 1, 3, and 4 are factors
#' dat <- datar::read_csv(file = "mydata", factors = c(1, 3:4))
#' }
#______________________________________________________________________________
read_csv <- function(file, factors = NULL, ...) {
  # Import data from CSV file with header labels
  data <- utils::read.csv(file, ...)

  if (!is.null(factors)) {
    # Encode columns as factors
    for (i in factors) {
      data[, i] <- factor(data[, i])
    }
  }
  data  # return
}

#______________________________________________________________________________
# Changelog
# 2017-04-28  Created.
# 2017-10-25  Incorporated into 'datar' package.
# 2021-04-18  Formatting.
#______________________________________________________________________________
