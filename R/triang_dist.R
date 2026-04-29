#' @title Density function for the Triangular Distribution
#' @description Calculates the density at x for a triangular distribution with
#' parameters min, max, and mode.
#' @param x Vector of quantiles.
#' @param min Lower limit of the distribution (a).
#' @param max Upper limit of the distribution (b).
#' @param mode Mode of the distribution (c).
#' @return A numeric vector of densities.
#' @export
dtriang <- function(x, min, max, mode) {
  if (any(min > max)) stop("min must be less than or equal to max")
  if (any(mode < min | mode > max)) stop("mode must be within [min, max]")

  n <- max(length(x), length(min), length(max), length(mode))
  x <- rep_len(x, n)
  min <- rep_len(min, n)
  max <- rep_len(max, n)
  mode <- rep_len(mode, n)

  res <- numeric(n)

  idx1 <- x >= min & x < mode
  res[idx1] <- (2 * (x[idx1] - min[idx1])) / ((max[idx1] - min[idx1]) * (mode[idx1] - min[idx1]))

  idx2 <- x == mode
  res[idx2] <- 2 / (max[idx2] - min[idx2])

  idx3 <- x > mode & x <= max
  res[idx3] <- (2 * (max[idx3] - x[idx3])) / ((max[idx3] - min[idx3]) * (max[idx3] - mode[idx3]))

  return(res)
}
