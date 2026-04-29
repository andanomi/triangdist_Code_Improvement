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


#' @title Distribution function for the Triangular Distribution
#' @description Cumulative distribution function (CDF).
#' @param q Vector of quantiles.
#' @param min Lower limit (a).
#' @param max Upper limit (b).
#' @param mode Mode (c).
#' @return A numeric vector of cumulative probabilities.
#' @export
ptriang <- function(q, min, max, mode) {
  if (any(min > max)) stop("min must be less than or equal to max")
  if (any(mode < min | mode > max)) stop("mode must be within [min, max]")

  n <- max(length(q), length(min), length(max), length(mode))
  q <- rep_len(q, n)
  min <- rep_len(min, n)
  max <- rep_len(max, n)
  mode <- rep_len(mode, n)

  p <- numeric(n)

  p[q <= min] <- 0

  idx1 <- q > min & q <= mode
  p[idx1] <- (q[idx1] - min[idx1])^2 / ((max[idx1] - min[idx1]) * (mode[idx1] - min[idx1]))

  idx2 <- q > mode & q < max
  p[idx2] <- 1 - (max[idx2] - q[idx2])^2 / ((max[idx2] - min[idx2]) * (max[idx2] - mode[idx2]))

  p[q >= max] <- 1

  return(p)
}


#' @title Quantile function for the Triangular Distribution
#' @description Returns the quantile for a given probability p.
#' @param p Vector of probabilities.
#' @param min Lower limit (a).
#' @param max Upper limit (b).
#' @param mode Mode (c).
#' @return A numeric vector of quantiles.
#' @export
qtriang <- function(p, min, max, mode) {
  if (any(p < 0 | p > 1)) stop("p must be between 0 and 1")
  if (any(min > max)) stop("min must be less than or equal to max")

  n <- max(length(p), length(min), length(max), length(mode))
  p <- rep_len(p, n)
  min <- rep_len(min, n)
  max <- rep_len(max, n)
  mode <- rep_len(mode, n)

  p_mode <- (mode - min) / (max - min)
  res <- numeric(n)

  idx1 <- p < p_mode
  res[idx1] <- min[idx1] + sqrt(p[idx1] * (max[idx1] - min[idx1]) * (mode[idx1] - min[idx1]))

  idx2 <- p >= p_mode
  res[idx2] <- max[idx2] - sqrt((1 - p[idx2]) * (max[idx2] - min[idx2]) * (max[idx2] - mode[idx2]))

  return(res)
}
