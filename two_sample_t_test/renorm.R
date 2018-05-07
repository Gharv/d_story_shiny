
#' renorm function
#'
#' This function returns a vector of numbers with the exact mean and sd requested.
#' @param numeric input requested observations, mean, and standard deviation.
#' @keywords exact random normal distribution
#' @export
#' @examples 
#' renorm(n, x_bar = 0, sdv = 1)

renorm <- function(n, x_bar = 0, sdv = 1) 
{
  # Validate arguments.
  if (! is.numeric(n)) {message('Argument n is not a number!'); return(NA)}
  if (! is.numeric(sdv)) {message('Argument sdv is not a number!'); return(NA)}
  if (! is.numeric(x_bar)) {message('Argument x_bar is not a number!'); return(NA)}
  
  # Create vector with the requested mean + standard deviation.  Because
  # the numbers returned are a sample, the mean and sd do not equal what
  # was requested -- hence, the need for this function.
  vctr <- rnorm(n = n, mean = x_bar, sd = sdv )

  x_bar_obs <- mean(vctr)
  sd_obs    <- sd(vctr)

  # Calculate a standard scroe for each observation and then recalculate 
  # observations using the requested standard deviation and mean.
  vctr     <- (vctr - x_bar_obs) / sd_obs
  vctr     <- sdv * vctr + x_bar

  return(vctr)
}


