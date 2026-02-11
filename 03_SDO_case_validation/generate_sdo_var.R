###############################################################
## Info
## SDO, 19 Oct 2025
## Core implementation for creating SDO (Second-Dimension Outlier) variables
## Used for generating spatial outlier features in the SDO modeling framework
###############################################################

# Function
generate_sdo_var <- function(pointlocation, gridlocation, gridvar,
                   distbuf = seq(2, 10, 1), sd = 2) {

  samples <- as.matrix(pointlocation)
  grids <- as.matrix(gridlocation)

  nbuf <- length(distbuf)

  # positive and negative outliers
  p_xx <- data.frame(matrix(NA, nrow(samples), nbuf))
  buf_names <- paste0("p_b", distbuf)
  names(p_xx) <- buf_names
  n_xx <- data.frame(matrix(NA, nrow(samples), nbuf))
  buf_names <- paste0("n_b", distbuf)
  names(n_xx) <- buf_names
  
  f_zscore <- function(x) (x - mean(x))/sd(x)
  f_positive_sum_localoutlier <- function(x) {
    z <- f_zscore(x)
    m <- sum(z[z > sd]) ## z or x: z is better as it shows outlier degree
    return(m)
  }
  f_negative_sum_localoutlier <- function(x) {
    z <- f_zscore(x)
    m <- sum(z[z < -sd])
    return(m)
  }

  for (i in 1:nrow(samples)) {
    sample_row <- matrix(samples[i, ], nrow = nrow(grids), ncol = ncol(grids), byrow = TRUE)
    dif_grid_sample <- grids - sample_row
    distances <- sqrt(rowSums((dif_grid_sample)^2))
    h <- lapply(distbuf, function(x) which(distances < x))
    
    pxxi <- c()
    nxxi <- c()

    for (t in 1:nbuf) {
      gridt <- gridvar[h[[t]]]
      pxxi[t] <- f_positive_sum_localoutlier(gridt)
      nxxi[t] <- f_negative_sum_localoutlier(gridt)
    }
    p_xx[i, ] = as.vector(t(pxxi))
    n_xx[i, ] = as.vector(t(nxxi))
  }

  out <- cbind(p_xx, n_xx)

  return(out)
}
