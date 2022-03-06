# Implementing PCA function

  # Function: run_pca, performs PCA on a data matrix
  # Input: X, must be a dataframe or matrix; scale, must be logical value
  # Output: List of elements
  # prin_comps - matrix with the same dimensions as X that contains the principal components vectors
  # loadings - matrix of variable loadings, should be a square matrix where the number of rows and columns are equal to the number of columns of X
  # portion_var - vector of variances explained by each principal component
  run_pca <- function(X, scale) {
    # Checks that X is a matrix
    if (!is.matrix(X)) {
      # Checks that X is a dataframe, if not a matrix
      if (!is.data.frame(X)) {
        stop("X must be a matrix or dataframe!")
      }
    }
    # Checks that scale is a logical value
    if (!is.logical(scale)) {
      stop("scale must be a logical value!")
    }
    
    # In the case that X is a dataframe, we will convert it to a matrix
    X <- as.matrix(X)
    # Checks if scale is true
    if(scale == TRUE) {
      # For each variable
      for (i in 1:ncol(X)) {
        # Calculate mean of the variable
        mean = mean(X[, i])
        # Calculate standard deviation of the variable
        sd = sd(X[, i])
        # For each observation
        for (j in 1:nrow(X)) {
          # Calculate the subtraction of the mean from the observation value and division of the standard deviation
          X[j, i] <- (X[j, i] - mean) / sd
        }
      }
      # Otherwise (scale is not true)
    } else {
      # For each variable
      for (i in 1:ncol(X)) {
        # Calculate mean of the variable
        mean = mean(X[, i])
        # For each observation
        for (j in 1:nrow(X)) {
          # Calculate the subtraction of the mean from the observation value
          X[j, i] <- (X[j, i] - mean)
        }
      }
    }
    
    # Using `cov` to generate the empirical covariance matrix of the data matrix X
    cov_mat <- cov(X)
    # Using eigen() to perform eigendecomposition of covariance matrix
    decomp_mat <- eigen(cov_mat)
    # Saving the vector of variances to a variable
    portion_var <- decomp_mat$values
    # Saving the matrix of principal component vectors to a variable
    loadings <- decomp_mat$vectors
    # Saving the matrix of loadings to a variable
    prin_comps <- X %*% loadings
    
    # Creating a list that contains the loadings matrix, principal components matrix and variances vector to be returned
    result <- list("loadings" = loadings,
                   "prin_comps" = prin_comps,
                   "portion_var" = portion_var)
    
    return(result)
  }