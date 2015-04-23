# Functions makeCaheMatrix and cacheSolve compute inverse matrix for a given matrix and store its value into cache.
# If inversed matrix has already been computed for given input, cached value is returned.

# makeCacheMatrix creates special matrix including functions for :
# - returning original matrix
# - caching inversed matrix 
# - returning inversed matrix
# Input: matrix or vector (e.g.: 1:4 or rnorm(9)).
# Store the result to a variable and then use it as an input variable for the cacheSolve function.

makeCacheMatrix <- function(x = vector()) {  # Parameter: Matrix values
  cim <- NULL                                # Variable inicialization
  if(!is.matrix(x)){                         # If the input parameter is not matrix convert it to metrix
    matrix <- as.matrix(x)                   # Converting to matrix
    y <- sqrt(length(x))                     # Getting dimensions for matrix
    dim(matrix) <- c(y,y)                    # Setting dimensions
  }
  else {                                     # Input parameter is matrix
    matrix <- x
  }
  get <- function()                          # Creating function for returning original matrix
    matrix
  
  set_cim <- function(inversed_matrix)       # Creating function for setting inversed matrix
    cim <<- inversed_matrix
  
  get_cim <- function()                      # Creating function for returning inversed matrix
    cim
  
  list(                                      # Making list with created functions
    get = get, 
    set_cim = set_cim, 
    get_cim = get_cim) 
}

# cacheSolve returns an iverted matrix and stores its value to cache. Use the variable from makeCacheMatrix function as an input parameter.
# Next time you call this function for the same matrix, cached value is returned.

cacheSolve <- function(x, ...) {             # Parameter: Variable containing matrix values
  cim <- x$get_cim()                         # Assigning inversed matrix to a variable
  if(!is.null(cim)) {                        # Testing if inversed matrix has been calculated
    message("getting cached data")
    return(cim)                              # Returning already calculated inversed matrix
  }
  data <- x$get()                            # Getting matrix
  inversed_matrix <- solve(data, ...)        # Calculating inversed matrix
  x$set_cim(inversed_matrix)                 # Setting inversed matrix to cache
  inversed_matrix                            # Returning inversed matrix
}

