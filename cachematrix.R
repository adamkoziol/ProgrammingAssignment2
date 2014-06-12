# These two functions work together to calculate and subsequently cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # Initialises the inverse variable as null
  inverse <- NULL
  # Sets and caches the value of the matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  # Returns the value of the matrix
  get <- function() x
  # Caches the inverse of the matrix calculated in the cacheSolve() function
  calcInverse <- function(inverted) inverse <<- inverted
  # Retrieves the value of inverse
  getInverse <- function() inverse
  # Constructs a list, which allows the calling of functions
  list(set = set, get = get, calcInverse = calcInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  # Calls the getInverse() function from makeCacheMatrix()
  inverse <- x$getInverse()
  # As inverse was initialised as NULL in makeCacheMatrix(), if it is not null, then there must be a cached value for it 
  if(!is.null(inverse)) {
    # Message to indicate that it is the cached version of inverse being used
    message("Getting cached data")
    # Return inverse
    return(inverse)
  }
  # If there is no cached value for inverse, use the get() function from makeCacheMatrix() to assign the matrix values to data
  data <- x$get()
  # Use the solve() function to calculate the inverse of the matrix
  message("Calculating the inverse of the matrix")
  inverse <- solve(data, ...)
  # Call the calcInverse() function to pass the inverted matrix to makeCacheMatrix()
  x$calcInverse(inverse)
  # Print the inverted matrix
  inverse
}


## Not that this is part of the peer review requirements, but if you want to test if these functions are running properly, here is a general workflow
# this is based on the helpful post by Adam Gruer on the forums. Please follow the URL below if interested
# https://class.coursera.org/rprog-004/forum/thread?thread_id=153#post-913
# 
# 1. call makeCacheMatrix() and assign the return value to a variable
# q <- makeCacheMatrix()
# 
# 2. Create a test matrix to be processed by the functions - I make the matricies thusly:
# This creates a vector of length 25 taken from randomly chosen (without replacement) numbers between 1 and 40
# x <- sample(1:40, 25, replace=F)
# This gives dimensions to x, turning it into a 5 x 5 matrix
# dim(x) <- c(5, 5)
# 
# 3. use the set() function assigned to q in the previous step to create a matrix
# q$set(x)
# 
# 4. use q's get() function to return the created matrix
# q$get
# 
# 5. pass the newly created matrix to the cacheSolve() function - the inverse of the matrix will be returned
# cacheSolve(q)
# 
# 6. pass the matrix to the cacheSolve() function again. Since the inverse matrix has previously been calculated and cached, the cached value
# will be returned, and the message "Getting cached data" wiil be printed
# cacheSolve(q)
# 
# 8. Create a new test matrix
# x1 <- sample(1:40, 16, replace=F)
# This gives dimensions to x1, turning it into a 4 x 4 matrix
# dim(x1) <- c(4, 4)
# 
# 9. use q's set() function to create a new matrix
# q$set(x1)
# 
# 10. pass q to the cacheSolve() function to calculate the inverse of q matrix
# cacheSolve(q)
# 
# 11. pass q to the cacheSolve() function to retrieve the cached inverse value
# cacheSolve(q)

