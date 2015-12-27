## @Assignment 2: Lexical Scoping
## This R source file contains two functions makeCacheMatrix & cacheSolve
## The function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse
## The second function "cacheSolve" computes the inverse of the "special matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache
## Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.
## Assumption: The matrix supplied is always invertible. 




## This function initializes with input matrix "x"
## 
makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize a inv setting its value to null
  inv <- NULL
  
  ## Set the value of the matrix with function(y)
  set <- function(y) {
    
    ## Caches the input matrix so that the cacheSolve can validate whether the matrix has been changed or not 
    x <<- y
    ## Set the value of inv to NULL				
    inv <<- NULL
  }
  ## In this stage the matrix is assigned to get
  
  get <- function() x
  
  ## Set the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  ## Get the inverse of the matrix if already computed       
  getInverse <- function() inv
  ## Create a list to accommodate the value of four functions which is used as an input to the function cacheSolve below
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special matrix returned by makeCacheMatrix as indicated in question 
## Initialize the function cacheSolve with function input of x

cacheSolve <- function(x, ...) {
  ## x is the output of makeCacheMatrix function above
  ## The inverse of the original matrix input to the makeCacheMatrix is returned 
  inv <- x$getInverse()
  
  ## Check if the inverse has already being calculated which is in inv variable. 		 
  if (!is.null(inv)) {
    ## If it is already computed, then get data from cache by skipping the further complex computation and returning the inv matrix		
    ## Display the message that the data is being returned from cache
     message("Getting data from cache")
    ## Retrun the inv matrix from cache
    return(inv)
  }
  ## Start the matrix computation by fetching the matrix value to variable mat		
  mat <- x$get()
  ## Set the solve of mat to variable inv		
  inv <- solve(mat, ...)
  ## Set the value of the inverse in the cache using the seInverse function		
  x$setInverse(inv)
  ## Return the inverse of matrix stored in variable "inv"		
  return(inv)
}