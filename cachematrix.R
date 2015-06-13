## Implement a special matrix that can 'remember' its own inverse
## to save computing time. The inverse is automatically deleted when
## the matrix is assigned a new value. Also implement a function 
## that returnes the inverse, returning the cached value if available
## and calculating & caching the result if not.

## Usage:
## M <- matrix(c(1,0,0,1), 2,2)
##
## cacheM <- makeCacheMatrix( M )
##
## or
## cacheM <- makeCacheMatrix()
## cacheM$set(M)
##
## cacheM$get() ## returns M
## 
## cacheSolve(M) ## returns M^-1
##


## Special kind of matrix that can cache its own inverse.
## Implemented as a list of functions.

## Arguments: x: (invertable) matrix, default: 0*0 matrix.
## Returns: List of functions to get/set the matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  ## placeholder for inverse matrix
  inverse <- NULL
  
  ## Set a new matrix, forget the old inverse.
  set <- function(y)
  {
    x <<- y
    inverse <<- NULL
  }
  
  ## Get the matrix
  get <- function() 
  {
    x
  }
  
  ## Cache the inverse
  setinverse <- function(inv) 
  {
    inverse <<- inv 
  }
  
  ## Get the cached inverse
  getinverse <- function() 
  {
    inverse
  }
  
  ## Return the list of functions that we just defined
  ## so they can be used elsewhere.
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}




## Invert a given matrix, using cached inverse if it exists.

## Arguments: x: cacheMatrix
## Returns: inverse of matrix
cacheSolve <- function(x, ...) {
  
  ## first, check if a cached inverse exists or not.
  if(is.null(x$getinverse()))
  {
    ## No cached value exists, must calculate & set inverse.

    ## get the matrix
    matrix <- x$get()
    
    ## invert it
    inverse <- solve(matrix, ...)
    
    ## cache inverse matrix    
    x$setinverse( inverse )
  }  
  else
  {
    message("Using cached inverse")
  }
  
  ## return inverse matrix
  x$getinverse()
  
}
