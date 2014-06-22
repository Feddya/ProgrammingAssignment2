## A set of functions creating and handling caching of inverse 
## matrix of the given one

## Creates an object, which contains matrix data itself, its cached
## inverse matrix and accessors (get/set data, get/set inverse matrix)

makeCacheMatrix <- function(x = matrix()) 
{
  ## initializing cached inverse matrix by null
  cachedInv <- NULL
  
  ## defining set function
  set <- function(y) 
  {
    ## saving data itself
    x <<- y
    ## resetting cached inverse matrix
    cachedInv <<- NULL
  }
  
  ## defining get function - just returns data
  get <- function() x
  
  ## defining setinv function - saves inverse matrix
  setinv <- function(inv) cachedInv <<- inv
  
  ## defining getinv function - returns saved inverse matrix
  getinv <- function() cachedInv
  
  ## generating list from those functions and returning it
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## calculates inverse matrix for x
## assumes x is always invertible, so no any sanity checks here

cacheSolve <- function(x) 
{
  ## getting cached results
  cachedInv <- x$getinv()
  
  ## if it is not null - can just return it
  if(!is.null(cachedInv)) 
  {
    ## printing a message about cached data usage and returning it
    message("getting cached data")
    return(cachedInv)
  }
  
  ## if cached inverse matrix is null - need to calculate it
  
  # getting data
  data <- x$get()
  
  ## calculating inverse matrix (solve(a) without b assumes b is identity 
  ## and returns inverse matrix for a)
  cachedInv <- solve(data)
  
  ## saving calculated inverse matrix to cache
  x$setinv(cachedInv)
  
  ## and returning it
  cachedInv  
}
