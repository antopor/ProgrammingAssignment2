## Construct special structure to store original matrix and cached inverse
## along with functions to obtain that data
makeCacheMatrix <- function(x = matrix()) 
{
  inv_m = NULL
  get <- function() x
  set <- function(y)
  {
    x <<- y
    inv_m <<- NULL
  }
  getInv <- function() inv_m
  setInv <- function(m)
  {
    inv_m <<- m
  }
  list(get = get, set = set, getInv = getInv, setInv = setInv)
}


## Cached version of function solve
## Computes inverse of matrix or return already computed value of inverse
## that was stored for cache purposes
cacheSolve <- function(x, ...) {
  inv_m <- x$getInv()
  if(!is.null(inv_m))
    return(inv_m)
  inv_m <- solve(x$get())
  x$setInv(inv_m)
  inv_m
}
