
## functions do

## Write a short comment describing this function

## The function creates a list of functions to
## 1. get the matrix namely get()
## 2. Store the inverse of a matrix and an original
##    copy in cache namely setInv()
## 3. get the Original value of the matrix stored 
##    earlier that was Inversed namely getOrig()
## 4. get the inverse of a matrix namely get()
makeCacheMatrix <- function(x = matrix())
{
  Inv <- NULL
  
  get <- function() 
## 1. get()
  {
    x
  }
  setInv <- function(Inverted,Original)
## 2. setInv()
  {
    Inv <<- Inverted
    Orig<<- Original
  }
## 3. getOrig()
  getOrig <- function() 
  {
    Orig
  }
## 4. getInv()
  getInv <- function()
  {
    Inv
  }
  list(set = set, get = get,setInv = setInv,getInv = getInv)
}


##  The function computes the inverse of the special "matrix" 
##  returned by makeCacheMatrix above. If the inverse has already
##  been calculated (and the matrix has not changed), then the
##  cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) 
{
  Inv <- x$getInv()
  Orig <-x$getOrig()
  if(!is.null(Inv) && identical(x,Orig))
## If Inv is NULL, it is the first execution and hence no data in cache.
## Second condition will not be checked in this case.
## If Inv has already been calculated, it is checked whether
## matrix is still the same or has been changed
  {
    message("getting cached data")
    return(Inv)
  }
  data <- x$get()
  Inv <- solve(data, ...)
  x$setInv(Inv,data)
## Caches the inverse and the original matrix
  Inv
## Returns the inverse
}
