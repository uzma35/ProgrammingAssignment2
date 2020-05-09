## Writing two R functions that is able to cache the Inverse of a matrix

## creating a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

## inverse property
  i <- NULL
  
## set the matrix
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }

##get the matrix
  
  get <- function() 
    x    
  
##set the inverse of the matrix

    setInv <- function(inverse) {
    i <<- inverse
  }
  
##get the inverse of the matrix
  
  getInv <- function() {
    
    i
    
  }

  ## list of methods

    list(set = set, get = get,
       setInv = setInv, getInv = getInv)
  
}


## cacheSolve function computes the inverse of the special matrix returned by makeCacheMatrix above.
## If the inverse has already been calculated, then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      
## Return a matrix that is the inverse of 'x'
  
    m <- x$getInv()
## Return the inverse if its already set
    
    if(!is.null(m)) {
      message ("getting the cached data")
      return(m)
      
    }
    
##get the matrix     
    
    data <- x$get()
    
##calculate the inverse
    
    m <- solve(data) %% data
    
##set the inverse
    
      x$setInv(m)

##return the matrix    
    m
    
  }
  
B1 <- makeCacheMatrix(B)
cacheSolve(B1)

