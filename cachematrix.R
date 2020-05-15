## In this script are two functions. The firstone create a list of four, four function
## The secondone, return a matrix that is the inverse of 'x'

## This function allows:
## [[1]] set = set the value of a matrix y
## [[2]] get = return the value of a matrix contanined in makeCacheMatrix[[1]]
## [[3]] setInv = recieve a value and theInv variable gets this value
## [[4]] getInv = return the value of the variable theInv

makeCacheMatrix <- function(x = matrix()) {
  theInv <- NULL                    #at first, the variable theInv is NULL
  set <- function(y) {
    x <<- y
    theInv <<- NULL
  }
  get <- function() x
  setInv <- function(solve) theInv <<- solve
  getInv <- function() theInv
  list(set = set, get = get, 
       setInv = setInv,
       getInv = getInv)
}


## This function take the matrix x, return its inverse looking first if it's in x$getInv
## and 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  
  theInv <- x$getInv()
  if(!is.null(theInv)) 
  {
    message("getting cached data")
    return(theInv)
  }
  data <- x$get()
  theInv <- solve(data, ...)
  x$setInv(theInv)
  theInv
  
}