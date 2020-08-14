
## Creates list that stores the information in the matrix and its inverse
## along with functions that will also change the information and the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Caches the inverse. parameter = list from "makeCacheMatrix"

cacheinverse <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)){
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
