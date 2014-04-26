## HW2 assigment, implementation of cache inversion matrix calculation

## Prepare a special object to be used by cacheSolve
## Function return a list of 4 function: set, get, setinverse , getinverse

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inver <<- inverse
  getinverse <- function() inver
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


##Function get a object build by makeCacheMatrix 
##returned cached version of inversed matrix

cacheSolve <- function(x, ...) {
  inver <- x$getinverse()
  if(!is.null(inver)) {
    message("getting inversed matrix from cache")
    return(inver)
  }
  matrix <- x$get()
  inver <- solve(matrix, ...)
  x$setinverse(inver)
  inver
}
