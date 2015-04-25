## Return Inverse of given Matrix
## 

## Cache input matrix.

makeCacheMatrix <- function(x = matrix()) {
  iv <- NULL
  set <- function(y) {
    x <- y
    iv <<- NULL
  }
  get <- function() x
  
  setinverse <- function(inverse) iv <<- inverse
  getinverse <- function() iv
  
  list( set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Inverse and return inversed matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  iv <- x$getinverse()
  if (!is.null(iv)) {
    return (iv)
  }
  
  data <- x$get()
  iv <- solve(data)  
  x$setinverse(iv)
  iv
}

