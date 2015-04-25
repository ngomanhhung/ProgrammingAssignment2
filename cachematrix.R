## Return Inverse of given Matrix
## 

## Cache input matrix.

makeCacheMatrix <- function(x = matrix()) {
    # initiallise inversed matrix.
    iv <- NULL
    
    # get and set original matrix
    set <- function(y) {
        # Do not set the same matrix again.
        if(!matrixEqual(x, y)){
            x <- y
            iv <<- NULL
        }
    }
    get <- function() x
    
    #get and set inversed matrix.
    setInverse <- function(inverse) iv <<- inverse
    getInverse <- function() iv
    
    #return construction list
    list( set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Inverse and return inversed matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    iv <- x$getInverse()
    
    #If inversed matrix was cached, return it.
    if (!is.null(iv)) {
        return (iv)
    }
    
    #get original set matrix
    data <- x$get()
    
    #actual work
    iv <- solve(data)
    
    #cache the inversed matrix.
    x$setInverse(iv)
    
    #return inversed matrix
    iv
}


#Compare two matrix,
#they are identical if both dimensions and values are equal.
matrixEqual <- function(m1, m2){
    is.matrix(m1) && is.matrix(m2) && dim(m1) == dim(m2) && all(m1 == m2)
}
