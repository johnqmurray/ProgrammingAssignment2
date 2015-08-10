## Lexical scoping programming questions
## John Q Murray 2015-08-09

## makeVector example - demonstrates how to cache mean of a vector

makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
}

## Similar get, set code as makeVector sample - use matrix and solve rather than vector and mean

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Adapt cachemean for matrix (rather than vector) and matrix inverse (rather than mean)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
}
## myMatrix <- matrix(rnorm(9,mean=5), nrow=3, ncol=3)  matrix must be square
## det(myMatrix)  make sure determinant is non-zero, cannot invert singular matrix
## myInverse <- solve(myMatrix)   check it in advance
## myCachedMatrix <- makeCacheMatrix(myMatrix)
## myCachedInverse <- cacheSolve(myCachedMatrix)
