# Put comments here that give an overall description of what your
# functions do

# makeCacheMatrix is a function that stores 4 functions: set, get, setmatr, getmatr

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatr <- function(matr) m <<- matr
        getmatr <- function() m
        list(set = set, get = get,
             setmatr = setmatr,
             getmatr = getmatr)
}

# cacheSolve verifies if the value of m exists and is not NULL

cacheSolve <- function(x, ...) {
        m <- x$getmatr()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
                
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatr(m)
        m
}


#### Sample run:
## > x = rbind(c(1, -1/4), c(-1/4, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00

## No cache in the first run
## > cacheSolve(m)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data.
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
