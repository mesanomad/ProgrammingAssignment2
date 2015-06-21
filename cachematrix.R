## This script solves for the inverse of a matrix, stores the matrix inversion in the
## cached environment, and then is able to return it when it is called for again.

## makeCacheMatrix is a function that stores 4 functions: set, get, setmatr, getmatr

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

## cacheSolve verifies if the value of m exists and is not NULL.  If so, it returns the
## value m with the message "getting cached data."  If not, it will calculate the 
## inverted matrix.

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


## Return a matrix that is the inverse of 'x'

## > x = rbind(c(3,5), c(-7,2))
## > m = makeCacheMatrix(x)
## > m$get()
##            [,1] [,2]
##      [1,]    3    5
##      [2,]   -7    2

## > cacheSolve(m)
##              [,1]        [,2]
##      [1,] 0.04878049 -0.12195122
##      [2,] 0.17073171  0.07317073

## > cacheSolve(m)
## getting cached data.
##              [,1]        [,2]
##      [1,] 0.04878049 -0.12195122
##      [2,] 0.17073171  0.07317073
