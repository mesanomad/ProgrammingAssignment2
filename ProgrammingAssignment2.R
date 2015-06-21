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