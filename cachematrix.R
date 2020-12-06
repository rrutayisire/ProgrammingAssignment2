## 1. makeCacheMatrix function
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## 2. cacheSolve function

cacheSolve <- function(x, ...) {
                inv <- x$getInverse()
        if (!is.null(inv)) {
                message("Here is the cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

##Testing the two functions 
> my_matrix <- makeCacheMatrix(matrix(1:6, 3, 3))
> my_matrix$get()
> my_matrix$getInverse()
> cacheSolve(my_matrix)

